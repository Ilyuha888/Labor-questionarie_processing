library(tidyverse)
library(psych)
library(lavaan)
library(semPlot)
library(GPArotation)
library(data.table)
library(lavaanPlot)

anket <- read_csv('anket_clean.csv')

anket %>% #Распределение по полу и возрасту возрасту
  group_by(d1) %>% 
  summarise(n = n(),
            mean = mean(d2),
            median = median(d2),
            var = var(d2),
            sd = sd(d2),
            min = min(d2),
            max = max(d2))

#Посмотрим нормальность

skewness <- datawizard::skewness(anket)
kurtosis <- datawizard::kurtosis(anket)

View(skewness)
View(kurtosis)

#Удалим те, где ассиметрия и эксцесс по модулю выходят за 2. 
#Эти вопросы были сильно смещены вправо, люди часто отвечали на них положительно
#Вариативность низкая


anket %>% 
  select(-a1,- a2, -a9, -a19) -> anket

#Выравнеем номера

i = 14

for (x in paste0('a', c(1:52))) {
  names(anket)[i] <- x
  i <- i+1
}

View(anket)

#Пересчитаем суммы

anket %>% 
  mutate(
    a_sum_1 = rowSums(across(c(a1:a10))),
    a_sum_2 = rowSums(across(c(a11:a23))),
    a_sum_3 = rowSums(across(c(a24:a38))),
    a_sum_4 = rowSums(across(c(a39:a52)))) -> anket

#МОДЕЛЬ 1
#Альфа Кронбаха

anket %>% 
  select(a1:a52) %>%
  alpha()

#Ну, кайф. Альфа Кронбаха нормальная, даже хорошая. Удаление ни одного пункта, её не увеличит. Тут не выкидываем

#Kaiser-Meyer-Olkin (KMO)
anket %>% 
  select(a1:a52) %>% 
  KMO() %>% 
  .[["MSAi"]] %>% sort()

#Ну, пойдёт, хотя часть вопросов выкинуть бы. Так давайте и выкинем те,
#где показатель ниже 0.7

anket %>% 
  select(-a32, -a15, -a50, -a30, -a31, -a3, -a23, -a36, -a18, -a9, -a44, -a22, -a11) -> anket

#Выравнеем номера

i = 14

for (x in paste0('a', c(1:39))) {
  names(anket)[i] <- x
  i <- i+1
}

View(anket)

#Пересчитаем суммы

anket %>% 
  mutate(
    a_sum_1 = rowSums(across(c(a1:a8))),
    a_sum_2 = rowSums(across(c(a9:a16))),
    a_sum_3 = rowSums(across(c(a17:a27))),
    a_sum_4 = rowSums(across(c(a28:a39)))) -> anket

#Альфа Кронбаха

anket %>% 
  select(a1:a39) %>%
  alpha()

#Ну, кайф. Альфа Кронбаха нормальная, даже хорошая. Удаление ни одного пункта, её не увеличит. Тут не выкидываем

#Kaiser-Meyer-Olkin (KMO)
anket %>% 
  select(a1:a39) %>% 
  KMO() %>% 
  .[["MSAi"]] %>% sort()

#Пойдёт, всё больше 0.7


#ЭФА


#Определим количество факторов
anket %>% 
  select(a1:a39) %>% 
  fa.parallel(fa = "fa", fm = "ml")

#Давайте попробуем 1 фактор

anket %>% 
  select(a1:a39) %>%
  factanal(factors = 1) ->
  anket.fa1

print(anket.fa1$loadings, cutoff = 0.4)

#Машина говорит, что 3-4. Но мы возьмём 4, потому что теория!!!

anket %>% 
  select(a1:a39) %>%
  factanal(factors = 4, rotation = 'oblimin') ->
  anket.fa4

print(anket.fa4$loadings, cutoff = 0.4)

#Ну грустно, нагрузки распределились не так, как нам надо
#Давайте КФА

#КФА

mdl1 <- ('F1 =~ a4+a1+a2+a3+a5+a6+a7+a8
          F2 =~ a12+a9+a10+a11+a13+a14+a15+a16
          F3 =~ a23+a17+a18+a19+a20+a21+a22+a24+a25+a26+a27
          F4 =~ a30+a28+a29+a31+a32+a33+a34+a35+a36+a37+a38+a39')

model1 <- cfa(mdl1, data = anket)
summary(model1)
fitmeasures(model1, c("chisq","cfi", "tli", "srmr", "rmsea"))

# Так, оставим везде по 5-6 самых нагурженных вопросов

mdl1.1 <- ('F1 =~ a4+a3+a5+a7+a8
          F2 =~ a12+a9+a10+a15+a16
          F3 =~ a26+a23+a18+a19+a20+a27
          F4 =~ a28+a30+a29+a31+a36')

model1.1 <- cfa(mdl1.1, data = anket)
summary(model1.1)
fitmeasures(model1.1, c("chisq","cfi", "tli", "srmr", "rmsea"))

semPaths(model1.1, 'std')
modificationindices(model1.1) %>% filter(mi > 20) %>% arrange(-mi)

#Lavaan предлагает добавить: 
# a36 в F3 - нет
# a12 в F1 - нет
# a12 в F3 - нет
# a36 в F2 - нет

#Попробуем напоследок подправить список вопросов

anket %>% 
  select(-a1,-a2,-a6,-a11,-a13,-a14,-a17,-a21,-a22,-a24,-a25,-a32,-a33,-a34,-a35,-a37,-a38,-a39) -> anket

#Выравнеем номера

i = 14

for (x in paste0('a', c(1:21))) {
  names(anket)[i] <- x
  i <- i+1
}

View(anket)

#Бахаем напоследок всё то-же самое

#Пересчитаем суммы

anket %>% 
  mutate(
    a_sum_1 = rowSums(across(c(a1:a5))),
    a_sum_2 = rowSums(across(c(a6:a10))),
    a_sum_3 = rowSums(across(c(a11:a16))),
    a_sum_4 = rowSums(across(c(a17:a21)))) -> anket

#Альфа Кронбаха

anket %>% 
  select(a1:a21) %>%
  alpha()

#Ну, кайф. Альфа Кронбаха нормальная, даже хорошая. Удаление ни одного пункта, её не увеличит. Тут не выкидываем

#Kaiser-Meyer-Olkin (KMO)
anket %>% 
  select(a1:a21) %>% 
  KMO() %>% 
  .[["MSAi"]] %>% sort()

#Пойдёт, всё больше 0.8

#МОДЕЛЬ 2

#ЭФА


#Определим количество факторов
anket %>% 
  select(a1:a21) %>% 
  fa.parallel(fa = "fa", fm = "ml")
#Давайте попробуем 1 фактор

anket %>% 
  select(a1:a21) %>%
  factanal(factors = 1) ->
  anket.fa1

print(anket.fa1$loadings, cutoff = 0.4)

#Симмуляция говорит, что больше 4 норм

anket %>% 
  select(a1:a21) %>%
  factanal(factors = 4, rotation = 'oblimin') ->
  anket.fa4

print(anket.fa4$loadings, cutoff = 0.4)

#Ну грустно, нагрузки распределились не так, как нам надо, но как-то ближе
#Давайте КФА

#КФА

mdl2 <- ('F1 =~ a2+a1+a3+a4+a5
          F2 =~ a8+a6+a7+a9+a10
          F3 =~ a14+a11+a12+a13+a15+a16
          F4 =~ a17+a18+a19+a20+a21')

model2 <- cfa(mdl2, data = anket)
summary(model2)
fitmeasures(model2, c("chisq","cfi", "tli", "srmr", "rmsea"))

#ESEM

anket %>% 
  select(a1:a21) %>%
  factanal(factors = 3, rotation = 'geominQ', delta = .5) ->
  anket.esem.efa
print(anket.esem.efa$loadings, cutoff = 0.5)

esem_loadings <- data.table(matrix(round(anket.esem.efa$loadings, 2),
                                   nrow = 21, ncol = 3))

names(esem_loadings) <- c("F1","F2","F3")
esem_loadings$item <- paste0('a', c(1:21))
esem_loadings <- melt(esem_loadings, "item", variable.name = "latent")

esem_loadings

anchors <- c(F1 = "a14", F2 = "a1", F3 = "a20")

make_esem_model <- function (loadings_dt, anchors){
  
  # make is_anchor variable
  loadings_dt[, is_anchor := 0]
  for (l in names(anchors)) loadings_dt[latent != l & item == anchors[l], is_anchor := 1]
  
  # make syntax column per item; syntax is different depending on is_anchor
  loadings_dt[is_anchor == 0, syntax := paste0("start(",value,")*", item)]
  loadings_dt[is_anchor == 1, syntax := paste0(value,"*", item)]
  
  #Make syntax for each latent variable
  each_syntax <- function (l){
    paste(l, "=~", paste0(loadings_dt[latent == l, syntax], collapse = "+"),"\n")
  }
  
  # Put all syntaxes together
  paste(sapply(unique(loadings_dt$latent), each_syntax), collapse = " ")
}

esem_model <- make_esem_model(esem_loadings, anchors)

esem_model

esem_fit <- cfa(esem_model, select(anket, a1:a21), std.lv=T)
summary(esem_fit, fit.measures = T, standardized = T) %>% 
  .[["pe"]] %>% 
  filter(std.lv >= 0.7)

lavaanPlot::lavaanPlot(model = esem_fit, coefs = TRUE,
           stand = TRUE,
           edge_options = list(color ='grey'))
View(anket)

#Ну, подумкаем. Волбшебная машина говорит, что фактора 3: 
# 1) 9,10,12,14,15
# 2) 1,2,3,5,8
# 3) 17,19,20
# Обратимся теперь к магии теории, что это могут быть за факторы
# 1) 9,10,12,14,15,21 - про качество исполнения работы (ну будто бы тоже неплохо)
# 2) 1,2,3,5,8 - польза для людей (ну, ожидаемо)
# 3) 17,19,20 - связь с колегами, кайф

# А теперь возьмём то, что нам сказал E-SEM, и бахнем ещё одну CFA
# Отсеим стандартный вклад ниже 0.7


#МОДЕЛЬ 3
#CFA - once again

mdl3 <- ('F1 =~ a9+a10+a12+a14+a15
          F2 =~ a1+a2+a3+a5+a8
          F3 =~ a17+a19+a20')

model3 <- cfa(mdl3, data = anket)
summary(model3)
fitmeasures(model3, c("chisq","cfi", "tli", "srmr", "rmsea"))

#Ну, выберем из двух моделек одну

fitmeasures(model2, c("chisq","cfi", "tli", "srmr", "rmsea"))
fitmeasures(model3, c("chisq","cfi", "tli", "srmr", "rmsea"))

# Окей, определённо модель с тремя латентными факторами, окей
# Давайте её пофайнтюним и всё
# Но для начала снова почистим вопросы и пересчитаем суммы

anket %>% 
  select(-a4,-a6,-a7,-a11,-a13,-a16,-a18,-a21) -> anket

#Выравнеем номера

i = 14

for (x in paste0('a', c(1:13))) {
  names(anket)[i] <- x
  i <- i+1
}

View(anket)

# Посмотрим, что Lavaan для модификации предложит

mdl3.1 <- ('F1 =~ a9+a6+a7+a8+a10
          F2 =~ a2+a1+a3+a4+a5
          F3 =~ a11+a12+a13
          a4 ~~  a5')

model3.1 <- cfa(mdl3.1, data = anket)
summary(model3.1)
fitmeasures(model3.1, c("chisq","cfi", "tli", "srmr", "rmsea"))

semPaths(model3.1, 'std')
modificationindices(model3.1) %>% filter(mi > 10) %>% arrange(-mi)

#Lavaan предлагает учесть следующие взаимодействия:
# Ковариация a4 и a5; a4 и a10
# Включить a1 и а4 в F1
# Я считаю оправданным учесть ковариацию a4 и a5 - это две строны одного вопроса
# (4 - индивидуальная, 5 - общественная)
# Ковариацию а4 и a10 учитывать не будем
# теоретически включить a1 и a4 в F1 можно, но первый пункт выходит перегруженым, поэтому не буду

#Пересчитаем суммы

anket %>% 
  mutate(
    a_sum_1 = rowSums(across(c(a6:a10))),
    a_sum_2 = rowSums(across(c(a1:a5))),
    a_sum_3 = rowSums(across(c(a11:a13)))) -> anket

anket %>% select(-a_sum_4) -> anket

#Конвергентная валидность

cor(select(anket, c(a_sum_1, c_sum_5)))
cor(select(anket, c(a_sum_2, c_sum_2, c_sum_5, b_sum_1, b_sum_4)))
cor(select(anket, c(a_sum_1, c_sum_4, b_sum_5, b_sum_6)))