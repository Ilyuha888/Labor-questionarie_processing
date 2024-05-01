#Импорт библиотек -----
library(tidyverse)
library(psych)
library(lavaan)
library(semPlot)
library(GPArotation)
library(data.table)
library(lavaanPlot)

#Подготовка ----

#Импортируем почищенные данные
anket <- read_csv(paste0(getwd(),'/2nd_survey/anket_clean.csv'))

#Распределение по полу и возрасту возрасту
anket %>% 
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
#Вариативность низкая


anket %>% 
  select(-a_cosv15, -a3) -> anket

#Пересчитаем суммы

anket %>% 
  mutate(
    a_sum_1 = rowSums(across(c(a1:a13, a_cosv1:a_cosv5))),
    a_sum_4 = rowSums(across(c(a43:a56, a_cosv16:a_cosv19)))) -> anket

#МОДЕЛЬ 1 -----
#Альфа Кронбаха

anket %>% 
  select(a1:a56, a_cosv1:a_cosv19) %>%
  alpha()

#Ну, кайф. Альфа Кронбаха нормальная, даже хорошая. Удаление ни одного пункта, 
#её не увеличит. Тут не выкидываем

#Kaiser-Meyer-Olkin (KMO)
anket %>% 
  select(a1:a56, a_cosv1:a_cosv19) %>% 
  KMO() %>% 
  .[["MSAi"]] %>% sort()

#Ну, пойдёт, хотя часть вопросов выкинуть бы. Так давайте и выкинем те,
#где показатель ниже 0.7

anket %>% 
  select(-a12, -a48, -a18, -a33, -a_cosv6, -a36, -a54, -a40) -> anket

anket %>% select(a1:a_cosv19) %>% colnames() -> vars_left

vars_left

#Пересчитаем суммы

anket %>% 
  mutate(
    a_sum_1 = rowSums(across(c(a1:a13, a_cosv1:a_cosv5))),
    a_sum_2 = rowSums(across(c(a14:a27, a_cosv7:a_cosv10))),
    a_sum_3 = rowSums(across(c(a28:a42, a_cosv11:a_cosv14))),
    a_sum_4 = rowSums(across(c(a43:a56, a_cosv16:a_cosv19)))) -> anket

#Альфа Кронбаха

anket %>% 
  select(a1:a39, a_cosv1:a_cosv19) %>%
  alpha()

#Ну, кайф. Альфа Кронбаха нормальная, даже хорошая. 
#Удаление ни одного пункта, её не увеличит. Тут не выкидываем

#Kaiser-Meyer-Olkin (KMO)
anket %>% 
  select(a1:a_cosv19) %>% 
  KMO() %>% 
  .[["MSAi"]] %>% sort()

#Удаляем ещё пару элементов

anket %>% 
  select(-a5, -a26, -a21, -a22) -> anket

anket %>% select(a1:a_cosv19) %>% colnames() -> vars_left

vars_left

#Пересчитаем суммы

anket %>% 
  mutate(
    a_sum_1 = rowSums(across(c(a1:a13, a_cosv1:a_cosv5))),
    a_sum_2 = rowSums(across(c(a14:a27, a_cosv7:a_cosv10))),
    a_sum_3 = rowSums(across(c(a28:a42, a_cosv11:a_cosv14))),
    a_sum_4 = rowSums(across(c(a43:a56, a_cosv16:a_cosv19)))) -> anket

#Альфа Кронбаха

anket %>% 
  select(a1:a_cosv19) %>%
  alpha()

#Ну, кайф. Альфа Кронбаха нормальная, даже хорошая. 
#Удаление ни одного пункта, её не увеличит. Тут не выкидываем

#Kaiser-Meyer-Olkin (KMO)
anket %>% 
  select(a1:a_cosv19) %>% 
  KMO() %>% 
  .[["MSAi"]] %>% sort()

#ЭФА

#Давайте попробуем 1 фактор

anket %>% 
  select(a1:a_cosv19) %>%
  factanal(factors = 1) ->
  anket.fa1

print(anket.fa1$loadings, cutoff = 0.4)

#Определим количество факторов
anket %>% 
  select(a1:a_cosv19) %>% 
  fa.parallel(fa = "fa", fm = "ml")

#Машина говорит, что 3-4. Но мы возьмём 4, потому что теория!!!

anket %>% 
  select(a1:a_cosv19) %>%
  factanal(factors = 4, rotation = 'oblimin') ->
  anket.fa4

print(anket.fa4$loadings, cutoff = 0.4)

#Ну грустно, нагрузки распределились не так, как нам надо
#Давайте КФА

vars_left

#КФА

mdl1 <- ('F1 =~ a8+a1+a2+a4+a6+a7+a9+a10+a11+a13+a_cosv1+a_cosv2+a_cosv3+a_cosv4+a_cosv5
          F2 =~ a19+a14+a13+a14+a15+a16+a17+a20+a23+a24+a25+a27+a_cosv7+a_cosv8+a_cosv9+a_cosv10
          F3 =~ a42+a28+a29+a30+a31+a32+a34+a35+a37+a38+a39+a41+a_cosv11+a_cosv12+a_cosv13+a_cosv14
          F4 =~ a51+a43+a44+a45+a46+a47+a49+a50+a52+a53+a55+a56+a_cosv16+a_cosv17+a_cosv18+a_cosv19')

model1 <- cfa(mdl1, data = anket)
summary(model1)
fitmeasures(model1, c("chisq","cfi", "tli", "srmr", "rmsea"))


#МОДЕЛЬ 2 -----

# Так, оставим везде по 5-6 самых нагурженных вопросов

anket %>% select(c(ID:a8, a7, a11, a_cosv3, a_cosv4, a6,
                 a16, a15, a19, a25, a_cosv7, a_cosv9,
                 a42, a37, a38, a41, a39, a_cosv13,
                 a51, a43, a52, a55, a_cosv16, a_cosv18, b1:s_sum_1)) -> anket

#Бахаем напоследок всё то-же самое

#Пересчитаем суммы

anket %>% 
  mutate(
    a_sum_1 = rowSums(across(c(a8, a7, a11, a_cosv3, a_cosv4, a6))),
    a_sum_2 = rowSums(across(c(a16, a15, a19, a25, a_cosv7, a_cosv9))),
    a_sum_3 = rowSums(across(c(a42, a37, a38, a41, a39, a_cosv13))),
    a_sum_4 = rowSums(across(c(a51, a43, a52, a55, a_cosv16, a_cosv18)))) -> anket

#Альфа Кронбаха

anket %>% 
  select(a6:a_cosv18) %>%
  alpha()

#Ну, кайф. Альфа Кронбаха нормальная, даже хорошая. 
#Удаление ни одного пункта, её не увеличит. Тут не выкидываем

#Kaiser-Meyer-Olkin (KMO)
anket %>% 
  select(a6:a_cosv18) %>% 
  KMO() %>% 
  .[["MSAi"]] %>% sort()

#Пойдёт, всё больше 0.8

#ЭФА

anket %>% select(a6:a_cosv18) %>% colnames() -> vars_left

#Определим количество факторов
anket %>% 
  select(a6:a_cosv18) %>% 
  fa.parallel(fa = "fa", fm = "ml")
#Давайте попробуем 1 фактор

anket %>% 
  select(a6:a_cosv18) %>%
  factanal(factors = 1) ->
  anket.fa1

print(anket.fa1$loadings, cutoff = 0.4)

# Попробуем теоретическую модель

anket %>% 
  select(a6:a_cosv18) %>%
  factanal(factors = 4, rotation = 'oblimin') ->
  anket.fa4

print(anket.fa4$loadings, cutoff = 0.4)

#Ну грустно, нагрузки распределились не так, как нам надо, но как-то ближе

#КФА

mdl2 <- ('F1 =~ a8+a7+a11+a_cosv3+a_cosv4+a6
          F2 =~ a16+a15+a19+a25+a_cosv7+a_cosv9
          F3 =~ a42+a37+a38+a41+a39+a_cosv13
          F4 =~ a51+a43+a52+a55+a_cosv16+a_cosv18')

model2 <- cfa(mdl2, data = anket)
lavInspect(model2, "cov.lv")
summary(model2)
fitmeasures(model2, c("chisq","cfi", "tli", "srmr", "rmsea"))

semPaths(model2, 'std')
modificationindices(model2) %>% filter(mi > 20) %>% arrange(-mi)

#Lavaan говорит, что беда модель не работает, матрица не считается(((


#МОДЕЛЬ 3 -----

#Попробуем ESEM

#Первое вращение, выбираем якоря
anket %>% 
  select(a6:a_cosv18) %>%
  factanal(factors = 3, rotation = 'geominQ', delta = .5) ->
  anket.esem.efa
print(anket.esem.efa$loadings, cutoff = 0.4)

#Cтроим матрицу со всеми нагрузками
esem_loadings <- data.table(matrix(round(anket.esem.efa$loadings, 2),
                                   nrow = 24, ncol = 3))

#Меняем формат матрицы, готовя к анализу
names(esem_loadings) <- c("F1","F2","F3")
esem_loadings$item <- vars_left
esem_loadings <- melt(esem_loadings, "item", variable.name = "latent")

esem_loadings

#Устанавливаем якоря
anchors <- c(F1 = "a_cosv18", F2 = "a6", F3 = "a25")

#Задаём функцию для подсчёта ESEM модели
make_esem_model <- function (loadings_dt, anchors){
  
  #Добавляем is_anchor (является ли якорем)
  loadings_dt[, is_anchor := 0]
  for (l in names(anchors)) loadings_dt[latent != l & item == anchors[l], 
                                        is_anchor := 1]
  
  #Добавляем лавановский синтакс для каждой переменной (зависит от is_anchor)
  loadings_dt[is_anchor == 0, syntax := paste0("start(",value,")*", item)]
  loadings_dt[is_anchor == 1, syntax := paste0(value,"*", item)]
  
  #Добавляем лавановский синтакс для латентных переменных
  each_syntax <- function (l){
    paste(l, "=~", paste0(loadings_dt[latent == l, syntax], collapse = "+"),"\n")
  }
  
  #Генерируем финальный лавановский синтакс
  paste(sapply(unique(loadings_dt$latent), each_syntax), collapse = " ")
}

#Делаем ESEM с нашими параметрами
esem_model <- make_esem_model(esem_loadings, anchors)

esem_model

#Смотрим стандартные нагрузки, с фильтром больше или равно 0.7
esem_fit <- cfa(esem_model, select(anket, a6:a_cosv18), std.lv=T)
summary(esem_fit, fit.measures = T, standardized = T) %>% 
  .[["pe"]] %>% 
  filter(std.lv >= 0.7)

#Строим график
lavaanPlot::lavaanPlot(model = esem_fit, coefs = TRUE,
                       stand = TRUE,
                       edge_options = list(color ='grey'))

#Ну, подумкаем. Волбшебная машина говорит, что фактора 3: 
# 1) cosv9, 42, 41, cosv13, a51, a55, cosv16, cosv18
# 2) 6, 7, 8, cosv3, cosv4, 43
# 3) 25, 39
# Обратимся теперь к магии теории, что это могут быть за факторы
# 1) cosv9, 42, 41, cosv13, a51, a55, cosv16, cosv18 - с натяжкой что-то про страсть
# 2) 6, 7, 8, cosv3, cosv4, 43 - про людей
# 3) 25, 39 - что-то про настроение

# Но в целом... Пациент скорее мёртв

# А теперь возьмём то, что нам сказал E-SEM, и бахнем ещё одну CFA
# Отсеим 2-4 вопроса для шкалы с самым большим вкладом

#CFA - once again

mdl3 <- ('F1 =~ a_cosv13+a51+a55+a_cosv18
          F2 =~ a6+a7+a8+a_cosv3
          F3 =~ a25+a39')

model3 <- cfa(mdl3, data = anket)
summary(model3)
anket %>% 
  select(c(ID:a6, a_cosv13, a51, a55, 
           a_cosv18, a7, a8, a_cosv3, a25, a39, b1:s_sum_1)) -> anket
mdl3 <- ('F1 =~ a51+a_cosv13+a55+a_cosv18
          F2 =~ a8+a6+a7+a_cosv3
          F3 =~ a39+a25')

model3 <- cfa(mdl3, data = anket)
summary(model3)


fitmeasures(model3, c("chisq","cfi", "tli", "srmr", "rmsea"))

#Ну, выберем из двух моделек одну

fitmeasures(model2, c("chisq","cfi", "tli", "srmr", "rmsea"))
fitmeasures(model3, c("chisq","cfi", "tli", "srmr", "rmsea"))

# Окей, определённо модель с тремя латентными факторами, окей
# Даже почти дотягивается до уровня приемлимости
# Давайте её пофайнтюним и всё


# Посмотрим, что Lavaan для модификации предложит

semPaths(model3, 'std')
modificationindices(model3) %>% filter(mi > 10) %>% arrange(-mi)

# Lavaan предлагает учесть следующие взаимодействия:
# Включить a39 и a25 в F1, F2
# Включить a_cosv3 в F1 и а55 в F3

# Я считаю оправданным включить a55 в F3
# Включение других пунктов в другие факторы считаю не оправданным

#Посмотрим после учёта

mdl3.1 <- ('F1 =~ a51+a_cosv13+a55+a_cosv18
          F2 =~ a8+a6+a7+a_cosv3
          F3 =~ a39+a25+a55')

model3.1 <- cfa(mdl3.1, data = anket)
summary(model3.1)
fitmeasures(model3.1, c("chisq","cfi", "tli", "srmr", "rmsea"))
# В целом модель проходит по границе приемлимости

#Строим график

semPaths(model3.1, 'std')


anket %>% select(a6:a_cosv18) %>% colnames() -> vars_left

#Пересчитаем суммы
#Посчитаем веса
as_tibble_col(unlist(model3.1@ParTable[2], use.names=FALSE)[1:15], 
                       column_name = 'lat_var') %>% 
  mutate(var = unlist(model3.1@ParTable[4], use.names=FALSE)[1:15], 
         coef = unlist(model3.1@ParTable[13], use.names=FALSE)[1:15]) -> coefs

coefs %>% arrange(lat_var,var) %>% select(coef)-> coefss
#Домножим на веса
i = 1
for (x in c(14:28)) {
  anket[x] = anket[x] * coefss$coef[i]
  i = i+1
}
#Сами суммы
anket %>% 
  mutate(
    a_sum_1 = rowSums(across(c(a51,a_cosv13,a55,a_cosv18))),
    a_sum_2 = rowSums(across(c(a8,a6,a7,a_cosv3))),
    a_sum_3 = rowSums(across(c(a39,a25,a55)))) -> anket

anket %>% select(-a_sum_4) -> anket

#Сохраним результаты для дальнешей работы: ----

anket %>% select(ID:d12, a_sum_1:s_sum_1) %>%
  write_csv(paste0(getwd(),'/2nd_survey/anket_an_1.csv'))

# Теоретическая интерпретация

# F1 (a51,a_cosv13,a55,a_cosv18) - смысл работы, значимость
# F2 (a8,a6,a7,a_cosv3) - про людей
# F3 (a39,a25,a55) - про настроение

#Проверим методику "Сокращенная версия опросника проактивный копиг" -----
#Альфа Кронбаха

anket %>% 
  select(b1:b27) %>%
  alpha()

#Kaiser-Meyer-Olkin (KMO)
anket %>% 
  select(b1:b27) %>% 
  KMO() %>% 
  .[["MSAi"]] %>% sort()

#КФА
mdl1 <- ('F1 =~ b1+b2+b3+b4+b5+b6 
          F2 =~ b7+b8+b9+b10+b11
          F3 =~ b12+b13+b14
          F4 =~ b15+b16+b17+b18+b19
          F5 =~ b20+b21+b22+b23
          F6 =~ b24+b25+b26+b27')

model1 <- cfa(mdl1, data = anket)
summary(model1)
fitmeasures(model1, c("chisq","cfi", "tli", "srmr", "rmsea"))

#Проверим методику "Удовлетворенность трудом" -----
#Альфа Кронбаха

anket %>% 
  select(c1:c19) %>%
  alpha()

#Kaiser-Meyer-Olkin (KMO)
anket %>% 
  select(c1:c19) %>% 
  KMO() %>% 
  .[["MSAi"]] %>% sort()

#КФА
mdl1 <- ('F1 =~ c1+c2+c3+c4 
          F2 =~ c5+c6+c7+c8
          F3 =~ c9+c10+c11
          F4 =~ c12+c13+c14
          F5 =~ c15+c16+c17+c18+c19')

model1 <- cfa(mdl1, data = anket)
summary(model1)
fitmeasures(model1, c("chisq","cfi", "tli", "srmr", "rmsea"))

#Проверим методику "Шкала проффесиональной апатии" -----
#Альфа Кронбаха

anket %>% 
  select(u1:u10) %>%
  alpha()

#Kaiser-Meyer-Olkin (KMO)
anket %>% 
  select(u1:u10) %>% 
  KMO() %>% 
  .[["MSAi"]] %>% sort()

#КФА
mdl1 <- ('F1 =~ u1+u2+u3+u4+u5 
          F2 =~ u6+u7+u8+u9+u10')

model1 <- cfa(mdl1, data = anket)
summary(model1)
fitmeasures(model1, c("chisq","cfi", "tli", "srmr", "rmsea"))

#Проверим методику "Шкалы толерантности и интолерантности к неопределенности в модификации опросника" -----
#Альфа Кронбаха

anket %>% 
  select(t1:t13) %>%
  alpha()

#Kaiser-Meyer-Olkin (KMO)
anket %>% 
  select(t1:t13) %>% 
  KMO() %>% 
  .[["MSAi"]] %>% sort()

#КФА
mdl1 <- ('F1 =~ t1+t2+t3+t4+t5+t6+t7 
          F2 =~ t8+t9+t10+t11+t12+t13')

model1 <- cfa(mdl1, data = anket)
summary(model1)
fitmeasures(model1, c("chisq","cfi", "tli", "srmr", "rmsea"))

#Проверим методику "Удовлетворенность жизнью" -----
#Альфа Кронбаха

anket %>% 
  select(s1:s5) %>%
  alpha()

#Kaiser-Meyer-Olkin (KMO)
anket %>% 
  select(s1:s5) %>% 
  KMO() %>% 
  .[["MSAi"]] %>% sort()

#КФА
mdl1 <- ('F1 =~ s1+s2+s3+s4+s5')

model1 <- cfa(mdl1, data = anket)
summary(model1)
fitmeasures(model1, c("chisq","cfi", "tli", "srmr", "rmsea"))
