#install.packages("tidySEM")
#Импорт библиотек -----
library(tidyverse)
library(psych)
library(lavaan)
library(semPlot)
library(GPArotation)
library(data.table)
library(lavaanPlot)
library(broom)
library(flextable)
library(tidySEM)

#Подготовка ----

#Импортируем почищенные данные
anket <- read_csv(paste0(getwd(), '/thesis/anket_clean.csv'))

#Распределение по полу и возрасту возрасту
anket %>%
  group_by(dem1) %>%
  summarise(
    n = n(),
    mean = mean(dem2),
    median = median(dem2),
    var = var(dem2),
    sd = sd(dem2),
    min = min(dem2),
    max = max(dem2)
  )

#Посмотрим нормальность

skewness <- datawizard::skewness(anket)
kurtosis <- datawizard::kurtosis(anket)

View(skewness)
View(kurtosis)

#Удалим те, где ассиметрия и эксцесс нигде по модулю не выходят за 2.

#МОДЕЛЬ 1 -----
#Альфа Кронбаха

anket %>%
  select(klim1:klim28) %>%
  alpha()

#Ну, кайф. Альфа Кронбаха нормальная, даже хорошая. Удаление ни одного пункта,
#её не увеличит. Тут не выкидываем

#Kaiser-Meyer-Olkin (KMO)
anket %>%
  select(klim1:klim28) %>%
  KMO() %>%
  .[["MSAi"]] %>% sort()

#Ну, пойдёт, хотя часть вопросов под сомнение. 2 вопроса, которые в обратной формулировке и про инструменты. Запомним

#КФА

mdl1 <- (
  'F1 =~ klim4+klim1+klim2+klim3+klim5+klim6+klim7
          F2 =~ klim9+klim8+klim10+klim11+klim12+klim13+klim14
          F3 =~ klim16+klim15+klim17+klim18+klim19+klim20+klim21
          F4 =~ klim25+klim22+klim23+klim24+klim26+klim27+klim28'
)

model1 <- cfa(mdl1, data = anket)
summary(model1)
fitmeasures(model1, c("chisq", "cfi", "tli", "srmr", "rmsea"))

write.csv(fitmeasures(model1, c("chisq", "cfi", "tli", "srmr", "rmsea")),
          paste0(getwd(), '/thesis/cfa1.quality.csv'))

#Показатели модели не удовлетворительны. Хуже всего себя показали элементы klim3, klim7, klim11, klim13, klim18, klim20, klim24, klim27
#По совместительству это элементы, которые были в обратной форме

#Строим график

semPaths(
  model1,
  "std",
  layout = "circle",
  style = "ram",
  residuals = FALSE,
  intercepts = FALSE,
  nCharNodes = 5,
  edge.label.cex = 0.8,
  label.scale = FALSE,
  curvePivot = TRUE,
  pastel = TRUE,
  shapeMan = 'circle',
  node.width = 0.9,
  # Увеличение ширины узлов
  node.height = 0.7,
  # Увеличение высоты узлов
  label.cex = 1.2,
  # Масштабирование меток
  mar = c(1, 1, 1, 1),
  filetype = "png",
  filename = 'thesis_model1',
  width = 11.7,
  height = 8.3
) # Увеличение отступов

#МОДЕЛЬ 2 -----

# Так, оставим везде по 5-6 самых нагурженных вопросов

anket %>% select(
  c(
    ID:a8,
    a7,
    a11,
    cv3,
    cv4,
    a6,
    a16,
    a15,
    a19,
    a25,
    cv7,
    cv9,
    a42,
    a37,
    a38,
    a41,
    a39,
    cv13,
    a51,
    a43,
    a52,
    a55,
    cv16,
    cv18,
    b1:s_sum_1
  )
) -> anket

#Бахаем напоследок всё то-же самое

#Пересчитаем суммы

anket %>%
  mutate(
    a_sum_1 = rowSums(across(c(
      a8, a7, a11, cv3, cv4, a6
    ))),
    a_sum_2 = rowSums(across(c(
      a16, a15, a19, a25, cv7, cv9
    ))),
    a_sum_3 = rowSums(across(c(
      a42, a37, a38, a41, a39, cv13
    ))),
    a_sum_4 = rowSums(across(c(
      a51, a43, a52, a55, cv16, cv18
    )))
  ) -> anket

#Альфа Кронбаха

anket %>%
  select(a6:cv18) %>%
  alpha()

#Ну, кайф. Альфа Кронбаха нормальная, даже хорошая.
#Удаление ни одного пункта, её не увеличит. Тут не выкидываем

#Kaiser-Meyer-Olkin (KMO)
anket %>%
  select(a6:cv18) %>%
  KMO() %>%
  .[["MSAi"]] %>% sort()

#Пойдёт, всё больше 0.8

#ЭФА

anket %>% select(a6:cv18) %>% colnames() -> vars_left

#Определим количество факторов
anket %>%
  select(a6:cv18) %>%
  fa.parallel(fa = "fa", fm = "ml")
#Давайте попробуем 1 фактор

anket %>%
  select(a6:cv18) %>%
  factanal(factors = 1) ->
  anket.fa1

print(anket.fa1$loadings, cutoff = 0.4)

# Попробуем теоретическую модель

anket %>%
  select(a6:cv18) %>%
  factanal(factors = 4, rotation = 'oblimin') ->
  anket.fa4

print(anket.fa4$loadings, cutoff = 0.4)

#Ну грустно, нагрузки распределились не так, как нам надо, но как-то ближе

#КФА

mdl2 <- (
  'F1 =~ a8+a7+a11+cv3+cv4+a6
          F2 =~ a16+a15+a19+a25+cv7+cv9
          F3 =~ a42+a37+a38+a41+a39+cv13
          F4 =~ a51+a43+a52+a55+cv16+cv18'
)

model2 <- cfa(mdl2, data = anket)
lavInspect(model2, "cov.lv")
summary(model2)
fitmeasures(model2, c("chisq", "cfi", "tli", "srmr", "rmsea"))

write.csv(fitmeasures(model2, c("chisq", "cfi", "tli", "srmr", "rmsea")),
          paste0(getwd(), '/thesis/cfa2.quality.csv'))

#Строим график

semPaths(
  model2,
  "std",
  layout = "circle",
  style = "ram",
  residuals = FALSE,
  intercepts = FALSE,
  nCharNodes = 5,
  edge.label.cex = 0.8,
  label.scale = FALSE,
  curvePivot = TRUE,
  pastel = TRUE,
  shapeMan = 'circle',
  node.width = 0.9,
  # Увеличение ширины узлов
  node.height = 0.7,
  # Увеличение высоты узлов
  label.cex = 1.2,
  # Масштабирование меток
  mar = c(1, 1, 1, 1),
  filetype = "png",
  filename = 'model2',
  width = 11.7,
  height = 8.3
) # Увеличение отступов

modificationindices(model2) %>% filter(mi > 20) %>% arrange(-mi)


View(model2)
#Lavaan говорит, что беда модель не работает, матрица не считается(((


#МОДЕЛЬ 3 -----

#Попробуем ESEM

#Первое вращение, выбираем якоря
anket %>%
  select(a6:cv18) %>%
  factanal(factors = 3,
           rotation = 'geominQ',
           delta = .5) ->
  anket.esem.efa
print(anket.esem.efa$loadings, cutoff = 0.4)

#Cтроим матрицу со всеми нагрузками
esem_loadings <-
  data.table(matrix(
    round(anket.esem.efa$loadings, 2),
    nrow = 24,
    ncol = 3
  ))

#Меняем формат матрицы, готовя к анализу
names(esem_loadings) <- c("F1", "F2", "F3")
esem_loadings$item <- vars_left
esem_loadings <-
  melt(esem_loadings, "item", variable.name = "latent")

esem_loadings

#Устанавливаем якоря
anchors <- c(F1 = "cv18", F2 = "a6", F3 = "a25")

#Задаём функцию для подсчёта ESEM модели
make_esem_model <- function (loadings_dt, anchors) {
  #Добавляем is_anchor (является ли якорем)
  loadings_dt[, is_anchor := 0]
  for (l in names(anchors))
    loadings_dt[latent != l & item == anchors[l],
                is_anchor := 1]
  
  #Добавляем лавановский синтакс для каждой переменной (зависит от is_anchor)
  loadings_dt[is_anchor == 0, syntax := paste0("start(", value, ")*", item)]
  loadings_dt[is_anchor == 1, syntax := paste0(value, "*", item)]
  
  #Добавляем лавановский синтакс для латентных переменных
  each_syntax <- function (l) {
    paste(l, "=~", paste0(loadings_dt[latent == l, syntax], collapse = "+"), "\n")
  }
  
  #Генерируем финальный лавановский синтакс
  paste(sapply(unique(loadings_dt$latent), each_syntax), collapse = " ")
}

#Делаем ESEM с нашими параметрами
esem_model <- make_esem_model(esem_loadings, anchors)

esem_model

#Смотрим стандартные нагрузки, с фильтром больше или равно 0.7
esem_fit <- cfa(esem_model, select(anket, a6:cv18), std.lv = T)
summary(esem_fit, fit.measures = T, standardized = T) %>%
  .[["pe"]] %>%
  filter(std.lv >= 0.7)

#Строим график
lavaanPlot::lavaanPlot(
  model = esem_fit,
  coefs = TRUE,
  stand = TRUE,
  edge_options = list(color = 'grey')
)

#Ну, подумкаем. Волбшебная машина говорит, что фактора 3:
# 1) cv9, 42, 41, cv13, a51, a55, cv16, cv18
# 2) 6, 7, 8, cv3, cv4, 43
# 3) 25, 39
# Обратимся теперь к магии теории, что это могут быть за факторы
# 1) cv9, 42, 41, cv13, a51, a55, cv16, cv18 - с натяжкой что-то про страсть
# 2) 6, 7, 8, cv3, cv4, 43 - про людей
# 3) 25, 39 - что-то про настроение

# Но в целом... Пациент скорее мёртв

# А теперь возьмём то, что нам сказал E-SEM, и бахнем ещё одну CFA
# Отсеим 2-4 вопроса для шкалы с самым большим вкладом

#CFA - once again

mdl3 <- ('F1 =~ cv13+a51+a55+cv18
          F2 =~ a6+a7+a8+cv3
          F3 =~ a25+a39')

model3 <- cfa(mdl3, data = anket)
summary(model3)
anket %>%
  select(c(ID:a6, cv13, a51, a55,
           cv18, a7, a8, cv3, a25, a39, b1:s_sum_1)) -> anket
mdl3 <- ('F1 =~ a51+cv13+a55+cv18
          F2 =~ a8+a6+a7+cv3
          F3 =~ a39+a25')


model3 <- cfa(mdl3, data = anket)
summary(model3)


fitmeasures(model3, c("chisq", "cfi", "tli", "srmr", "rmsea"))

#Ну, выберем из двух моделек одну

fitmeasures(model2, c("chisq", "cfi", "tli", "srmr", "rmsea"))
fitmeasures(model3, c("chisq", "cfi", "tli", "srmr", "rmsea"))

# Окей, определённо модель с тремя латентными факторами, окей
# Даже почти дотягивается до уровня приемлимости
# Давайте её пофайнтюним и всё


# Посмотрим, что Lavaan для модификации предложит

semPaths(model3, 'std')
modificationindices(model3) %>% filter(mi > 10) %>% arrange(-mi)

# Lavaan предлагает учесть следующие взаимодействия:
# Включить a39 и a25 в F1, F2
# Включить cv3 в F1 и а55 в F3

# Я считаю оправданным включить a55 в F3
# Включение других пунктов в другие факторы считаю не оправданным

#Посмотрим после учёта

mdl3.1 <- ('F1 =~ a51+cv13+a55+cv18
          F2 =~ a8+a6+a7+cv3
           F3 =~ a39+a25+a55')

model3.1 <- cfa(mdl3.1, data = anket)
View(summary(model3.1))
fitmeasures(model3.1, c("chisq", "cfi", "tli", "srmr", "rmsea"))
# В целом модель проходит по границе приемлимости

gsub("F1", "Важность", model3.1@ParTable$lhs) -> model3.1@ParTable[['lhs']]
gsub("F2", "Польза", model3.1@ParTable$lhs) -> model3.1@ParTable[['lhs']]
gsub("F3", "Аффективная\nнасыщенность", model3.1@ParTable$lhs) -> model3.1@ParTable[['lhs']]
gsub("F1", "Важность", model3.1@ParTable$rhs) -> model3.1@ParTable[['rhs']]
gsub("F2", "Польза", model3.1@ParTable$rhs) -> model3.1@ParTable[['rhs']]
gsub("F3", "Аффективная\nнасыщенность", model3.1@ParTable$rhs) -> model3.1@ParTable[['rhs']]

#Строим график
semPaths(
  model3.1,
  "std",
  layout = "circle",
  style = "ram",
  residuals = FALSE,
  intercepts = FALSE,
  nCharNodes = 100,
  edge.label.cex = 0.8,
  label.scale = TRUE,
  curvePivot = TRUE,
  pastel = TRUE,
  shapeMan = 'circle',
  node.width = 1.2,
  # Увеличение ширины узлов
  node.height = 0.7,
  # Увеличение высоты узлов
  label.cex = 1.2,
  # Масштабирование меток
  mar = c(1, 1, 1, 1),
  filetype = "png",
  filename = 'model3.1',
  width = 11.7,
  height = 8.3
) # Увеличение отступов

anket %>% select(a6:cv18) %>% colnames() -> vars_left

#Пересчитаем суммы
#Посчитаем веса
as_tibble_col(unlist(model3.1@ParTable[2], use.names = FALSE)[1:15],
              column_name = 'lat_var') %>%
  mutate(
    var = unlist(model3.1@ParTable[4], use.names = FALSE)[1:15],
    coef = unlist(model3.1@ParTable[13], use.names = FALSE)[1:15]
  ) -> coefs

coefs %>% arrange(lat_var, var) %>% select(coef) -> coefss
#Домножим на веса
i = 1
for (x in c(14:28)) {
  anket[x] = anket[x] * coefss$coef[i]
  i = i + 1
}
#Сами суммы
anket %>%
  mutate(
    a_sum_1 = rowSums(across(c(a51, cv13, a55, cv18))),
    a_sum_2 = rowSums(across(c(a8, a6, a7, cv3))),
    a_sum_3 = rowSums(across(c(a39, a25, a55)))
  ) -> anket

anket %>% select(-a_sum_4) -> anket

#Сохраним результаты для дальнешей работы: ----

anket %>% select(ID:d12, a_sum_1:s_sum_1) %>%
  write_csv(paste0(getwd(), '/thesis/anket_an_1.csv'))

# Теоретическая интерпретация

# F1 (a51,cv13,a55,cv18) - смысл работы, значимость
# F2 (a8,a6,a7,cv3) - про людей
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
mdl1 <- (
  'F1 =~ b1+b2+b3+b4+b5+b6
          F2 =~ b7+b8+b9+b10+b11
          F3 =~ b12+b13+b14
          F4 =~ b15+b16+b17+b18+b19
          F5 =~ b20+b21+b22+b23
          F6 =~ b24+b25+b26+b27'
)

model1 <- cfa(mdl1, data = anket)
summary(model1)
fitmeasures(model1, c("chisq", "cfi", "tli", "srmr", "rmsea"))

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
mdl1 <- (
  'F1 =~ c1+c2+c3+c4
          F2 =~ c5+c6+c7+c8
          F3 =~ c9+c10+c11
          F4 =~ c12+c13+c14
          F5 =~ c15+c16+c17+c18+c19'
)

model1 <- cfa(mdl1, data = anket)
summary(model1)
fitmeasures(model1, c("chisq", "cfi", "tli", "srmr", "rmsea"))

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
fitmeasures(model1, c("chisq", "cfi", "tli", "srmr", "rmsea"))

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
fitmeasures(model1, c("chisq", "cfi", "tli", "srmr", "rmsea"))

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
fitmeasures(model1, c("chisq", "cfi", "tli", "srmr", "rmsea"))
