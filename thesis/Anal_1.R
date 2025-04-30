#install.packages("tidySEM")
#install.packages("purrr")
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
library(purrr)

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

#Распределение по образованию
anket %>%
  group_by(dem4) %>%
  summarise(
    n = n()
  )

#Распределение по населённому пункту
anket %>%
  group_by(dem3) %>%
  summarise(
    n = n()
  )

#Распределение по сфере компании
anket <- anket %>%
  mutate(
    dem7_recoded = case_when(
      dem7 %in% c("Обслуживание;", "Продажи, закупки;", "Образование;", "Торговля;", 
                  "Логистика;", "Транспорт;", "Общественное питание;", "Гостиничный бизнес;", 
                  "Красота и здоровье;", "Маркетинг, реклама, PR;", "Финансы, бухгалтерия, банки;", 
                  "Медицина;", "Производство;", "Юриспруденция;") ~ dem7,
      TRUE ~ "Другое"
    ),
    # Устанавливаем порядок уровней фактора
    dem7_recoded = factor(
      dem7_recoded,
      levels = c(
        "Обслуживание;", "Продажи, закупки;", "Образование;", "Торговля;", 
        "Логистика;", "Транспорт;", "Общественное питание;", "Гостиничный бизнес;", 
        "Красота и здоровье;", "Маркетинг, реклама, PR;", "Финансы, бухгалтерия, банки;", 
        "Медицина;", "Производство;", "Юриспруденция;", "Другое"
      )
    )
  )

anket %>%
  # Группируем и считаем частоты
  count(dem7_recoded, name = "n") %>%
  # Добавляем проценты (доля от общего числа ответов)
  mutate(
    percent = round(n / sum(n) * 100, 2)  # Округляем до 2 знаков
  ) %>%
  # Упорядочиваем по заданному порядку уровней
  arrange(dem7_recoded)

#Посмотрим нормальность

skewness <- datawizard::skewness(anket)
kurtosis <- datawizard::kurtosis(anket)

#View(skewness)
#View(kurtosis)

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
  filename = 'thesis/thesis_model1',
  width = 11.7,
  height = 8.3
) # Увеличение отступов

#МОДЕЛЬ 2 -----


# Попробуем убрать эти элементы и посмотреть на показатели, тем более мы
# заложили себе возможность убрать по 2 элемента на субшкалу


#Альфа Кронбаха
anket %>%
  select(
    c(
      klim1:klim2,
      klim4:klim6,
      klim8:klim10,
      klim12,
      klim14:klim17,
      klim19,
      klim21:klim23,
      klim25:klim26,
      klim28
    )
  ) %>%
  alpha()

#Ну, кайф. Альфа Кронбаха ещё выше - 0.92
#Kaiser-Meyer-Olkin (KMO)
anket %>%
  select(
    c(
      klim1:klim2,
      klim4:klim6,
      klim8:klim10,
      klim12,
      klim14:klim17,
      klim19,
      klim21:klim23,
      klim25:klim26,
      klim28
    )
  ) %>%
  KMO() %>%
  .[["MSAi"]] %>% sort()

#Пойдёт, всё больше 0.84

#КФА

mdl2 <- (
  'F1 =~ klim4+klim1+klim2+klim5+klim6
          F2 =~ klim9+klim8+klim10+klim12+klim14
          F3 =~ klim16+klim15+klim17+klim19+klim21
          F4 =~ klim25+klim22+klim23+klim26+klim28'
)

model2 <- cfa(mdl2, data = anket)
summary(model2)

fitmeasures(model2, c("chisq", "cfi", "tli", "srmr", "rmsea"))

write.csv(fitmeasures(model2, c("chisq", "cfi", "tli", "srmr", "rmsea")),
          paste0(getwd(), '/thesis/cfa2.quality.csv'))

# В целом приемлемые показатели

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
  filename = 'thesis/thesis_model2',
  width = 11.7,
  height = 8.3
) # Увеличение отступов

modificationindices(model2) %>% filter(mi > 15) %>% arrange(-mi)


# Lavaan предлагает учесть следующие взаимодействия:
# Включить корелляцию klim19 ~~ klim21 - звучит оправдано
# Включить корелляцию klim8 ~~ klim10 - звучит оправдано
# Включить корелляцию klim16 ~~ klim15 - звучит оправдано
# Включить корелляцию klim2 ~~ klim6 - звучит оправдано
# Включить klim19 в F4 - не оправдано


#Попробуем посмотреть с этими поправками

mdl2_5 <- (
  'F1 =~ klim4+klim1+klim2+klim5+klim6
          F2 =~ klim9+klim8+klim10+klim12+klim14
          F3 =~ klim16+klim15+klim17+klim19+klim21
          F4 =~ klim25+klim22+klim23+klim26+klim28
          klim19 ~~ klim21
          klim8 ~~ klim10
          klim16 ~~ klim15
  klim2 ~~ klim6'
)

model2_5 <- cfa(mdl2_5, data = anket)
summary(model2_5)
fitmeasures(model2_5, c("chisq", "cfi", "tli", "srmr", "rmsea"))

write.csv(fitmeasures(model2_5, c("chisq", "cfi", "tli", "srmr", "rmsea")),
          paste0(getwd(), '/thesis/cfa2_5.quality.csv'))

# В целом приемлемые показатели

#Строим график

# Получаем вектор всех имён узлов
model_plot <- semPlot::semPlotModel(model2_5)

labels_all <- model_plot@Vars$name

# Создаём копию
labels_new <- labels_all

# Меняем только латентные переменные
labels_new[labels_new == "F1"] <- "Связь с коллегами"
labels_new[labels_new == "F2"] <- "Польза труда"
labels_new[labels_new == "F3"] <- "Инструментальный\nаспект"
labels_new[labels_new == "F4"] <- "Эмоциональное\nвлияние"

#Строим график
semPaths(
  model2_5,
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
  filename = 'thesis/thesis_model2_5',
  width = 11.7,
  height = 8.3,
  nodeLabels = labels_new
) # Увеличение отступов

#Посмотрим на надёжность субшкал

anket %>%
  select(c(klim4, klim1, klim2, klim5, klim6)) %>%
  alpha() # 0.8

anket %>%
  select(c(klim9, klim8, klim10, klim12, klim14)) %>%
  alpha() # 0.91

anket %>%
  select(c(klim16, klim15, klim17, klim19, klim21)) %>%
  alpha() # 0.68

anket %>%
  select(c(klim25, klim22, klim23, klim26, klim28)) %>%
  alpha() # 0.77

#Пересчитаем суммы
#Посчитаем веса
coefs <- data.frame(
  lat_var = unlist(model2_5@ParTable$lhs)[1:20],
  var = unlist(model2_5@ParTable$rhs)[1:20],
  coef = unlist(model2_5@ParTable$est)[1:20]
) %>% filter(var != "" & !is.na(var))

#Домножим на веса

# 1. Создаем именованный список коэффициентов
coefs_list <- set_names(coefs$coef, coefs$var)

# 2. Фильтруем только существующие переменные
valid_vars <- intersect(names(coefs_list), names(anket))

# 3. Создаем новые колонки с помощью map_dfc
new_cols <- map_dfc(.x = valid_vars,
                    .f = ~ anket[[.x]] * coefs_list[[.x]],
                    .id = "variable") %>%
  set_names(paste0("mdl2_5_", valid_vars))

# 4. Объединяем с исходными данными
anket <- bind_cols(anket, new_cols)
colnames(anket)
#Сами суммы
anket %>%
  mutate(
    # Исправляем дублирование имени mdl4_5_sum_3
    mdl2_5_sum = rowSums(
      pick(
        mdl2_5_klim4,
        mdl2_5_klim1,
        mdl2_5_klim2,
        mdl2_5_klim5,
        mdl2_5_klim6,
        mdl2_5_klim9,
        mdl2_5_klim8,
        mdl2_5_klim10,
        mdl2_5_klim12,
        mdl2_5_klim14,
        mdl2_5_klim16,
        mdl2_5_klim15,
        mdl2_5_klim17,
        mdl2_5_klim19,
        mdl2_5_klim21,
        mdl2_5_klim25,
        mdl2_5_klim22,
        mdl2_5_klim23,
        mdl2_5_klim26,
        mdl2_5_klim28
      )
    ),
    mdl2_5_sum_1 = rowSums(
      pick(
        mdl2_5_klim4,
        mdl2_5_klim1,
        mdl2_5_klim2,
        mdl2_5_klim5,
        mdl2_5_klim6
      )
    ),
    mdl2_5_sum_2 = rowSums(
      pick(
        mdl2_5_klim9,
        mdl2_5_klim8,
        mdl2_5_klim10,
        mdl2_5_klim12,
        mdl2_5_klim14
      )
    ),
    mdl2_5_sum_3 = rowSums(
      pick(
        mdl2_5_klim16,
        mdl2_5_klim15,
        mdl2_5_klim17,
        mdl2_5_klim19,
        mdl2_5_klim21
      )
    ),
    mdl2_5_sum_4 = rowSums(
      pick(
        mdl2_5_klim25,
        mdl2_5_klim22,
        mdl2_5_klim23,
        mdl2_5_klim26,
        mdl2_5_klim28
      )
    )  # Правильное имя для новой суммы
  ) -> anket
anket %>%
  select(last_col(offset = 29):last_col()) %>%
  glimpse()  # или head(), или View()

#МОДЕЛЬ 3 -----

#Попробуем ESEM

#Первое вращение, выбираем якоря
anket %>%
  select(klim1:klim28) %>%
  factanal(factors = 4,
           rotation = 'geominQ',
           delta = .4) ->
  anket.esem.efa
print(anket.esem.efa$loadings, cutoff = 0.4)

#Cтроим матрицу со всеми нагрузками
esem_loadings <-
  data.table(matrix(
    round(anket.esem.efa$loadings, 2),
    nrow = 28,
    ncol = 4
  ))

#Меняем формат матрицы, готовя к анализу
names(esem_loadings) <- c("F1", "F2", "F3", "F4")
esem_loadings$item <- anket %>%
  select(klim1:klim28) %>% colnames()
esem_loadings <-
  melt(esem_loadings, "item", variable.name = "latent")

#Устанавливаем якоря
anchors <-
  c(F1 = "klim4",
    F2 = "klim9",
    F3 = "klim16",
    F4 = "klim25")

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
esem_fit <- cfa(esem_model, select(anket, klim1:klim28), std.lv = T)
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

# Применение этого метода дало структуру слишком далую от нашего опросника +
# сложно интерпретируемю теоретически
# также часть вопросов попадала в несколько фаткоров

# Поробуем версию с удалёнными обратными вопросами

#МОДЕЛЬ 4 -----

#Попробуем ESEM

#Первое вращение, выбираем якоря
anket %>%
  select(
    c(
      klim1:klim2,
      klim4:klim6,
      klim8:klim10,
      klim12,
      klim14:klim17,
      klim19,
      klim21:klim23,
      klim25:klim26,
      klim28
    )
  ) %>%
  factanal(factors = 4,
           rotation = 'geominQ',
           delta = .4) ->
  anket.esem.efa
print(anket.esem.efa$loadings, cutoff = 0.4)

#Cтроим матрицу со всеми нагрузками
esem_loadings <-
  data.table(matrix(
    round(anket.esem.efa$loadings, 2),
    nrow = 20,
    ncol = 4
  ))

#Меняем формат матрицы, готовя к анализу
names(esem_loadings) <- c("F1", "F2", "F3", "F4")
esem_loadings$item <- anket %>%
  select(
    c(
      klim1:klim2,
      klim4:klim6,
      klim8:klim10,
      klim12,
      klim14:klim17,
      klim19,
      klim21:klim23,
      klim25:klim26,
      klim28
    )
  ) %>% colnames()
esem_loadings <-
  melt(esem_loadings, "item", variable.name = "latent")

#Устанавливаем якоря
anchors <-
  c(F1 = "klim10",
    F2 = "klim2",
    F3 = "klim25",
    F4 = "klim19")

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
esem_fit <- cfa(esem_model, select(anket, klim1:klim28), std.lv = T)
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


# Применение этого метода дало структуру похожую на наш опросник
# однако часть вопросов 3 и 4 факторов перемешались
# При этом нет вопросов попавших в один фактор

anket %>%
  select(
    c(
      klim1,
      klim2,
      klim4,
      klim5,
      klim6,
      klim8,
      klim9,
      klim10,
      klim12,
      klim14,
      klim15,
      klim16,
      klim22,
      klim23,
      klim28,
      klim19,
      klim21
    )
  ) %>%
  alpha() # 0.9

#Ну, кайф. Альфа Кронбаха ещё выше - 0.92
#Kaiser-Meyer-Olkin (KMO)
anket %>%
  select(
    c(
      klim1,
      klim2,
      klim4,
      klim5,
      klim6,
      klim8,
      klim9,
      klim10,
      klim12,
      klim14,
      klim15,
      klim16,
      klim22,
      klim23,
      klim28,
      klim19,
      klim21
    )
  ) %>%
  KMO() %>%
  .[["MSAi"]] %>% sort()

#CFA - once again

mdl4 <- (
  'F1 =~ klim1+klim2+klim4+klim5+klim6
          F2 =~ klim8+klim9+klim10+klim12+klim14
          F3 =~ klim15+klim16+klim22+klim23+klim28
          F4 =~ klim19+klim21'
)

model4 <- cfa(mdl4, data = anket)
summary(model4)

fitmeasures(model4, c("chisq", "cfi", "tli", "srmr", "rmsea"))

#Строим график
semPaths(
  model4,
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
  filename = 'thesis/thesis_model4',
  width = 11.7,
  height = 8.3,
  nodeLabels = labels_new
) # Увеличение отступов

modificationindices(model4) %>% filter(mi > 15) %>% arrange(-mi)

# Lavaan предлагает учесть следующие взаимодействия:
# Включить корелляцию klim8 ~~ klim10 - звучит оправдано
# Включить корелляцию klim5 ~~  klim6 - звучит оправдано
# Включить корелляцию klim2 ~~  klim6 - звучит оправдано

mdl4_5 <- (
  'F1 =~ klim4+klim1+klim2+klim5+klim6
          F2 =~ klim9+klim8+klim10+klim12+klim14
          F3 =~ klim15+klim16+klim22+klim23+klim28
          F4 =~ klim21+klim19
        klim8 ~~ klim10
         klim5 ~~  klim6
         klim2 ~~  klim6'
)


model4_5 <- cfa(mdl4_5, data = anket)
summary(model4_5)

fitmeasures(model4_5, c("chisq", "cfi", "tli", "srmr", "rmsea"))

write.csv(fitmeasures(model4_5, c("chisq", "cfi", "tli", "srmr", "rmsea")),
          paste0(getwd(), '/thesis/cfa4_5.quality.csv'))

# В целом приемлемые показатели

#Строим график

# Получаем вектор всех имён узлов
model_plot <- semPlot::semPlotModel(model4_5)

labels_all <- model_plot@Vars$name

# Создаём копию
labels_new <- labels_all

# Меняем только латентные переменные
labels_new[labels_new == "F1"] <- "Связь с коллегами"
labels_new[labels_new == "F2"] <- "Польза труда"
labels_new[labels_new == "F3"] <- "Страсть"
labels_new[labels_new == "F4"] <- "Инструментальный\nаспект"

#Строим график
semPaths(
  model4_5,
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
  filename = 'thesis/thesis_model4_5',
  width = 11.7,
  height = 8.3,
  nodeLabels = labels_new
) # Увеличение отступов

#Посмотрим на надёжность субшкал

anket %>%
  select(c(klim1, klim2, klim4, klim5, klim6)) %>%
  alpha() # 0.8

anket %>%
  select(c(klim8, klim9, klim10, klim12, klim14)) %>%
  alpha() # 0.91

anket %>%
  select(c(klim19, klim21)) %>%
  alpha() # 0.61

anket %>%
  select(c(klim15, klim16, klim22, klim23, klim28)) %>%
  alpha() # 0.73

#Ну, выберем из двух моделек одну

fitmeasures(model2_5, c("chisq", "cfi", "tli", "srmr", "rmsea"))
fitmeasures(model4_5, c("chisq", "cfi", "tli", "srmr", "rmsea"))

#Обе модели показывают удовлетворительные показатели, хотя вторая имеет лучшие
#Смысловая структура у них похожа, но первая легче интерпретируется

# Надёжность первых двух субшкал в обеих моделях отличная и хорошая
# Надёжность двух других субшкал умеренные и хорошие

# Однозначно определить лидера не получается
# Проведём оценку внешней валидности для обеих моделей и таким образом выберем лучшую

#Пересчитаем суммы
#Посчитаем веса
coefs <- data.frame(
  lat_var = unlist(model4_5@ParTable$lhs)[1:17],
  var = unlist(model4_5@ParTable$rhs)[1:17],
  coef = unlist(model4_5@ParTable$est)[1:17]
) %>% filter(var != "" & !is.na(var))

#Домножим на веса

# 1. Создаем именованный список коэффициентов
coefs_list <- set_names(coefs$coef, coefs$var)

# 2. Фильтруем только существующие переменные
valid_vars <- intersect(names(coefs_list), names(anket))

# 3. Создаем новые колонки с помощью map_dfc
new_cols <- map_dfc(.x = valid_vars,
                    .f = ~ anket[[.x]] * coefs_list[[.x]],
                    .id = "variable") %>%
  set_names(paste0("mdl4_5_", valid_vars))

# 4. Объединяем с исходными данными
anket <- bind_cols(anket, new_cols)
colnames(anket)
#Сами суммы
anket %>%
  mutate(
    # Исправляем дублирование имени mdl4_5_sum_3
    mdl4_5_sum = rowSums(
      pick(
        mdl4_5_klim4,
        mdl4_5_klim1,
        mdl4_5_klim2,
        mdl4_5_klim5,
        mdl4_5_klim6,
        mdl4_5_klim9,
        mdl4_5_klim8,
        mdl4_5_klim10,
        mdl4_5_klim12,
        mdl4_5_klim14,
        mdl4_5_klim15,
        mdl4_5_klim16,
        mdl4_5_klim22,
        mdl4_5_klim23,
        mdl4_5_klim28,
        mdl4_5_klim21, 
        mdl4_5_klim19
      )
    ),
    mdl4_5_sum_1 = rowSums(
      pick(
        mdl4_5_klim4,
        mdl4_5_klim1,
        mdl4_5_klim2,
        mdl4_5_klim5,
        mdl4_5_klim6
      )
    ),
    mdl4_5_sum_2 = rowSums(
      pick(
        mdl4_5_klim9,
        mdl4_5_klim8,
        mdl4_5_klim10,
        mdl4_5_klim12,
        mdl4_5_klim14
      )
    ),
    mdl4_5_sum_4 = rowSums(
      pick(
        mdl4_5_klim15,
        mdl4_5_klim16,
        mdl4_5_klim22,
        mdl4_5_klim23,
        mdl4_5_klim28
      )
    ),
    mdl4_5_sum_3 = rowSums(pick(mdl4_5_klim21, mdl4_5_klim19))  # Правильное имя для новой суммы
  ) -> anket


#Сохраним результаты для дальнешей работы: ----

anket %>%
  write_csv(paste0(getwd(), '/thesis/anket_an_1.csv'))
