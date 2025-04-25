#Готовимся к работе -----

#Импорт библиотек
library(tidyverse)

#Прописвыаем функции

# Пять функциий, которые переводят из тех вариантов, которые давали респам, в числа
# Функция переводит сначала char в иерархический фактор, а потом numeric
# В результатена месте слова остаётся её место в порядке


words_to_num1 <- function(x) {
  y <- as.numeric(factor(
    x,
    levels = c(
      'Практически никогда',
      'Редко',
      'Скорее редко',
      'Иногда',
      'Скорее часто',
      'Часто',
      'Практически всегда'
    ),
    ordered = TRUE
  ))
}


words_to_num2 <- function(x) {
  y <- as.numeric(factor(
    x,
    levels = c(
      'Не согласен',
      'Скорее не согласен',
      'Где-то посередине',
      'Скорее согласен',
      'Согласен'
    ),
    ordered = TRUE
  ))
}

words_to_num3 <- function(x) {
  y <- as.numeric(factor(
    x,
    levels = c(
      'Категорически не согласен',
      'Не согласен',
      'Скорее не согласен, чем согласен',
      'Затрудняюсь ответить',
      'Скорее согласен, чем не согласен',
      'Согласен',
      'Абсолютно согласен'
    ),
    ordered = TRUE
  ))
}

words_to_num4 <- function(x) {
  y <- as.numeric(factor(
    x,
    levels = c(
      'Полностью не согласен',
      'Не согласен',
      'Скорее не согласен',
      'Нечто среднее',
      'Скорее согласен',
      'Согласен',
      'Полностью согласен'
    ),
    ordered = TRUE
  ))
}

#Импортируем грязные данные
anket <- read_csv2(paste0(getwd(), '/thesis/anket.csv'))

#Чистим данные ----

# Дадим имена соответствующие
new_names <- c(
  paste0('dem', 1:12),# Демографические вопросы
  paste0('klim', 1:28),# Наша анкета
  paste0('job_sat', 1:19),# Удовлетворённость работой
  paste0('tol_intol', 1:13),# Толерантность и интолерантность к неопр
  paste0('life_sat', 1:5),# Удовлетворённость жизнью
  paste0('purp', 1:5),# Наличие смысла
  paste0('neuro', 1:2) # Нейротизм
)

colnames(anket) <- new_names

#Применяем 3 функции (потому что разный ликерт), каждую к своей шкале
anket %>% mutate(across(klim1:klim28, words_to_num1)) -> anket
anket %>% mutate(across(job_sat1:job_sat19, words_to_num2)) -> anket
anket %>% mutate(across(tol_intol1:tol_intol13, words_to_num3)) -> anket
anket %>% mutate(across(life_sat1:life_sat5, words_to_num4)) -> anket
anket %>% mutate(across(purp1:purp5, words_to_num4)) -> anket
anket %>% mutate(across(neuro1:neuro2, words_to_num4)) -> anket


#Меняем тип данных
anket %>%
  mutate(across(dem1, as.factor)) %>%
  mutate(across(c(dem3:dem8), as.factor)) %>%
  mutate(across(c(dem11:dem12), as.factor)) -> anket


#Сделаем ревёрс факторов, которые должны быть отрицательными
anket %>%
  mutate(across(
    c(
      klim3,
      klim7,
      klim11,
      klim13,
      klim18,
      klim20,
      klim22,
      klim24,
      klim27,
      job_sat8,
      job_sat10,
      job_sat11,
      purp5,
      neuro2
    ),
    as.factor
  )) %>%
  mutate(across(
    c(
      klim3,
      klim7,
      klim11,
      klim13,
      klim18,
      klim20,
      klim22,
      klim24,
      klim27,
      job_sat8,
      job_sat10,
      job_sat11,
      purp5,
      neuro2
    ),
    fct_rev
  )) %>%
  mutate(across(
    c(
      klim3,
      klim7,
      klim11,
      klim13,
      klim18,
      klim20,
      klim22,
      klim24,
      klim27,
      job_sat8,
      job_sat10,
      job_sat11,
      purp5,
      neuro2
    ),
    as.numeric
  )) -> anket

#Посчитаем суммы факторов
anket %>%
  mutate(
    klim_sum_1 = rowSums(across(c(
      klim1: klim7
    ))),
    klim_sum_2 = rowSums(across(c(
      klim8: klim14
    ))),
    klim_sum_3 = rowSums(across(c(
      klim15: klim21
    ))),
    klim_sum_4 = rowSums(across(c(
      klim22: klim28
    ))),
    job_sat_sum_1 = rowSums(across(c(job_sat1:job_sat4))),
    job_sat_sum_2 = rowSums(across(c(job_sat5:job_sat8))),
    job_sat_sum_3 = rowSums(across(c(job_sat9:job_sat11))),
    job_sat_sum_4 = rowSums(across(c(job_sat12:job_sat14))),
    job_sat_sum_5 = rowSums(across(c(job_sat15:job_sat19))),
    tol_intol_sum_1 = rowSums(across(c(tol_intol1:tol_intol7))),
    tol_intol_sum_2 = rowSums(across(c(tol_intol8:tol_intol13))),
    life_sat_sum_1 = rowSums(across(c(life_sat1:life_sat5))),
    purp_sum_1 = rowSums(across(c(purp1:purp5))),
    neuro_sum_1 = rowSums(across(c(neuro1:neuro2)))
  ) -> anket

drop_na(anket) -> anket

# Сохраняем почищенный файл
write_csv(anket, paste0(getwd(), '/thesis/anket_clean.csv'))
