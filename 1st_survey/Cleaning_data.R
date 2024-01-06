#Готовимся к работе -----

#Импорт библиотек
library(tidyverse)

#Импортируем грязные данные
anket <- read_csv2(paste0(getwd(),'/1st_survey/anket.csv'))

#Прописвыаем функции

# Три функции, которые переводят из тех вариантов, которые давали респам, в числа
# Функция переводит сначала char в иерархический фактор, а потом numeric
# В результатена месте слова остаётся её место в порядке 


words_to_num1 <- function(x){y <- as.numeric(factor(x,
  levels = c('Практически никогда', 'Редко', 'Скорее редко',
             'Иногда', 'Скорее часто', 'Часто', 'Практически всегда'), 
  ordered = TRUE))
}

words_to_num2 <- function(x){y <- as.numeric(factor(x,
                                                    levels = c('Абсолютно не согласен', 
                                                               'Частично согласен', 
                                                               'Скорее согласен, чем не согласен', 
                                                               'Полностью согласен'), 
                                                    ordered = TRUE))
}

words_to_num3 <- function(x){y <- as.numeric(factor(x,
                                                    levels = c('Не согласен', 
                                                               'Скорее не согласен', 
                                                               'Где-то посередине', 
                                                               'Скорее согласен', 
                                                               'Согласен'), 
                                                    ordered = TRUE))
}

#Чистим данные ----

# Дадим имена соответствующие
new_names <- c('ID', 
               paste0('d',1:12), # Демографические вопросы
               paste0('a',1:56), # Наша анкета
               paste0('b',1:27), # Проактивный копинг
               paste0('c',1:19)) # Удовлетворённость работой
colnames(anket) <- new_names

#Применяем 3 функции (потому что разный ликерт), каждую к своей шкале
anket %>% mutate(across(a1:a56, words_to_num1)) -> anket
anket %>% mutate(across(b1:b27, words_to_num2)) -> anket
anket %>% mutate(across(c1:c19, words_to_num3)) -> anket

#Меняем тип данных
anket %>% 
  mutate(across(d1, as.factor)) %>% 
  mutate(across(c(d3:d8), as.factor)) %>% 
  mutate(across(c(d11:d12), as.factor)) -> anket

#Сделаем ревёрс факторов, которые должны быть отрицательными (наш опросник)
anket %>% 
  mutate(across(c(a4, a5, a9, a10, a17, a18, a21, a22, a26, a27, a33, a34, 
                  a36, a48, a54, c8, c10, c11), as.factor)) %>% 
  mutate(across(c(a4, a5, a9, a10, a17, a18, a21, a22, a26, a27, a33, a34, 
                  a36, a48, a54, c8, c10, c11), fct_rev)) %>% 
  mutate(across(c(a4, a5, a9, a10, a17, a18, a21, a22, a26, a27, a33, a34, 
                  a36, a48, a54, c8, c10, c11), as.numeric)) -> anket

#Посчитаем суммы факторов
anket %>% 
  mutate(
    a_sum_1 = rowSums(across(c(a1:a13))),
    a_sum_2 = rowSums(across(c(a14:a27))),
    a_sum_3 = rowSums(across(c(a28:a42))),
    a_sum_4 = rowSums(across(c(a43:a56))),
    b_sum_1 = rowSums(across(c(b1:b6))),
    b_sum_2 = rowSums(across(c(b7:b11))),
    b_sum_3 = rowSums(across(c(b12:b14))),
    b_sum_4 = rowSums(across(c(b15:b19))),
    b_sum_5 = rowSums(across(c(b20:b23))),
    b_sum_6 = rowSums(across(c(b24:b27))),
    c_sum_1 = rowSums(across(c(c1:c4))),
    c_sum_2 = rowSums(across(c(c5:c8))),
    c_sum_3 = rowSums(across(c(c9:c11))),
    c_sum_4 = rowSums(across(c(c12:c14))),
    c_sum_5 = rowSums(across(c(c15:c19)))
  )-> anket

#Сохраняем почищенный файл
write_csv(anket, paste0(getwd(),'/1st_survey/anket_clean.csv'))

