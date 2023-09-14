install.packages('rstatix')
library(tidyverse)
library(rstatix)

anket_an_1 <- read_csv('anket_an_1.csv')

#Конвергентная валидность -----

cor(select(anket_an_1, c(a_sum_1, c_sum_5)))
cor(select(anket_an_1, c(a_sum_2, c_sum_2, c_sum_5, b_sum_1, b_sum_4)))
cor(select(anket_an_1, c(a_sum_1, c_sum_4, b_sum_5, b_sum_6)))

#Посмотрим соц-дем ----
anket_an_1 %>% filter(d1 %in% c('Женщина','Мужчина')) %>% t_test(a_sum_1 ~ d1) # незначимо
anket_an_1 %>% filter(d1 %in% c('Женщина','Мужчина')) %>% t_test(a_sum_2 ~ d1) # незначимо
anket_an_1 %>% filter(d1 %in% c('Женщина','Мужчина')) %>% t_test(a_sum_3 ~ d1) # незначимо

cor.test(anket_an_1$d2,anket_an_1$a_sum_1) # незначимо
cor.test(anket_an_1$d2,anket_an_1$a_sum_2) # значимо
cor.test(anket_an_1$d2,anket_an_1$a_sum_3) # незначимо