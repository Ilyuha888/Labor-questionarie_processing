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
ggplot(anket_an_1, aes(x=d1, y=a_sum_1)) + 
  geom_boxplot()
anket_an_1 %>% filter(d1 %in% c('Женщина','Мужчина')) %>% t_test(a_sum_2 ~ d1) # незначимо
ggplot(anket_an_1, aes(x=d1, y=a_sum_2)) + 
  geom_boxplot()
anket_an_1 %>% filter(d1 %in% c('Женщина','Мужчина')) %>% t_test(a_sum_3 ~ d1) # незначимо
ggplot(anket_an_1, aes(x=d1, y=a_sum_3)) + 
  geom_boxplot()

cor.test(anket_an_1$d2,anket_an_1$a_sum_1) # незначимо
ggplot(anket_an_1, aes(x=d2, y=a_sum_1)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$d2,anket_an_1$a_sum_2) # значимо
ggplot(anket_an_1, aes(x=d2, y=a_sum_2)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$d2,anket_an_1$a_sum_3) # незначимо
ggplot(anket_an_1, aes(x=d2, y=a_sum_3)) + 
  geom_point() +
  geom_smooth(method = lm)
