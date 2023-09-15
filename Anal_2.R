library(tidyverse)
library(rstatix)

anket_an_1 <- read_csv('anket_an_1.csv')

#Конвергентная валидность -----

cor(select(anket_an_1, c(a_sum_1, c_sum_5)))
cor(select(anket_an_1, c(a_sum_2, c_sum_2, c_sum_5, b_sum_1, b_sum_4)))
cor(select(anket_an_1, c(a_sum_3, c_sum_4, b_sum_5, b_sum_6)))

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

cor.test(anket_an_1$d2,anket_an_1$a_sum_1) # значимо
ggplot(anket_an_1, aes(x=d2, y=a_sum_1)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$d2,anket_an_1$a_sum_2) # незначимо
ggplot(anket_an_1, aes(x=d2, y=a_sum_2)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$d2,anket_an_1$a_sum_3) # незначимо
ggplot(anket_an_1, aes(x=d2, y=a_sum_3)) + 
  geom_point() +
  geom_smooth(method = lm)

#Глубже исследуем связь первого фактора и удовлетворённости трудом ----
#Для начала создадим категориальную переменную для первого фактора
#Разобъём на три категории по 15 и 85 перцентилям

hist(anket_an_1$a_sum_1,15)
per <- quantile(anket_an_1$a_sum_1, c(.15, .85))

anket_an_1 %>% mutate(a1_cat = 'Норм') %>%  
  mutate(a1_cat = case_when(a_sum_1 <= per[1] ~ 'Низк',
                            a_sum_1 >= per[2] ~ 'Высок',
                            .default = "Норм")) -> anket_an_1

#Проведём t-тесты

#Удовлетворенность заработной платой - незначимо
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  t_test(c_sum_1 ~ a1_cat)
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  cohens_d(
    c_sum_1 ~ a1_cat, var.equal = TRUE, 
    hedges.correction = TRUE
  )
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  ggplot(aes(x=a1_cat, y=c_sum_1)) + 
  geom_boxplot()
#Удовлетворенность организацией труда - значимо (moderate)
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  t_test(c_sum_2 ~ a1_cat)
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  cohens_d(
    c_sum_2 ~ a1_cat, var.equal = TRUE, 
    hedges.correction = TRUE
  )
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  ggplot(aes(x=a1_cat, y=c_sum_2)) + 
  geom_boxplot()
#Удовлетворенность руководством - значимо (moderate)
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  t_test(c_sum_3 ~ a1_cat)
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  cohens_d(
    c_sum_3 ~ a1_cat, var.equal = TRUE, 
    hedges.correction = TRUE
  )
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  ggplot(aes(x=a1_cat, y=c_sum_3)) + 
  geom_boxplot()
#Удовлетворенность коллективом - значимо (moderate)
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  t_test(c_sum_4 ~ a1_cat)
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  cohens_d(
    c_sum_4 ~ a1_cat, var.equal = TRUE, 
    hedges.correction = TRUE
  )
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  ggplot(aes(x=a1_cat, y=c_sum_4)) + 
  geom_boxplot()
#Удовлетворенность процессом и содержанием труда - значимо (large)
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  t_test(c_sum_5 ~ a1_cat)
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  cohens_d(
    c_sum_5 ~ a1_cat, var.equal = TRUE, 
    hedges.correction = TRUE
  )
anket_an_1 %>% filter(a1_cat %in% c('Высок','Низк')) %>% 
  ggplot(aes(x=a1_cat, y=c_sum_5)) + 
  geom_boxplot()