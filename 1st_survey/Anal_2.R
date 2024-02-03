#Импорт библиотек -----
library(tidyverse)
library(rstatix)
library(glmnet)
library(rempsyc)

anket_an_1 <- read_csv(paste0(getwd(),'/1st_survey/anket_an_1.csv'))

#Конвергентная валидность -----

cor(select(anket_an_1, c(a_sum_1, c_sum_5)))
cor(select(anket_an_1, c(a_sum_2, c_sum_2, c_sum_5, b_sum_1, b_sum_4)))
cor(select(anket_an_1, c(a_sum_3, c_sum_4, b_sum_5, b_sum_6)))

#Посмотрим соц-дем ----

#Пол
anket_an_1 %>% filter(d1 %in% c('Женщина','Мужчина')) %>% t_test(a_sum_1 ~ d1) # незначимо
ggplot(anket_an_1, aes(x=d1, y=a_sum_1)) + 
  geom_boxplot()
anket_an_1 %>% filter(d1 %in% c('Женщина','Мужчина')) %>% t_test(a_sum_2 ~ d1) # незначимо
ggplot(anket_an_1, aes(x=d1, y=a_sum_2)) + 
  geom_boxplot()
anket_an_1 %>% filter(d1 %in% c('Женщина','Мужчина')) %>% t_test(a_sum_3 ~ d1) # незначимо
ggplot(anket_an_1, aes(x=d1, y=a_sum_3)) + 
  geom_boxplot()

#Возраст
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

#Строим Ридж-регрессию для желания оставить работу ----

#Уберём неуверенных
anket_an_1 %>% filter(d12 != 'Затрудняюсь ответить') -> anket_ridg

#Подготовим переменные
y <- anket_ridg$d12 == 'Да'

names(anket_ridg)

x <- data.matrix(anket_ridg[, c("a_sum_1", "a_sum_2", "a_sum_3", "c_sum_1", 
                                "c_sum_2", "c_sum_3", "c_sum_4", "c_sum_5")])
#Модель
model <- glmnet(x, y, alpha = 0)

#Найдём оптимальную лямбду
#k-fold cross-validation
cv_model <- cv.glmnet(x, y, alpha = 0)

best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model) 

#Посчитаем итоговую модельку
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)

#Давайте R2 посмотрим
y_predicted <- predict(model, s = best_lambda, newx = x)

sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

rsq <- 1 - sse/sst
rsq

# Табличка

tb <- tibble(c('Интерцепт',
               'Качество исполнения работы',
               'Польза для людей',
               'Связь с коллегами',
               'Удовлетворенность заработной платой', 
               'Удовлетворенность организацией труда', 
               'Удовлетворенность руководством', 
               'Удовлетворенность коллективом', 
               'Удовлетворенность процессом и содержанием труда'),coef(best_model)[,1])
names(tb) <- c('Предиктор', 'Коэффициент')

my_table <- nice_table(
  tb,
  title = c("Таблица 1", "Предикторы желания покинуть работу"),
  note = c(
    "Коэффициенты получены в результате применения Ридж-регресси с лямбда = 0.4"
  )
)

flextable::save_as_docx(my_table, path = paste0(getwd(),'/1st_survey/ridge.docx'))
