#Импорт библиотек -----
library(tidyverse)
library(rstatix)
library(glmnet)
library(rempsyc)

anket_an_1 <- read_csv(paste0(getwd(),'/thesis//anket_an_1.csv'))


#МОДЕЛЬ 2_5 -----

#Посмотрим соц-дем ----

#Пол
anket_an_1 %>% filter(dem1 %in% c('Женщина','Мужчина')) %>% t_test(mdl2_5_sum_1 ~ dem1) # незначимо
ggplot(anket_an_1, aes(x=dem1, y=mdl2_5_sum_1)) + 
  geom_boxplot()
anket_an_1 %>% filter(dem1 %in% c('Женщина','Мужчина')) %>% t_test(mdl2_5_sum_2 ~ dem1) # незначимо
ggplot(anket_an_1, aes(x=dem1, y=mdl2_5_sum_2)) + 
  geom_boxplot()
anket_an_1 %>% filter(dem1 %in% c('Женщина','Мужчина')) %>% t_test(mdl2_5_sum_3 ~ dem1) # незначимо
ggplot(anket_an_1, aes(x=dem1, y=mdl2_5_sum_3)) + 
  geom_boxplot()
anket_an_1 %>% filter(dem1 %in% c('Женщина','Мужчина')) %>% t_test(mdl2_5_sum_4 ~ dem1) # незначимо
ggplot(anket_an_1, aes(x=dem1, y=mdl2_5_sum_3)) + 
  geom_boxplot()

#Возраст
cor.test(anket_an_1$dem2,anket_an_1$mdl2_5_sum_1) # незначимо
ggplot(anket_an_1, aes(x=dem2, y=mdl2_5_sum_1)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$dem2,anket_an_1$mdl2_5_sum_2) # незначимо
ggplot(anket_an_1, aes(x=dem2, y=mdl2_5_sum_2)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$dem2,anket_an_1$mdl2_5_sum_3) # незначимо
ggplot(anket_an_1, aes(x=dem2, y=mdl2_5_sum_3)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$dem2,anket_an_1$mdl2_5_sum_4) # значимо
ggplot(anket_an_1, aes(x=dem2, y=mdl2_5_sum_4)) + 
  geom_point() +
  geom_smooth(method = lm)

# Более взрослые люди демонстрируют менее выраженную аффективную связь

#Конвергентная валидность -----
## Удовлетворённость трудом
cor.test(anket_an_1$job_sat_sum,anket_an_1$mdl2_5_sum) # значимо
ggplot(anket_an_1, aes(x=job_sat_sum, y=mdl2_5_sum)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$job_sat_sum,anket_an_1$mdl2_5_sum_1) # значимо
ggplot(anket_an_1, aes(x=job_sat_sum, y=mdl2_5_sum_1)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$job_sat_sum,anket_an_1$mdl2_5_sum_2) # значимо
ggplot(anket_an_1, aes(x=job_sat_sum, y=mdl2_5_sum_2)) + 
  geom_point() +
  geom_smooth(method = lm)

#Создадим категориальную переменную для 4го фактора
#Разобъём на три категории по 25 и 75 перцентилям

hist(anket_an_1$mdl2_5_sum_4,15)
per <- quantile(anket_an_1$mdl2_5_sum_4, c(.25, .75))

anket_an_1 %>% mutate(mdl2_5_sum_4_cat = 'Норм') %>%  
  mutate(mdl2_5_sum_4_cat = case_when(mdl2_5_sum_4 <= per[1] ~ '1. Низк',
                                      mdl2_5_sum_4 >= per[2] ~ '3. Высок',
                                      .default = "2. Норм")) -> anket_an_1
# Анова

anova_model <- aov(job_sat_sum ~ mdl2_5_sum_4_cat, data = anket_an_1)
summary(anova_model)

# Улучшенная версия с доверительными интервалами
ggplot(anket_an_1, aes(x = mdl2_5_sum_4_cat, y = job_sat_sum)) +
  geom_boxplot(width = 0.5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2, color = "red") +
  geom_jitter(width = 0.1, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Сравнение групп с доверительными интервалами",
       subtitle = "Красные линии показывают 95% доверительные интервалы для средних",
       x = "Категории",
       y = "Количественный показатель")

#Проведём t-тесты

# Поправка Тьюки
TukeyHSD(anova_model)

# Попарные t-тесты с поправкой Бонферрони
pairwise.t.test(anket_an_1$job_sat_sum, 
                anket_an_1$mdl2_5_sum_4_cat,
                p.adjust.method = "bonferroni")

# Везде всё значимо, но наше предположение о U-образной фомре не удовлетворяется

## Шкала толерантности и интолерантности к неопределённости
cor.test(anket_an_1$tol_intol_sum_1,anket_an_1$mdl2_5_sum_3) # незначимо
ggplot(anket_an_1, aes(x=tol_intol_sum_1, y=mdl2_5_sum)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$tol_intol_sum_2,anket_an_1$mdl2_5_sum_3) # значимо
ggplot(anket_an_1, aes(x=tol_intol_sum_2, y=mdl2_5_sum)) + 
  geom_point() +
  geom_smooth(method = lm)

# Предположение относительно толерантности выполнилось, относительно интолерантности - нет

## Шкала толерантности и интолерантности к неопределённости
cor.test(anket_an_1$tol_intol_sum_1,anket_an_1$mdl2_5_sum_3) # незначимо
ggplot(anket_an_1, aes(x=tol_intol_sum_1, y=mdl2_5_sum_3)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$tol_intol_sum_2,anket_an_1$mdl2_5_sum) # значимо
ggplot(anket_an_1, aes(x=tol_intol_sum_2, y=mdl2_5_sum)) + 
  geom_point() +
  geom_smooth(method = lm)

# Предположение относительно толерантности выполнилось, относительно инструментального аспекта - нет

## Шкала удовлетворённости жизнью
cor.test(anket_an_1$life_sat_sum_1,anket_an_1$mdl2_5_sum) # значимо
ggplot(anket_an_1, aes(x=life_sat_sum_1, y=mdl2_5_sum)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$life_sat_sum_1,anket_an_1$mdl2_5_sum_1) # значимо
ggplot(anket_an_1, aes(x=life_sat_sum_1, y=mdl2_5_sum_1)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$life_sat_sum_1,anket_an_1$mdl2_5_sum_2) # значимо
ggplot(anket_an_1, aes(x=life_sat_sum_1, y=mdl2_5_sum_2)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$life_sat_sum_1,anket_an_1$mdl2_5_sum_3) # значимо
ggplot(anket_an_1, aes(x=life_sat_sum_1, y=mdl2_5_sum_3)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$life_sat_sum_1,anket_an_1$mdl2_5_sum_4) # значимо
ggplot(anket_an_1, aes(x=life_sat_sum_1, y=mdl2_5_sum_4)) + 
  geom_point() +
  geom_smooth(method = lm)

# Связь есть, но отдельные субшкалы не так сильно друг от друга выделяются

## Субшкала наличия смысла из опросника смысла жизни

cor.test(anket_an_1$purp_sum_1,anket_an_1$mdl2_5_sum) # значимо
ggplot(anket_an_1, aes(x=purp_sum_1, y=mdl2_5_sum)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$purp_sum_1,anket_an_1$mdl2_5_sum_1) # значимо
ggplot(anket_an_1, aes(x=purp_sum_1, y=mdl2_5_sum_1)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$purp_sum_1,anket_an_1$mdl2_5_sum_2) # значимо
ggplot(anket_an_1, aes(x=purp_sum_1, y=mdl2_5_sum_2)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$purp_sum_1,anket_an_1$mdl2_5_sum_3) # значимо
ggplot(anket_an_1, aes(x=purp_sum_1, y=mdl2_5_sum_3)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$purp_sum_1,anket_an_1$mdl2_5_sum_4) # значимо
ggplot(anket_an_1, aes(x=purp_sum_1, y=mdl2_5_sum_4)) + 
  geom_point() +
  geom_smooth(method = lm)

# Ожидаемая корреляция есть, но самым сильным фактором оказалась связь с коллегами

## Шкала нейротизма из краткого опросника большой пятёрки

# Анова

anova_model <- aov(neuro_sum_1 ~ mdl2_5_sum_4_cat, data = anket_an_1)
summary(anova_model)

# Улучшенная версия с доверительными интервалами
ggplot(anket_an_1, aes(x = mdl2_5_sum_4_cat, y = neuro_sum_1)) +
  geom_boxplot(width = 0.5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2, color = "red") +
  geom_jitter(width = 0.1, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Сравнение групп с доверительными интервалами",
       subtitle = "Красные линии показывают 95% доверительные интервалы для средних",
       x = "Категории",
       y = "Количественный показатель")

#Проведём t-тесты

# Поправка Тьюки
TukeyHSD(anova_model)

# Попарные t-тесты с поправкой Бонферрони
pairwise.t.test(anket_an_1$neuro_sum_1, 
                anket_an_1$mdl2_5_sum_4_cat,
                p.adjust.method = "bonferroni")

# Мы получили статистически значимое отличие только "Высокой группы" от все остальных
# U-образной зависимости не вышло


#МОДЕЛЬ 4_5 -----

#Посмотрим соц-дем ----

#Пол
anket_an_1 %>% filter(dem1 %in% c('Женщина','Мужчина')) %>% t_test(mdl4_5_sum_1 ~ dem1) # незначимо
ggplot(anket_an_1, aes(x=dem1, y=mdl4_5_sum_1)) + 
  geom_boxplot()
anket_an_1 %>% filter(dem1 %in% c('Женщина','Мужчина')) %>% t_test(mdl4_5_sum_2 ~ dem1) # незначимо
ggplot(anket_an_1, aes(x=dem1, y=mdl4_5_sum_2)) + 
  geom_boxplot()
anket_an_1 %>% filter(dem1 %in% c('Женщина','Мужчина')) %>% t_test(mdl4_5_sum_3 ~ dem1) # незначимо
ggplot(anket_an_1, aes(x=dem1, y=mdl4_5_sum_3)) + 
  geom_boxplot()
anket_an_1 %>% filter(dem1 %in% c('Женщина','Мужчина')) %>% t_test(mdl4_5_sum_4 ~ dem1) # незначимо
ggplot(anket_an_1, aes(x=dem1, y=mdl4_5_sum_3)) + 
  geom_boxplot()

#Возраст
cor.test(anket_an_1$dem2,anket_an_1$mdl4_5_sum_1) # незначимо
ggplot(anket_an_1, aes(x=dem2, y=mdl4_5_sum_1)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$dem2,anket_an_1$mdl4_5_sum_2) # незначимо
ggplot(anket_an_1, aes(x=dem2, y=mdl4_5_sum_2)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$dem2,anket_an_1$mdl4_5_sum_3) # незначимо
ggplot(anket_an_1, aes(x=dem2, y=mdl4_5_sum_3)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$dem2,anket_an_1$mdl4_5_sum_4) # незначимо
ggplot(anket_an_1, aes(x=dem2, y=mdl4_5_sum_4)) + 
  geom_point() +
  geom_smooth(method = lm)

#Конвергентная валидность -----
## Удовлетворённость трудом
cor.test(anket_an_1$job_sat_sum,anket_an_1$mdl4_5_sum) # значимо
ggplot(anket_an_1, aes(x=job_sat_sum, y=mdl4_5_sum)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$job_sat_sum,anket_an_1$mdl4_5_sum_1) # значимо
ggplot(anket_an_1, aes(x=job_sat_sum, y=mdl4_5_sum_1)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$job_sat_sum,anket_an_1$mdl4_5_sum_2) # значимо
ggplot(anket_an_1, aes(x=job_sat_sum, y=mdl4_5_sum_2)) + 
  geom_point() +
  geom_smooth(method = lm)

#Создадим категориальную переменную для 4го фактора
#Разобъём на три категории по 25 и 75 перцентилям

hist(anket_an_1$mdl4_5_sum_4,15)
per <- quantile(anket_an_1$mdl4_5_sum_4, c(.25, .75))

anket_an_1 %>% mutate(mdl4_5_sum_4_cat = 'Норм') %>%  
  mutate(mdl4_5_sum_4_cat = case_when(mdl4_5_sum_4 <= per[1] ~ '1. Низк',
                            mdl4_5_sum_4 >= per[2] ~ '3. Высок',
                            .default = "2. Норм")) -> anket_an_1
# Анова

anova_model <- aov(job_sat_sum ~ mdl4_5_sum_4_cat, data = anket_an_1)
summary(anova_model)

# Улучшенная версия с доверительными интервалами
ggplot(anket_an_1, aes(x = mdl4_5_sum_4_cat, y = job_sat_sum)) +
  geom_boxplot(width = 0.5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2, color = "red") +
  geom_jitter(width = 0.1, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Сравнение групп с доверительными интервалами",
       subtitle = "Красные линии показывают 95% доверительные интервалы для средних",
       x = "Категории",
       y = "Количественный показатель")

#Проведём t-тесты

# Поправка Тьюки
TukeyHSD(anova_model)

# Попарные t-тесты с поправкой Бонферрони
pairwise.t.test(anket_an_1$job_sat_sum, 
                anket_an_1$mdl4_5_sum_4_cat,
                p.adjust.method = "bonferroni")

# Везде всё значимо, но наше предположение о U-образной фомре не удовлетворяется

## Шкала толерантности и интолерантности к неопределённости
cor.test(anket_an_1$tol_intol_sum_1,anket_an_1$mdl4_5_sum_3) # незначимо
ggplot(anket_an_1, aes(x=tol_intol_sum_1, y=mdl4_5_sum_3)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$tol_intol_sum_2,anket_an_1$mdl4_5_sum) # значимо
ggplot(anket_an_1, aes(x=tol_intol_sum_2, y=mdl4_5_sum)) + 
  geom_point() +
  geom_smooth(method = lm)

# Предположение относительно толерантности выполнилось, относительно интолерантности - нет

## Шкала толерантности и интолерантности к неопределённости
cor.test(anket_an_1$tol_intol_sum_1,anket_an_1$mdl4_5_sum_3) # незначимо
ggplot(anket_an_1, aes(x=tol_intol_sum_1, y=mdl4_5_sum_3)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$tol_intol_sum_2,anket_an_1$mdl4_5_sum) # значимо
ggplot(anket_an_1, aes(x=tol_intol_sum_2, y=mdl4_5_sum)) + 
  geom_point() +
  geom_smooth(method = lm)

# Предположение относительно толерантности выполнилось, относительно инструментального аспекта - нет

## Шкала удовлетворённости жизнью
cor.test(anket_an_1$life_sat_sum_1,anket_an_1$mdl4_5_sum) # значимо
ggplot(anket_an_1, aes(x=life_sat_sum_1, y=mdl4_5_sum)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$life_sat_sum_1,anket_an_1$mdl4_5_sum_1) # значимо
ggplot(anket_an_1, aes(x=life_sat_sum_1, y=mdl4_5_sum_1)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$life_sat_sum_1,anket_an_1$mdl4_5_sum_2) # значимо
ggplot(anket_an_1, aes(x=life_sat_sum_1, y=mdl4_5_sum_2)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$life_sat_sum_1,anket_an_1$mdl4_5_sum_3) # значимо
ggplot(anket_an_1, aes(x=life_sat_sum_1, y=mdl4_5_sum_3)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$life_sat_sum_1,anket_an_1$mdl4_5_sum_4) # значимо
ggplot(anket_an_1, aes(x=life_sat_sum_1, y=mdl4_5_sum_4)) + 
  geom_point() +
  geom_smooth(method = lm)

# Все предположения верны

## Субшкала наличия смысла из опросника смысла жизни

cor.test(anket_an_1$purp_sum_1,anket_an_1$mdl4_5_sum) # значимо
ggplot(anket_an_1, aes(x=purp_sum_1, y=mdl4_5_sum)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$purp_sum_1,anket_an_1$mdl4_5_sum_1) # значимо
ggplot(anket_an_1, aes(x=purp_sum_1, y=mdl4_5_sum_1)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$purp_sum_1,anket_an_1$mdl4_5_sum_2) # значимо
ggplot(anket_an_1, aes(x=purp_sum_1, y=mdl4_5_sum_2)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$purp_sum_1,anket_an_1$mdl4_5_sum_3) # значимо
ggplot(anket_an_1, aes(x=purp_sum_1, y=mdl4_5_sum_3)) + 
  geom_point() +
  geom_smooth(method = lm)
cor.test(anket_an_1$purp_sum_1,anket_an_1$mdl4_5_sum_4) # значимо
ggplot(anket_an_1, aes(x=purp_sum_1, y=mdl4_5_sum_4)) + 
  geom_point() +
  geom_smooth(method = lm)

# Ожидаемая корреляция есть, но самым сильным фактором оказалась связь с коллегами

## Шкала нейротизма из краткого опросника большой пятёрки

# Анова

anova_model <- aov(neuro_sum_1 ~ mdl4_5_sum_4_cat, data = anket_an_1)
summary(anova_model)

# Улучшенная версия с доверительными интервалами
ggplot(anket_an_1, aes(x = mdl4_5_sum_4_cat, y = neuro_sum_1)) +
  geom_boxplot(width = 0.5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.2, color = "red") +
  geom_jitter(width = 0.1, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Сравнение групп с доверительными интервалами",
       subtitle = "Красные линии показывают 95% доверительные интервалы для средних",
       x = "Категории",
       y = "Количественный показатель")

#Проведём t-тесты

# Поправка Тьюки
TukeyHSD(anova_model)

# Попарные t-тесты с поправкой Бонферрони
pairwise.t.test(anket_an_1$neuro_sum_1, 
                anket_an_1$mdl4_5_sum_4_cat,
                p.adjust.method = "bonferroni")

# Мы получили статистически значимое отличие только "Высокой группы" от все остальных
# U-образной зависимости не вышло
