# Разработка и валдизация опросника "Психологические признаки труда"

## Контекст и цель 
Разработка и валидизация психометрического инструментарий для анализа трудовой деятельности с целью публикации в Q1 и Q2 рецензируемых журналах (русская версия в Q2, английская в Q1)
## Стек
### Сбор данных: 
anketolog.ru
### Обработка данных:
### R
- library(tidyverse) - тиблы и пайплайн
- library(psych) - факторные анализы
- library(lavaan) - структурное моделирование
- library(semPlot) - строить график факторных анализов
- library(GPArotation) - для вращений в факторных анализах
- library(data.table) - таблички
- library(lavaanPlot) - график для структурных моделей
- library(rstatix) - для т-тестов
## Этапы работы
### No code:
1. Разработка itemов для опросника в составе экспертной группы
2. Поиск других инструментариев для проверки внешней валидности
3. Первый сбор данных (весна 2023)
4. Добор внешних методик
5. Второй сбор данных на новой анкете (зима 2024)
### Code:
1. Проверка надёжности, выполнения допущений
2. Анализ распределения демографических признаков
3. Проверка с помощью эксплаторного факторного анализа
4. Проверка с помощью конфирматорного факторного анализа
5. Проверка с помощью эксплаторного моделирования структурными уравнениями
6. Рассчёт весов для итоговой факторной структуры
7. Проверка внешней валидности через связь с другими рабочими методиками
## Результат
В результате был разработан и валидизирован опросник "Психологические признаки" с трёхфакторной структурой: субъективная значимость работы, связь с коллективом, аффективный аспект. Этот инструментарий может быть использован в сфере HR-аналитики для диагностики внутреннего состояния сотрудников и потенциальных факторов роста коллектива. Важнейним от существующих аналогов является подтвердившаяся на данных идея о значимости инструментальной составляющей для интегрального впечатления от деятельности, что особенно актуально в IT-сфере.
