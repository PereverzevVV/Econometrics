#загрузим все необходимые библиотекои в R
library("memisc")  # две и более регрессий в одной табличке
library("psych")  # описательные статистики
library("lmtest")  # тестирование гипотез в линейных моделях
library("sgof")
library("foreign")  # загрузка данных в разных форматах
library("car")
library("hexbin")  # графики
library("tidyverse") # вместо ggplot2 (графики) и dplyr (манипуляции с данными)
library("rlms")  # загрузка данных в формате rlms (spss)
library("memisc")
library("lmtest")
library("foreign")
library("hexbin")
library("pander")
library("tidyverse")
library("knitr")
library("ggplot2")
install.packages("Rcpp")

#загрузим данные
df <- Database2005_1_
#проверим, что данные загрузились правильно
head(df)
#посмотрим на дескриптивные статистики
describe(df)
#создадим переменные для тех, что в excel + несколько даммм-переменных для угла наклона
gdp <- df$GDPpercapppp
urb <- df$Urbanshare
educ <- df$Higheduc
west <- df$WEST
unemp <- df$Unemployment
enter <- df$Enterprises
pop <- df$PopulationGrowth
repub <- df$NationalRepublics
df$west_mult_gdp <- west*gdp
df$west_mult_urb <- west*urb

#посмотрим на гистограммы переменных
hist(unemp)
hist(gdp)
hist(urb)
hist(educ)
hist(income)
hist(enter)
hist(pop)

#теперь для начала посмотрим на банальную линейную модель со всеми выделенными переменными
#а потом уже будем преобразовывать её (выбор функциональной модели, спецификация, мультиколлинеарность, нормальность ошибок и т.д.)

#для начала посмотрим на диаграммы рассеяния безработицы от переменных
qplot(data = df, gdp, unemp)
qplot(data = df, urb, unemp)
qplot(data = df, educ, unemp)
qplot(data = df, pop, unemp)
qplot(data = df, enter, unemp)


#оценим простую регрессионную модель
model_start <- lm(data=df, unemp~gdp+urb+educ+west+pop+repub+enter+west_mult_urb+west_mult_gdp)
summary(model_start)
#R2adj = 0.3912, что уже неплохо. Посмотрим, как можно улучшить модель

#для начала разберёмся с выбросами

#посмотрим, как изменится R2adj без выбросов
boxplot(unemp)
boxplot(gdp)
boxplot.stats(unemp)$out
ind <- which(unemp %in% boxplot.stats(unemp)$out)
vybrosy <- data.frame(gdp=gdp[i], unemp=unemp[ind])
plot(gdp,unemp,col='green', pch=18, ylim=c(0,max(unemp)))

points(vybrosy$gdp, vybrosy$unemp, col='red',pch=18)

pop1 <- pop[-ind]
enter1 <- enter[-ind]
repub1 <- repub[-ind]
unemp1 <- unemp[-ind]
gdp1 <- gdp[-ind]
urb1 <- urb[-ind]
educ1 <- educ[-ind]
west1 <- west[-ind]
west_mult_urb1 <- df$west_mult_urb[-ind]
west_mult_gdp1 <- df$west_mult_gdp[-ind]

boxplot(unemp1)

model_new <- lm(data=df[-ind, ], unemp1 ~ gdp1+urb1+educ1+west1+pop1+repub1+enter1+west_mult_urb1+west_mult_gdp1)
summary(model_new)

#видим, что аналогичная модель, но без учёта выбросов даже хуже, чем с ними (R2adj=0.2795)
#может тогда попробуем ввести дамми для выбросов?
summary(model_start)

df$special<-0
for (i in 1:length(unemp)) {
  if (unemp[i] > 15) {
    df$special[i] = "1"
  }
  else df$special[i] = "0"
}

model_dummy <- lm(data=df, unemp~special+gdp+urb+educ+west+west_mult_urb+west_mult_gdp+pop+enter+repub)
summary(model_dummy)

#заметим, что R2adj увеличился по сравнению с моделью без выбросов и стартовой моделью

#теперь посмотрим на регрессию Хубера
rr.huber <- rlm(data=df, unemp ~ gdp+urb+educ+west+west_mult_urb+west_mult_gdp+pop+enter+repub)
summary(rr.huber)

#residual standard error больше в регрессии Хубера,чем когда мы создаём дамми-переменную
#значит, создаём дамми для выбросов

#теперь проверим тест Чоу
df_west <- database_west_1
unemp_west <- df_west$Unemployment
gdp_west <- df_west$GDPpercapppp
df_east <- database_east_1
unemp_east <- df_east$Unemployment
gdp_east <- df_east$GDPpercapppp
model_west <- lm(data=df_west,unemp_west~ gdp_west+pop_west+enter_west+repub_west+urb_west+educ_west)
anova(model_west)
summary(model_west)
model_east <- lm(data=df_east,unemp_east~ gdp_east+pop_east+enter_east+repub_east+urb_east+educ_east)
anova(model_east)
summary(model_east)
model_chow <- lm(data = df, unemp ~ gdp+pop+enter+repub+educ+urb)
anova(model_chow)
#chow <- {(RSS - (RSS1+RSS2))/(k+1)}/(RSS1+RSS2)/(n-(2(k+1)))
chow <- ((2796.82-(152.739+1962.37))/(7))/((152.739+1962.37)/65)
chow
#ищем F-статистику для (7;65)= 2.08 на уровне 5%
#гипотеза о единой выборке отвергается -> нужно рассматривать запад-восток отдельно

#посмотрим на запад

df_west$special_west<-0
for (i in 1:length(unemp_west)) {
  if (unemp_west[i] > 15) {
    df_west$special_west[i] = "1"
  }
  else df_west$special_west[i] = "0"
}

df_west$special_west

pop_west <-df_west$PopulationGrowth
enter_west <- df_west$Enterprises
repub_west <- df_west$NationalRepublics
urb_west <- df_west$Urbanshare
educ_west <- df_west$Higheduc
model_dummy_west<- lm(data=df_west, unemp_west~special_west+gdp_west+urb_west+educ_west+pop_west+enter_west+repub_west)
summary(model_dummy_west)

#как видим R2adj выше, чем была до этого
#теперь поэксперементируем с функциональной моделью

#создаём лог-переменные для НЕ-дамми
l_gdp_west <- log(gdp_west)
l_urb_west <- log(urb_west)
l_educ_west <- log(educ_west)
l_unemp_west <- log(unemp_west)
l_pop_west <- log(pop_west)
l_enter_west <- log(enter_west)

#оцениваем логарифмическую модель
model_west_log <- lm(data=df_west, l_unemp_west~l_gdp_west+l_urb_west+l_educ_west+l_pop_west+l_enter_west+repub_west+special_west)
summary(model_west_log)

#смотрим на полулогарифмическую модель
model_west_lin_log <- lm(data=df_west, l_unemp_west~gdp_west+urb_west+educ_west+pop_west+enter_west+repub_west+special_west)
summary(model_west_lin_log)

#видно сразу, что полулогарифмическая лучше линейной в логарифмах по R2adj

model_west_lin <- model_dummy_west
summary(model_west_lin)

#проведём тест кокса-бокса
library("MASS")
boxcox(model_west_lin)

#bera-mcaleer (полулогарифмическая и линейная)
#оценим вспомогательные регрессии
#1)с зависимыми переменными экспоненты от оценки зависимой переменной полулогарифмической модели 
#2)и логарифма оценки в линейной

model_west_log_lin_help <- lm(data=df_west, exp(model_west_lin_log$fitted.values)~gdp_west+urb_west+educ_west+pop_west+enter_west)
summary(model_west_log_lin_help)
v1 <- model_west_log_lin_help$residuals
length(v1)
v1

model_west_lin$residuals[18]
log(model_west_lin$residuals)[-18]
model_west_lin_help <- lm(data=df_west, log(model_west_lin$fitted.values)[-18]~gdp_west[-18]+urb_west[-18]+educ_west[-18]+pop_west[-18]+enter_west[-18]+repub_west[-18])
summary(model_west_lin_help)
v2 <- model_west_lin_help$residuals
v2
length(v2)
length(unemp_west)
v2[19]

#вспомогательные регрессии номер 2
model_west_log_lin_help_help <- lm(data=df_west, l_unemp_west~gdp_west+urb_west+educ_west+pop_west+enter_west+v1)
model_west_lin_help_help <-lm(data=df_west, unemp_west[-18]~gdp_west[-18]+urb_west[-18]+educ_west[-18]+pop_west[-18]+enter_west[-18]+v2)
summary(model_west_log_lin_help_help)
#меньше чем 5 % отвергается -> значимый, что v1 != 0 -> log_lin хуже
summary(model_west_lin_help_help)
#нулевая гипотеза не отвергается на уровне 5 % -> остаток незначим -> v2 = 0 lin лучше, чем log_lin

#вывод после теста bera-mcaleer: линейная модель точно лучше полулогарифмической

#теперь посмотрим на спецификацию
library("lmtest")
resettest(model_west_lin)

#нулевая гипотеза не отвергается на 5% -> можно сказать, что нет неучтенных переменных в модели.

#теперь проверим мультиколлинеарность
#посчитаем VIF

model_west_gdp <- lm(data=df_west, gdp_west~urb_west+educ_west+pop_west+enter_west+repub_west)
summary(model_west_gdp)
vif_gdp <- 1/(1-0.6237)
vif_gdp
model_west_urb <- lm(data=df_west, urb_west~gdp_west+educ_west+pop_west+enter_west+repub_west)
summary(model_west_urb)
vif_urb <- 1/(1-0.6118)
vif_urb
model_west_educ <- lm(data=df_west, educ_west~gdp_west+pop_west+enter_west+urb_west+repub_west)
summary(model_west_educ)
vif_educ <- 1/(1-0.5706)
vif_educ

model_west_pop<- lm(data=df_west, pop_west~gdp_west+educ_west+enter_west+urb_west+repub_west)
summary(model_west_pop)
vif_educ <- 1/(1-0.4736)
vif_educ

model_west_enter <- lm(data=df_west, enter_west~gdp_west+pop_west+educ_west+urb_west+repub_west)
summary(model_west_enter)
vif_enter <- 1/(1-0.7135)
vif_enter

model_west_repub <- lm(data=df_west, enter_west~gdp_west+pop_west+educ_west+urb_west+enter_west)
summary(model_west_repub)
vif_repub <- 1/(1-0.6968)
vif_repub

#нет больше 10 -> значит, есть мультиколлинеарность

summary(model_west_lin)

#методом исключения 
#шаг № 1: уберём enter_west
model1_west <- lm(data=df_west,unemp_west~gdp_west+urb_west+pop_west+educ_west+repub_west+special_west)
summary(model1_west)

#теперь уберём educ_west
model2_west <- lm(data=df_west,unemp_west~gdp_west+urb_west+pop_west+repub_west+special_west)
summary(model2_west)

#убираем repub
model4_west <- lm(data=df_west,unemp_west~urb_west+special_west+gdp_west+pop_west)
summary(model4_west)


#убираем urb 
model6_west <- lm(data=df_west,unemp_west~gdp_west+special_west+pop_west)
summary(model6_west)

#все коэффициенты теперь значимы

#теперь проверим на гетероскедастичность тестом Бройша-Пагана
bptest(model6_west)

summary(model6_west)

#на уровне 5% H0 отвергается и у нас гетероскедастичность

#проведём коррекцию
#попробуем залогарифмировать
model7_west <- lm(data=df_west,log(unemp_west)~log(gdp_west)+log(pop_west)+special_west)
summary(model7_west)

bptest(model7_west)
#гетероскедастичность не исчезла, но стала намного лучше
boxcox(model6_west)
#0 попал в интервал -> логарифмическая лучше, чем линейная

#таким образом, лучшая модель - model7_west

summary(model7_west)

#осталось проверить нормальность остатков
residuals_west <- model7_west$residuals
qqnorm(residuals_west)
qqline(residuals, col = "steelblue", lwd = 2)

#видно, что не совпадает с нормальным распределением, но чтобы окончательно в этом убедиться проверим ряд тестов

#тест Колмогорова-Смирнова
install.packages("nortest")
library("nortest")
#посмотрим на тест Колмогорова-Смирнова 
#Оценивается выборочное среднее и дисперсия;
#Так же как при использовании критерия Колмогорова, находится максимальное отклонение между выборочной и теоретической интегральными функциями распределения;
#Принимается решение, является ли статистически значимым наблюдаемое отклонение выборочной функции распределения от теоретической. 
#В случае положительного ответа, нулевая гипотеза отвергается.
lillie.test(residuals)

#H0 отвергаем -> нельзя сказать, что остатки подчиняются закону нормального распределения

#тест Харке Бера
install.packages(("tseries"))
library("tseries")
jarque.bera.test(residuals)

#отвергаем H0 о нормальности остатков

#тест Шапиро-Уилка
shapiro.test(residuals)
#H0 отвергается -> нельзя утверждать о нормальности остатков

summary(model7_west)


#и сделать все для востока

#посмотрим на восток

summary(model)

df_east$special_east<-0
for (i in 1:length(unemp_east)) {
  if (unemp_east[i] > 15) {
    df_east$special_east[i] = "1"
  }
  else df_east$special_east[i] = "0"
}

df_east$special_east

pop_east <-df_east$PopulationGrowth
enter_east <- df_east$Enterprises
repub_east <- df_east$NationalRepublics
urb_east <- df_east$Urbanshare
educ_east <- df_east$Higheduc
model_dummy_east <- lm(data=df_east, unemp_east~special_east+gdp_east+urb_east+educ_east+pop_east+enter_east+repub_east)
summary(model_dummy_east)

#как видим R2adj выше, чем была до этого
#теперь поэксперементируем с функциональной моделью

#создаём лог-переменные для НЕ-дамми
l_gdp_east <- log(gdp_east)
l_urb_east <- log(urb_east)
l_educ_east <- log(educ_east)
l_unemp_east <- log(unemp_east)
l_pop_east <- log(pop_east)
l_enter_east <- log(enter_east)

#оцениваем логарифмическую модель
model_east_log <- lm(data=df_east, l_unemp_east~l_gdp_east+l_urb_east+l_educ_east+l_pop_east+l_enter_east+repub_east+special_east)
summary(model_east_log)

#смотрим на полулогарифмическую модель
model_east_lin_log <- lm(data=df_east, l_unemp_east~gdp_east+urb_east+educ_east+pop_east+enter_east+repub_east+special_east)
summary(model_east_lin_log)

model_east_lin <- model_dummy_east
summary(model_east_lin)

#проведём тест кокса-бокса
library("MASS")
boxcox(model_east_lin)

#линейная лучше, потому что 1 попала в интервал, а 0 - нет

#bera-mcaleer (полулогарифмическая и линейная)
#оценим вспомогательные регрессии
?lm
#1)с зависимыми переменными экспоненты от оценки зависимой переменной полулогарифмической модели 
#2)и логарифма оценки в линейной

model_east_log_lin_help <- lm(data=df_east, exp(model_east_lin_log$fitted.values)~gdp_east+urb_east+educ_east+pop_east+enter_east)
summary(model_east_log_lin_help)
v1 <- model_east_log_lin_help$residuals
length(v1)

model_east_lin_help <- lm(data=df_east, log(model_east_lin$fitted.values)~gdp_east+urb_east+educ_east+pop_east+enter_east)
summary(model_east_lin_help)
v2 <- model_east_lin_help$residuals
v2
length(v2)
length(unemp_east)

#вспомогательные регрессии номер 2
model_east_log_lin_help_help <- lm(data=df_east, l_unemp_east~gdp_east+urb_east+educ_east+pop_east+enter_east+v1)
model_east_lin_help_help <-lm(data=df_east, unemp_east~gdp_east+urb_east+educ_east+pop_east+enter_east+v2)
summary(model_west_log_lin_help_help)
#больше чем 5 % не отвергается -> незначимый, что v1 = 0 -> log_lin лучше
summary(model_east_lin_help_help)
#v2 отвергается на уровне 5 % -> остаток значим -> lin хуже, чем log_lin

#тест bera-mcaleer не помогает

#теперь посмотрим на спецификацию
library("lmtest")
resettest(model_east_lin)
summary(model_east_lin)

#нулевая гипотеза не отвергается -> можно сказать, что нет неучтенных переменных в модели и модель правильно специфирована

#теперь проверим мультиколлинеарность
#посчитаем VIF

model_east_gdp <- lm(data=df_east, gdp_east~urb_east+educ_east+pop_east+enter_east+repub_east)
summary(model_east_gdp)
vif_gdp <- 1/(1-0.2405)
vif_gdp
model_east_urb <- lm(data=df_east, urb_east~gdp_east+educ_east+pop_east+enter_east+repub_east)
summary(model_east_urb)
vif_urb <- 1/(1-0.541)
vif_urb
model_east_educ <- lm(data=df_east, educ_east~gdp_east+pop_east+enter_east+urb_east+repub_east)
summary(model_east_educ)
vif_educ <- 1/(1-0.09827)
vif_educ

model_east_pop<- lm(data=df_east, pop_east~gdp_east+educ_east+enter_east+urb_east+repub_east)
summary(model_east_pop)
vif_pop <- 1/(1-0.6192)
vif_pop

model_east_enter <- lm(data=df_east, enter_east~gdp_east+pop_east+educ_east+urb_east+repub_east)
summary(model_east_enter)
vif_enter <- 1/(1-0.3087)
vif_enter

model_east_repub <- lm(data=df_east, repub_east~gdp_east+pop_east+educ_east+enter_east+urb_east)
summary(model_east_repub)
vif_repub <- 1/(1-0.5347)
vif_repub

#все vif меньше 10 -> значит, есть мультиколлинеарности нет

summary(model_east_lin)
#методом исключения 
#шаг № 1: уберём gdp_east
model1_east <- lm(data=df_east,unemp_east~enter_east+urb_east+pop_east+educ_east+repub_east+special_east)
summary(model1_east)

#теперь уберём educ_east
model2_east <- lm(data=df_east,unemp_east~enter_east+urb_east+pop_east+repub_east+special_east)
summary(model2_east)

#убираем repub
model4_east <- lm(data=df_east,unemp_east~urb_east+special_east+enter_east+pop_east)
summary(model4_east)

#убираем enter
model5_east <- lm(data=df_east,unemp_east~urb_east+special_east+pop_east)
summary(model5_east)

model6_east <- lm(data=df_east,unemp_east~urb_east+special_east)
summary(model6_east)

#коэффициенты теперь все значимы, но R^2adj уменьшилась


#теперь проверим на гетероскедастичность тестом Бройша-Пагана
bptest(model_east_lin)

#гетероскедастичности нет, p-value слишком большой, что говорит о гомоскедастичности

#осталось проверить нормальность остатков
residuals_east <- model_east_lin$residuals
qqnorm(residuals_east)
qqline(residuals_east, col = "steelblue", lwd = 2)

#не очень понятно по qqnorm можно ли сказать, что остатки принадлежат нормальному распределению или нет
#поэтому проверим ряд тестов

#тест Колмогорова-Смирнова
library("nortest")
#посмотрим на тест Колмогорова-Смирнова 
#Оценивается выборочное среднее и дисперсия;
#Так же как при использовании критерия Колмогорова, находится максимальное отклонение между выборочной и теоретической интегральными функциями распределения;
#Принимается решение, является ли статистически значимым наблюдаемое отклонение выборочной функции распределения от теоретической. 
#В случае положительного ответа, нулевая гипотеза отвергается.
lillie.test(residuals)

#H0 отвергаем -> нельзя сказать, что остатки подчиняются закону нормального распределения

#тест Харке Бера
install.packages(("tseries"))
library("tseries")
jarque.bera.test(residuals)

#отвергаем H0 о нормальности остатков

#тест Шапиро-Уилка
shapiro.test(residuals)
#H0 отвергается -> нельзя утверждать о нормальности остатков

summary(model_east_lin)


