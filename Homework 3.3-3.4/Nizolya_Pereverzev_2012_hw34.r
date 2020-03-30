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

library('dplyr')
library("rlms")
library("memisc")  # две и более регрессий в одной табличке
library("psych")  # описательные статистики
library("lmtest")  # тестирование гипотез в линейных моделях
library("sjPlot")  # графики
library("sgof")
library("foreign")  # загрузка данных в разных форматах
library("car")
library("hexbin")  # графики
library("tidyverse") # вместо ggplot2 (графики) и dplyr (манипуляции с данными)
library("rlms")  # загрузка данных в формате rlms (spss)

df <- Nizolya_Pereverzev_rlms
glimpse(df)

#посмотрим на дескриптивные статистики
describe(df)

#создадим переменные для тех, что в excel + несколько даммм-переменных для угла наклона
salary <- df$salary
age <- df$age
sex <- df$sex
status <- df$status
diplom <- df$diplom
job_satisfaction <- df$job_satisfaction
region <- df$region
population <- df$population
married <- df$married
professional_group <- df$professional_group
branch_work <- df$branch_work
children <- df$children

#посмотрим на гистограммы переменных
hist(salary)
hist(age)
hist(sex)
hist(status)
hist(diplom)
hist(job_satisfaction)
hist(region)
hist(population)
hist(married)
hist(professional_group)
hist(branch_work)
hist(children)

#отберём только тех, кто в области или городе
df1 <- filter(df, status == 1|status == 2)
#оставим только сильные ответы удовл. ли работой
df2 <- filter(df1, job_satisfaction == 1|job_satisfaction == 5)
#1 вариант просто какой-то не оч
df3 <- filter(df2, married == 2|married == 3|married == 4|married == 5|married == 6)

glimpse(df3)
describe(df3)

df3 <- mutate_each(df3, "factor", status, job_satisfaction, sex, diplom, married, professional_group)
df3$status <- memisc::recode(df3$status, 0 <- 1, 1 <- 2)
df3$job_satisfaction <- memisc::recode(df3$job_satisfaction, 1 <- 1, 0 <- 5)
df3$sex <- memisc::recode(df3$sex, 1 <- 1, 0 <- 2)
df3$diplom_0 <- memisc::recode(df3$diplom, 1 <- 1, 1 <- 2, 1 <- 3, 0 <-4, 0<-5, 0<-6)
glimpse(df3)
length(df3$diplom_0)
df3$diplom_1 <- memisc::recode(df3$diplom, 0 <- 1, 0 <- 2, 0 <- 3, 1 <-4, 0<-5, 0<-6)
df3$diplom_2 <- memisc::recode(df3$diplom, 0 <- 1, 0 <- 2, 0 <- 3, 0 <-4, 1<-5, 0<-6)
df3$diplom_3 <- memisc::recode(df3$diplom, 0 <- 1, 0 <- 2, 0 <- 3, 0 <-4, 0<-5, 1<-6)
df3$married <- memisc::recode(df3$married, 1 <- 2, 1 <- 3, 0 <-4, 0<-5, 0<-6)
df3$prof_war <- memisc::recode(df3$professional_group, 1 <- 0, 0 <- 1, 0 <- 2, 0 <-3, 0<-4, 0<-5, 0<-6, 0<-7, 0<-8, 0<-9)
df3$prof_rul <- memisc::recode(df3$professional_group, 0 <- 0, 1 <- 1, 0 <- 2, 0 <-3, 0<-4, 0<-5, 0<-6,0<-7, 0<-8, 0<-9)
df3$prof_spec <- memisc::recode(df3$professional_group, 0 <- 0, 0 <- 1, 1 <- 2, 1 <-3, 0<-4, 0<-5, 0<-6, 0<-7, 0<-8, 0<-9)
df3$prof_service <- memisc::recode(df3$professional_group, 0 <- 0, 0 <- 1, 0 <- 2, 0 <-3, 1<-4, 1<-5, 0<-6, 0<-7, 0 <-8, 0<-9)
df3$prof_qual <- memisc::recode(df3$professional_group, 0 <- 0, 0 <- 1, 0 <- 2, 0 <-3, 0<-4, 0<-5, 1<-6, 1<-7, 1<-8, 0<-9)
df3$prof_nonqual <- memisc::recode(df3$professional_group, 0 <- 0, 0 <- 1, 0 <- 2, 0 <-3, 0<-4, 0<-5, 0<-6, 0<-7, 0<-8, 1<-9)

r_diplom_0 <- memisc::recode(df3$diplom, 1 <- 1, 1 <- 2, 1 <- 3, 0 <-4, 0<-5, 0<-6)
prof_nonqual <- memisc::recode(df3$professional_group, 0 <- 0, 0 <- 1, 0 <- 2, 0 <-3, 0<-4, 0<-5, 0<-6, 0<-7, 0<-8, 1<-9)


glimpse(df3)

#для начала посмотрим на диаграммы рассеяния безработицы от переменных
qplot(data = df3, age, salary)
qplot(data = df3, sex, salary)
qplot(data = df3, status, salary)
qplot(data = df3, diplom, salary)
qplot(data = df3, job_satisfaction, salary)
qplot(data = df, region, salary)
qplot(data = df3, population, salary)
qplot(data = df3, married, salary)
qplot(data = df3, professional_group, salary)
qplot(data = df, branch_work, salary)
qplot(data = df3, children, salary)

#теперь для начала посмотрим на банальную линейную модель со всеми выделенными переменными
#а потом уже будем преобразовывать её (выбор функциональной модели, спецификация, мультиколлинеарность, нормальность ошибок и т.д.)

#оценим стартовую модель
model_start <- lm(data=df3, salary~sex+age+status+diplom_1+diplom_2+diplom_3+population+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul+children+job_satisfaction)
summary(model_start)
residuals <- model_start$residuals

#R2adj = 0.3283, что уже неплохо. Посмотрим, как можно улучшить модель

####################################################################
#для начала разберёмся с выбросами

#стьюдентизированные остатки регрессии
residuals
res_st <- rstudent(model_start)
qplot(df3$salary, res_st)


#
boxplot(salary)
boxplot.stats(salary)$out
ind <- which(salary %in% boxplot.stats(salary)$out)
vybrosy <- data.frame(salary = salary[ind])

#посмотрим, как изменится R2adj без выбросов
salary_f <- df3$salary[-ind]
sex_f <- df3$sex[-ind]
age_f <- df3$age[-ind]
status_f <- df3$status[-ind]
diplom_1_f <- df3$diplom_1[-ind]
diplom_2_f <- df3$diplom_2[-ind]
diplom_3_f <- df3$diplom_3[-ind]
population_f <- df3$population[-ind]
married_f <- df3$married[-ind]
prof_war_f <- df3$prof_war[-ind]
prof_qual_f <- df3$prof_qual[-ind]
prof_service_f <- df3$prof_service[-ind]
prof_spec_f <- df3$prof_spec[-ind]
prof_rul_f <- df3$prof_rul[-ind]
children_f <- df3$children[-ind]
job_satisfaction_f <- df3$job_satisfaction[-ind]

model_new <- lm(data=df[-ind, ], salary_f ~ sex_f+age_f+status_f+diplom_1_f+diplom_2_f+diplom_3_f+population_f+married_f+prof_war_f+prof_qual_f+prof_service_f+prof_spec_f+prof_rul_f+children_f+job_satisfaction_f)
summary(model_new)

#Radj = 0.3267; модель без выбросов лучше, но не сильно

df3$special <- 0
for (i in 1:length(df3$salary)) {
  if (df3$salary[i] > 150000) {
    df3$special[i] = "1"
  }
  else df3$special[i] = "0"
}

special <- df3$special


model_dumny <- lm(data=df3, salary~special+sex+age+status+diplom_1+diplom_2+diplom_3+population+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul+children+job_satisfaction)
summary(model_dumny)

#теперь посмотрим на регрессию Хубера
#rr.huber <- rlm(data=df3, salary~sex+age+status+diplom_1+diplom_2+diplom_3+population+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul+children+job_satisfaction)
#summary(rr.huber)
#residual standard error (8982) меньше в регрессии Хубера,чем когда мы создаём дамми-переменную (14280)
#значит, НЕ создаём дамми для выбросов ?

#Как мы видим, качество заметно улучшилось. И это замечательно.
####################################################################

#оцениваем полулогарифмическую модель

df3$l_salary <- log(df3$salary)
df3$l_age <- log(df3$age)
df3$l_population <- log(df3$population)
df3$l_children <- log(df3$children)

#оцениваем полулогарифмическую модель
model_log <- lm(data=df3, salary~l_age+l_population+l_children+sex+status+diplom_1+diplom_2+diplom_3+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul+job_satisfaction+special)
summary(model_log)


####################################################################

#оцениваем линейную в логарифмах модель
model_lin_log <- lm(data=df3, l_salary~l_age+l_population+l_children+sex+status+diplom_1+diplom_2+diplom_3+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul+job_satisfaction+special)
summary(model_lin_log)


#проведём тест кокса-бокса
library("MASS")
boxcox(model_start)
#так как 0 попал в в интервал, то точно линейная обычная хуже, чем лог-модели
#а между полулог и лог законно сделать вывод по R2adj, потому что у них одинаковая левая часть
#следовательно, берём лог модель (у неё R2adj больше)

##################################################################################


#теперь посмотрим на спецификацию
library("lmtest")
resettest(model_log)

#есть неучтённые переменные, поэтому попробуем ввести какие-нибудь ещё
#к примеру, age^2

df3$age2 <- df3$age^2
df3$l_age
l_age2
df3$age3 <- df3$age^3
df3$population2 <- df3$population^2
df3$population3 <- df3$population^3
#l_children2 <- log(df3$children)^2
#l_children3 <- log(df3$children)^3
#model_log_reset <- lm(data=df3, salary~l_age + l_age2+l_age3 +l_population+l_population2+l_population3+l_children + l_children2+l_children3+sex+status+diplom_1+diplom_2+diplom_3+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul+job_satisfaction+special)
model_log_reset <- lm(data=df3, salary~l_age + age2+age3 +l_population+population2+population3+l_children +sex+status+diplom_1+diplom_2+diplom_3+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul+job_satisfaction+special)
summary(model_log_reset)

resettest(model_log_reset)

#что неудивительно вообще, потому что у нас очень много потенциальных переменных в самой базе данных
#и очень сложно учесть все факторы,влияющие на зп индивида

model_age <- lm(data=df3, l_age~df3$age2+df3$age3+population3+population2+l_population+l_children+sex+status+diplom_1+diplom_2+diplom_3+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul+job_satisfaction+special)
summary(model_age)
vif_age <- 1/(1-0.9905)
vif_age

model_pop <- lm(data=df3, l_population~l_age+age2+age3+population2+population3+l_children+sex+status+diplom_1+diplom_2+diplom_3+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul+job_satisfaction+special)
summary(model_pop)
vif_pop <- 1/(1-0.93)
vif_pop

model_child <- lm(data=df3, l_children~l_population+population2+population3+age2+age3+l_age+sex+status+diplom_1+diplom_2+diplom_3+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul+job_satisfaction+special)
summary(model_child)
vif_child <- 1/(1-0.131)
vif_child


#уже по первым двум коэффицентам понятно, что в модели присутствует мультиколлинеарность 
#вообще это логично после исправления результатов теста рамсея
#теперь попробуем исправить мультиколлинеарность методом последовательного исключения

model_log_reset <- lm(data=df3, salary~l_age + age2+age3 +l_population+population2+population3+l_children +sex+status+diplom_1+diplom_2+diplom_3+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul+job_satisfaction+special)
summary(model_log_reset)

#уберём status 
model_1 <- lm(data=df3, salary~l_age + age2+age3 +l_population+population2+population3+l_children +sex+diplom_1+diplom_2+diplom_3+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul+job_satisfaction+special)
summary(model_1)

#теперь попробуем убрать l_children
model_2 <- lm(data=df3, salary~l_age + age2+age3 +l_population+population2+population3 +sex+diplom_1+diplom_2+diplom_3+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul+job_satisfaction+special)
summary(model_2)

#уберём diplom1
model_3<- lm(data=df3, salary~l_age + age2+age3 +l_population+population2+population3 +sex+diplom_2+diplom_3+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul+job_satisfaction+special)
summary(model_3)

#уберём prof_war
model_4 <- lm(data=df3, salary~l_age + age2+age3 +l_population+population2+population3 +sex+diplom_2+diplom_3+married+prof_qual+prof_service+prof_spec+prof_rul+job_satisfaction+special)
summary(model_4)

#population3
model_5 <- lm(data=df3, salary~l_age + age2+age3 +l_population+population2 +sex+diplom_2+diplom_3+married+prof_qual+prof_service+prof_spec+prof_rul+job_satisfaction+special)
summary(model_5)

#уберём prof_service
model_6 <- lm(data=df3, salary~l_age + age2+age3 +l_population+population2 +sex+diplom_2+diplom_3+married+prof_qual+prof_spec+prof_rul+job_satisfaction+special)
summary(model_6)

#уберём married
model_7 <- lm(data=df3, salary~l_age + age2+age3 +l_population+population2+sex+diplom_2+diplom_3+prof_qual+prof_spec+prof_rul+job_satisfaction+special)
summary(model_7)

#уберём age3
model_8 <- lm(data=df3, salary~l_age + age2+l_population+population2 +sex+diplom_2+diplom_3+prof_qual+prof_spec+prof_rul+job_satisfaction+special)
summary(model_8)

###########################################################################

##теперь проверим на гетероскедастичность тестом Бройша-Пагана
bptest(model_8)

#гетероскедастичности есть, p-value слишком маленький, что говорит об отвержении нулевой гипотезы о гомоскедастичности
#Тест Голдфелда-Квандта
gqtest(model_8, order.by=~l_age, data = df3, fraction = 0.33)
gqtest(model_8, order.by=~l_population, data = df3, fraction = 0.2)
#Как мы видим, гипотеза H0  не отвергается, гетероскедастичность данного вида признается незначимой.

model_mnk <- lm(data=df3, salary~l_age + age2+l_population+population2 +sex+diplom_2+diplom_3+prof_qual+prof_spec+prof_rul+job_satisfaction+special, weights = I(1 / l_population))
summary(model_mnk)
bptest(model_mnk)
gqtest(model_mnk, order.by=~l_population, data = df3, fraction = 0.33)
#не знаю, может, это я не вижу разницы, но ее там реально нет...

#осталось проверить нормальность остатков
residuals <- model_8$residuals
qqnorm(residuals)
qqline(residuals, col = "steelblue", lwd = 2)

hist(residuals, col = "steelblue")

#тут, к сожалению, понятно, что нам оч плохо и у нас остатки не являются нормальными

#проверим тест Шапиро-Уилка, чтобы посмотреть насколько всё плохо
#тест Шапиро-Уилка
shapiro.test(residuals)
#H0 отвергается -> нельзя утверждать о нормальности остатков
#и что дальше с этим делать?


#Проверьте интересные с экономической точки зрения гипотезы с помощью теста
#Вальда

#проверим гипотезы о линейных ограничениях
#1) влияет ли пол значительно?
model_8 <- lm(data=df3, salary~l_age + age2+l_population+population2 +sex+diplom_2+diplom_3+prof_qual+prof_spec+prof_rul+job_satisfaction+special, weights = I(1/l_population))
#теперь посмотрим на ограниченную модель (ограничение 'без пола')
model_w1 <- lm(data=df3, salary~l_age + age2+l_population+population2+diplom_2+diplom_3+prof_qual+prof_spec+prof_rul+job_satisfaction+special, weights = I(1/l_population))

waldtest(model_w1, model_8)
#p-value очень маленькое, следовательно, гипотезу H0 (верна ограниченная модель без пола) нужно отвергнуть
#значит, необходимо учитывать пол как объясняющий фактор

#и с помощью теста отношения правдоподобия
lrtest(model_w1, model_8)
#p-value снова очень маленькое, следовательно, гипотеза о том, что пол не влияет на зп, отвергается
#пол влияет

#Выберите (при необходимости создайте) бинарную зависимую переменную
#возьмём job_satisfaction респондента 

#Оцените модели бинарного выбора
m_logit <- glm(data=df3, job_satisfaction~salary+age+population+children+sex+status+diplom_1+diplom_2+diplom_3+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul,
               family = binomial(link='logit'), x=TRUE)

m_probit <- glm(data=df3, job_satisfaction~salary+age+population+children+sex+status+diplom_1+diplom_2+diplom_3+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul,
               family = binomial(link='probit'), x=TRUE)



m_probit <- glm(data=df3, job_satisfaction~salary+age+population+children+sex+status+diplom_1+diplom_2+diplom_3+married+prof_war+prof_qual+prof_service+prof_spec+prof_rul,
                family = binomial(link='probit'), x=TRUE)
# Проверьте их качество
summary(m_logit)
Anova(m_logit)
summary(m_probit)

#по пробит: с ростом зп отношение шансов,что индивид удовлетворён своей работой к неудовлетворённости, падат на -2.558e-05 * 100%.
#отношение шансов, что квал рабочий больше любит свою работу, равно exp (-5.320e-01)
#значит, при переходе от других видов рабочих к квалиф, раз коэффициент отриц, то при переходе от 0 к 1 отношение шансов падает примерно в exp(...)

#легче интерпретироват по предельным эффектам
install.packages('erer')
library('erer')
maBina(m_logit, digits = 6)
maBina(m_logit, x.mean=FALSE, digits = 6)
maBina(m_probit)
maBina(m_probit, x.mean=FALSE)
?maBina
