library('GGally') 
library('ISLR')
library('MASS')
library('boot')
#Загружаем данные
data <- read.csv("Bank_for_models.csv", header = TRUE, dec = ",", sep = ";")
head(data)
df.train <- data
my.seed <- 11

#Представляем переменные как факторы
df.train$job <- as.factor(df.train$job)
df.train$marital <- as.factor(df.train$marital)
df.train$education <- as.factor(df.train$education)
df.train$default <- as.factor(df.train$default)
df.train$housing <- as.factor(df.train$housing)
df.train$loan <- as.factor(df.train$loan)
df.train$contact <- as.factor(df.train$contact)
df.train$month <- as.factor(df.train$month)
df.train$poutcome <- as.factor(df.train$poutcome)
df.train$y <- as.factor(df.train$y)


str(df.train)
summary(df.train)
ggpairs(df.train)

ggp <- ggpairs(df.train[, c('job', 'marital', 'education', 'default', 'housing', 'loan', 'contact', 'month', 'poutcome', 'y')])
print(ggp, progress = F)

df.tr1 <- df.train[, c('age', 'balance', 'day', 'duration', 'campaign', 'pdays', 'previous', 'y')]
df.tr1$y <- as.numeric(df.tr1$y)

cor(df.tr1)
cor.test(y=df.tr1$y, df.tr1[, 1])
cor.test(y=df.tr1$y, df.tr1[, 2])
cor.test(y=df.tr1$y, df.tr1[, 3])
cor.test(y=df.tr1$y, df.tr1[, 4])
cor.test(y=df.tr1$y, df.tr1[, 5])
cor.test(y=df.tr1$y, df.tr1[, 6])
cor.test(y=df.tr1$y, df.tr1[, 7])
#все значимы

df.tr2 <- df.train[, c('job', 'marital', 'education', 'default', 'housing', 'loan', 'contact', 'month', 'poutcome', 'y')]


chisq.test(ftable(df.tr2$job,df.tr2$y))
chisq.test(ftable(df.tr2$marital,df.tr2$y))
chisq.test(ftable(df.tr2$education,df.tr2$y))
chisq.test(ftable(df.tr2$default,df.tr2$y))
chisq.test(ftable(df.tr2$housing,df.tr2$y))
chisq.test(ftable(df.tr2$loan,df.tr2$y))
chisq.test(ftable(df.tr2$contact,df.tr2$y))
chisq.test(ftable(df.tr2$month,df.tr2$y))
chisq.test(ftable(df.tr2$poutcome,df.tr2$y))
#все значимы

# Логистическая регрессия ======================================================
#Строим модель на всех переменных
model.logit <- glm(y ~ age+job+marital+education+default+balance+housing+loan+contact+day+month+duration+campaign+pdays+previous+poutcome,
                   data = df.train, family = 'binomial')


summary(model.logit)
#Исключаем возраст
model.logit <- glm(y ~ job+marital+education+default+balance+housing+loan+contact+day+month+duration+campaign+pdays+previous+poutcome,
                   data = df.train, family = 'binomial')

summary(model.logit)

#Исключаем дефолт
model.logit <- glm(y ~ job+marital+education+balance+housing+loan+contact+day+month+duration+campaign+pdays+previous+poutcome,
                   data = df.train, family = 'binomial')

summary(model.logit)
#Исключаем остальные незначимые
model.logit <- glm(y ~ job+education+balance+housing+loan+month+duration+campaign+poutcome,
                   data = df.train, family = 'binomial')

summary(model.logit)

#Полиномы
attach(df.train)
y <- as.numeric(y)

cv.err.k.fold <- rep(0, 5)
names(cv.err.k.fold) <- 1:5


cv.err <- cv.glm(df.train, model.qda)
cv.err$delta
#10 кратная перекрестная проверка


cv.err.k.fold0 <- cv.glm(df.train, model.logit, K = 10)$delta[1]

# цикл по степеням полиномов
df.train$y <- as.numeric(df.train$y)
for (i in 1:5) {
  fit.glm <- glm(y ~ poly(age, i)+poly(balance, i)+poly(duration, i)+poly(previous, i)+poly(campaign, i)+poly(pdays, i)+poly(day, i))
  cv.err.k.fold[i] <- cv.glm(data=df.train, fit.glm, K = 10)$delta[1]
  
}
cv.err.k.fold0
cv.err.k.fold
detach(df.train)

#Прогноз
data1 <- read.csv("Bank_for_forecast.csv", header = TRUE, dec = ",", sep = ";")
df.test <- data1
p.logit <- predict(model.logit, df.test, type = 'response')

Прогноз <- factor(ifelse(p.logit > 0.5, 2, 1), levels = c(1 ,2),
                  labels = c('No', 'Yes'))
head(Прогноз)

