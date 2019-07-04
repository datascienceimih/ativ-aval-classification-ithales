## Atividade 2 - Classificação
### Machine Learning
### Igor Thales dos Santos

10)
require(ISLR)

require(MASS)

require(class)

## A)
summary(Weekly)


plot(Today~Lag1, col="darkred", data=Weekly)
simplelm = lm(Today~Lag1, data=Weekly)
abline(simplelm, lwd= 3, col= "darkgreen")


pairs(Weekly)  

#### b)
logmod = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family = "binomial", data=Weekly)
summary(logmod)



#### c)

probs = predict(logmod, type="response")
preds = rep("Down", 1089)
preds[probs > 0.5] = "Up"
table(preds, Weekly$Direction)


hist(probs, breaks= 100, col= "darkred")
abline(v = mean(probs), lwd = 2)


plot(probs, col= ifelse(Weekly$Direction=="Down", "red","green"), pch=16)
abline(h = 0.5, lwd= 3)



#### d)

training.data = Weekly[Weekly$Year<2009,]
test.data = Weekly[Weekly$Year>2008,]
simpglm = glm(Direction~Lag2, data= training.data, family = "binomial")
summary(simpglm)


testprobs = predict(simpglm, type="response", newdata = test.data)
testdirs = Weekly$Direction[Weekly$Year>2008]
plot(testprobs, col= ifelse(Weekly$Direction[Weekly$Year>2008]=="Down", "red","green"), pch=16)
abline(h = 0.5, lwd= 3)



testpreds = rep("Down", 104)
testpreds[testprobs>0.5] = "Up"
mean(probs)


table(testpreds, testdirs)

#### e)

lda.fit = lda(Direction~Lag2, data= training.data)

plot(lda.fit)

lda.pred = predict(lda.fit, newdata=test.data, type="response")
lda.class = lda.pred$class
table(lda.class, test.data$Direction)


#### f)
qda.fit = qda(Direction~Lag2, data= training.data)
qda.fit

qda.pred = predict(qda.fit, newdata=test.data, type="response")
qda.class = qda.pred$class
table(qda.class, test.data$Direction)


#################### 11 

#### a)
auto = Auto
auto$mpg01 = rep(0, length(auto$mpg))
auto$mpg01[auto$mpg>median(auto$mpg)] = 1
head(auto)


#### b) 
par(mfrow=c(2,2))
plot(auto$year, auto$acceleration,, col= ifelse(auto$mpg01==0, "red", "green"), pch = 16)
plot(auto$year, auto$weight, col= ifelse(auto$mpg01==0, "red", "green"), pch = 16)
plot(auto$year, auto$horsepower, col= ifelse(auto$mpg01==0, "red", "green"), pch = 16)
plot(auto$year, auto$displacement, col= ifelse(auto$mpg01==0, "red", "green"), pch = 16)


cor(na.omit(auto[-9]))

#### c)
idxs <- sample(1:dim(auto)[1], size=dim(auto)[1]*0.75)
training <- auto[idxs,]
test = auto[-idxs,]

#### d)
lda.fit2 = lda(mpg01~displacement+weight+cylinders+year, data=training)
lda.fit2


plot(lda.fit2)

pred.lda2 = predict(lda.fit2, newdata=test, type="response")$class
table(pred.lda2, test$mpg01)

#### e) 
qda.fit2 = qda(mpg01~displacement+weight+cylinders+year, data=training)
qda.fit2


pred.qda2 = predict(qda.fit2, newdata=test, type="response")$class
table(pred.qda2, test$mpg01)

#### f)
logreg = glm(mpg01~displacement+weight+cylinders+year, family="binomial", data=training)
summary(logreg)

probs = predict(logreg, test, type="response")
preds = rep(0, dim(test)[1])
preds[probs>0.5]=1            
table(preds, auto$mpg01[-idxs])



par(mfrow=c(1,1))
plot(probs, col= ifelse(test$mpg01==0, "red", "blue"), pch = 16)
abline(h=0.5, lwd=3)


hist(probs, breaks = 100, col = "darkred")


##################### 13

#### a)
library(MASS)
data("Boston")
crim01 <- rep(0, length(Boston$crim))
crim01[Boston$crim > median(Boston$crim)] <- 1
Boston <- data.frame(Boston, crim01)
summary(Boston)


set.seed(1234)
train <- sample(1:dim(Boston)[1], dim(Boston)[1]*.7, rep=FALSE)
test <- -train
Boston.train <- Boston[train, ]
Boston.test <- Boston[test, ]
crim01.test <- crim01[test]

# Regressão Logistica
fit.glm1 <- glm(crim01 ~ . - crim01 - crim, data = Boston, family = binomial)
fit.glm1





library(corrplot)
corrplot::corrplot.mixed(cor(Boston[, -1]), upper="circle")

fit.glm <- glm(crim01 ~ nox + indus + age + rad, data = Boston, family = binomial)
probs <- predict(fit.glm, Boston.test, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, crim01.test)

mean(pred.glm != crim01.test)


# Modelo LDA
fit.lda <- lda(crim01 ~ nox + indus + age + rad , data = Boston)
pred.lda <- predict(fit.lda, Boston.test)
table(pred.lda$class, crim01.test)


mean(pred.lda$class != crim01.test)