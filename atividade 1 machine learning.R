# Machine Learning
# Atividade Avaliativa 1 
####Igor Thales dos Santos

library(dplyr)
library(ggplot2)
library(MASS)
library(ISLR)

#Exercicio 8



y<-lm(mpg~horsepower,data=Auto)
summary(y)


### A)
## i, ii , iii
predict(y,data.frame(horsepower=98),interval="confidence")

predict(y,data.frame(horsepower=98),interval="prediction")



#B)
plot(Auto$horsepower,Auto$mpg,col="red")
abline(y)



#C)
par(mfrow=c(2,2))
plot(y)



# Exericio 9 
#a)
plot(Auto)


#b)

Autowithoutnames<-Auto
Autowithoutnames$name=NULL
cor(Autowithoutnames)


#c

y1<-lm(mpg~ .-name,data=Auto)
summary(y1)
## 
## Call:
## lm(formula = mpg ~ . - name, data = Auto)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.5903 -2.1565 -0.1169  1.8690 13.0604 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -17.218435   4.644294  -3.707  0.00024 ***
## cylinders     -0.493376   0.323282  -1.526  0.12780    
## displacement   0.019896   0.007515   2.647  0.00844 ** 
## horsepower    -0.016951   0.013787  -1.230  0.21963    
## weight        -0.006474   0.000652  -9.929  < 2e-16 ***
## acceleration   0.080576   0.098845   0.815  0.41548    
## year           0.750773   0.050973  14.729  < 2e-16 ***
## origin         1.426141   0.278136   5.127 4.67e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.328 on 384 degrees of freedom
## Multiple R-squared:  0.8215, Adjusted R-squared:  0.8182 
## F-statistic: 252.4 on 7 and 384 DF,  p-value: < 2.2e-16



# i) O valor de p inferior a 0,05 para estatística f sugere que
# podemos rejeitar a hipótese nula e concluir que existe pelo menos
# uma variável que é significativa na previsão de mpg.

# ii) displacement, weight, year e origin têm relação estatisticamente
# significativa com a resposta baseada em p-valores mais baixos

# iii) O coeficiente da variável year é significativo e positivo, 
# o que sugere que, se todas as outras variáveis forem constantes,
# em média, o mpg aumenta em 0.75 a cada ano.


#d)
par(mfrow=c(2,2))
plot(y1)


#residuals vs valor ajustado gráfico mostra a forma de u que 
#sugere não linearidade na relação
plot(predict(y1),rstudent(y1))

# Um gráfico de valor residual versus valor adaptado sugere que há
#uma certa observação para a qual os resíduos estudados são> 3, 
#indicando, portanto, outliers

plot(hatvalues(y1))

# which.max dá o índice de observação com maior estatística 
#de alavancagem

which.max(hatvalues(y1))




#e)
y2<-lm(mpg~.:.,Autowithoutnames)
summary(y2)

## 
## Call:
## lm(formula = mpg ~ .:., data = Autowithoutnames)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.6303 -1.4481  0.0596  1.2739 11.1386 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)   
## (Intercept)                3.548e+01  5.314e+01   0.668  0.50475   
## cylinders                  6.989e+00  8.248e+00   0.847  0.39738   
## displacement              -4.785e-01  1.894e-01  -2.527  0.01192 * 
## horsepower                 5.034e-01  3.470e-01   1.451  0.14769   
## weight                     4.133e-03  1.759e-02   0.235  0.81442   
## acceleration              -5.859e+00  2.174e+00  -2.696  0.00735 **
## year                       6.974e-01  6.097e-01   1.144  0.25340   
## origin                    -2.090e+01  7.097e+00  -2.944  0.00345 **
## cylinders:displacement    -3.383e-03  6.455e-03  -0.524  0.60051   
## cylinders:horsepower       1.161e-02  2.420e-02   0.480  0.63157   
## cylinders:weight           3.575e-04  8.955e-04   0.399  0.69000   
## cylinders:acceleration     2.779e-01  1.664e-01   1.670  0.09584 . 
## cylinders:year            -1.741e-01  9.714e-02  -1.793  0.07389 . 
## cylinders:origin           4.022e-01  4.926e-01   0.816  0.41482   
## displacement:horsepower   -8.491e-05  2.885e-04  -0.294  0.76867   
## displacement:weight        2.472e-05  1.470e-05   1.682  0.09342 . 
## displacement:acceleration -3.479e-03  3.342e-03  -1.041  0.29853   
## displacement:year          5.934e-03  2.391e-03   2.482  0.01352 * 
## displacement:origin        2.398e-02  1.947e-02   1.232  0.21875   
## horsepower:weight         -1.968e-05  2.924e-05  -0.673  0.50124   
## horsepower:acceleration   -7.213e-03  3.719e-03  -1.939  0.05325 . 
## horsepower:year           -5.838e-03  3.938e-03  -1.482  0.13916   
## horsepower:origin          2.233e-03  2.930e-02   0.076  0.93931   
## weight:acceleration        2.346e-04  2.289e-04   1.025  0.30596   
## weight:year               -2.245e-04  2.127e-04  -1.056  0.29182   
## weight:origin             -5.789e-04  1.591e-03  -0.364  0.71623   
## acceleration:year          5.562e-02  2.558e-02   2.174  0.03033 * 
## acceleration:origin        4.583e-01  1.567e-01   2.926  0.00365 **
## year:origin                1.393e-01  7.399e-02   1.882  0.06062 . 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.695 on 363 degrees of freedom
## Multiple R-squared:  0.8893, Adjusted R-squared:  0.8808 
## F-statistic: 104.2 on 28 and 363 DF,  p-value: < 2.2e-16
#interaction between displacement - acceleration, acceleration - origin, and acceleration - year are found to be statitically significant
#Adjusted R-square increased from 0.81 to 0.88 with addition of interaction terms.

#  interação entre:
# "displacement - acceleration", 
# "acceleration - origin",
# "acceleration - year" 
# São estatisticamente significantes. O R-quadrado ajustado
# aumentou de 0,81 para 0,88 com a adição de termos de interação.
anova(y1,y2)


#f)

y3<-lm(mpg~weight+I((weight)^2),Auto)
summary(y3)

plot(y3)


# Aqui o gráfico mostra a distribuição não normal dos termos de erro, 
# também a forma do funil é vista para os resíduos vs o gráfico de
# alavancagem

#_____________________________________________________________
# EXERCICIO 10


#a
sales<-lm(Sales~Price+Urban+US,data=Carseats)
summary(sales)


#b) O preço é uma variável contígua e seu coeficiente pode ser interpretado como um aumento médio nas vendas por aumento unitário nos preços, mantendo outras variáveis constantes
# US e Urban são variáveis categóricas com SIM codificado como 1
# e não como base 0. O coeficiente pode ser interpretado como
# vendas médias mais para 1 comparação com 0 quando outros parâmetros
# são mantidos constantes

#c)
#Sales=13.043469 - 0.054459*(Price) - (0.021916)*(1 ,if Urban is Yes 0 otherwise) + 1.200573 (1,if US is Yes 0 otherwise)

#d)
# Hipótese nula pode ser rejeitada para preço e USYes
# como p-valor é inferior a 0,05

#e)
sales1<-lm(Sales~Price+US,data=Carseats)
summary(sales1)

#f
anova(sales,sales1)

#g
confint(sales1)

#h
plot(predict(sales1),rstudent(sales1))

lev<-hat(model.matrix(sales1))
plot(lev)

4/nrow(Carseats)

#______________________________________________
# Exercicio 11

#a)
set.seed(1)
x=rnorm(100)

#b)
eps<-rnorm(100,mean=0,sd=sqrt(0.25))

#c)
y<- -1+0.5*x+eps
length(y)
#O comprimento do vetor y é 100. As estimativas dos coeficientes 
#B0 e B1 são dadas por (-1) e 0,5 respectivamente

#d)plot(x,y)

#e)
sim<-lm(y~x)
summary(sim)

#f)
plot(x,y)
abline(sim,col='red')
abline(-1,0.5,col="green")
legend("topleft",c("Least square","Population"),
       col=c("red","green"),lty=c(1,1))

#g)
polyn<-lm(y~x+I(x^2))
summary(polyn)

anova(polyn,sim)

# A adição do termo x2 não melhora o modelo. 
# Isso é quantificado pelo teste anova entre dois modelos que não 
# rejeitam a hipótese nula de dois modelos serem diferentes.
# Além disso, o valor de p do coeficiente x2 é maior que 0,5,
# indicando sua insignificância estatística.

#h)
set.seed(1)
x=rnorm(100)
eps<-rnorm(100,mean=0,sd=sqrt(0.1))
y=-1+0.5*x+eps
simlow<-lm(y~x)
summary(simlow)


plot(x,y)
abline(simlow,col='red')
abline(-1,0.5,col="blue")
legend("topleft",
       c("Least square line","True Population line - less Variance"),
       col=c("red","blue"),lty=c(1,1))

#i)
set.seed(1)
x=rnorm(100)
eps<-rnorm(100,mean=0,sd=sqrt(4))
y=-1+0.5*x+eps
simhigh<-lm(y~x)
summary(simhigh)


plot(x,y)
abline(simhigh,col='orange')
abline(-1,0.5,col="blue")
legend("topleft",
       c("Least square line","True Population line - high Variance"),
       col=c("orange","blue"),lty=c(1,1))


#j)

#Os intervalos de confiança para coeficientes para dataset
#com menos ruído são dados por -1,07, 0,43, -0,95, 0,57

# Os intervalos de confiança para os coeficientes do conjunto de dados
# original são fornecidos por -1,12, 0,39, -0,92, 0,61.

# Os intervalos de confiança para coeficientes para dataset com mais 
# ruído são dados por -1.46, 0.07, -0.69, 0.93.

# Com o aumento do ruído, os intervalos de confiança 
#aumentam.

#_____________________________________________________________
#EXERCICIO 15

#a)
boston.zn<-lm(crim~zn,data=Boston)
summary(boston.zn)

boston.indus<-lm(crim~indus,data=Boston)
summary(boston.indus)

boston.chas<-lm(crim~chas,data=Boston)
summary(boston.chas)

boston.nox<-lm(crim~nox,data=Boston)
summary(boston.nox)

boston.rm<-lm(crim~rm,data=Boston)
summary(boston.rm)

boston.age<-lm(crim~age,data=Boston)
summary(boston.age)

boston.dis<-lm(crim~dis,data=Boston)
summary(boston.dis)

boston.rad<-lm(crim~rad,data=Boston)
summary(boston.rad)

boston.tax<-lm(crim~tax,data=Boston)
summary(boston.tax)

boston.ptratio<-lm(crim~ptratio,data=Boston)
summary(boston.ptratio)

boston.black<-lm(crim~black,data=Boston)
summary(boston.black)

boston.lstat<-lm(crim~lstat,data=Boston)
summary(boston.lstat)

boston.medv<-lm(crim~medv,data=Boston)
summary(boston.medv)


#Os modelos acima mostram que apenas a variável chas não é 
#significativa na previsão da taxa de criminalidade per capita.
#Com base no valor p de sua estatística t, não podemos rejeitar 
#a hipótese nula. Para todas as outras variáveis, o valor p é muito 
#pequeno e podemos rejeitar a hipótese nula e concluir que existe uma
#relação estatisticamente significativa entre o preditor e a resposta.


#b)
boston.all<-lm(crim~.,Boston)
summary(boston.all)

#c)simple<-vector("numeric",0)
simple<-c(simple,boston.zn$coefficients[2])
simple<-c(simple,boston.indus$coefficients[2])
simple<-c(simple,boston.chas$coefficients[2])
simple<-c(simple,boston.nox$coefficients[2])
simple<-c(simple,boston.rm$coefficients[2])
simple<-c(simple,boston.age$coefficients[2])
simple<-c(simple,boston.dis$coefficients[2])
simple<-c(simple,boston.rad$coefficients[2])
simple<-c(simple,boston.tax$coefficients[2])
simple<-c(simple,boston.ptratio$coefficients[2])
simple<-c(simple,boston.black$coefficients[2])
simple<-c(simple,boston.lstat$coefficients[2])
simple<-c(simple,boston.medv$coefficients[2])
multi<-vector("numeric",0)
multi<-c(multi,boston.all$coefficients)
multi<-multi[-1]
plot(simple,multi,col='blue')

#d)
boston.zn1<-lm(crim~poly(zn,3),data=Boston)
summary(boston.zn1)

par(mfrow=c(2,2))
plot(boston.zn1)

boston.indus1<-lm(crim~poly(indus,3),data=Boston)
summary(boston.indus1)

par(mfrow=c(2,2))
plot(boston.indus1)

boston.nox1<-lm(crim~poly(nox,3),data=Boston)
summary(boston.nox1)

par(mfrow=c(2,2))

plot(boston.nox1)

boston.rm1<-lm(crim~poly(rm,3),data=Boston)
summary(boston.rm1)

par(mfrow=c(2,2))

plot(boston.rm1)

boston.age1<-lm(crim~poly(age,3),data=Boston)
summary(boston.age1)

par(mfrow=c(2,2))

plot(boston.age1)

boston.dis1<-lm(crim~poly(dis,3),data=Boston)
summary(boston.dis1)

par(mfrow=c(2,2))

plot(boston.dis1)

boston.rad1<-lm(crim~poly(rad,3),data=Boston)
summary(boston.rad1)

plot(boston.rad1)




