library(MASS) ##dados
library(ISLR2) ##dados
head(Boston)
View(Boston)
?Boston
dim(Boston)
attach(Boston)

chas <- as.factor(chas)

plot(crim,medv)
abline(lm(medv ~ crim, Boston)) #reta de regressão
cor(crim,medv, method = "pearson") #correlação de Pearson

abline(3,4)#desenha uma reta com declive 4 e ordenada na origem 3


plot(zn,medv)
abline(lm(medv ~ zn, Boston))
cor(zn,medv, method = "pearson")

plot(indus,medv)
abline(lm(medv ~ indus, Boston))
cor(indus,medv, method = "pearson")

plot(chas,medv)

plot(nox,medv)
abline(lm(medv ~ nox, Boston))
cor(nox,medv, method = "pearson")

plot(rm,medv)
abline(lm(medv ~ rm, Boston))
cor(rm,medv, method = "pearson")

plot(age,medv)
abline(lm(medv ~ age, Boston))
cor(age,medv, method = "pearson")

plot(dis,medv)
abline(lm(medv ~ dis, Boston))
cor(dis,medv, method = "pearson")

plot(rad,medv)
abline(lm(medv ~ rad, Boston))
cor(rad,medv, method = "pearson")

plot(tax,medv)
abline(lm(medv ~ tax, Boston))
cor(tax,medv, method = "pearson")

plot(ptratio,medv)
abline(lm(medv ~ ptratio, Boston))
cor(ptratio,medv, method = "pearson")

plot(lstat,medv, col="blue", pch=20)
abline(lm(medv ~ lstat, Boston), lwd=3, col="red")
cor(lstat,medv, method = "pearson")

plot(1:20, 1:20, pch = 1:20)


cor(Boston, method = "pearson")

lm.fit <- lm(medv ~ crim + zn + indus+ chas +
               nox + rm + age + dis + rad + 
               tax + ptratio + lstat, Boston)


?lm
lm.fit
summary(lm.fit)
lm.fit <- lm(medv ~ ., Boston)
lm.fit
summary(lm.fit)


?lm
names(lm.fit)
lm.fit$residuals
coef(lm.fit)
coef<-coef(lm.fit)
coef[2]

library(car)
library(carData)
vif(lm.fit)
 
####foi verificada multicolinearidade com a variavel tax
####rad e tax têm correlações elevadas



lm.fit2 <- lm(medv ~ . - tax, Boston)
summary(lm.fit2)
vif(lm.fit2)
 

lm.fit8 <- lm(medv ~ . - rad, Boston)
summary(lm.fit8)
vif(lm.fit8)

###preferi retirar a variável tax
###nesse modelo a variável age e indus não são significativas


lm.fit3 <- lm(medv ~ . -tax -age, Boston)
summary(lm.fit3)
vif(lm.fit3)

lm.fit4 <- lm(medv ~ . - tax-age-indus, Boston)
summary(lm.fit4)
vif(lm.fit4)

lm.fit5 <- lm(medv ~ . - tax -age -indus -rad, Boston)
summary(lm.fit5)
vif(lm.fit5)


?confint

new=data.frame(crim = (c(0.5, 1.0, 0.3)), zn = (c(20, 80,30)), 
               chas = (c(0, 1,1)), nox = (c(0.5, 0.45, 0.6)),
               rm = (c(6, 5,7)), dis = (c(3, 4,5)), 
               ptratio = (c(20, 19,18)), lstat = (c(5, 10,7)))




lm.fit6 <- lm(medv ~ lstat+age+chas+crim+zn+nox+rm+dis+ptratio, Boston)
summary(lm.fit6)
confint(lm.fit6, level=0.95)

predict(lm.fit6 , data.frame(lstat = c(5, 10, 15), age=c(0.5,4,5), 
                             chas=c(0,1,1),crim=c(0.5,1,0.3), zn=c(20, 80,30), 
                             nox = c(0.5, 0.45, 0.6), rm = c(6, 5,7), 
                             dis = c(3, 4,5), ptratio = c(20, 19,18)),
        interval = "prediction")

predict(lm.fit6 , data.frame(lstat = c(5, 10, 15), age=c(0.5,4,5), 
                             chas=c(0,1,1),crim=c(0.5,1,0.3), zn=c(20, 80,30), 
                             nox = c(0.5, 0.45, 0.6), rm = c(6, 5,7), 
                             dis = c(3, 4,5), ptratio = c(20, 19,18)),
          interval = "confidence")




###seleção de variáveis
library(leaps)
regfit.1 <- regsubsets(medv ~ .-tax, Boston, nvmax = 11) #A função regsubsets() (parte da biblioteca leaps)
# identifica o melhor modelo que contém um determinado número de preditores, 
#onde "melhor" é quantificado usando RSS (Residual Sum of Squares), que é a soma dos quadrados dos resíduos.
?regsubsets
summary(regfit.1)
summary(regfit.1)$which
summary(regfit.1)$adjr2
summary(regfit.1)$cp
summary(regfit.1)$bic


par(mfrow = c(2, 2))
plot(summary(regfit.1)$rss , xlab = "Number of Variables",
       ylab = "RSS", type = "l")
which.min(summary(regfit.1)$rss)
points(which.min(summary(regfit.1)$rss), summary(regfit.1)$rss[which.min(summary(regfit.1)$rss)], col = "red", cex = 2,
       pch = 20)

plot(summary(regfit.1)$adjr2 , xlab = "Number of Variables",
       ylab = "Adjusted RSq", type = "l")

which.max(summary(regfit.1)$adjr2)
points(which.max(summary(regfit.1)$adjr2), summary(regfit.1)$adjr2[which.max(summary(regfit.1)$adjr2)], col = "red", cex = 2,
         pch = 20)

plot(summary(regfit.1)$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "l")
which.min(summary(regfit.1)$cp)
points(which.min(summary(regfit.1)$cp), summary(regfit.1)$cp[which.min(summary(regfit.1)$cp)], col = "red", cex = 2,
         pch = 20)

plot(summary(regfit.1)$bic , xlab = "Number of Variables",
       ylab = "BIC", type = "l")
which.min(summary(regfit.1)$bic)
points(which.min(summary(regfit.1)$bic), summary(regfit.1)$bic[which.min(summary(regfit.1)$bic)], col = "red", cex = 2,
         pch = 20)

summary(regfit.1)$which

coef(regfit.1, 9)

lm.fit7 <- lm(medv ~ . -tax -age -indus, Boston)
summary(lm.fit7)
vif(lm.fit7)





###seleção de variáveis Forward and Backward Stepwise 

regfit.fwd <- regsubsets(medv ~ .-tax, Boston, nvmax = 11,
                         method = "forward")
summary(regfit.fwd)

par(mfrow = c(2, 2))
plot(summary(regfit.fwd)$rss , xlab = "Number of Variables",
     ylab = "RSS", type = "l")
which.min(summary(regfit.fwd)$rss)
points(which.min(summary(regfit.fwd)$rss), summary(regfit.fwd)$rss[which.min(summary(regfit.fwd)$rss)], col = "red", cex = 2,
       pch = 20)

plot(summary(regfit.fwd)$adjr2 , xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

which.max(summary(regfit.fwd)$adjr2)
points(which.max(summary(regfit.fwd)$adjr2), summary(regfit.fwd)$adjr2[which.max(summary(regfit.fwd)$adjr2)], col = "red", cex = 2,
       pch = 20)

plot(summary(regfit.fwd)$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "l")
which.min(summary(regfit.fwd)$cp)
points(which.min(summary(regfit.fwd)$cp), summary(regfit.fwd)$cp[which.min(summary(regfit.fwd)$cp)], col = "red", cex = 2,
       pch = 20)

plot(summary(regfit.fwd)$bic , xlab = "Number of Variables",
     ylab = "BIC", type = "l")
which.min(summary(regfit.fwd)$bic)
points(which.min(summary(regfit.fwd)$bic), summary(regfit.fwd)$bic[which.min(summary(regfit.fwd)$bic)], col = "red", cex = 2,
       pch = 20)

summary(regfit.fwd)$which

coef(regfit.fwd , 9)


regfit.bwd <- regsubsets(medv ~ .-tax, Boston, nvmax = 11, 
                         method = "backward")
summary(regfit.bwd)

par(mfrow = c(2, 2))
plot(summary(regfit.bwd)$rss , xlab = "Number of Variables",
     ylab = "RSS", type = "l")
which.min(summary(regfit.bwd)$rss)
points(which.min(summary(regfit.bwd)$rss), summary(regfit.bwd)$rss[which.min(summary(regfit.bwd)$rss)], col = "red", cex = 2,
       pch = 20)

plot(summary(regfit.bwd)$adjr2 , xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

which.max(summary(regfit.bwd)$adjr2)
points(which.max(summary(regfit.bwd)$adjr2), summary(regfit.bwd)$adjr2[which.max(summary(regfit.bwd)$adjr2)], col = "red", cex = 2,
       pch = 20)

plot(summary(regfit.bwd)$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "l")
which.min(summary(regfit.bwd)$cp)
points(which.min(summary(regfit.bwd)$cp), summary(regfit.bwd)$cp[which.min(summary(regfit.bwd)$cp)], col = "red", cex = 2,
       pch = 20)

plot(summary(regfit.bwd)$bic , xlab = "Number of Variables",
     ylab = "BIC", type = "l")
which.min(summary(regfit.bwd)$bic)
points(which.min(summary(regfit.bwd)$bic), summary(regfit.bwd)$bic[which.min(summary(regfit.bwd)$bic)], col = "red", cex = 2,
       pch = 20)

summary(regfit.bwd)$which

coef(regfit.bwd , 10)

#####outra funcão para seleção
regfit.ste<-step()
?step


######
new=data.frame(crim = (c(0.5, 1.0, 0.3)), zn = (c(20, 80,30)), 
               chas = (c(0, 1,1)), nox = (c(0.5, 0.45, 0.6)),
               rm = (c(6, 5,7)), dis = (c(3, 4,5)), 
               ptratio = (c(20, 19,18)), rad = (c(1,3,5)), lstat = (c(5, 10,7)))

View(new)


lm.fit6 <- lm(medv ~ lstat+chas+crim+zn+nox+rm+dis+ptratio+rad, Boston)
summary(lm.fit6)
confint(lm.fit6, level=0.95)

predict(lm.fit6 , new,
        interval = "prediction")

predict(lm.fit6 , new,
        interval = "confidence")





######Com variáveis qualitativas com mais de duas categorias

View(Carseats)
?Carseats
attach(Carseats)
lm.fit <- lm(Sales ~ .,
             data = Carseats)
summary(lm.fit)
contrasts(ShelveLoc)
