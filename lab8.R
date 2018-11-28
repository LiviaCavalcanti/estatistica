data = read.csv("DadosCiaMB.csv",sep = ";", dec = ",")
cor(data$idade,data$salario)

X<-data$idade
Y<-data$salario
model=lm(Y~X)
summary(model)

plot(X,Y)
abline(lm(Y ~ X))



#### QUESTÃO 2

cor(Y,X)
modelA=lm(Y~X)
summary(modelA)
plot(X,Y)
abline(lm(Y ~ X))

cor(Y,(X**2))
modelB=lm(Y~(X**2))
summary(modelB)
plot(Y,(X**2))
abline(lm((X**2)~Y))

cor(Y,log(X))
modelC=lm(Y~log(X))
summary(modelC)
plot(log(X),Y)
abline(lm(Y ~ log(X)))

cor(Y,sqrt(X))
modelD=lm(Y~sqrt(X))
summary(modelD)
plot(Y,sqrt(X))
abline(lm(sqrt(X)~Y))

Z<-1/X
cor(Y,Z)
modelE=lm(Y~Z)
summary(modelE)
plot(Y,Z)
abline(lm(Z~Y))
