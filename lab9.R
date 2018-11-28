dados<-scan("vendas.dat",what=list(telhados=0, gastos=0, clientes=0, marcas=0, potencial=0))
attach(dados)

Y=telhados
X=gastos
plot(X,Y)


X=clientes
plot(X,Y)


X=marcas
plot(X,Y)

X=potencial
plot(X,Y)

summary(potencial_lm)
correlacoes<-cor(matrix(unlist(dados),nrow=26))[1,]
correlacoes

#apenas com marcas
lm1<-lm(Y ~ marcas)

# usando marcas clientes
lm2<-lm(Y ~ marcas+clientes)

# usando marcas clientes potencial
lm3<-lm(Y ~ marcas+clientes+potencial)

# usando marcas clientes potencial gastos
lm4<-lm(Y ~ marcas+clientes+potencial+gastos)

summary(lm1)
# melhor modelo: p-valor pequeno, entre os menores erros, coeficientes significativamente grandes
summary(lm2)
summary(lm3)
summary(lm4)

anova(lm1)
anova(lm2)
anova(lm3)
anova(lm4)


## QUESTÃO 2
data2 <- matrix(c(37.310, 37.380,34.135,36.985,38.715,40.620,39.200,40.320,10,5,3,6,8,20,8,14,2,6,1,5,8,0,4,6,16,16,12,14,16,12,18,17), nrow = 8 , ncol =4 , byrow = FALSE)
Y=data2[,1]
Y
X=data2
X[,1]=1

beta=solve((t(X)%*%X))%*%(t(X) %*%Y)

modelo_matriz = function(beta1,x1,x2,x3){beta[1,] + beta[2,]*x1 + beta[3,]*x2 + beta[4,]*x3}

y_chapeu=c(modelo_matriz(beta = beta[1,],X[1,2],X[1,3],X[1,4]),
modelo_matriz(beta = beta[1,],X[2,2],X[2,3],X[2,4]),
modelo_matriz(beta = beta[1,],X[3,2],X[3,3],X[3,4]),
modelo_matriz(beta = beta[1,],X[4,2],X[4,3],X[4,4]),
modelo_matriz(beta = beta[1,],X[5,2],X[5,3],X[5,4]),
modelo_matriz(beta = beta[1,],X[6,2],X[6,3],X[6,4]),
modelo_matriz(beta = beta[1,],X[7,2],X[7,3],X[7,4]),
modelo_matriz(beta = beta[1,],X[7,2],X[8,3],X[8,4]))

modelo=lm(Y~X)

to_fit=c(beta[1,],7,3,12)
modelo_matriz(beta[1,],7,3,12)

residuos = Y-y_chapeu
ks.test(residuos,"pnorm")
plot(modelo)
