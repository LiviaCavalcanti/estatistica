# QUESTÃO 1
X = c(1,2,3,3,5)
# LETRA A
N = c(2,3,5,10)
k = 1000

paste('média de X = ', mean(X))
paste('variância  de X = ', var(X)/2)

amostragem(qntAmostras = k, reposicao = T, X = X, tamAmostras = N)

# LETRA B
N = c(2,3)
k = 1000

amostragem <- function(qntAmostras, reposicao, X, tamAmostras) {
  for (n in tamAmostras) {
    meanVector = c()
    
    for (i in 1:qntAmostras) {
      v = mean(sample(X, n, replace = reposicao))
      meanVector = c(meanVector, v)
    }
    
    str = paste('para amostra de tamanho',n, sep = " ")
    print(str)
    mediaDasMedias = mean(meanVector)
    print(mediaDasMedias)
    print(paste('variância  de X = ', var(X)/n))
    print(var(meanVector))
    hist(meanVector, main = str)
  }
}

size = 100
# QUESTÃO 2

varVector = c()
meanVector = c() 
for (i in 1:k) {
  normalDist = rnorm(size, mean=100, sd=5)
  m = mean(normalDist)
  var = sum((normalDist - m)**2)/(100-1)
  print(paste("Média da Distribuição normal de número", i,"-", m, sep=" "))
  print(paste("Variância da Distribuição normal de número", i,"-", var, sep=" "))
  varVector=c(varVector, m)
  meanVector=c(meanVector, var)
}
hist(varVector, main = "Histograma das médias das Amostras de Distribuição Normal")
hist(meanVector, main = "Histograma da variância das Amostras de Distribuição Normal")
  

# QUESTÃO 3
alpha = 10
expMeanDist = c()
for (i in 1:k){
  expDistribution = rexp(size, rate=alpha)
  expMeanDist = c(expMeanDist, mean(expDistribution))
  
}
T = 1/expMeanDist
hist(T, main="Histograma do parâmetro T das médias das amostras Exp(10)")
