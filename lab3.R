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
# QUESTÃO 2
NormalDist = rnorm(1000,mean = 100, sd =5)

for (i in 1:1000) {
  v = NormalDist
  meanVector = c(meanVector, v)
}
  
  str = paste('para amostra de tamanho',n, sep = " ")
  print(str)
  mediaDasMedias = mean(meanVector)
  print(mediaDasMedias)
  print(paste('variância  de X = ', var(X)/n))
  print(var(meanVector))
  hist(meanVector, main = str)

