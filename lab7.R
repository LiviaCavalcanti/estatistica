## QUESTÃO 1 ##
prob_Cara = 0.67
prob_Coroa = 1/3


probs = dbinom(c(0:4), 4, 2/5)

freq_esperadas = probs*625
freq_observadas = c(72,204,228,101,20)

qui_quadrado = sum(((freq_observadas - freq_esperadas)**2)/freq_esperadas)

p = pchisq(qui_quadrado, df=4, lower.tail = F)

# como p_valor é muito maior do que o alfa(0.05) então não rejeitamos H0, logo a moeda eh viciada




## QUESTÃO 2 ##

data = read.csv("DadosCiaMB.csv",sep = ";")
tabela_cruzada = table(data[,c("Instrucao","procedencia")])
tabela_cruzada
aux = c(sum(tabela_cruzada[,1]), sum(tabela_cruzada[,2]), sum(tabela_cruzada[,3]))
aux1 = c(sum(tabela_cruzada[1,]), sum(tabela_cruzada[2,]), sum(tabela_cruzada[3,]))
m = matrix(, nrow = 3, ncol = 3)

total = sum(sum(aux))

for (i in c(1:3)) {
  for (j in c(1:3)) {
    m[i,j] = (aux[i] * aux1[j])/total
  }  
}

sum=0
for (i in c(1:3)) {
  for (j in c(1:3)) {
    sum  = sum + (((tabela_cruzada[i,j] - m[i,j])**2)/m[i,j])
    print (((tabela_cruzada[i,j] - m[i,j])**2)/m[i,j])
    print (((tabela_cruzada[i,j] - m[i,j])**2))
  }  
}

p2 = pchisq(sum, df=4, )
p2
sum
crosstable

tabela_cruzada
m
total
