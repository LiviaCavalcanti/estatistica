casas_comodos = read.csv(file = "num_comodos.csv")
head(casas_comodos)


#### QUESTÃO 1 #####
total_domicilios = nrow(casas_comodos)
mais5 = sum(casas_comodos["num_comodos"] > 5)
total_comodos = sum(casas_comodos["num_comodos"])

## Letra A
p = mais5 / total_domicilios
media = total_comodos/total_domicilios

## Letra B
n = 20
alfa=0.05
cont=0

qnt_amostras = 10000
for (i in 1:qnt_amostras){
  amostra_domicilio = sample(casas_comodos$num_comodos,n, replace=FALSE)
  p_valor=t.test(amostra_domicilio, mu=7, alternative = "two.sided")$p.value
  if(p_valor<=alfa) {
    cont=cont+1
  }
}

prop_rejeicao=cont/qnt_amostras
prop_rejeicao

## Letra D
n = 20
alfa=0.05
contD=0

qnt_amostras = 10000
for (i in 1:qnt_amostras){
  amostra_domicilio = sample(casas_comodos$num_comodos,n)
  
  p_valor=prop.test(sum(amostra_domicilio>5), n=20,p=0.5)$p.value
  p_valor
  if(p_valor<=alfa) {
    contD=contD+1
  }
}

prop_rejeicaoD=contD/qnt_amostras
prop_rejeicaoD
contD

#aumentar no alfa, aumenta amostra

## Letra B
nC = 30
alfa=0.05
contC=0

qnt_amostras = 10000
for (i in 1:qnt_amostras){
  amostra_domicilio = sample(casas_comodos$num_comodos,n, replace=FALSE)
  p_valor=t.test(amostra_domicilio, mu=7, alternative = "two.sided")$p.value
  if(p_valor<=alfa) {
    contC=contC+1
  }
}

prop_rejeicaoC=cont/qnt_amostras
prop_rejeicaoC

nC = 20
alfa=0.10
contC=0

qnt_amostras = 10000
for (i in 1:qnt_amostras){
  amostra_domicilio = sample(casas_comodos$num_comodos,n, replace=FALSE)
  p_valor=t.test(amostra_domicilio, mu=7, alternative = "two.sided")$p.value
  if(p_valor<=alfa) {
    contC=contC+1
  }
}

prop_rejeicaoC=cont/qnt_amostras
prop_rejeicaoC

