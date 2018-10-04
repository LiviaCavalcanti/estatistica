casas_comodos = read.csv(file = "num_comodos.csv")
head(casas_comodos)

funcao_p_chapeu <- function(amostra_comodos, n) {
  qnt_comodos_maior5 = sum(amostra_comodos>5)  
  return(qnt_comodos_maior5/n)
}

erro_p_chapeu <- function(p_chapeu, z_gama, n) {
  return(z_gama * (sqrt((p_chapeu*(1-p_chapeu)) / n)))
}

erro_mediaDesconhecida<- function(t_alfa, desvio_padrao, n) {
  return(t_alfa * (sd(amostra_domicilio) / sqrt(n)))
}

proporcao_parametroVerdadeiro = function(limiteSuperior, limiteInferior, parametro){
  contador = 0
  for(i in 1:10000){
    if(limiteSuperior[i] >= parametro && limiteInferior[i] <= parametro){
      contador = contador + 1
    }
  }
  return(contador/10000)
}

#### QUESTÃO 1 #####
total_domicilios = nrow(casas_comodos)
mais5 = sum(casas_comodos["num_comodos"] > 5)
total_comodos = sum(casas_comodos["num_comodos"])

## Letra A
p = mais5 / total_domicilios
media = total_comodos/total_domicilios

## Letra B

n = 20
gama = 0.95
z_gama = 1.96
t_alfa = 2.262
qnt_amostras = 10000
limite_p1 = c()
limite_p2 = c()
limite_media1 = c()
limite_media2 = c()

for (i in 1:qnt_amostras){
  amostra_domicilio = sample(casas_comodos$num_comodos,n, replace=FALSE)
   p_chapeu = funcao_p_chapeu(amostra_domicilio, n)
   media_amostra = mean(amostra_domicilio)
   
   erro_p = erro_p_chapeu(p_chapeu = p_chapeu, z_gama=z_gama, n=n)
   inferior_p = p_chapeu - erro_p
   superior_p = p_chapeu + erro_p
   

   erro_media = erro_mediaDesconhecida(t_alfa,sd(amostra_domicilio),n)
   inferior_media = media_amostra - erro_media
   superior_media = media_amostra + erro_media
   
   limite_p1 = c(limite_p1, superior_p)
   limite_p2 = c(limite_p2, inferior_p)
   limite_media1 = c(limite_media1, superior_media)
   limite_media2 = c(limite_media2, inferior_media)
 
   
}


## Letra C
proporcao_p = proporcao_parametroVerdadeiro(limite_p1, limite_p2, p)
proporcao_p
proporcao_media = proporcao_parametroVerdadeiro(limite_media1, limite_media2, media)
proporcao_media
