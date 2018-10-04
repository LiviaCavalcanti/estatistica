casas_comodos = read.csv(file = "num_comodos.csv")
head(casas_comodos)

funcao_p_chapeu <- function(amostra_comodos, n) {
  qnt_comodos_maior5 = sum(amostra_comodos>5)  
  return(qnt_comodos_maior5/n)
}

erro_p_conservador <- function(z_gama, n) {
  return(z_gama*(sqrt(1/(4*n))))
}

erro_p <- function(z_gama, p_chapeu, n) {
  return(z_gama*(sqrt(((1-p_chapeu)*p_chapeu)/n)))
}

intervaloConfianca_p_conservador <- function(p_chapeu, z_gama, n) {
  erro = erro_p_conservador(z_gama, n)
  return(c(p_chapeu-erro, p_chapeu+erro))
}

intervaloConfianca_p <- function(p_chapeu, z_gama, n) {
  erro = erro_p(z_gama, p_chapeu, n)
  return(c(p_chapeu-erro, p_chapeu+erro))
}

# media

desvio_padrao <- function(amostra_comodos, media_amostra, n) {
  return(sqrt( sum((amostra_comodos-media_amostra)^2) / (n-1) ))
}

erro_media <- function(t_alfa, desvio_padrao, n) {
  return(t_alfa*(desvio_padrao/sqrt(n)))
}
intervaloConficanca_mediaDesconhecida<- function(media_amostra, t_alfa, desvio_padrao, n) {
  erro = erro_media(t_alfa, desvio_padrao, n)
  return(c(media_amostra-erro, media_amostra+erro))
}
#### QUESTÃO 1 #####
total_domicilios = nrow(casas_comodos)
mais5 = sum(casas_comodos["num_comodos"] > 5)
total_comodos = sum(casas_comodos["num_comodos"])

## Letra A
p = mais5 / total_domicilios
media = total_comodos/total_domicilios

## Letra B
dummy_matrix = matrix(NA, nrow=10000, ncol=6)
n = 20
gama = 0.95
z_gama = 1.96
t_alfa = 2.262

for (i in 1:10000){
  amostra_domicilio = sample(1:total_domicilios,n) 
   p_chapeu = funcao_p_chapeu(casas_comodos[amostra_domicilio,], n)
   ## Intervalo Proporção
   p_intervalo = intervaloConfianca_p(p_chapeu, z_gama, n)
   dummy_matrix[i,1] = p_intervalo[1]
   dummy_matrix[i,2] = p_intervalo[2]
   p_intervalo_conservador = intervaloConfianca_p_conservador(p_chapeu, z_gama, n)
   dummy_matrix[i,3] = p_intervalo_conservador[1]
   dummy_matrix[i,4] = p_intervalo_conservador[2]
   ## Intervalo média
   media_amostra = mean(amostra_domicilio)
   media_intervalo = intervaloConficanca_mediaDesconhecida(media_amostra, t_alfa, desvio_padrao(amostra_domicilio, media_amostra, n), n)
   dummy_matrix[i,5] = media_intervalo[1]
   dummy_matrix[i,6] = media_intervalo[2]
   
}

intervalos_confianca = as.data.frame(dummy_matrix)
colnames(intervalos_confianca) <- c('proporcao1', 'proporcao2', 'proporcao_conservador1', 'proporcao_conservador2', 'media1', 'media2')

## Letra C
nrow(intervalos_confianca[intervalos_confianca[,1] < p && intervalos_confianca[,2]>p])
nrow(intervalos_confianca[intervalos_confianca[,3] < p && intervalos_confianca[,4]>p])
nrow(intervalos_confianca[intervalos_confianca[,5] < media && intervalos_confianca[,6]>media])
