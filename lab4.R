#### QUESTÃO 1 ####

n = c(10, 20,30,50,100)
dummie_matrix <- matrix(NA,ncol = 4, nrow = 5)


for (i in 1:5){
  medias_dist=c()
  medianas_dist=c()
  coluna_indice=1
  for (j in 1:10000) {
    pop=rnorm(n[i], mean=500, sd=sqrt(10))
    medias_dist=c(medias_dist, mean(pop))
    medianas_dist=c(medianas_dist, median(pop))
  }
  hist(medias_dist, main = paste("Histograma das medias para n = ", n[i]))
  hist(medianas_dist, main = paste("Histograma das medianas para n = ", n[i]))
  
  # variancia e distribuição do vetor de médias
  media_medias=mean(medias_dist)
  dummie_matrix[i,coluna_indice]=media_medias  
  coluna_indice=coluna_indice+1
  
  var_medias=var(medias_dist)
  dummie_matrix[i,coluna_indice]=var_medias  
  coluna_indice=coluna_indice+1
  
  # variancia e distribuição do vetor de medianas
  media_medianas=mean(medianas_dist)
  dummie_matrix[i,coluna_indice]=media_medianas  
  coluna_indice=coluna_indice+1
  var_medianas=var(medianas_dist)
  dummie_matrix[i,coluna_indice]=var_medianas  
  
}

df_dist = data.frame(dummie_matrix)
colnames(df_dist) = c("media_medias", "var_medias", "media_medianas", "var_medianas")
df_dist

#### QUESTÃO 2 ####

dummie_matrix2 <- matrix(NA,ncol = 2, nrow = 1)
qnt_amostra_unif=100
T1_dist_unif=c()
T2_dist_unif=c()
coluna_indice=1
for (j in 1:10000) {
  pop_unif=runif(qnt_amostra_unif, min=0, max=500)
  max_pop=max(pop_unif)
  T1_dist_unif=c(T1_dist_unif, 2*(mean(pop_unif)))
  T2_dist_unif=c(T2_dist_unif, (((qnt_amostra_unif+1)/qnt_amostra_unif)*max_pop))
}

hist(T1_dist_unif, main="Histograma do estimador T1")
hist(T2_dist_unif, main="Histograma do estimador T2")
mean(T1_dist_unif)-500
mean(T2_dist_unif)-500
var(T1_dist_unif)
var(T2_dist_unif)
