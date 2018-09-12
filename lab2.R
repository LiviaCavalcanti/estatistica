# QUESTÂO 1
M = 5000
a = 1
c = 5
X_i = 1
vetor = c(X_i)

for(d in 2:10000) {

  X_i = (a * X_i + c)  %% M
  vetor = c(vetor, X_i)
}
vetor = vetor/M
hist(vetor, main = "Histograma de valores aleatórios por congruência")

# QUESTÂO 2

v = -(log(1 - vetor)) / 2
hist(v, main = "Variáveis alatórias a partir da f.d.a")

# QUESTÂO 3
bin_dist = rbinom(1000, 10,  0.2)
hist(rbinom(1000, 10,  0.2) )

# QUESTÃO 4
chi_sq = rnorm(1000)^2 + rnorm(1000)^2 + rnorm(1000)^2
hist(chi_sq, main = "Distribuição qui-quadrada por meio da Normal")
