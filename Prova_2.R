# Introdução a softwares estatísticos
# Data: 26/08/2019
# Segunda Prova

# Primeira questão ------------------------------------------------

# Implemente a simulação de n lançamentos de uma moeda honesta
# e aproxime a probabilidade de obter resultado cara.

p_moeda <- function(n){
  resultados <- numeric(n)
  for(i in 1:n)
    resultados[i] <- sample(x = c(0,1), size = 1)
  mean(resultados)
}

# Fixando a semente e aplicando a função
set.seed(0)
p_moeda(1e4)
# Resultado: Para 100.000 repetições, estima-se que a probabilidade
# de dar cara é de aproximadamente 0.496

# Segunda Questão --------------------------------------------------------

# Seja um experimento aleatório com as seguintes regras:
# 1) Se escolhe um número entre 2 e 400
# 2) Retira-se duas bolas numeradas de 1 a 200 de uma urna (com reposição)
# 3) As retiradas ocorrem até a soma do números retirados for igual
#    ao número fixado inicialmente.
# Simule 10000 repetições desse experimento e estime o número médio de
# retiradas esperado.

m_retiradas <- function(n, N = 1e4){
  resultados <- numeric(N)
  for(i in 1:N){
    repeat{
      retirada <- sum(sample(x = 1:200, size = 2, replace = T))
      resultados[i] <- resultados[i] + 1
      if(retirada == n)
        break
    }
  }
  mean(resultados)
}

# Fixando-se a semente e aplicando a função
set.seed(0)
m_retiradas(13, 1000)
# Resultado: Para 1000 repetições, espera-se que a quantidade de
# retiradas até a soma ser igual ao número desejado é, em média,
# aproximadamente 3197 retiradas.

# Terceira Questão ---------------------------------------------------

# Considere um jogo com as regras abaixo:
# 1) Um jogador paga 15 dólares para começar, e ganha 1,50 por
#   lançamento de dois dados,
# 2) Caso a soma do primeiro lançamento seja par e múltipla de 3
#    o jogo acaba imediatamente,
# 3) Caso contrário, o jogador realisa os próximos lançamentos até
#    obter soma 11 ou 12.
# Simule 100000 repetições desse experimento e estime o lucro
# esperado de um jogador.

m_jogo <- function(N = 1e5){
  lancamentos <- numeric(N)
  lucro <- numeric(N)
  for(i in 1:N){
    lucro[i] <- -15
    repeat{
      soma_dados <- sum(sample(x = 1:6, size = 2, replace = T))
      lancamentos[i] <- lancamentos[i] + 1
      lucro[i] <- lucro[i] + 1
      if((soma_dados%%6) == 0 & lancamentos[i] == 1)
        break
      if(soma_dados%in% c(11,12) & lancamentos[i] != 1)
        break
    }
  }
  mean(lucro)
}

# Fixando a semente e aplicando-se a função:
set.seed(0)
m_jogo()
# Resultado: Espera-se que um jogador perca, em média,
# 3,97 dólares por partida.


# Quarta Questão -----------------------------------------------------

# Crie uma função myderiv() que determine a derivada de uma função em 
# um ponto p, sabendo que ela é definida pelo limite de 
# (f(p + h) - f(p))/h quando h tende a zero, isto é, pode ser aproximada por
# um h suficientemente pequeno, mas diferente de zero.


myderiv <- function(f,x,h = 1e-3){
  (f(x + h) - f(x))/h
}


# Exemplos de funções:
f_polinomial_1 <- function(x){
  x^2 + x + 1
}

f_polinomial_2 <- function(x){
  x^3 - 2*x^2 + x
}

f_exponencial <- function(x){
  exp(2*x)
}

f_logaritmo <- function(x){
  log(3*x)
}

myderiv(f_polinomial_1, 1)
myderiv(f_polinomial_2, 1)
myderiv(f_exponencial, 0)
myderiv(f_logaritmo, 1)