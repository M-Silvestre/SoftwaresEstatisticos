# Introdu��o a softwares estat�sticos
# Data: 28/08/2019
# Aula: 18
# Assunto: Algoritmos para gera��o de n�meros aleat�rios

# Gera��o de N�meros pseudoaleat�rios -------------------------


# Os princ�pios e conceitos por tr�s da gera��o de n�meros aleat�rios
# como abordados nos slides de aula (fun��o quantil�ca, etc)
# v�o al�m do objetivo da disciplina.


# Apesar disso, a implementa��o de seus algoritmos, n�o est� al�m
# dos conhecimentos abordados em aula.


# Exerc�cio 1: implemente uma fun��o que recebe uma quantidade
# n�merica e retorna essa quantidade de n�meros que seguem uma
# distribui��o normal pelo m�todo polar. O algoritmo �:

# 1) Gerar duas observa��es uniformes em 0 e 1: U1 e U2.
# 2) Efetuar a transforma��o U'1 = 2*U1 - 1 e U'2 = 2*U2 - 1,
#    W = (U'1)^2 + (U'2)^2
# 3) Se W > 1, volte a 1)
# 4) Retorne as observa��es X = sqrt(-log(W)/W)*U'1 e
#    Y = sqrt(-log(W)/W)*U'2


# Solu��o:

mynorm <- function(n = 1){
  
  vetor_norm <- NULL
  
  repeat{
    # Geramos as observa��es uniformes.
    u <- runif(n = 2L, min = 0, max = 1)
    
    # As transformamos em u_1, u_2, e w
    u_1 <- 2*u[1] - 1
    u_2 <- 2*u[2] - 1
    w <- u_1^2 + u_2^2
    
    # Caso w  seja maior que, descartar o valor e tentar de novo
    if(w > 1)
      next
    
    x = sqrt(-log(w)/w)*u_1
    y = sqrt(-log(w)/w)*u_2
    
    # Concatenar o vetor de valores com a novas observa��es x e y
    vetor_norm <- c(vetor_norm, x, y)
    
    # Parar quando a quantidade n for alcan�ada ou superada
    if(length(vetor_norm) >= n)
      break
  }
  
  # OBS: O algoritmo sempre gera uma quantidade par de valores,
  # mas a fun��o deve retornar exatamante a quantidade n de valores:
  
  vetor_norm[1:n]
  
}

mynorm(5)

# O comportamento desses valores se aproxima da normal?

hist(mynorm(30000))


# Exerc�cio 2: Crie uma fun��o para gerar n�meros pseudoaleat�rios
# que seguem uma distribui��o exponencial a partir da fun��o quant�lica
# (inverso da acumulada) da exponencial: x = -log(1 - u)/lambda, onde
# u � uniforme entre 0 e 1, x � exponencial e lambda � o par�metro de x.

myexp <- function(n = 1, lambda = 1){
  
  u <- runif(n,0,1)
  
  - log(1 - u)/lambda

}

myexp()

hist(myexp(1e4))
hist(myexp(1e4, 0.2))
hist(myexp(1e4, 3))

# OBS: o algoritmo que transforma observa��es uniformes em exponenciais
# � bem mais simples que os que transformam uniformes em normais porque
# a fun��o de distribui��o acumulada da exponencial � invert�vel.


# Exerc�cio 3: Implemente outro algoritmo que gere n�meros pseudoaleat�rios
# normais, agora pelo algoritmo dado pelo m�todo de Box-Muller:
# 1) Gere U1 e U2 uniformemente entre 0 e 1.
# 2) Gere R^2 = -2*log(U1) (R^2 � Exponencial) e S^2 = 2*pi*U2 (S^2 � Uniforme)
# 3) Retorne X = R*cos(S^2) e Y = R*sin(S^2)

normal_bm <- function(n = 1){
  
  vetor <- NULL
  
  repeat{
    u <- runif(2L,0,1)
    
    r2 <- -2*log(u[1])
    s2 <- 2*pi*u[2]
    
    x <- sqrt(r2)*cos(s2)
    y <- sqrt(r2)*sin(s2)
    
    vetor <- c(vetor,x,y)
    
    if(length(vetor) >= n)
      break
  }
  
  vetor[1:n]
}

normal_bm()

# Visualizando:
hist(normal_bm(3e4))


# Salvando arquivos --------------------------------------------


# Digamos que queremos salvar os resultados de uma fun��o em arquivo,
# para n�o precisar executar tudo de novo toda fez que fechar o IDE.

observacoes <- normal_bm(3e4)

# Antes de salvar o arquivo, � importante saber qual o diret�rio (pasta)
# em que o arquivo ser� salvo. O comando getwd() retorna o diret�rio de trabalho:

getwd()

# Para alterar o diret�rio em usa, usa-se o comando setwd() com o caminho
# do diret�rio como argumento (o exemplo abaixo se aplica ao meu computador)

setwd("C:\\Users\\Carvalho\\Documents\\UFPB\\P3\\Softwares")

# OBS: A \ � um caractere reservado em R. Para acess�-lo normalmente, deve-se
# usar \\. Alternativamente, no endere�o de um arquivo ou diret�rio pode-se
# usar a contrabarra (/)

# Ou alternativamente, Ctrl + Shift + H


# Ap�s escolher onde salvar o arquivo, basta usar o comando save com
# o objeto e o nome do arquivo (incluindo o formato) a ser criado
# como argumentos.

save(observacoes, file = "vetor_dados_normalBM.RData")

# O formato .RData � conveniente por sua compress�o de dados e 
# por ser criptografado (n�o se tem acesso a suas informa��es
# ao abri-lo com um editor de texto).


# Que tal checar se o arquivo foi realmente salvo?

# Cheque o diret�rio onde o arquivo foi salvo. Alternativamente,
# use o comando dir(), que lista todos os arquivos salvos no diret�rio
# atual:

dir()

open(file = "vetor_dados_normalBM.RData")


