# Introdu��o a softwares estat�sticos
# Data: 07/08/2019
# Aula: 15
# Assunto: Fun��es e C�lculo de Integrais por Monte Carlo


# Fun��es (continua��o) ---------------------------------------

# Observe que no programa da aula anterior, se a condi��o avaliada
# no if() for verdadeira, a fun��o retorna NULL e nada mais ser� executado.


# Seguem aqui algumas fun��es matem�ticas e estat�sticas.
# Sejam x e y vetores de dados.

# sum(x): soma de todos os elementos de x
# max(x): o maior dos elementos de x
# min(x): o menor dos elementos de x
# which.max(x): posi��o do maior elemento de x
# range(x): extremos de x (m�ximo e m�nimo)

# mean(x): m�dia dos elementos de x
# median(x): mediana
# scale(x): normaliza x (subtrai a m�dia e divide pelo desvio padr�o)
# sort(x): ordena os termos de x (po padr�o, em ordem crescente)
# rank(x): cria um ranking com os elementos de x

# log(x, base): Logaritmo dos n�me
# exp(x): exponencial dos elementos de x
# sqrt(x): raiz quadrada de x
# abs(x): valor absoluto de x
# round(x, n): valores de x aproximados com n casas decimais
# OBS: n�o confundir arredondamento com truncamento.
# Arredondar:
round(c(2.3, 1.9, 3.5))
# Truncar: descartar as casas decimais
trunc(c(2.3, 1.9, 3.5))

# cumsum(x): soma acumulada dos elementos de x
# cumprod(x): produto acumulador dos elementos de x
# match(x): retorna
# union(x): retorna os elementos que pertencem a  ou y
# intersect(x): retorna os elementos que pertencem a x e y


# setdiff(x,y): elementos de x que n�o pertencem a y
# is.element(x,y): se os elementos de pertencem a y.
# semelhante ao operador %%in%%



# Algumas fun��es para �lgebra de matrizes.
# Sejam A e B matrizes e b um vetor:

# diag(b, nrow, ncol): retorna uma matriz diagonal com
#                      os elementos de b

# t(A): retorna a matriz transposta de A

# A%*%B: produto matricial entra A e B

# det(A): retorna o determinante da matriz quadrada A

# solve(A): retorna a matriz inversa de A

# solve(A,b): resolve o sistema de equa��es lineares Ax = b

# eigen(A): calcula os autovalores e autovetores de A, se A for uma
#           matriz quadrada



# Exerc�cio 1: apresente exemplos das fun��es apresentadas acima

# Alguns exemplos:

sort(c(10,9,13,12))
rank(c(3,7,4,8,2))
union(1:3, 4:8)
intersect(1:5, 4:8)
setdiff(c(1,2,3,4), c(2,4))
is.element(c(1,12,5,7,19,5), 1:8)

b <- c(1,3)
diag(b, nrow = 4)


# Exerc�cio 2: Refa�a o exerc�cio de estimadores usando fun��es



# Aproxima��o de intergrais por Monte Carlo ---------------------

# Imagine que queremos calcular a integral da fun��o f(x) = sen(x)
# entre 0 e 2*pi. 
# Analiticamente, temos que a primitiva de sen(x) � -cos(x), e
# avaliando-a em 2* e 0, temos -1 -(-1) = -1 + 1 = 0

# Mas como far�amos isso computacionalmente, isto �, aproximando
# atrav�s de um algoritmo em vez de determinar a primitiva?


# Pelo c�lculo integral, podemos aproximar a �rea sobre o gr�fico de
# uma fun��o f(x) entre x = a e x = b dividindo esse intervalo em
# n intervalos menores, avaliando a fun��o f para x em cada um desses
# intervalos menores e multiplicando pelo tamanho desses subintervalos.
# Isso corresponde a dividir o gr�fico em "ret�ngulos" e aproximar a 
# �rea sobre a curva pela soma das �reas dos n ret�ngulos.

# A integral definida de f(x) entre x = a e x = b � o limite dessa soma
# de n ret�ngulos quando n tende ao infinito.

# Podemos simular esse c�lculo gerando gerando um vetor de n n�meros 
# pseudoaleat�rios uniformes no intervalo (a,b), aplicando-os em f,
# e depois multiplicando o tamanho do intervalo pelo valor m�dio de f.

# Vamos voltar para nosso exemplo de f(x) = sen(x), com x entre 0 e 2*pi.

# Analiticamente, j� sabemos que o valor da integral � zero. Veja agora
# como aproximamos por uma simula��o de Monte Carlo.

set.seed(0)

vetor_ab <- runif(1e4, 0, 2*pi) # Aqui temos 10000 valores no intervalo

mean(sin(vetor_ab)) # Nosso Valor m�dio da fun��o no intervalo

(2*pi - 0)*mean(sin(vetor_ab)) # Nossa aproxima��o para a integral


# Exerc�cio: determine o valor da integral de f(x) = x^2 com x entre -1
# e 3, computacionalmente (por fun��es) e analiticamente (pela integral).


# Computacionalmente:

# Fun��o que aproxima integrais recebendo o n�mero de pontos a serem gerados,
# a fun��o, e pontos inicial e final do intervalo.
int_mc <- function(N = 1e4, FUN, a, b){
  x <- runif(n = N, min = a, max = b)
  (b - a) * mean(FUN(x))
}

# Fun��o f(x) = x^2
x_quadrado <- function(x){
  x ^ 2
}

# Fixando a semente
set.seed(0)

# Temos aqui nossa aproxima��o:
int_mc(N = 1000, x_quadrado, a = -1, b = 3)


# Analiticamente:

# A primitiva de x^2 � (x^3)/3. Avaliando em 3 e -1, temos
# 27/3 - (-1)/3 = 28/3

28/3

# Compare os resultados. Eles s�o pr�ximos?


# Exerc�cio: Vamos agora integrar a fun��o densidade de uma v.a.
# X~Exp(1.5) de 0 a 5

x_exp <- function(x){
  lambda = 1.5
  lambda * exp( - lambda * x)
}

set.seed(0)

int_mc(N = 1000, x_exp, a = 0, b = 5)


# Exerc�cio: Crie uma fun��o que aproxime pi pela �rea delimitada
# por uma circunfer�ncia de raio 1.

# Nos exemplos anteriores, estimamos a �rea sobre a curva do gr�fico de uma fun��o.

# Podemos utilizar um racioc�nio semelhante aqui: se considerarmos a curva 
# formada pela circunfer�ncia de raio 1 no primeiro quadrante, a propor��o
# entre os pontos dentro da circunfer�ncia sobre a �rea total corresponde
# � �rea da parte da circunfer�ncia no primeiro quadrante, pi/4.

aprox_pi <- function(N = 1e4){
  
  vetor_x <- runif(n = N, min = 0, max = 1)
  
  vetor_y <- runif(n = N, min = 0, max = 1)
  
  vetor_raio <- vetor_x^2 + vetor_y^2
  
  4*length(vetor_raio[vetor_raio <= 1])/length(vetor_raio)
}

set.seed(0)

aprox_pi(1e6)

# Outra implementa��o do mesmo racioc�nio:

aproxpi <- function(N = 1e4){
  
  x <- runif(n = N, min = 0, max = 1)
  
  y <- runif(n = N, min = 0, max = 1)
  
  # A fun��o sucesso() checa se o ponto t�m distancia ao centro
  # menot ou igual a 1
  sucesso <- function(x, y){
    ifelse(((x^2 + y^2) <= 1), TRUE, FALSE)
  }
  
  pontos_dentro <- sum(mapply(FUN = sucesso, x, y))
  
  4*pontos_dentro/N
}

set.seed(0)

aproxpi(1e6)