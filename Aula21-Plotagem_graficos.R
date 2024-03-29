# Introdu��o a softwares estat�sticos
# Data: 09/09/2019
# Aula: 21
# Assunto: Gr�ficos 


# G�ficos (continua��o) --------------------------------------------------

# Corra o c�digo abaixo.

plot.new() # Cria um novo gr�fico em branco
plot.window(xlim = c(0,1), ylim = c(5,10)) # Estabelece a altura e largura do gr�fico
abline(a = 6, b = 3) # Gera o gr�fico de uma fun��o linear (y = b*x + a) e outras retas
axis(1) # Plotar o eixo X
axis(2) # Plotar o eixo Y
title(main = "T�tulo principal")
title(xlab = "Eixo X")
title(ylab = "Eixo Y")
box() # "Fecha" o gr�fico
grid() # Desenha uma "malha" no gr�fico.


# Exerc�cio: Use a fun��o abline() para adicionar ao gr�fico anterior uma
# linha vertical em x = 0 e uma horizontal em y = 0. Adicione tamb�m no
# gr�fico as retas y = 2*x + 4 e y = 3*x + 2 com cores diferentes. Construa
# o gr�fico omitindo a chamada da fun��o box(). Dica: construa o gr�fico
# com x e y entre -10 e 10.

plot.new()
plot.window(xlim = c(-10,10), ylim = c(-10,10))


# O comando axis() aceita o argumento at, que indica onde as marca��es no
# eixo dever�o ser feitas
axis(1, at = -10:10)
axis(2, at = -10:10)

grid()

# O comando abline() aceita alguns outros argumentos opcionais:
# h ou v: gera uma reta horizontal nas coordenadas informadas
# argumentos gr�ficos: col (cores), lwd (espessura de linha),
# lty (tipo de linha), etc.

abline(a = 4, b = 2, col = "red", lwd = 2)
abline(a = 2, b = 3, col = "blue", lwd = 2)
abline(h = 0, v = 0, lwd = 2)

title(main = "Gr�fico de retas")
title(xlab = "Eixo X")
title(ylab = "Eixo Y")

# OBS: a ordem em que os comandos s�o executados altera o resultado.



# Operador '...':
# As retic�ncias (...) servem para indicar uma lista de argumentos opcionais
# de uma fun��o. Os argumentos opcionais s�o representados tamb�m
# por '...' na fun��o e usados no argumento de fun��es interiores.


# Exemplo 1: f1() recebe dois valores e realiza uma opera��o com eles antes
# de plotar um ponto.

f1 <- function(x, y, ...){
  
  z <- sqrt(x) + y
  
  plot(z, ...)
}

# Argumentos adicionais podem ser informados, e ser�o usados pelo
# plot() dentro de f1().

f1(1,1)
f1(2,2, pch = 10) # pch controla o caractere usado para representar um ponto
f1(0,2, col = "blue")
f1(1,0, pch = 20, col = "green")


# Exemplo 2:

f2 <- function(x, a = 1L){
  if(x >= 0)
    sqrt(x) * a
  else
    x^2 + a
}

f3 <- function(f, y, b = 0, ...){
  f(y, ...) + b
}

f3(f = f2, b = 3, y = 7)
f3(f = f2, b = 3, y = 7, a = 2L)



f4 <- function(f, y, b, ...){
  
  # O comando '...length()' retorna a quantidade de argumentos opcionais informados
  n <- ...length()
  
  if(n >= 3)
    stop("O n�mero de argumentos adicionais poder� ser no m�ximo 2.")
  if(n == 2)
    ..1 + ..2 # os comandos "..1" e "..2" acessam o valor do primeiro e segundo
              # argumentos opcionais
  else
    f(x = y, ...)
}

f4(f = f2, y = 4, b = 1)
f4(f = f2, y = 2, a = 2)
f4(f = f2, y = 2, a = 2, 2,3)
f4(f = f2, y = 2, a = 2,2,3,2)


# Exerc�cio: use a fun��o points() para adicionar ao gr�fico do exerc�cio
# anterior um ponto na intersec��o das duas retas.

# COmandos anteriores:
plot.new()
plot.window(xlim = c(-10,10), ylim = c(-10,10))
axis(1, at = -10:10)
axis(2, at = -10:10)
grid()
abline(a = 4, b = 2, col = "red", lwd = 2)
abline(a = 2, b = 3, col = "blue", lwd = 2)
abline(h = 0, v = 0, lwd = 2)
title(main = "Gr�fico de retas")
title(xlab = "Eixo X")
title(ylab = "Eixo Y")

# Comando points():
points(x = 2, y = 8, pch = 19, col = "green")



# Assim como v�rios comandos R, o points() aceita vetores como argumentos.
# Isso pode ser usado para representar o gr�fico de uma fun��o atrav�s de
# uma s�rie de pontos.

funcao_reta <- function(x) 2*x + 4

line_points <- function(f, n = 50, lower = -10, upper = 10, ...){
  x <- seq(lower, upper, length.out = 100L)
  y <- f(x)

  plot.new()
  plot.window(xlim = c(lower,upper), ylim = c(lower,upper))
  axis(1, at = lower:upper); axis(2, at = lower:upper)

  grid()
  
  abline(h = 0, v = 0, lwd = 2)

  title(main = "Gr�fico de pontos")
  title(xlab = "Eixo X")
  title(ylab = "Eixo Y")
  
  points(x, y, ...)
}

line_points(funcao_reta, col = "purple", pch = 20)

# OBS 1: O uso de rgb() como valor do argumento col permite escolha mais precisa de cores
# do que simplesmente usar o nome delas. Ele recebe tr�s valores antre 0 e 1 que
# representam o brilho de cada uma das cores prim�rias usada para gerar a resultante.
# Cores geradas pelo rgb() tamb�m podem ter sua transpar�ncia alterada, pelo argumento
# alpha, tamb�m um n�mero entre 0 e 1.

# OBS 2: o argumento cex representa o tamanho relativo de s�mbolos e texto de gr�ficos
# em rela��o ao normal.

line_points(sin, n = 30 , lower = -6, upper = 6,
            col = rgb(0,0,0, alpha = 0.4), pch = 19, cex = 3)

line_points(cos, n = 30 , lower = -6, upper = 6,
            col = rgb(0,0,0, alpha = 0.6), pch = 19, cex = 3)


f_quadratica <- function(x) x^2 - 1

line_points(f_quadratica, n = 30 , lower = -2, upper = 2,
            col = rgb(0.25,0,0.15, alpha = 0.6), pch = 19, cex = 1)
