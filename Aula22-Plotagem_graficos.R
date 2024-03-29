# Introdu��o a softwares estat�sticos
# Data: 09/09/2019
# Aula: 22
# Assunto: Gr�ficos 


# Gr�ficos (continua��o) --------------------------------------------------

# Exerc�cio 1: baseado no exerc�cio anterior co comando points(),
# trace um segmento de reta horizontal usando a fun��o segments() que vai
# at� o ponto verde e um outro segmento de reta  vertical at� o ponto verde.
# Os segmentos deve ser tracejados e ser da cor laranja.

# C�digos anteriores:
plot.new()
plot.window(xlim = c(-10,10), ylim = c(-10,10))
axis(1, at = -10:10)
axis(2, at = -10:10)
grid()
abline(a = 4, b = 2, col = "red", lwd = 2)
abline(a = 2, b = 3, col = "blue", lwd = 2)
abline(h = 0, v = 0, lwd = 2)
title(main = "Gr�fico de segmentos de reta")
title(xlab = "Eixo X")
title(ylab = "Eixo Y")
points(x = 2, y = 8, pch = 19, col = "green")



# Documenta��o da fun��o:
?segments

# Argumentos uados:
# x0, y0: coordenadas do ponto inicial do segmento
# x1, y1: coordenadas do ponto final do segmento
# argumentos gr�ficos: lty (tipo de linha),
# lwd (espessura de linha), col (cor), etc.

segments(x0 = -11, y0 = 8, x1 = 2, y1 = 8, col = "orange", lty = 2, lwd = 2)
segments(x0 = 2, y0 = -11, x1 = 2, y1 = 8, col = "orange", lty = 2, lwd = 2)



# Exerc�cio 2: apresente texto no gr�fico acima utilizando a fun��o text().
# Nesse exerc�cio queremos acrescentar ao lado do ponto (2,8) o texto
# "P(2,8)" e ao lado esquerdo do ponto (0,0) acrescentar o texto '0'.

# Documenta��o da fun��o
?text

text(x = 3,y = 8, labels = "P(2,8)", cex = 1.2)
text(x = -0.5,y = -0.5, labels = "0", cex = 1.2)


# Exerc�cio 3: Acrescente com a fun��o mtext() um subt�tulo ao gr�fico,
# por exemplo, "Status - Empresa J�nior de Estat�stica - UFPB".

# Documenta��o da fun��o:
?mtext

mtext("Status - Empresa J�nior de Estat�stica - UFPB")

# A fun��o mtext() serve para inserir textos nas margens do gr�fico, n�o apenas
# subt�tulos. Sua posi��o em rela��o ao gr�fico � controlado pelo argumento "side":
# 1 - embaixo, 2 - � esquerda, 3 - acima (padr�o), 4 - � direita.
# As margens podem conter m�ltiplas linhas de texto, e qual recebe o texto �
# controlado pelo argumento "line".

mtext("Status", line = 1, side = 4)



# O R tamb�m permite que o usu�rio altere alguns par�metros gr�ficos pelo
# comando par().

?par

# Digamos que, por exemplo, queiramos alterar as margens do gr�fico para 3 linhas
# acima e abaixo e 2 nas laterais:

plot.new()
par(oma = c(3,2,3,2))
plot.window(xlim = c(-10,10), ylim = c(-10,10))
grid(lwd = 2)
abline(a = 4, b = 2, col = "red", lwd = 2)
abline(a = 2, b = 3, col = "blue", lwd = 2)
abline(h = 0, v = 0, lwd = 2)
axis(1, at = -10:10, cex.axis = .8, font = 1)
axis(2, at = -10:10, cex.axis = .8, font = 1)
title(main = "Gr�fico de segmentos de reta")
title(xlab = "Eixo X"); title(ylab = "Eixo Y")
segments(x0 = -11, y0 = 8, x1 = 2, y1 = 8, col = "orange", lty = 2, lwd = 2)
segments(x0 = 2, y0 = -11, x1 = 2, y1 = 8, col = "orange", lty = 2, lwd = 2)
points(x = 2, y = 8, pch = 19, col = "green")
text(x = 4,y = 8, labels = "P(2,8)", cex = 1.5)
text(x = -0.4,y = -0.5, labels = "0", cex = 1.5)
mtext("Status - Empresa J�nior de Estat�stica - UFPB")
mtext("Status", line = 2, side = 4)
mtext("Status", line = 5, side = 1)



# Exerc�cio 4: Digamos que agora queremos adicionar uma cor de fundo aos nossos
# gr�ficos (o que � poss�vel, mas nem sempre utilizado):

par(bg = "gray50")
plot.new()
plot.window(xlim = c(-10,10), ylim = c(-10,10))
grid(lwd = 2)
abline(a = 4, b = 2, col = "red", lwd = 2)
abline(a = 2, b = 3, col = "blue", lwd = 2)
abline(h = 0, v = 0, lwd = 2)
axis(1, at = -10:10, cex.axis = .8, font = 1)
axis(2, at = -10:10, cex.axis = .8, font = 1)
title(main = "Gr�fico de segmentos de reta")
title(xlab = "Eixo X"); title(ylab = "Eixo Y")
segments(x0 = -11, y0 = 8, x1 = 2, y1 = 8, col = "orange", lty = 2, lwd = 2)
segments(x0 = 2, y0 = -11, x1 = 2, y1 = 8, col = "orange", lty = 2, lwd = 2)
points(x = 2, y = 8, pch = 19, col = "green")
text(x = 3,y = 8, labels = "P(2,8)", cex = 1.3)
text(x = -0.2,y = -0.5, labels = "0", cex = 1.3)
mtext("Status - Empresa J�nior de Estat�stica - UFPB")
mtext("Status", line = 3, side = 4)
mtext("Status", line = 5, side = 1)


# Exerc�cio 5: agora queremos plotar gr�ficos de fun��es al�m de retas,
# como por exemplo, a densidade  de probabilidade de uma vari�vel exponencial.
# Para isso, usamos o comando lines().

# Desfazer as mudan�as dos par�metros gr�ficos:
par(bg = "white", oma = c(1,1,1,1))


plot.new()
plot.window(xlim = c(0,5), ylim = c(0,2))
grid(lwd = 2)
abline(h = 0, v = 0, lwd = 2)
axis(1, at = 0:5, cex.axis = .8, font = 1)
axis(2, at = 0:2, cex.axis = .8, font = 1)
title(main = "Gr�fico de densidade de V.A. exponencial")
title(xlab = "Eixo X"); title(ylab = "Eixo Y")

# Nossa fun��o densidade de probabilidade de v.a. exponencial.
densidade_exp <- function(x, lambda = 1){
    exp(-lambda*x)*lambda
}

# Geramos um vetor com uma sequ�ncia de pontos num intervalo em que a
# densidade admite valores, aplicamos a densidade nesse vetor e usamos
# esses vetores com argumento de lines().

x <- seq(from = 0, to = 10,length.out = 1e3)
y <- densidade_exp(x)
lines(x,y, lwd = 2, col = "red")

# Para outros valores do par�metro lambda:
x <- seq(from = 0, to = 10,length.out = 1e3)
y <- densidade_exp(x,2)
lines(x,y, lwd = 2, col = "purple")

x <- seq(from = 0, to = 10,length.out = 1e3)
y <- densidade_exp(x,0.5)
lines(x,y, lwd = 2, col = "blue")


# O R permite usar s�mbolos matem�ticos e outras nota��es e caracteres
# especiais nos textos dos gr�ficos atrav�s do argumento expression().
# Queremos usar nota��es para especificar o que cada uma das nosa retas
# representa

text(1.5,0.5,expression(X[1]%~%Exp(1)), col = "red")
text(1,1,expression(X[2]%~%Exp(2)), col = "purple")
text(3,0.25,expression(X[3]%~%Exp(0.5)), col = "blue")

# Para mais informa��o sobre nota��es espec�ficas:
?symbol


# Fazendo algo semelhante para a distrbui��o normal:

plot.new()
plot.window(xlim = c(-5,5), ylim = c(-0,1))
grid(lwd = 2)
abline(h = 0, v = 0, lwd = 2)
axis(1, at = -5:5, cex.axis = .8, font = 1)
axis(2, at = -0:1, cex.axis = .8, font = 1)
title(main = "Gr�fico de densidade de V.A. Normal")
title(xlab = "Eixo X"); title(ylab = "Eixo Y")

densidade_normal <- function(x, par = c(0,1)){
  mi <- par[1]
  sigma2 <- par[2]
  
  (1/sqrt(2*pi*sigma2))*exp(-((x - mi)^2)/(2*sigma2))
}

x <- seq(from = -5, to = 5,length.out = 1e3)
y <- densidade_normal(x)
lines(x,y, lwd = 2, col = "red")

x <- seq(from = -5, to = 5,length.out = 1e3)
y <- densidade_normal(x, c(0,0.5))
lines(x,y, lwd = 2, col = "purple")

x <- seq(from = -5, to = 5,length.out = 1e3)
y <- densidade_normal(x, c(2,1))
lines(x,y, lwd = 2, col = "blue")


text(-2,0.3,expression(X[1]%~%N(0,1)), col = "red")
text(-2,0.45,expression(X[2]%~%N(0,0.64)), col = "purple")
text(3,0.5,expression(X[3]%~%N(2,1)), col = "blue")



# Alguns materais para quem estiver interessado em estudar mais:
# Site do Hadley Wickham, desenvolvedor importante para a linguagem R:
# http://hadley.nz/
# Alguns de seus livros virtuais incluem:
# R for Data Science
# ggplot 2
# R Advanced