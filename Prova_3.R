# Introdu��o a softwares estat�sticos
# Data: 18/09/2019
# Aula: 24
# Assunto: Terceira Prova 

# Quest�o 1 ---------------------------------------------------------------

# Implemente uma fun��o em R que gere n�meros pseudoaleat�rios  que seguem
# o seguinte algoritmo (m�todo polar):

# 1) Gerar duas observa��es que seguem uma uniforme entre 0 e 1, U1 e U2.
# 2) Transformar U1 e U2 em U'1 = 2*U1 - 1  e U'2 = 2*U2 -1
# 3) Fazer W = (U'1)^2 e (U'2)^2. Caso W>1, voltar ao passo 1)
# 4) Fazer X = raiz de -(log(W)/W)*U'1 e Y = raiz de -(log(W)/W)*U'2
# 5) X e Y seguir�o uma normal padr�o.


normal <- function(n = 1){
  
  vetor_norm <- NULL
  
  repeat{

    u <- runif(n = 2L, min = 0, max = 1)

    u_1 <- 2*u[1] - 1
    u_2 <- 2*u[2] - 1
    w <- u_1^2 + u_2^2

    if(w > 1)
      next
    
    x = sqrt(-log(w)/w)*u_1
    y = sqrt(-log(w)/w)*u_2
    
    vetor_norm <- c(vetor_norm, x, y)
    if(length(vetor_norm) >= n)
      break
  }
  
  vetor_norm[1:n]
  
}



# Quest�o 2 ---------------------------------------------------------------



# Quest�o 3 ---------------------------------------------------------------



empirical1 <- function(x,...){
  plot.new()
  plot.window(xlim = range(x), ylim = c(0,1))
  axis(1);axis(2)
  title(xlab = "x", ylab = "Probabilidade",
        main = "Gr�fico da Distribui��o Emp�rica")
  
  grid(lwd = 2)
  
  
  
  t <- seq(min(x),max(x),length.out = length(x))
  
  y <- numeric(length(x))
  
  for(i in 1:length(x)){
    y[i] <- length(subset(x, x <= t[i]))/length(x)
  }  
  
  lines(t,y,...)
}

# Uma maneira mais direta, mas que exige connhecimento da fun��o
# quantile:

?quantile

empirical2 <- function(x,...){
  plot.new()
  plot.window(xlim = range(x), ylim = c(0,1))
  axis(1);axis(2)
  title(xlab = "x", ylab = "Probabilidade",
        main = "Gr�fico da Distribui��o Emp�rica")
  
  grid(lwd = 2)
  
  mtext("Dados")
  
  lines(quantile(x, probs = seq(0,1,length.out = length(x))),
        seq(0,1,length.out = length(x)),...)
}


# Testando:

set.seed(0)

x <- rnorm(100,0,1)

empirical1(x, lwd = 2, col = "blue")

empirical2(x, lwd = 2, col = "purple")



# Quest�o 4 ---------------------------------------------------------------

# Implemente uma fun��o que plot o gr�fico de uma circunfer�ncia.
# Algumas especifica��es s�o:

# 1) A circunfer�ncia ser� centrada na origem e o plano abrange todo o
#    raio da cicunfer�ncia. Este dever� ser recebido como argumento
#    (por padr�o 1)
# 2) O gr�fico tem t�tulo e subt�tulo (o raio ser� exibido no subt�tulo)
# 3) A circunfer�ncia � cortada por dois segmentos de reta (um horizontal
#      e outro vertical) que passam pela origem. Um ponto marca a origem.
# 4) A fun��o recebe argumentos gr�ficos opcionaos para a curva da
#     circunfer�ncia, e apenas para a curva.

# Dica: Uma circunfer�ncia de raio 2 no plano xy e centrada na origem
# � expressa pela equa��o x^2 + y^2 = r^2, e o caractere do ponto
# na origem � o 19.


circunferencia <- function(r = 1,...){
  plot.new()
  plot.window(xlim = c(-r,r), ylim = c(-r,r))
  axis(1);axis(2)
  title(xlab = "x", ylab = "y",
        main = "Gr�fico de Cicunfer�ncia")
  mtext(paste("r = ", r, sep = ""))
  
  x <- seq(from = -r, to = r, length.out = 1e3)
  y1 <- sqrt(r^2 - x^2)
  y2 <- -y1
  
  lines(x,y1,...)
  lines(x,y2,...)
  
  segments(x0 = 0, y0 = -r, x1 = 0, y1 = r)
  segments(x0 = -r, y0 = 0, x1 = r, y1 = 0)
  
  points(0,0,pch = 19)
}

# Teste:
circunferencia(3,col = "red", lwd = 2, lty = 2)






      