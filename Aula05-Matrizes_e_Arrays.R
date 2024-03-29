# Introdu��o a softwares estat�sticos
# Data: 17/06/2019
# Aula: 05
# Assunto: Matrizes e Arrays


# Compare os usos do comando paste()
paste(c("a", "b"), c("c", "d"), sep = " - ")

paste(c("a", "b"), c("c", "d"), collapse = " - ")

# OBS: existe uma fun��o de nome semelhante, a paste0()
# Elas diferem apenas no valor padr�o do argumento sep

paste(c("a", "b"), c("c", "d"))

paste0(c("a", "b"), c("c", "d"))


# � poss�vel remover todos os objetos com um �nico comando?
# Sim, usando as fun��es rm() (remove objetos)
# e ls() (lista objetos em uso) da maneira abaixo

rm(list = ls(all = TRUE)) # O all = TRUE pode ser omitido, por ser o padr�o


# O comando gc() ("Garbage collection" ou coleta de lixo) serve para
# devolver mem�ria em uso ao sistema operacional. � recomend�vel em
# vez do rm() para objetos grandes.

?gc()

# Veja um exemplo:

x <- rnorm(1e6)
# 1e6 = 1 * 1-^6 (nota��o cient�fica).
1e6 == 1*10^6
# rnorm() gera n�meros (pseudo)aleat�rios seguindo uma distribui��o normal
# Se a m�dia e o desvio padr�o n�o forem informados,
# os valores seguir�o a normal padr�o (m�dia = 0, desvio = 1)

print(object.size(x), units = "Mb")
# objects.size() retorna o valor em bytes de determino objeto
# ou objeto

rm(x)

gc()



# Matrizes

# Adicionar o atributo dim() � um vetor at�mico permite que
# ele se comporte como uma matriz multidimensional.
# Um caso especial � o array, matriz de duas ou mais dimens�es

?matrix
?array

# Ap�s ler a documenta��o da fun��o dim(), construa um vetor at�mico
# de tamanho 10 e em seguida construa uma matriz M de dimens�o 5 x 2.
# Usando essa matriz, construa o vetor myvector
?dim

x <- 1:10
x <- rnorm()
dim(x) <- c(5, 2)
x
dim(x)
is.matrix(x)

x <- unlist(x)
x


# Matrizes e arrays podem ser criados pelos comandos matrix()
# e array(), ou tamb�m por atribui��o usando o dim()

# Os n�meros de linhas e colunas s�o os argumentos
a <- matrix(1:6, ncol = 3, nrow = 2)

# Com o array, passamos as dimen�es como argumento
# (linhas, colunas, n�mero de matrizes)
b <- array(1:12, dim = c(2, 3, 2))
b[,2,1]
b[2,,2]

#
c <- 1:6
dim(c) <- c(3,2)

# OBS: matriz � um subtipo de array
m <- b[,,2]
m
is.array(b)
is.array(m)
is.matrix(m)
is.matrix(b)

# Pode se realizar algumas opera��es com matrizes
m[,-2]
m[,c(2,3)]
diag(m[,c(2,3)])

# Os comandos length(), ncol() e nrow() retornam a n�mero de
# elementos, colunas e linhas da matriz (ou array) informada.
# Corra o exemplo abaixo

m <- array(1:12, dim = c(2,2,3))
length(m)
ncol(m)
nrow(m)
dim(m)
m

# Como em vetores, � poss�vel nomear os elementos de uma matriz
x <- 1:10
names(x) <- letters[1:length(x)]
x
x["j"]


# Vamos nomear as linhas e clunas de uma matriz
m <- matrix(1:1e3, nrow = 500, ncol = 2)

nomes_linhas <- paste("l", 1:nrow(m), sep = "_")
nomes_colunas <- paste("c", 1:ncol(m), sep = "_")
rownames(m) <- nomes_linhas; colnames(m) <- nomes_colunas
# Assim, podemos acessar os elementos pelos nomes
m["l_212",]
m["l_300","c_1"]

# E tamb�m com um array
b <- array(1:12, dim = c(2, 3, 2))
dimnames(b) <- list(c("linha_1", "linha_2"), c("Col_1", "Col_2", "Col_3"), c("Matriz A", "Matriz B"))
b
b[,,"Matriz A"]
b[1,,"Matriz B"]

# OBS: � poss�vel criar uma matriz vazia usando NA
n <- matrix(NA, ncol = 4, nrow = 4)
n


# Os array s� �teis quando necessitamos armazenar uma sequ�ncia de
# matrizes que poder�o ser utilizados posteriormente no c�digo
# co, por exemplo, para fazer um produto de duas matrizes
myarray <- array(data = NA, dim = c(2,2,3))
a1 <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
a2 <- 2*a1
a3 <- 3*a2
myarray[,,1] <- a1; myarray[,,2] <- a2; myarray[,,3] <- a3
myarray

# Aten��o! Trocar a ordem dos argumentos em renome�-los
# gera comportamento indesejados

# Isto gera 2*3*2 = 12 elementos organizados em
# 2 matrizes 2 por 3 com n�meros de 1 a 7
certo <- array(dim = c(2,3,2), data = 1:7)
certo
length(certo)
object.size(certo) #Tamanho de certo

# Isso gera um total de 7! = 5040 elementos organizados em
# matrizes 1 por 2 com os elementos seguindo o padr�o 2, 3, 2
errado <- array(c(2,3,2), 1:7)
errado
length(errado)
object.size(errado) # Tamanho de errado


# � poss�vel concatenar matrizes por linha ou coluna usando os comandos
# rbind() e cbind(), respectivamente.
v1 <- matrix(1:12, ncol = 4, nrow = 3)
w1 <- matrix(1:8, ncol = 4, nrow = 2)
v1; w1
rbind(v1, w1)

v2 <- matrix(1:12, ncol = 2, nrow = 4)
w2 <- matrix(1:12, ncol = 4, nrow = 4)
v2; w2
cbind(v2, w2)


# OBS: Os elementos de uma matriz podem ser listas, assim
# como os elementos de uma lista podem ser matrizes

l <- list(1:3, "a", TRUE, 1.0)
dim(l) <- c(2,2)
l
l[[1]]
is.list(l)
is.matrix(l)
