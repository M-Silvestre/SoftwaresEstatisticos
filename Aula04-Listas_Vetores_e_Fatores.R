# Introducao a softwares estatisticos
# Data: 12/06/2019
# Aula: 04
# Assunto: Listas, vetores, fatores



# Listas (continua��o) ----------------------------------------------------


# Listas s�o vers�teis porque direrentemente de vetores at�micos, elas pode ser heterog�neas,
# isto �, eus elementos podem ter tipos e estruturas diferentes, incluindo outras listas.
# Veja os exemlplos:

x <- list(list(list(list())))
str(x)
is.recursive(x)

x <- list(list(list(1)))
x


# Concatena��o: 

c(c(1,c(2,3)))

x <- c(1, 2, 3, 4, 5)
y <- c(6, 7, 8, 9, 10)
c(x, y)

x <- list(1, 2, 3, 4, 5)
y <- list(6, 7, 8, 9, 10)
list(x, y)

# Aten��o aos termos: concatena��o n�o � o mesmo que uni�o.
x <- c(1, 2, 3, 4)
y <- c(1, 2, 5, 6)
c(x, y) # Concatena��o
union(x, y) # Uni�o (no sentido de conjuntos)


# Usando o comando em listas:
# O comando c() pode ser usado para combinar v�rias listas em uma s�.
# Caso seja usada uma combina��o at�mica (mesmo tipo) de vetores e listas
# a fun��o c() transforma os vetores em uma lista

x <- list(list(1,2), c(3,4))
y <- c(list(1,2), c(3, 4))
str(x)
str(y)

x <- list(list("1","2"), c(3,4))
y <- c(list("1","2"), c(3, 4))
str(x)
str(y)


# OBS: Outra maneira de enxergar listas � como vetores n�o at�micos.
# De fato, � assim que o R as v�:

is.vector(list())
is.atomic(list())


# O comado unlist() transforma os elementos de uma lista em elementos de
# um vetor. Lembrando que, em caso de elementos de tipos diferentes,
# o R converte todos para o tipo mais vers�til.

y <- unlist(c(list("1", "2"), c(3, 4)))


# Rode o c�digo abaixo e tente intepretar suas sa�das

l <- c(list(c(2,3), "a"), c(1,2))
str(l)

m <- list(list(c(2,3), "a"), c(1,2))
str(m)



# O comando is.list()
# OBS: O banco de dados mtcars j� vem instalado com o R, e serve para testar comandos
View(mtcars)
is.list(mtcars)

mod <- lm(mpg ~ wt, data = mtcars) # O comando lm() gera um modelo de regress�o linear
is.list(mod)



# Exerc�cio:

# A) qual a sa�da experada de c(1, FALSE), c("a", 1), c(list(1), "a"), c(TRUE, 1L)? Explique:

# B) Por que 1 == "1" retorna TRUE? E por que -1 < FALSE retorna TRUE?



# Em R � poss�vel nomear os elementos de um vetor de 3 deferente maneiras:

# 1) Declarar os nomes na cria��o do vetor
x <- c(a = 1, b = 2, c = 3)

# Pode-se acessar um elemento tanto por seu �ndice como por seu nome
x[2]
x["b"]

# 2) Tamb�m � poss�vel nomear os elementos de um vetor preexistente
x <- 1:3
names(x) <- c("a", "b", "c")

# 3) usando a fun��o setNames()
x <- setNames(1:3, c("a", "b", "c"))


# A fun��o names() tamb�m retorna os nomes de um vetor
y <- c(a = 1, 2, 3)
names(y)
z <- 1:3
names(z)


# Tamb�m � poss�vel retirar os nomes dos elementos do vetor
x <- c(a = 2, b = 3, c = 4)
names(x)
names(x) <- NULL
names(x)


# Alternativamente
x <- c(a = 2, b = 3, c = 4)
names(x)
x <- unname(x)
x



# Fatores -----------------------------------------------------------------

# Fator � um vetor que recebe apenas valores predefinidos

x <- factor(c("a", "b", "b", "a")) # Elementos de x s�o apenas "a" ou "b"
x
class(x)
levels(x) # Vetor com os valores (n�veis) que os elementos de x podem assumir


# Caso se tente atribuir um elemento que n�o corresponde aos n�veis admitidos pelo fator
# ocorrer� uma mensagem de erro e o termo receber� NA
x <- factor(c("a", "b", "b", "a"))
x[2] <- "c"
x

# OBS: Tentar concatenar fatores resulta em comportamento indefinido
c(factor("a", "b"), factor("c", "d"))


# A fun��o table() pode criar uma tabelas de frequ�cias.
# Pode, por exemplo, criar uma tabela com os n�veis de um vetor:

sex_char <- c("m", "m", "m", "f", "f", "f", "f")
sex_factor <- factor(sex_char, levels = c("m", "f", "Outros"))
table(sex_factor)
sex_factor

# Para alterar os n�veis de um fator, usa-se a fun��o levels()

sex_char <- c("m", "m", "m", "f", "f", "f", "f")
sex_factor <- factor(sex_char)
table(sex_factor)
sex_factor
levels = c("m", "f", "Outros")
table(sex_factor)
sex_factor


# O que fazem a fun��o rev() e o objeto letters? Rode o c�digo abaixo

# Fator com as 26 letras do alfabeto
f1 <- factor(letters); f1

# Revertendo n�veis e elementos
levels(f1) <- rev(letters); f1

# Revertendo o fator
f2 <- rev(factor(letters)); f2

# Revertedo os n�veis
f3 <- factor(letters, levels = rev(letters)); f3



# Removendo objetos -------------------------------------------------------

s1 <- "Eu"
s2 <- "amo"
s3 <- "a"
s4 <- "Estat�stica"
s5 <- paste(s1, s2, s3, s4, sep = " ")


# A fun��o paste() concatena strings (cadeias de caracteres) usando
# um separadaor (por padr�o, n�o h� separador)
s5


# Outro exemplo
matriculas <- 1:10
nomes <- letters[1:length(matriculas)]; nomes

paste(nomes, "(", matriculas, ")", sep = "")

paste(nomes, matriculas, sep = "-")

# Listar os objetos em uso
ls()
object.size(ls())
