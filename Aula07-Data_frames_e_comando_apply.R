# Introdu��o a softwares estat�sticos
# Data: 26/06/2019
# Aula: 06
# Assunto: Dataframes

# Data frames (continua��o)------------------------------------

# Na aula passada, tentamos criar um data frame onde uma das
# vari�veis � uma lista. Veja que primeiro criamos o data frame 
# com a vari�vel vazia, e ent�o atribu�mos a lista como valor.
# O prop�sito foi mostrar que cada c�lula de um data frame pode
# ser um vetor

alunos <- paste("Aluno", 1:5, sep = "_")

avaliacoes <- list(c(6.5, 7.2, 8.1), c(7.7, 5.6, 6.9), c(4.3, 5.5, 8.3, 6.0),
                   c(9.5, 7.2, 6.0), c(8.0, 9.5))

df <- data.frame(aluno = alunos, notas = NA)

df$notas <- avaliacoes

# Por�m, observe que atribuir uma lista no momento de cria��o do data frame.
# Veja:

df <- data.frame(aluno = alunos, 
                 notas = list(c(6.5, 7.2, 8.1),c(7.7, 5.6, 6.9), c(4.3, 5.5, 8.3, 6.0),
                                              c(9.5, 7.2, 6.0), c(8.0, 9.5)))

# O comando I() previne esse erro

dfl <- data.frame(x = 1:3, y = I(list(1:2, 1:3, 1:4)))

# Mais informa��es
?I


# A fun��o lapply() permite que se aplica uma fun��o a todos
# os elementos de uma lista. Veja os exemplos para m�dia e
# soma das notas do nosso daa frame anterior.

lapply(X = df[,2], FUN = mean)

lapply(X = df[,2], FUN = sum)

# Tamb�m � poss�vel passar fun��es al�m das convencionais como argumento.

soma_um <- function(x){
  x + 1
}

lista <- c(1,2,3,4)

lapply(X = lista, FUN = soma_um)

# O R tamb�m permite que o conte�do de uma vari�vel em um data frame
# seja uma matriz

dfm <- data.frame(x = 1:3, y = I(matrix(1:9, nrow = 3)))

# Podemos acessar a matriz y pelo operador $
dfm$y

# Tamb�m podemos especificar a linha da matriz y, tamb�m
dfm[2, "y"]


# Assim como o comando lapply() funciona com listas, o apply()
# funciona com matrizes e data frames. A diferen�a est� no fato
# de serem estruturas multidimensionais: � preciso especificar se
# a fun��o ser� aplicada por linhas atrav�s do argumento MARGIN


# No nosso exemplo, vamos gerar as notas aleatoriamente seguindo 
# uma distribui��o uniforme. O comando runif faz isso, recebendo
# como argumento a quantidade de n�meros a gerar e os valores
# m�ximo e m�nimo.

avaliacoes <- matrix(runif(n = 15, min = 6, max = 10), ncol = 3, nrow = 5)

# Para mais detalhes:
?runif

alunos <- paste("Aluno", 1:5)

historico <- data.frame(aluno = alunos, notas = I(avaliacoes))

historico$notas

# Qual � soma das notas de cada aluno?
apply(historico$notas, MARGIN = 1, FUN = sum)

# Qual � m�dia das notas de cada aluno?
apply(historico$notas, MARGIN = 1, FUN = mean)

# Quais s�o os quartis das distribui�oes de notas de cada aluno?
apply(historico$notas, MARGIN = 1, FUN = summary)


# OBS: Adicionado e removendo colunas de um data frame-------

# Adicionado uma nova coluna
notas <- apply(historico$notas, MARGIN = 1, FUN = mean)

historico$nota4 <- notas


# Renomeando uma coluna

nomes <- colnames(historico)

nomes[3] <- "media"

colnames(historico) <- nomes


# Removendo uma coluna

historico <- historico[,-3]

# Isso remove todos os elementos da terceira coluna. Veja com
# mais detalhes na se��o a seguir



# Acessando subconjunto de um vetor -------------------------------

x <- c(2.1, 4.2, 3.3, 5.4)

# O comando order() serve para ordenar os termos de um vetor

# Por padr�o retornar� as posi��es...
order(x)

# ... e quando passado como �ndice, retorna o vetor ordenado
x[order(x)]


# � possivel acessar subconjuntos de um vetor passando um vetor
# para �ndices dos valores desejados.
x[c(1,3)]


# OBS1: �ndices repetidos retornam o mesmo valor
x[c(1,1)]


# OBS2: Valores reais s�o truncados (a parte real � desconsiderada)
x[c(2.5, 3.9)]


# Tamb�m se pode remover termos de um vetor desse m�todo
x[-4] # Sem o quarto elemento
x[-c(1,2)] # Sem o primeiro e o segundo elementos
x[c(-3,-4)] # Sem o terceiro e o quarto elementos


# OBS3: Tamb�m se pode passar valores l�gicos como �ndices.

x[c(TRUE, FALSE, TRUE, FALSE)]

# Caso o vetor l�gico seja menor que x, ele ser� reciclado (repetido)
x[c(FALSE, TRUE)]


# Isso permite a busca de termos atrav�s de express�es l�gicas

x[x > 3] # Elementos de x maiores que 3
x[x > 3 & x < 5] # Elementos de x maiores que 3 e menores que 5


# OBS4: NA no �ndice gera NA na posi��o. N�O � o mesmo que FALSE.
x[c(TRUE, FALSE, NA, TRUE)]


# Tamb�m podemos chamar elementos por seus nomes
y <- setNames(x, letters[1:4])
y[c("a", "c", "b")]

# Chamar um nome que n�o corresponde a elemento algum retorna NA
z <- c(abc = 1, def = 2)
z
z[c("a", "b")]


# Essa sintaxe tamb�m funciona para arrays e data frames.
# Veja o exemplo abaixo com uma matriz

a <- matrix(1:9, nrow = 3) # matriz 3x3 com os n�meros de 1 a 9

colnames(a) <- c("A", "B", "C") # Nomeando as colunas
a[1:2,] # Linhas 1 e 2, todos as colunas

a[c(T,F,T), c("B", "A")] # Primeira e terceira linhas, colunas "B" e "A"

a[0, -2] # Nomes das colunas, excluindo a segunda

# Comando outer()------------------------------------------------------

# O comando outer() aplica uma determinada opera��o em todas as fun��es
# poss�veis dos elementos dos argumentos X e Y, gerando uma array
# que cont�m os resultados.

x <- 1:3
y <- 4:6

soma_xy <- outer(X = x, Y = y, FUN = "+")
dif_xy <- outer(X = x, Y = y, FUN = "-")

?outer


# O c�digo abaixo est� aplicando o comando paste() para todos
# os n�meros de 1 a 5
vals <- outer(1:5, 1:5, FUN = "paste", sep = ",")


# Ainda � poss�vel buscar elementos com um vetor como �ndice.
# Note que os elementos s�o contados por coluna.
vals[c(4, 15)]


# Tamb�m � poss�vel acessar os elementos de um array usando uma
# matriz com os pares ordenados dos elementos desejados
 
select <- matrix(ncol = 2, byrow = TRUE, c(1,1,3,1,2,4))
select
vals[select]

