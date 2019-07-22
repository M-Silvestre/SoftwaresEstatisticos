# Introdu��o a softwares estat�sticos
# Data: 15/07/2019
# Aula: 09
# Assunto: Filtros de data frames

# Filtro de dados de um data frame -------------------------

# O comando data() exibe os conjuntos de dados presentes por padr�o
# no R, que podem ser usados para testar fun��es e praticar c�digos.

# Exerc�cio:
# A) Vamos usar o conjunto statex.77 para construir o data frame "dados"

class(state.x77)
dados <- as.data.frame(state.x77)
class(dados)
View(dados)

# Nosso data frame cont�m dados sobre estados norte americanos, como
# popula��o, renda, expectativa de vida, escolaridade, etc.

# B) obtenha um data frame de nome dados_1 com as observa��es de dados
# que possuam popula��o maior que 4246, isto �, os estados americanos
# de popula��o maior que 4246000 pessoas.

dados_1 <- dados[dados$Population > 4246, ]

# C) Agora, crie o dara frame com os estados de popula��o maior que
# 4246 e menor que 8000 (8 milh�es de pessoas)

dados_2 <- dados[dados$Population > 4246 & dados$Population < 8000, ]

# Quantos estados temos em dados_2?

dim(dados_2)

# D) Obtenha um vetor estados_c com os nomes dos estados selecionados
# no item anterior

estados_c <- rownames(dados_2)
estados_c

# E) Agora, construa o data frame dados_3 com os estados de popula��o
# maior que 1.5 vezes a m�dia dos 50 estados considerados, e obtenha um
# vetor com os nomes destes estados.

mean(dados$Population) # Aqui temos a m�dia
1.5 * mean(dados$Population) # Aqui temos 150% da m�dia
dados_3 <- dados[dados$Population > 1.5 * mean(dados$Population),]

estados_e <- rownames(dados_3)

# F) Construa o data frame dados_4 com os estados americanos com
# popula��o duas vezes maior que a mediana dos 50 estados ou
# de expectativa vida de maior que 71.84

dados[dados$Population > 2*median(dados$Population),]
dados[dados$'Life Exp' > 71.84,]
dados_4 <- dados[dados$Population > 2*median(dados$Population) |
                   dados$'Life Exp' > 71.84,]

dim(dados_4)

# G) Obtenha o data frame dados_5 com os estados esstadunidanses
# com renda maior que a m�dia nacional e expectativa de vida
# maior que 72 anos.

dados_5 <- dados[dados$Income > mean(dados$Income) &
                   dados$`Life Exp` > 72,]

# Quantos s�o?
dim(dados_5)

# Quais s�o?
estados_g <- rownames(dados_5)

# Operadores [] e [[]]-------------------------------------------

# Podemos acessar os operador $, [[]] ou [] para acessar os elementos de uma lista
# Qual � a diferen�a?

# Usamos [[]] para itens �nicos items, e $ para itens nomeados

# H� uma met�fora �til para os operadores [] e [[]]: vag�es de em trem e os
# objetos que eles cont�m. x[[3]] corresponderia ao conte�do do vag�o 3,
# enquanto x[3] corresponde ao pr�prio vag�o 3.

x <- list(1:3, "a", 4:6)

# Ao acessar nosso "trem" x, podemos acessar um vag�o ou conjunto de vag�es,
# ou o conte�do de um deles.

x[3]      # Lista com os n�meros 1,2, e 3
x[[3]]    # Vetor com os n�meros 1,2, e 3

# O operador [] permite acessar m�ltiplos elementos de uma lista,
# enquanto o operador [[]] acessa um elemento �nico. O operador $
# � uma abrevia��o de [[]], usado para elementos nomeados.


# Exemplo
b <- list(a = list(b = list(c = list(d = 1))))


b[[c("a", "b", "c", "d")]]
# ...que � o mesmo que...
b[["a"]][["b"]][["c"]][["d"]]
# que � o mesmo que...
b$a$b$c$d

# Aqui terminamos o assunto "Estruturas de dados em R" e o slide Aulas1.


# Simplifica��o vs Preserva��o-------------------------------


# Quando acessamos um subconjunto de uma estrutura de dados, �
# a estrutura do objeto gerado pode ser a mesma do original
# (preservada) ou alterada (simplificada)

# Exemplo: y abaixo � uma lista de dois elementos num�ricos nomeados.




# Consequ�ncias da simplifica��o:


# I) A simplifica��o descarta os nomes dos objetos

x <- c(a = 1, b = 2)

x[1]   # O operador [] preserva a estrutura, mantendo o nome "a" do elemento 1.
x[[1]] # Enquanto [[]] acessa apenas o valor do elemento.


# II) A simplifica��o retorna o objeto dentro da lista e n�o uma lista de um elemento.

y <- list(a = 1, b = 2)

str(y[1])     # Este comando gera uma lista com o primeiro elemento de y
str(y[[1]])   # J� este gera um vetor num�rico com o primeiro n�mero de y



# III) A simplifica��o de um fator descarta os n�veis n�o utilizados
z <- fator(c("a", "b"))
z[1]
z[1, drop = TRUE] # O argumento drop for�a preserva��o ou simplifica��o
                  # da estrutura do subconjunto de um objeto


# IV) Se os �ndices de ao menos uma das dimens�es tem comprimento 1,
#     a simplifica��o reduz a dimens�o.

a <- matrix(1:4, nrow = 2) # Matriz 2 x 2 com os n�meros de 1 a 4

a[1,,drop = FALSE] # Exibimos uma matriz 2 x 2com uma linha com os elementos
                   # da primeira linha de a, e uma linha vazia

a[1, ]             # Temos um vetor com os inteiros da primeira linha de a


# Por padr�o, filtros de um data frame simplificam a estrutura
# de dados

# Se uma das estruturas � informada e a outra � deixada em branco,
# a estrutura ser� preservada. Se uma das estruturas � informada
# a outra � selecionada por completo (ex: todas as linhas de uma coluna),
# a estrutura ser� simplificada.

df <- data.frame(a = 1:2, b = 1:2)
str(df[1])
str(df[[1]])
str(df[,"a", drop = FALSE])
df[,"a"]


# Exerc�cio: Vamos acessar o data frame mtcars, que � padr�o do R.

View(mtcars)
is.list(mtcars)

# Temos que os comandos abaixo s�o equivalentes
mtcars[[1]]
mtcars$mpg

# Veja que � poss�vel passar uma vari�vel que cont�m o nome do objeto
# com o operador [[]], mas n�o com o $
var <- "cyl"

mtcars$var
mtcars[[var]]

# Isso ocorre porque $ equivale a [[]] com o argumento exact como FALSE
# que por padr�o de [[]] � TRUE. O quer dizer que � poss�vel $ acessar
# um objeto quando o nome informado n�o � exatamente o real.

mtcars$cy # Em vez de "cyl"

# Veja este outro exemplo:

x <- list(abc = 1)
x$a
x[["a"]]

# Portanto, cuidado ao usar o operador $, pois euso inadequado dele
# pode levar a ambiguidades ou comportamento inesperado.


# Todos os operadores de subconjuntos estudados s�o compat�veis
# com a atribui��o de valores. Veja os exemplos abaixo:

x <- 1:5
x[c(1, 2)] <- 2:3
x

x[-1] <- 4:1
x

# Quando a mesma posi��o � informada mais de uma vez, as atribui��es
# ocorrem em mem�ria mas teremos como resultado apenas o valor final.
x[c(1, 1)] <- 2:3
x

# OBS: NA passados em vetor de �ndices geram erros em vetores de inteiros,
# mas s�o permitidos em vetores l�gicos (e tratados como FALSE)

x[c(1, NA)] <- c(1, 2)

x[c(T, F, NA)] <- 1


# Essa compatibilidade � �til quando queremos realizar uma atribui��o
# condicional em um objeto. 
df <- data.frame(a = c(1, 10, NA))
df$a[df$a < 5] <- 0
df$a


# Aprendemos que o comando lapply() serve para aplicar uma fun��o
# em todos os termos de uma lista. Veja os dois c�digos abaixo.
# Tente entender o que eles fazem, como as atribui��es funcionam
# e o que a fun��o informada faz.

dados <- mtcars
dados <- lapply(X = mtcars, FUN = as.integer)
dados

dados <- mtcars
dados[] <-lapply(X = mtcars, FUN = as.integer)
dados


# Correspond�ncia entre vetores-----------------------------------

# Exemplo:
grades <- c(1,2,2,3,1)
info <- data.frame(grade = 3:1, desc = c("Excellent", "Good", 
                                         "Poor"), fail = c(F,F,T))

# Usando o comando match() recebe dois um vetor como argumentos
# e associa cada elemento do primeiro com a posi��o em que ele
# aparece no segundo, retornado um vetor com essas posi��es.

# No nosso exemplo, temos um vetor "grades" com escores (notas, pontua��o)
# de alunos e um data frame que associa cada escore a um conceito correspondente
# (Excelente, Bom, Ruim) e a reprova��o ou n�o do aluno.

# O comando match() ir� retornar a posi��o (conceito) que corresponde
# a cada elemento de grades. Se chamarmos os elementos de info usando
# essas posi��es, exibiremos os conceitos e reprova��o (ou n�o)
# de cada escore.

id <- match(grades, info$grade)
info[id,]

