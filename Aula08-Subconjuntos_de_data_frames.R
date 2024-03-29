# Introdu��o a softwares estat�sticos
# Data: 10/07/2019
# Aula: 08
# Assunto: Dataframes

# Acessando subconjuntos de um data frame -------------------------

# Assim como em arrays, podemos selecionar subconjuntos de um
# data frame atrav�s de um vetor ou express�o l�gica com �ndice

df <- data.frame(x = 1:5, y = 5:1, z = letters[1:5])
df

# Para retornar as linhas de df onde x � igual a 2
df[df$x == 2, ]

# Para retornar as linhas de df onde x � diferente de 2
df[df$x != 2, ]

# Para retornar a primeira e a terceira linhas
df[c(1,3),]

# OBS: Operadores l�gicos "e' e "ou" t�m formas diferentes,
# dependo se se deseja um �nico valor l�gico ou um vetor

(c(1,2) < c(2,3)) | (c(2,3) <= c(3,2))
    # Equivale a c(TRUE, TRUE) | c(TRUE, FALSE)
    # Equivale a c(TRUE) | c(TRUE)
    # Retorna TRUE TRUE

(c(1,2) < c(2,3)) || (c(2,3) <= c(3,4))
    # Equivale a c(TRUE, TRUE) || c(TRUE, FALSE)
    # Equivale a c(TRUE) || c(TRUE)
    # Retorna TRUE

(c(1,3) < c(2, 4)) & (c(3,4) < c(3,5))
(c(1,3) <= c(2, 3)) && (c(3,4) <= c(4,5))


# Voltando...

# Se queremos as linhas onde x � "a" ou "c"
df[df$z == "a" | df$z == "c", ]

# Se queremos as linhas de �ndice �mpar 
df[c(TRUE, FALSE), ]

# Se queremos as duas primeiras colunas da quarta linha
df[4, c(1,2)]

# Se queremos todas as linhas onde x � maior que 2 e y � maior que 2
df[df$x >= 2 & df$y >= 2,]


# Isso ocorre pois as express�es l�gicas retornam as posi��es na
# forma de um vetor l�gico

df$x == 4 # Posi��es onde x � igual a 4
df$y != 3 # Posi��es onde y � diferente de 3

# Assim, podemos criar filtros que reornam apenas certos
# elementos de um data frame


# Exerc�cio: 

# A) Vamos criar uma lista de notas

notas <- list(aluno_1 = c(7.1, 3.2, NA),
              aluno_2 = c(2.7, 8.8, 10.0),
              aluno_3 = c(0.0, NA, NA),
              aluno_4 = c(7.7, 8.4, 6.3),
              aluno_5 = c(3.6, 6.6, 8.1),
              aluno_6 = c(NA, NA, NA),
              aluno_7 = c(7.4, 7.1, 7.3),
              aluno_8 = c(10.0, NA, 7.0),
              aluno_9 = c(1.6, 3.2, 5.3),
              aluno_10 = c(8.8, 9.2, 8.0))


# B) Agora, crie o vetor status com a situa��o dos 10
# alunos de acordo com as notas: Aprovado, Reprovado, Final

status <- c("F", "A", "REP","A", "F", "REP", "A", "F", "REP", "A")


# C) Agora, use o comando paste() para criar um vetor com
# os nomes dos alunos na forma aluno_n

nomes <- paste("Aluno", 1:10, sep = "_")


# D) Contrua uma data frame do nome historico com as vari�veis
# nomes,  notas e status

historico <- data.frame(nomes = nomes, notas = I(notas), status = status)
historico


# E) Agora, com o data frame historico, crie outros 3 data frames
# contendo os alunos aprovados, reprovados e na final atrav�s de filtros

aprovados <- historico[historico$status == "A",]
reprovados <- historico[historico$status == "REP",]
na_final <- historico[historico$status == "F",]

# Se quisermos saber quantos alunos est�o em cada situa��o,
# podemos contar as linhas dos data frames

dim(aprovados) # Retorna as dimens�es (linha e coluna) na forma de vetor
dim(aprovados)[1] # Retorna apenas o primeiro elemento do vetor
dim(reprovados)[1]
dim(na_final)[1]


# F) Suponha que o professor est� interessado em criar um data frame
# chamado "bons_alunos" com os alunos que foram aprovados e os que
# tem chance de ser aprovados. Como voc� faria isso?

# Podemos concatenar os data frames de alunos aprovados e na final...
bons_alunos <- rbind(aprovados, na_final)

# ... Ou simplesmente obter esses elementos diretamente de historico
bons_alunos <- historico[historico$status == "A" | historico$status == "F",]


# G) Mude os nomes das linhas do data frame historico colocando id_1 na
# primeira linha, e assim sucessivamente

rownames(historico) <- paste("id", 1:nrow(historico), sep = "_")


# H) Agora, a partir do data frame historico, crie um novo
# data frame historico_na que cont�m os alunos que precisam repor alguma nota

# Vamos usar o comando is.na() para encontrar as notas que faltam
# Ele nos retorna um vetor de valores l�gicos que
x <- c(2,3, NA, 5)
is.na(x)
# Como vamos aplicar na lista de notas, usaremos o comando lapply() para is.na

# Com um lapply(), temos uma lista com quais notas s�o NA
lapply(historico$notas, FUN = is.na)

# Com outro lapply(), podemos transformar os resultados em algo num�rico

# Com mais outro lapply(), podemos somar a quantidade de TRUE
lapply(lapply(historico$notas, FUN = is.na), FUN = sum)

# Com o comando unlist(), podemos tranformar essa lista de provas a repor
# em um vetor
situacao <- unlist(lapply(lapply(historico$notas, FUN = is.na), FUN = sum))

# Finalmente, podemos especificar que queremos as linhas de historico
# que correspondem a posi��es de situacao diferentes de zero.

historico_na <- historico[unlist(situacao) != 0,]


# Alternativamente, m vez de comparar com zero, poder�amos tamb�m converter
# os valores de situacao de n�merico para l�gico
historico_na <- historico[as.logical(situacao),]


# I) Agora, adicione ao data frame historico a m�dia apenas dos alunos
# que realizaram as tr�s provas.

# Para gerar as m�dias, usamos o comando lapply() para aplicar a m�dia
# para todos os alunos que n�o perderam nenhuma prova, isto �, 
# correspondem a 0 em situacao. 
# Tamb�m atribu�mos essa lista de m�dias para a vari�vel media do data
# frame. Para especificar que iremos atribuir apenas aos elementos
# de situacao 0, informamos o �ndice a historico tamb�m.
# Lembrando que, se tentarmos atribuir uma lista no momento da cra��o de uma
# vari�vel, precisaremos tamb�m do comando I()
historico[unlist(situacao) == 0,]$media <- I(lapply(X = historico[unlist(situacao) == 0,
                                                      ]$notas, FUN = mean))


