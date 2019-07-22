# Introdu��o a softwares estat�sticos
# Data: 17/07/2019
# Aula: 10
# Assunto: Estruturas condicionais


# OBS: Operadores infixos ------------------------------------------

# Em R existem os chamados "operadores infixos", cujos argumentos
# n�o precisam estar contidos por ().

2 + 3

# Observe que todo operador infixo tamb�m pode ser chamado de
# forma convencional
'+'(2,3)

# Exemplo: o operador %in% verifica se os elementos de um vetor tamb�m
# ocorrem em outro. 
c(1,2,7) %in% c(1,2,3,4,5)
c("Pedro", "Rafael") %in% c("Rafael", "Jo�o", "Jos�")


# Intera��o com o usu�rio --------------------------------------


# Assim como C, R tamb�m possui fun��es para interagir com o usu�rio,
# como o print(), que exibe valores em tela.


for(i in 1:10) 1:10
# Todas as atribui��es de valores a i ser�o feitas na mem�ria, mas
# nada ser� exibido

for(i in 1:10) print(i)
# Agora cada atribui��o ser� exibida.

# Exemplo:
for(x in c(1,2,3)) print("String de caracteres")


# OBS: n�o confundir exibi��o na tela e retorno. Retorno � tudo que pode
# ser utilizado (armazenado em uma vari�vel, passado como argumento
# de fun��o, etc.)


# A fun��o print() exibe apenas uma string ou o valor de um objeto.
# J� cat() � uma fun��o mais vers�til, pois � poss�vel intercalar objetos
# e strings, assim como exibir o valor de m�ltiplos objetos.

nota <- 7
cat("Jo�ozinho: Professor,\nqual a foi a minha nota?
    Professor: Sua nota foi", nota, "\nJo�ozinho:\t...", sep = " ")

# Para mais informa��o, leia as documenta��es das fun�oes print() e cat():
?print
?cat


# R tamb�m possui permite a inser��o de valores pelo usu�rio
# pela fun��o scan():

x <- scan(n = 3)

y <- scan(what = character())

cat(y)


# Estruturas condionais ------------------------------------------------

# O R possui estrutura condicional, o if(). Funciona semelhantemente
# como em C.
x <- 7
z <- x

if(x > 0){
  cat('x � positivo.')
  y <- x/z
} else{
  cat('x n�o � positivo')
  y <- z
}
y

# OBS: A boa identa��o (recuo) de um c�digo deixa-o mais leg�vel por
# ressaltar a sua estrutura. Por exemplo, no c�digo acima a identa��o
# deixa v�sivel quais linhas de comando pertencem ao bloco if e quais
# pertencem ao bloco else.

# O atalho Ctrl + Shift + A serve para identar automaticamente um
# bloco de c�digo.


# Tamb�m � poss�vel aninhar estruturas if():

idade <- 30
if(idade < 18){
  grupo <- 1
} else if(idade < 35{
  grupo <- 2
} else if(idade < 65){
  grupo <- 3
} else{
  grupo <- 4
}
grupo

# OBS: chamamos de bloco de instru��o o c�digo contido em um {}.
# Apesar do c�digo acima ter apenas uma linha por bloco, um bloco
# pode conter v�rias linhas de c�digo e, assim como em C, podemos
# omitir o {} o bloco � apenas uma linha. R tamb�m permite
# que todo o comando if else esteja contido em uma �nica linha

nota <- 7
if(nota > 7) cat("Aluno> =(") else cat("Aluno: =)")


# A fun��o ifelse � um comando if() else resumido.
# Ela recebe tr�s argumentos: a condi��o, a express�o a ser
# retornada caso a condi��o seja atendida, e o retorno caso
# a condi��o n�o seja atendida.

set.seed(2) # Fixamos a semente usada na gera��o de n�meros aleat�rios
x <- rnorm(n = 5, mean = 0, sd = 1)
sig <- ifelse(x < 0, "-", "+")
sig


# O comando set.seed() serve para fixar um conjunto de n�meros pseudo
# aleat�rios a serem gerados. Isso permite que os resultados de
# um teste possam ser reproduzidos com o mesmo resultado.


# Uma outra instru��o instru��o condicional que poder� ser utilizada
# para escolher uma de v�rias alternativas poss�veis � a instru��o switch().
# Ela consiste em uma s�rie de argumentos que a depender de uma condi��o
# (primeiro argumento) que ser�o ou n�o executados.

set.seed(0)
expressao <- 1
vetor_normal <- rnorm(10)
switch (EXPR = expressao,
        round(mean(vetor_normal),1),
        round(median(vetor_normal))
)

# Quando o nome dos cases n�o � especificado, por padr�o o R os
# nomeia usando o n�mero do case. No c�digo acima, o primeiro
# caso � a m�dia dos n�meros em vetor_normal arredondada para a primeira
# casa decimal e o segundo � a mediana, tamb�m arredondada.


# OBS: se o case fornecido n�o existe, o comando switch() n�o resulta
# em erros, mas retorna NULL. Deve-se ter isso em mente quando se
# atribui o retorno do switch() a uma vari�vel.


# Exemplo: escreva um programa que calcule o imposto pago por mulheres
# e por homens, sabendo que as mulheres pagam 10% e os homens pagam 5% a mais.

sexo <- "M"

salario <- 5e03

# Usando o if() else:

if(sexo == "M" || sexo == "m"){ 
  imposto <- salario*0.15
  cat("O imposto a pagar �: ", imposto)
} else { 
  imposto <- salario*0.10
  cat("O imposto a pagar �:", imposto)
}


# Usando o switch():

imposto <- switch (EXPR = sexo,
        "M" = salario*0.15,
        "F" = salario*0.1
)
cat("O imposto a pagar �:", imposto)

# Usando o ifelse()

imposto <- ifelse(sexo == "M"|| sexo == "m", salario*0.15, salario*0.1)
cat("O imposto a pagar �:", imposto)