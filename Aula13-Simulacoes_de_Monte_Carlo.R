# Introdu��o a softwares estat�sticos
# Data: 27/07/2019
# Aula: 13
# Assunto: Simula��es de Monte Carlo


# Comando mapply()-------------------------------------


# Corra o c�digo abaixo. Como voc� explicaria sua sa�da? E os argumentos?

mapply(rep, 1:4, 4:1)


# Leia a documenta��o.
?mapply


# O comando mapply() serve para passar os elementos de m�ltiplas
# listas ou vetores como argumentos para a fun��o. Veja um outro
# exemplo:

l1 <- list(a = LETTERS[c(4,6,12,6)],
           b = LETTERS[c(1,5,21,1)])

l2 <- list(c = LETTERS[c(4,14,22,20)],
           d = LETTERS[c(15,15,1,15)])

mapply(paste, l1$a, l1$b, l2$c, l2$d)

# Neste exemplo foram informados 4 vetores de caracteres para a fun��o paste().
# O mapply ir� executar o comando paste() 4 vezes (comprimento dos vetores)
# usando como argumento os i-�simos elementos de cada vetor. 


# Simula��es de Monte Carlo ------------------------------------------------

# Uma simula��o de Monte Carlo consiste em simular computacionalmente
# em experimento com n�meros pseudoaleat�rios in�meras vezes, armazenando
# os resultados de cada itera��o a fim se obter conclus�es sobre esse experimento.



# Exerc�cio 1: Walter est� jogando um jogo com dois dados equilibrados de 6 lados.
# O jogo consiste em jogar os dois dados e somar os resultados.

# Caso o resultado seja m�ltiplo de 3, ele ganhar� $ 6,00 e caso contr�rio,
# ele perder� $ 3,00. Simule 100 mil repeti��es e obtenha o valor esperado
# de um jogador.

# Analiticamente, temos 36 resultados poss�veis, e os m�ltiplos de 3 s�o:
# Soma 3: (1,2), (2,1):                      probabilidade 2/36
# Soma 6: (1,5), (2,4), (3,3), (4,2), (5,1): probabilidade 5/36
# Soma 9: (3,6), (4,5), (5,4), (6,3):        probabilidade 4/36 
# Soma 12: (6,6):                            probabilidade 1/36

# Portanto, a probabilidade da soma das facessuperiores dos dados ser igual
# a um n�mero m�ltiplo de 3 � de 12/36, ou 1/3 (aproximadamente 0,33 ou 33%).



# Para gerar nossos n�meros pseudo aleat�rios iremos utilizar o comando sample().
# Ele serve para simular uma amostra aleat�ria de elementos de um grupo inicial,
# retornando um vetor. Por padr�o o argumento replace, que controla a reposi��o
# ou n�o nas amostras, � FALSE.
# Vamos aplic�-lo a uma �nica retirada do nosso experimento:

sample(x = 1:6, size = 2, replace = TRUE)


# Para reproduzir o c�digo 100.000 vezes, podemos usar um loop.

# Vamos criar dois vetores inicialmente vazios para guardar os resultados

# Lembrando que R aceita nota��o cient�fica. 100000 pode ser representado
# por 1e5, que significa "1 vezes 10 elevado a 5"

sucessos <- numeric(1e5) 
fracassos <- numeric(1e5)


for(i in 1:1e5){
  dados <- sample(x = 1:6, size = 2, replace = TRUE)
  if( (sum(dados) %% 3) == 0)
    sucessos[i] <- 1
  else
    fracassos[i] <- 1
}

# Somando os sucessos/fracassos e dividindo pelo n�mero de repeti��es...

sum(sucessos)/1e5
sum(fracassos)/1e5

# ...os resultados encontrados correspondem ao esperado?



# Exerc�cio 2: Suponha que tenhamos uma urna com bolas de mesmo tamanho
# enumeradas de 1 a 100. Considere o experimento aleat�rio de retirar
# uma bola e observar seu n�mero at� obtermos a bola com o n�mero desejado,
# escolhido anteriormente de forma arbitr�ria.

# Neste experimento, ser� considerada a reposi��o, isto �, caso a observa��o
# n�o seja o n�mero desejado, a bola ser� devolvida � urna.
# Simule no computador 10.000 repeti��es e obtenha a m�dia das retiradas
# necess�rias para se obter o n�mero escolhido.

# Solu��o:

# Se definimos o total de retiradas at� se encontrar o n�mero desejado como uma 
# var�avel aleat�ria X, ou seja, repeti��es independentes de um experimento at�
# o primeiro sucesso, temos neste experimento uma v.a. Geom�trica com p = 1/100.
# Ou seja, analiticamente, nosso valor esperado para as retiradas � 1/p = 100 retiradas.


# Computacionalmente:

resultados <- numeric(1e4) # Vetor vazio para os resultados

num <- 13 # N�mero escolhido

for(i in 1:1e4){
  
  retiradas <- 0 
  
  repeat{ 
    bola <- sample(x =1:100L, size = 1L)
    retiradas <- retiradas + 1           
    
    if(bola == num) 
      break
  }
  
  resultados[i] <- retiradas
}

mean(resultados)


# Exerc�cio 3: Um dono de cassino estuda disponibilizar um novo jogo
# e solicita uma consultoria estat�stica para saber se o jogo ser�
# vi�vel para o cassino, isto �, se o valor esperado do lucro � positivo.
# Regras:

# 1) O jogador para $ 10,00 para come�ar a jogar e lucra 1,50 a cada lan�amento

# 2) Dois dados s�o lan�ados e caso a soma seja 5,6,7,8, ou 9, o
# jogo termina imediantamente.

# 3) Se nenhum dos lan��mentos acima for obtido, o jogador continua os lan�amentos
# at� obter soma igual a 11 ou 12.

# Realize uma siula��o de 100.000 jogos e obtenho o valor m�dio dos lan�amentos,
# a probabilidade aproximada do jogador realizar apenas um lan�amento,
# e o lucro esperado por jogador no cassino.


resultados <- numeric(1e5)
lucro <- numeric(1e5)

for(i in 1:1e5){
  
  lucro[i] <- -10
  lancamentos <- 0 
  
  repeat{
    lancamentos <- lancamentos + 1
    lucro[i] <- lucro[i] + 1.5
    dados <- sample(x =1:6, size = 2, replace = TRUE)
    
    # Para determinar se a soma � igual a um dos valores em um vetor,
    # usaremos o operador %in% (mais informa��es na Aula 10)
    
    # Para o primeiro lan�amento
    if((dados[1] + dados[2]) %in% c(5,6,7,8,9) && lancamentos == 1)
      break
    
    # Para os outros lan�amentos
    if((dados[1] + dados[2]) %in% c(11,12) && lancamentos != 1)
      break
  }
  
  resultados[i] <- lancamentos
}

# OBS: quando se lida com opera��es em loops, geralmente h� mais de uma
# maneira correta de se chegar a um mesmo resultado


# Lucro esperado de um jogador
mean(lucro)

# N�mero m�dio de lan�amentos
mean(resultados)

# Probabilidade aproximada de realizar apenas um lan�amento 
length(resultados[resultados == 1])/1e5

# Para mais informa��es sobre os resultados, como m�ximo e m�nimo,
# podemos usar o comando summary

summary(lucro)
summary(resultados)


# Caso queiramos uma representa��o visual do lucro, podemos criar
# um histograma simples

hist(lucro)
grid(lwd = 1)

# E ent�o, que conclus�es podemos tirar desse jogo do Exerc�cio 3?

