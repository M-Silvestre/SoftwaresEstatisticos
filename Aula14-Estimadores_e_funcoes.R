# Introdu��o a softwares estat�sticos
# Data: 31/07/2019
# Aula: 14
# Assunto: Estimadores e fun��es


# Estimadores --------------------------------------------------------

# Exerc�cio: Seja X1, X2, ... , Xn uma amostra aleat�ria da vari�vel
# aleat�ria X~N(mi, sigma2). Al�m disso, considere os seguintes 
# estimadores da vari�ncia sigma de X, sigma_c2 e S2, apresentados abaixo:

# sigma_c2 = somat�rio((Xi - media_amostral)^2) / n

# S2 =  somat�rio((Xi - media_amostral)^2) / (n - 1)

# Uma forma de compararmos dois estimadores � atrav�s do Erro Quadr�tico
# M�dio (EQM) dos estimadores. Dessa forma, sejam theta_1 e theta_2 dois
# estimadores para um par�metro theta desconhecido. Diremos que theta_1 
# � melhor que theta_2 se:

# EQM(theta_1) <= EQM(theta_2)

# Onde EQM(theta_) = E[(theta_c - theta)^2] (valor esperado do quadrado da
#                                           diferen�a entre o estimador e
#                                           o par�metro)

# Ou tamb�m: EQM(theta_c) = Var(theta_c) + (E(theta_c) - theta)^2
#            (soma da vari�ncia e do quadrado da diferen�a entre o valor
#             esperado do estimador e do par�metro. Esta diferen�a tamb�m
#             � chamada vi�s.)



# Exerc�cio: Abaixo est� representado um algoritmo para implementa��o
# da simula��o, em que levar� em considera��o 20000 itera��es.

# Para cada uma das 20000 itera��es:
# 1) Gere uma amostra aleat�ria de tamanho n, tal que Xi~N(0,1) (X tem
# distribui��o normal de m�dia zero e vari�ncia 1), para todo i.
# 2) Obtenha as estimativas sigma_c2 e S2
# 3) Calcule o EQM e o vi�s tanto de sigma_c2 e de S2 


# Temos que os estimadores sigma_c2 e S2 correspondem � vari�ncia
# populacional e amostral, respectivamente.


variancia <- function(x, pop = FALSE){
  
  # Para o argumento "pop" (popula��o)
  
  if(pop)
    sum((x - mean(x))^2)/length(x)
  else
    var(x) # var() � a o c�lculo de vari�ncia amostral:
           # var(x) = sum((x - mean(x))^2)/(length(x) - 1)
}


# Mais informa��es sobre var()

?var
stats::var


# Agora os dados

N <- 2E4
n <- 20 

# Vetores de zeros

vetor_sigma2 <- numeric(N)
vetor_s2 <- numeric(N)

# Para replica��o dos resultados

set.seed(0)

for(i in 1:N){
  amostra <- rnorm(n = n, mean = 0, sd = 1) # Amostra
  
  vetor_sigma2[i] <- variancia(x = amostra, pop = TRUE) # Vari�ncia Populacional
  vetor_s2[i] <- variancia(x = amostra, pop = FALSE)    # Vari�ncia Amostral

}

# M�dia dos estimadores. Qual deles se aproximou mais do valor
# real do par�metro, vari�ncia de X?

mean(vetor_sigma2); mean(vetor_s2)

# Vi�s dos estimadores.

bias_sigma2 <- mean(vetor_sigma2) - 1
bias_s2 <- mean(vetor_s2) - 1

bias_sigma2; bias_s2


# Vari�ncia dos estimadores. Qual deles � mais eficiente? (vari�ncia menor)

var_sigma2 <- var(vetor_sigma2); var_s2 <- var(vetor_s2)

var_sigma2; var_s2


# Erro Quadr�tico M�dio. Qual deles � mais consistente? (menor EQM)

eqm_sigma2 <- var_sigma2 - (bias_sigma2)^2
eqm_s2 <- var_s2 - (bias_s2)^2


# De acordo com seus conhecimentos de Probabilidade e Infer�ncia,
# o que se esperava desses resultados?


# Fun��es ---------------------------------

# Sintaxe da declara��o de uma fun��o em R:

# nome_da_funcao <- function(argumentos){
#                 bloco de c�digo da fun��o
# }

# OBS: Sobre fun��es de mesmo nome: a presen�a de duas fun��es de
# mesmo nome em pacotes R instalados n�o causa erros, mas sua
# execu��o, sim. Algumas solu�oes para se contornar esse problema s�o:

# 1) Criar fun��es cujo nome n�o est� sendo usado, como foi 
# feito anteriormente para o c�lculo da vari�ncia

# 2) Explicitar o ambiente da fun��o ao cham�-la

stats::var() # Fun��o para c�lculo de vari�ncia que vem do pacote stats

# A aba "Environment" permite ver os objetos (valores, data frames,
# fun��es, etc.) em uso. De acordo com seu ambiente. Assim, � poss�vel
# utilizar duas fun��es de mesmo nome mas comportamentos diferentes
# desde que se expecifique de onde elas v�m.


# Apesar de ser desej�vel que uma fun��o contenha argumentos que
# possam receber dados e informa��es de como processar as informa��es
# passadas, � poss�vel que uma fun��o n�o possua nenhum argumento.

funcao <- function(){
  cat(paste(c("<3", LETTERS[c(1,13,15)], " ", LETTERS[c(13,5,21)],
              " ", LETTERS[c(16,18,15,6,5,19,19,15,18)], "<3"),
            collapse = " "))
}


# Note que a cria��o de uma fun��o consiste na atribui��o do
# conte�do no bloco de c�digo que comp�e a fun��o e seus
# argumentos a um nome, como qualquer objeto da linguagem R.


# Exemplo: corra o c�digo abaixo. Ele corresponde a uma fun��o que
# converte o valor de uma temperatura em graus Celsius para o valor
# correspondente em graus Fahrenheit.

celsiustofar <- function(cel){ # Argumento da fun��o: temperatura informada
  
  temperatura <- 1.8 * cel + 32 # instru��o a ser executada
  
  temperatura # Resultado a ser retornado
}

# OBS1: em C, � preciso explicitar o que uma fun��o deve retornar.
# Em R, por padr�o se retorna o resultado da �ltima opera��o da fun��o.

# OBS2: Como em outros comandos, caso a fun��o a ser criada ocupe apenas
# uma linha, os {} s�o opcionais. A fun��o celsiustofar() poderia ter
# sido declarada assim:

celsiustofar <- function(cel) temperatura <- 1.8 * cel + 32


# Exerc�cio: Considere no exemplo o vetor abaixo. Quais altera��es s�o
# necess�rias para que nossa fun��o celsiustofar() converta todas as 
# 11 teperaturas de uma vez?

temp <- c(27.8, 19.3, 20.7, 18.29, 25.0, 25.1, 32.3,
          37.6, 32.2, 19.02, 22.75)

# Resposta: nenhuma! R � uma linguagem vetorial, ou seja, enquante em C
# poder�amos ter uma vari�vel com apenas um valor, em R ela � na verdade
# um vetor com apenas um elemento. Portanto, diferentemente de C, em R
# n�o � preciso um loop para se percorrer um vetor com nossa fun��o

celsiustofar(25)

celsiustofar(temp)


# Agora, altere a fun��o celsiustofar() apresentada no exemplo anterior
# de modo que a fun��o possa conveter a temperatura de graus Celsius
# para Fahrentheit ou de graus Fahrenheit para Celsius


temp_convert <- function(temp, c_para_f = TRUE){
  
  # � poss�vel instanciar valores padr�o para os argumentos de fun��es
  # Isso se faz quando se d� um valor na declara��o da fun��o, e este
  # ser� utilizado caso o usu�rio n�o o informe.
  
  
  c_para_f <- as.logical(c_para_f) # Caso c_para_f seja informado como uma
                                   # string, � poss�vel converter para 
                                   # um valor l�gico
  
  if(! is.logical(c_para_f) | is.na(c_para_f)) # Em caso de erros
    stop("Um valor l�gico n�o foi informado.")
  
  if(c_para_f == TRUE)
    temperatura <- 1.8 * temp + 32
  
  else
    temperatura <- (temp - 32) /1.8 # Opera��o reversa � anterior
    
  temperatura
    
}

temp_convert(100)
temp_convert(100, TRUE)
temp_convert(100, FALSE)

# OBS: Os comando stop() e warning() servem para exibir mensagens 
# de erro ou aviso, respectivamente, no prompt do R.

stop("Exemplo de uma mensagem de erro.")

warning("Exemplo de uma mensagem de aviso.")

# A diferen�a est� no fato de stop() interromper imediatamente a
# execu��o do c�digo e warning() n�o, sendo exibido apenas ap�s
# a execu��o do c�digo. Eles servem para indicar para o usu�rio
# que a fun��o est� sendo usada de forma incorreta ou inaporpriada.

for(i in 1:5){
  cat("Itera��o",i,"\n", sep = " ")
  if(i == 3)
    stop("O stop() parou o loop")
}

for(i in 1:5){
  cat("Itera��o",i,"\n", sep = " ")
  if(i == 3)
    warning("O warning() n�o parou o loop.")
}

# Exerc�cio: construa uma fun��o que calcula o IMC (�ndice de Massa
# corp�rea) de uma pesoa e retorne o IMC e a situa��o da pessoa
# em rela��o ao peso.

# Nota: IMC = peso / (altura^2)

funcao_imc <- function(peso, altura){
  
  # A altura dever� ser informada em metros.
  # O peso dever� ser informado em kg.
  
  # Podemos fazer com que o programa n�o retorne nada
  # para informa��es negativas
  
  if(peso <= 0 || altura <= 0)
    return(NULL)
  
  if(altura >= 3)
    warning("Dados... estranhos.")
  
  imc <- peso/(altura^2)
  
  if(imc < 17)
    situacao <- "Muito abaixo do peso"
  else if(imc >= 17 && imc < 18.49)
    situacao <- "Abaixo do peso"
  else if(imc >= 18.5 && imc < 24.49)
    situacao <- "Peso normal"
  else if(imc >= 25 && imc < 29.49)
    situacao <- "Acima do peso"
  else if(imc >= 30 && imc < 34.99)
    situacao <- "Obesidade I"
  else if(imc >= 35 && imc < 39.99)
    situacao <- "Obesidade II (severa)"
  else
    situacao <- "Obesidade III (m�rbida)"
  
  
  list(imc, situacao)
    
}

# OBS: caso se deseje que o programa termine e retorne algo
# antes do fim do bloco de c�digo, basta usar a instru��o return().