# Introducao a softwares estatisticos
# Data: 10/06/2019
# Aula: 03
# Assunto: Busca e documenta��o de fun��es e pacotes R



#O comando find() determina qual pacote R dentre os pacotes instalados
find("ls")

install.packages("AdequacyModel")

#� poss�vel usar uma fun��o sem carregar o pacote inteiro explicitando seu pacote
# EX:    AdequacyModel::pso()
# Isso � �til quando n�o se quer usar mem�ria desnecess�ria, ou quando fun��es de
# pacotes diferentes t�m mesmo nome (devido ao alto n�mero de pacotes implementados
# em total)

# O comando help() tamb�m informa o pacote que cont�m uma fun��o
?sum #base
?hist #graphics


# O comando sessionInfo() exibe informa��es sobre a sess�o, incluindo
# a vers�o do R usada, o sistema operacional,
# e quais pacotes s�o carrgados automaticamente
sessionInfo()


# A aba "Packages" do RStudio tamb�m permite carregar,
# descarregar e remover pacotes instalados


# OBS: O R usa a bibliotecta BLAS (Basic Linear Algebra Subprograms) para
# tratar com matrizes e �lgebra linar em geral. Seu c�digo n�o � otimizado
# do ponto de vista computacional, por isso existem alternativas (incluindo
# uma no github do prof.), mas sua instala��o vai al�m dos conte�dos
# abordados nesta disciplina.



# Outro comando �til para encontrar fun��es � o apropos(), que busca
# nomes de fun��es que cont�m um termo fornecido.
apropos("ps")
# OBS: o comando busca apenas em pacotes carregados. Caso a fun��o que se
# deseje esteja em um pacote instalado, mas n�o carregado, ela n�o aparecer�
# nos resultados


# O comando
temp <- 1:6
temp
rm(temp)
temp

# O comando example() roda um exemplo da fun��o informada
example(rm)


# O comando abaixo retorna uma lista com todos os objetos contidos no pacote "base"
objects(grep("base",search()))
class(search())
is.vector(search())

# OBS: O R l� as fun��es de dentro para fora. Leve isso em considera��o quando
# ler e escrever comandos.


# O que a fun��o grep() faz?
lista <- c("Elemento 1", "Elemento 2", "Elemento 3")
grep("2", lista)
grep("E", lista)

# Ela busca um termo em um vetor e retorna a posi��o (ou posi��es) em que ele
# ocorre. Tamb�m pode ser usada para retornar os termos nessas posi��es:
lista[grep("2", lista)]
lista[grep("E", lista)]


# Portanto, o comando abaixo retorna o n�mero de f
length(objects(grep("base",search())))

# Exerc�cio: para que serve o argumento ignore.case da fun��o grep()?
# Rode um comando modificando esse argumento
?grep
# Por padr�o, o comando grep() � Case Sensitive, isto �, diferencia
# letras mai�sculas e min�sculas. Isso � controlado pelo argumento ignore.case,
# que por padr�o � informado TRUE. � preciso explicit�-lo para faz�-lo FALSE:

cores <- c("Vermelho", "Azul", "Amarelo", "Verde")
grep("verde", cores)
grep("verde", cores, ignore.case = TRUE)

# � importante saber o padr�o para argumentos de certas fun��es para que
# elas se comportem da maneira como esperamos. Outro exemplo � o sum():
x <- c(1, 2, 3, NA)
sum(x)
sum(x, na.rm)


# O que os comandos abaixo retornam? Leia com aten��o antes de rodar,
# tente entender a l�gia envolvida.
length(objects(grep("AdequacyModel", search(), ignore.case = TRUE)))

objects(grep("AdequacyModel", search()))

search()[(grep("gr", search()))]

# Para fixar:
#   search(): retorna QUAIS objetos correspondem � busca
#   grep(): retorna as POSI��ES dos objetos que correspondem � busca


# O que fazer quando n�o lembrar do nome exato da fun��o?
library(AdequacyModel)
??PSo
# O comando retorna pacotes (nome de fun��es, documenta��o, etc.) que cont�m
# o termo buscado. Agora tente:

??Mean       #"Mean" significa "m�dia" em ingl�s

# Quantos resultados foram encontrados agora?


# O site StackOverflow � um f�rum sobre programa��o onde perguntas sobre
# in�meras linguagens de programa��o podem ser feitas e respondidas
# Muitas vezes uma d�vida sua pode j� ter sido questionada (e respondida)
# nos f�runs.


# Listas ------------------------------------------------------------------

# Para construir uma lista em R, usa-se list() em vez de c().
x <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))
str(x) #Estrutura de x
# Listas s�o uma estrutura vers�til, pois podem conter outras tipos e estruturas

# Sequ�ncias
# Por padr�o, o operador ":" gera seq�ncias de iterados em 1
seq1 <- 1:10
str(seq1)
# Tamb�m funciona para n�meros reais
seq2<- 1.7:7.7
str(seq2)
seq3 <- 1.7:6.1
str(seq3)

# Caso queira-se usar uma itera��o diferente de um, usa-se o comando seq()

# Explicitando o intervalo (argumento by)
seq(from = 0, to = 5, by = 0.5)
# Explocitando o n�mero de termos (argumento length.out)
seq(from = 3, to = 8, length.out = 9)

# OBS: Diferentemente de C, em R � poss�vel mudar a ordem dos argumentos
# de um comando. Por�m, se a ordem usada n�o for a definida como padr�o,
# � preciso explicitar o nome do argumento.

mean(TRUE, c(1, 2, NA, 4, 5, NA)) # N�o roda!
mean(na.rm = TRUE, x = c(1, 2, NA, 4, 5, NA)) # Roda normalmente retornando 3