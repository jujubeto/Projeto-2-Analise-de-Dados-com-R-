########################################################
###    EXPLORA??O E AN?LISE DOS DADOS - PROJETO 2    ###
########################################################

# CARREGAR PACOTES
library(dplyr)
library(rstatix)
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(plotly)) install.packages("plotly")
library(plotly)


# BUSCAR DIRET?RIO (PASTA COM OS ARQUIVOS)
setwd("C:/Users/gilbe/OneDrive/Documentos/Estudo Linguagem R/dados-covid-sp-master/data")

# ABRIR ARQUIVO
srag_sp <- read.csv('SRAG_2020.csv', sep = ";")
View(srag_sp)

# EXCLUIR COLUNAS
srag_sp_mod <- select(srag_sp, -c(51:133))
srag_sp_mod <- select(srag_sp_mod, -c(5:8))
View(srag_sp_mod)

srag_sp_mod <- select(srag_sp_mod, -c(6,8))

glimpse(srag_sp_mod)

srag_sp_mod$DT_NOTIFIC <- as.Date(srag_sp_mod$DT_NOTIFIC, format ='%m/%d/%Y')

# Renomeando vari?veis (colunas)
srag_sp_mod <- rename(srag_sp_mod, sexo = CS_SEXO, idade = NU_IDADE_N)
View(srag_sp_mod)


# Verificando valores missing (Ausentes)
# NA = valores ausentes
# NAN = not a number(valor indefinido)
sapply(srag_sp_mod, function(x) sum(is.na(x)))
sapply(srag_sp_mod, function(x) sum(is.nan(x)))








?graphics
library(help = "graphics")




# GR?FICO DE BARRAS

# Contagem COLUNA sexo
table(srag_sp_mod$sexo)


barplot(srag_sp_mod$sexo,col="blue")


grafico_barras=table(srag_sp_mod$sexo)
barplot(grafico_barras, col="yellow", main="QUANTIDADE POR SEXO")

?barplot


# COM O GGPLOT2
ggplot(srag_sp_mod, aes(x = sexo)) +
  geom_bar(fill ='red')+ labs(title="Quantidade por sexo",
                              subtitle = "SRAG",
                              x = "Sexo", y = "Contagem")
?geom_bar()



# GR?FICO POR RA?A
sapply(srag_sp_mod, function(x) sum(is.na(x)))
sapply(srag_sp_mod, function(x) sum(is.nan(x)))
srag_sp_mod$CS_RACA[which(is.na(srag_sp_mod$CS_RACA))] <- 9
View(srag_sp_mod)

srag_sp_mod$CS_RACA[srag_sp_mod$CS_RACA == 1] <- "Branca"
srag_sp_mod$CS_RACA[srag_sp_mod$CS_RACA == 2] <- "Preta"
srag_sp_mod$CS_RACA[srag_sp_mod$CS_RACA == 3] <- "Amarela"
srag_sp_mod$CS_RACA[srag_sp_mod$CS_RACA == 4] <- "Parda"
srag_sp_mod$CS_RACA[srag_sp_mod$CS_RACA == 5] <- "Ind?gena"
srag_sp_mod$CS_RACA[srag_sp_mod$CS_RACA == 9] <- "Ignorado"


# Contagem 
table(srag_sp_mod$CS_RACA)

grafico_barras=table(srag_sp_mod$CS_RACA)
barplot(grafico_barras, col="yellow", main="QUANTIDADE POR RA?A")


# COM O GGPLOT2
ggplot(srag_sp_mod, aes(x = CS_RACA)) +
  geom_bar(fill ='blue')+ labs(title="Quantidade por ra?a",
                              subtitle = "SRAG",
                              x = "Ra?a", y = "Contagem")



# GR?FICO POR RA?A, SEXO e REGI?O

sapply(srag_sp_mod, function(x) sum(is.na(x)))
srag_sp_mod$CS_ZONA[which(is.na(srag_sp_mod$CS_ZONA))] <- 9

srag_sp_mod$CS_ZONA[srag_sp_mod$CS_ZONA == 1] <- "Urbana"
srag_sp_mod$CS_ZONA[srag_sp_mod$CS_ZONA == 2] <- "Rural"
srag_sp_mod$CS_ZONA[srag_sp_mod$CS_ZONA == 3] <- "Periurbana"
srag_sp_mod$CS_ZONA[srag_sp_mod$CS_ZONA == 9] <- "Ignorado"

table(srag_sp_mod$CS_ZONA)

ggplot(srag_sp_mod, aes(x = CS_RACA, y = sexo, fill = factor(CS_ZONA))) +
  geom_col(position = "dodge") +
  labs(title = "Região por sexo e raça",
       x = "Raça",
       y = "Sexo",
       fill = "Região")



# Gr?fico de barras na horizontal
ggplot(srag_sp_mod, aes(x = CS_RACA, y = sexo, fill = factor(CS_ZONA))) +
  geom_col(position = "dodge") +
  labs(title = "Regi?o por sexo e ra?a",
       x = "Ra?a",
       y = "Sexo",
       fill = "Regi?o") +
  coord_flip()


# GR?FICO DE BARRAS EMPILHAD0

grafico <- aggregate(idade ~ sexo + CS_ZONA, data=srag_sp_mod, FUN=mean)

ggplot(grafico, aes(x = CS_ZONA, y = idade, fill = factor(sexo))) +
  geom_col(position = "stack")



# GR?FICO COM O PLOTLY
srag_sp_mod %>% plot_ly(x = ~ CS_RACA) %>%
                      layout(xaxis = list(title = "Ra?a"),
                      yaxis = list(title = "Quantidade")) 











# BOXPLOT PARA IDADE

# IDADE
srag_sp_mod$idade[srag_sp_mod$TP_IDADE == 2] <- 0
srag_sp_mod$idade[srag_sp_mod$TP_IDADE == 1] <- 0


summary(srag_sp_mod$idade)
boxplot(srag_sp_mod$idade)

srag_sp_mod %>% identify_outliers(idade)
outliers <- c(boxplot.stats(srag_sp_mod$idade)$out)
srag_atual <- srag_sp_mod[-c(which(srag_sp_mod$idade %in% outliers)),]

summary(srag_atual$idade)
boxplot(srag_atual$idade)


# COM O GGPLOT2
srag_sp_mod %>% filter(!is.na(idade)) %>% 
  ggplot(aes(x = " ", y = idade)) + 
  geom_boxplot(width = .3, outlier.colour = "purple")


srag_atual %>% filter(!is.na(idade)) %>% 
  ggplot(aes(x = " ", y = idade)) + 
  geom_boxplot(width = .7, outlier.colour = "red")

?geom_boxplot()

# COM PLOTLY
plot_ly(srag_sp_mod, y = srag_sp_mod$idade, 
        type = "box") %>%
        layout(title = "BOXPLOT POR IDADE",
        yaxis = list(title = "Idade"))



### BOXPLOT coletivo
par(mfrow=c(1,2)) # Gr?ficos na mesma linha
boxplot(srag_atual$idade, ylab="idade sem outliers")
boxplot(srag_sp_mod$idade, ylab="idade com outliers")


par(mfrow=c(1,1)) # Gr?ficos uma linha e duas colunas
boxplot(idade ~ sexo, srag_atual, ylab="Idade", xlab="Sexo")
boxplot(idade ~ CS_RACA, srag_atual, ylab="Idade", xlab="Ra?a")


par(mfrow=c(2,2)) # Gr?ficos duas linhas e duas colunas
boxplot(idade ~ sexo, srag_atual, ylab="Idade", xlab="Sexo")
boxplot(idade ~ CS_RACA, srag_atual, ylab="Idade", xlab="Ra?a")
boxplot(srag_atual$idade, ylab="idade sem outliers")
boxplot(idade ~ CS_ZONA, srag_atual, ylab="Idade", xlab="Regi?o")

par(mfrow=c(1,1)) # ?nico Gr?fico

# COM GGPLOT2
ggplot(srag_atual, aes(x = factor(sexo), y = idade)) +
  geom_boxplot(fill = "dodgerblue") +
  labs(y = "Idade",
       x = "Sexo",
       title = "Distribui??o das idades por sexo") 


# COM PLOTLY
plot_ly(srag_atual, y = srag_atual$idade, color = srag_atual$sexo, 
        type = "box") %>%
        layout(title = "BOXPLOT POR IDADE",
        xaxis = list(title = "Sexo"), yaxis = list(title = "Idade"))









# HISTOGRAMA PARA IDADE

hist(srag_atual$idade, col="blue", main = "SRAG POR IDADE",
     xlab = "Distribui??o das idades", ylab = "Frequ?ncia" )
?hist

hist(srag_atual$idade, probability=T, col="brown")
lines(density(srag_atual$idade) , col="orange")

summary(srag_atual$idade)



# Criando a fun??o moda
moda <- function(m) {
  valor_unico <- unique(m) #Busca o valor ?nico para a coluna valor
  valor_unico[which.max(tabulate(match(m, valor_unico)))] #tabular (contabilizar quantas vezes o valor ?nico aparece) e buscar o maior valor
}
moda(srag_atual$idade)


# QQPLOT (GR?FICO DE DISTRIBUI??O NORMAL)
qqnorm(srag_atual$idade, col = "gray")
qqline(srag_atual$idade, col= "red")

library(nortest)
# Teste de normalidade Shapiro-Wilk
shapiro.test(srag_atual$idade)

# Anderson-Darling
ad.test(srag_atual$idade)

# Distribui??o n?o ? normal!


# COM O GGPLOT2
ggplot(data = srag_atual, aes(x=idade)) +
  geom_histogram(fill ='red', bins = 25)+ labs(title="Histograma da idade",
                                    subtitle = "SRAG",
                                    x = "Idade", y = "Contagem")

# COM O PLOTY
plot_ly(x = srag_atual$idade, type = "histogram")%>%
         layout(title = "HISTOGRAMA POR IDADE",
         xaxis = list(title = "Idade"), yaxis = list(title = "Quantidade"))












# GR?FICO DE DISPERS?O

plot(srag_atual$DT_NOTIFIC, srag_atual$idade,
     title("Casos de SRAG por m?s e por idade"), col = "purple")

scatter.smooth(srag_atual$DT_NOTIFIC, srag_atual$idade)


# COM O GGPLOT2 (2 vari?veis)
ggplot(srag_atual, aes(x = DT_NOTIFIC, y = idade)) +
  geom_point() +
  labs(title = "Relação data de notificação e idade",
       x = "Data de notificação",
       y = "Idade")



# COM O GGPLOT2 (4 vari?veis)
srag_atual_camp <- srag_atual %>% filter(ID_MN_RESI=="CAMPINAS")
View(srag_atual_camp)

ggplot(srag_atual_camp, aes(x = DT_NOTIFIC, y = idade, 
                       color = CS_RACA, shape = sexo)) +
  geom_point() + 
  labs(title = "Relação entre data de notificação, idade e sexo",
       x = "Data de Notificação",
       y = "Idade")


# COM O PLOTLY
plot_ly(x=srag_atual_camp$DT_NOTIFIC,y=srag_atual_camp$idade,type='scatter',
        mode='markers', color = srag_atual_camp$sexo)




# GR?FICO DE BOLHAS
srag_atual_tupa <- srag_atual %>% filter(ID_MN_RESI=="TUPA")
View(srag_atual_tupa)

ggplot(srag_atual_tupa, aes(x = DT_NOTIFIC, y = CS_ZONA, 
                            size = idade)) +
  geom_point() + 
  labs(title = "Rela??o entre data e regi?o por idade",
       x = "Data de Notifica??o",
       y = "Regi?o")


# COM O PLOTLY
plot_ly(x=srag_atual_camp$DT_NOTIFIC,y=srag_atual_camp$CS_ZONA,type='scatter',
        mode='markers', size = srag_atual_camp$idade)












# Gr?fico de setores (pizza)

table(srag_atual_camp$sexo)
pie(table(srag_atual_camp$sexo),col=c("red","blue"), radius=1)


glimpse(srag_atual_camp)
srag_atual_camp$sexo <- as.factor(srag_atual_camp$sexo)


contagem <- table(srag_atual_camp$sexo)
nomes = levels(srag_atual_camp$sexo)
porcentagem = round(contagem/sum(contagem)*100, 2)
rotulo = paste(nomes," (",porcentagem,"%",")", sep=" ")
pie(table(srag_atual_camp$sexo),labels=rotulo, main="SEXO",
          col=c("red","blue"), radius = 1)



# COM O GGPLOT2
library(scales)
grafico_pizza <- ggplot(srag_atual_camp, aes(x=" ", fill=sexo))+
  geom_bar(width = 1)+
  coord_polar("y")
grafico_pizza+ theme(plot.background = element_rect(fill="gray", colour="red"))


table(srag_atual_camp$sexo)
grafico <- data.frame(
  grupo = c("Masculino", "Feminino"),
  valores = c(1311, 1041))
  soma = sum(table(srag_atual_camp$sexo))

grafico %>%
  ggplot(aes(x="", y=valores, fill=grupo)) +
  geom_col() +
  geom_text(aes(label = percent(valores/soma, accuracy = 0.1)), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds") +
  coord_polar("y") +
  theme_void() +
  labs(title = "QUANTIDADE POR SEXO",
       fill = "LEGENDA")


# COM PLOTLY
plot_ly(srag_atual_camp, labels = ~sexo, type = 'pie')
plot_ly(srag_atual_camp, labels = ~CS_RACA, type = 'pie')
plot_ly(srag_atual_camp, labels = ~CS_ZONA, type = 'pie')











### FREQU?NCIAS

if(!require(sampling)) install.packages("sampling")
if(!require(TeachingSampling)) install.packages("TeachingSampling")
library(sampling)
library(TeachingSampling)

# Tabela de Frequ?ncias Absolutas
freq_abs <- table(srag_atual$idade) 
View(freq_abs)

# Tabela de Frequ?ncias Relativas
freq_rel <- prop.table(freq_abs) 
View(freq_rel)

# Porcentagem da frequ?ncia relativa
p_freq_rel <- 100 * prop.table(freq_rel) 
View(p_freq_rel)

# Criar uma linha com o total
freq_abs <- c(freq_abs, sum(freq_abs)) 
View(freq_abs)
names(freq_abs)[112] <- "Total"
View(freq_abs)

# Juntando a frequ?ncia relativa e a frequ?ncia percentual com suas respectivas somas.
freq_rel <- c(freq_rel, sum(freq_rel))
p_freq_rel <- c(p_freq_rel, sum(p_freq_rel))

# Tabela final com todos os valores
tabela_final <- cbind(freq_abs, 
                      freq_rel = round(freq_rel, digits = 5), 
                      p_freq_rel = round(p_freq_rel, digits = 2))
View(tabela_final)


#CONSTRUINDO CLASSES DE FREQU?NCIAS
intervalo_classes <- seq(0,120,10)
View(intervalo_classes)
tabela_classes <- table(cut(srag_atual$idade, breaks=intervalo_classes, right=FALSE))
View(tabela_classes)








# GR?FICOS DE FREQU?NCIA

# HISTOGRAMA
hist(srag_atual$idade, col = "red")

df1 <- as.data.frame(tabela_classes)

df1 %>% plot_ly(x = ~Var1, y = ~Freq) %>%
        layout(xaxis = list(title = "Intervalo de idades"),
         yaxis = list(title = "Quantidade")) 

# Pol?gono de frequ?ncia
plot(tabela_classes,type='o')
?plot

# GR?FICO DE OGIVA

# Frequ?ncia Acumulada
freq_rel_classes <- prop.table(table(cut(srag_atual$idade,
                                         breaks = c(intervalo_classes))))
View(freq_rel_classes)
freq_acum <- cumsum(tabela_classes)[seq_along(intervalo_classes)]
View(freq_acum)

# GR?FICO
plot(intervalo_classes, freq_acum, type='o')


# GR?FICO OGIVA NO GGPLOT
df <- as.data.frame(freq_acum)

ggplot(df, aes(x = intervalo_classes, y = freq_acum)) +
  geom_line() +
  geom_point() +
  labs(title = "GR?FICO OGIVA: FREQU?NCIA ACUMULADA POR CLASSES DE IDADE",
       x = "Idade",
       y = "Frequ?ncia Acumulada de SRAG",
       color = "Meses") +
  theme_classic()


