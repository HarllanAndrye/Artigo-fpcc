#Instalando pacotes
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("moments")
#install.packages("nortest")
#install.packages("lawstat")

#Pacotes necessários
#Gráficos
library(ggplot2)
library(gridExtra)
#Métricas
require("nortest")
library("moments")
#Homoscedasticidade
#require(lawstat)

#Função para gerar gráfico qqplot
qqplotChart <- function(nome, dados){
  qqnorm(dados, main = nome, xlab = "Quantis teoricos", ylab = "Quantis observados", las=1)
  qqline(dados, lty = 2, col = "red", lwd=2)
  #sub="subtítulo no gráfico"
}

#FDP com linha normal (red line)
normalidade.fdp <- function(data, title) {
  title=paste("", title, sep="")
  plot(density(data), main=title, ylab="Densidade", las=1)
  curve(dnorm(x, mean=mean(data), sd=sd(data)), add=TRUE, col="red")
}



