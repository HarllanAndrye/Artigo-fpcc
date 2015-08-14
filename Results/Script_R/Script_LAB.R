#Obter os dados dos algoritmos
timeAlg = read.table("time_algorithms.txt", header = TRUE, sep = ",")

#Funcoes necessarias
source("Functions.R")

#Separando por frequencia
freq1 = subset(timeAlg, frequency==1)
freq2 = subset(timeAlg, frequency==2)
freq4 = subset(timeAlg, frequency==4)
freq8 = subset(timeAlg, frequency==8)
freq16 = subset(timeAlg, frequency==16)
freq32 = subset(timeAlg, frequency==32)

#--------------------------------------
#Analise para frequencia de 1 segundo
#--------------------------------------
par(mfrow = c(2,2))
#QQPlot FHMM
qqplotChart("Frequencia 1 seg\n(Treinamento - FHMM)", freq1$time.train[freq1$algorithms == 'FHMM'])
qqplotChart("Frequencia 1 seg\n(Desagregacao - FHMM)", freq1$time.disag[freq1$algorithms == 'FHMM'])
#QQPlot CO
qqplotChart("Frequencia 1 seg\n(Treinamento - CO)", freq1$time.train[freq1$algorithms == 'CO'])
qqplotChart("Frequencia 1 seg\n(Desagregacao - CO)", freq1$time.disag[freq1$algorithms == 'CO'])

par(mfrow = c(1,2)) 
#Boxplot FHMM e CO
boxplot(freq1$time.train ~ freq1$algorithms, ylab = "Tempo (s)", xlab="Algoritmos", main = "Frequência 1 seg\n(Treinamento)", las=1)
boxplot(freq1$time.disag ~ freq1$algorithms, ylab = "Tempo (s)", xlab="Algoritmos", main = "Frequência 1 seg\n(Desagregação)", las=1)

summary(freq1$time.train[freq1$algorithms == 'FHMM'])
summary(freq1$time.disag[freq1$algorithms == 'FHMM'])
summary(freq1$time.train[freq1$algorithms == 'CO'])
summary(freq1$time.disag[freq1$algorithms == 'CO'])

#Funcao densidade de probabilidade (FDP)
par(mfrow = c(2,2))
normalidade.fdp(freq1$time.train[freq1$algorithms == 'FHMM'], "Frequencia 1 seg\n(Treinamento - FHMM)")
normalidade.fdp(freq1$time.disag[freq1$algorithms == 'FHMM'], "Frequencia 1 seg\n(Desagregacao - FHMM)")
normalidade.fdp(freq1$time.train[freq1$algorithms == 'CO'], "Frequencia 1 seg\n(Treinamento - CO)")
normalidade.fdp(freq1$time.disag[freq1$algorithms == 'CO'], "Frequencia 1 seg\n(Desagregacao - CO)")

#Metricas
#Skewness=0 e Kurtosis=3 (ideal)
#FHMM
skewness(freq1$time.train[freq1$algorithms == "FHMM"]) #0.8306298
skewness(freq1$time.disag[freq1$algorithms == "FHMM"]) #0.6688054
kurtosis(freq1$time.train[freq1$algorithms == "FHMM"]) #3.452402
kurtosis(freq1$time.disag[freq1$algorithms == "FHMM"]) #2.033802
#CO
skewness(freq1$time.train[freq1$algorithms == "CO"]) #0.7332864
skewness(freq1$time.disag[freq1$algorithms == "CO"]) #3.752366
kurtosis(freq1$time.train[freq1$algorithms == "CO"]) #4.296233
kurtosis(freq1$time.disag[freq1$algorithms == "CO"]) #18.19405
#Conclui-se que não há simetria nos dados.

#Testes
#teste de normalidade shapiro-wilk
#Considera-se (assume) uma normal de p-value >= 0.05(empirico) (5%)
#FHMM
shapiro.test(freq1$time.train[freq1$algorithms == "FHMM"]) #p-value = 0.1016
shapiro.test(freq1$time.disag[freq1$algorithms == "FHMM"]) #p-value = 0.001575
#CO
shapiro.test(freq1$time.train[freq1$algorithms == "CO"]) #p-value = 0.105
shapiro.test(freq1$time.disag[freq1$algorithms == "CO"]) #p-value = 2.575e-08

#teste de normalidade Anderson Darling
#Considera-se (assume) uma normal de p-value >= 0.05(empirico) (5%)
#FHMM
ad.test(freq1$time.train[freq1$algorithms == "FHMM"]) #p-value = 0.07702
ad.test(freq1$time.disag[freq1$algorithms == "FHMM"]) #p-value = 0.0008067
#CO
ad.test(freq1$time.train[freq1$algorithms == "CO"]) #p-value = 0.05895
ad.test(freq1$time.disag[freq1$algorithms == "CO"]) #p-value = 5.897e-10

#NAO PRECISA VERIFICAR HOMOGENEIDADE, POIS ALGUNS DADOS NAO SAO NORMAIS E SERA UTILIZADO TESTE NAO-PARAMETRICO

#Teste de hipotese para frequencia de 1 segundo.
#H1-0: O tempo medio para treinamento e igual para os dois algoritmos.
#H2-0: O tempo medio para desagregacao e igual para os dois algoritmos.

#Obs.: p-Value > 0.05, não rejeita a hipótese nula, ou seja, aceita H0.

#Não-paramétrico (Não segue uma distribuição normal)
wilcox.test(freq1$time.train[freq1$algorithms == "FHMM"], freq1$time.train[freq1$algorithms == "CO"], paired = T)
#V=465, p-value = 1.863e-09, rejeita H1-0
wilcox.test(freq1$time.disag[freq1$algorithms == "FHMM"], freq1$time.disag[freq1$algorithms == "CO"], paired = T)
#V=0, p-value = 1.863e-09, rejeita H2-0

#Resultado: Como o p-Value de ambos foi menor que 5% (nível de significância), rejeitamos a hipótese nula e concluimos que
#há diferença no tempo médio entre os algoritmos no treinamento e na desagregação.


#--------------------------------------
#Análise para frequência de 2 segundos
#--------------------------------------
par(mfrow = c(3,2))
#QQPlot FHMM
qqplotChart("Frequencia 2 seg\n(Treinamento - FHMM)", freq2$time.train[freq2$algorithms == 'FHMM'])
qqplotChart("Frequencia 2 seg\n(Desagregacao - FHMM)", freq2$time.disag[freq2$algorithms == 'FHMM'])
#QQPlot CO
qqplotChart("Frequencia 2 seg\n(Treinamento - CO)", freq2$time.train[freq2$algorithms == 'CO'])
qqplotChart("Frequencia 2 seg\n(Desagregacao - CO)", freq2$time.disag[freq2$algorithms == 'CO'])

par(mfrow = c(1,2)) 
#Boxplot FHMM e CO
boxplot(freq2$time.train ~ freq2$algorithms, ylab = "Tempo (s)", xlab="Algoritmos", main = "Frequência 2 seg\n(Treinamento)", las=1)
boxplot(freq2$time.disag ~ freq2$algorithms, ylab = "Tempo (s)", xlab="Algoritmos", main = "Frequência 2 seg\n(Desagregação)", las=1)

summary(freq2$time.train[freq2$algorithms == 'FHMM'])
summary(freq2$time.disag[freq2$algorithms == 'FHMM'])
summary(freq2$time.train[freq2$algorithms == 'CO'])
summary(freq2$time.disag[freq2$algorithms == 'CO'])

#Função densidade de probabilidade (FDP)
par(mfrow = c(2,2))
normalidade.fdp(freq2$time.train[freq2$algorithms == 'FHMM'], "Frequencia 2 seg\n(Treinamento - FHMM)")
normalidade.fdp(freq2$time.disag[freq2$algorithms == 'FHMM'], "Frequencia 2 seg\n(Desagregacao - FHMM)")
normalidade.fdp(freq2$time.train[freq2$algorithms == 'CO'], "Frequencia 2 seg\n(Treinamento - CO)")
normalidade.fdp(freq2$time.disag[freq2$algorithms == 'CO'], "Frequencia 2 seg\n(Desagregacao - CO)")

#Métricas
#Skewness=0 e Kurtosis=3 (ideal)
#FHMM
skewness(freq2$time.train[freq2$algorithms == "FHMM"]) #0.3599759
skewness(freq2$time.disag[freq2$algorithms == "FHMM"]) #0.5433195
kurtosis(freq2$time.train[freq2$algorithms == "FHMM"]) #2.559241
kurtosis(freq2$time.disag[freq2$algorithms == "FHMM"]) #2.569262
#CO
skewness(freq2$time.train[freq2$algorithms == "CO"]) #2.409773
skewness(freq2$time.disag[freq2$algorithms == "CO"]) #1.525661
kurtosis(freq2$time.train[freq2$algorithms == "CO"]) #11.00197
kurtosis(freq2$time.disag[freq2$algorithms == "CO"]) #5.095965
#Concluí-se que não há simetria nos dados.

#Testes
#teste de normalidade shapiro-wilk
#Considera-se (assume) uma normal de p-value >= 0.05(empirico) (5%)
#FHMM
shapiro.test(freq2$time.train[freq2$algorithms == "FHMM"]) #p-value = 0.6859
shapiro.test(freq2$time.disag[freq2$algorithms == "FHMM"]) #p-value = 0.0552
#CO
shapiro.test(freq2$time.train[freq2$algorithms == "CO"]) #p-value = 2.566e-05
shapiro.test(freq2$time.disag[freq2$algorithms == "CO"]) #p-value = 0.0003321

#teste de normalidade Anderson Darling
#Considera-se (assume) uma normal de p-value >= 0.05(empirico) (5%)
#FHMM
ad.test(freq2$time.train[freq2$algorithms == "FHMM"]) #p-value = 0.5213
ad.test(freq2$time.disag[freq2$algorithms == "FHMM"]) #p-value = 0.0752
#CO
ad.test(freq2$time.train[freq2$algorithms == "CO"]) #p-value = 0.0004762
ad.test(freq2$time.disag[freq2$algorithms == "CO"]) #p-value = 0.0004655

#Teste de hipótese para frequência de 2 segundos.
#H1-0: O tempo médio para treinamento é igual para os dois algoritmos.
#H2-0: O tempo médio para desagregação é igual para os dois algoritmos.

#Obs.: p-Value > 0.05, não rejeita a hipótese nula, ou seja, aceita H0.

#Não-paramétrico (Não segue uma distribuição normal)
wilcox.test(freq2$time.train[freq2$algorithms == "FHMM"], freq2$time.train[freq2$algorithms == "CO"], paired = T)
#V=465, p-value = 1.863e-09, rejeita H1-0. (<5%)
wilcox.test(freq2$time.disag[freq2$algorithms == "FHMM"], freq2$time.disag[freq2$algorithms == "CO"], paired = T)
#V=0, p-value = 1.863e-09, rejeita H2-0. (<5%)


#--------------------------------------
#Análise para frequência de 4 segundos
#--------------------------------------
par(mfrow = c(3,2))
#QQPlot FHMM
qqplotChart("Frequencia 4 seg\n(Treinamento - FHMM)", freq4$time.train[freq4$algorithms == 'FHMM'])
qqplotChart("Frequencia 4 seg\n(Desagregacao - FHMM)", freq4$time.disag[freq4$algorithms == 'FHMM'])
#QQPlot CO
qqplotChart("Frequencia 4 seg\n(Treinamento - CO)", freq4$time.train[freq4$algorithms == 'CO'])
qqplotChart("Frequencia 4 seg\n(Desagregacao - CO)", freq4$time.disag[freq4$algorithms == 'CO'])

#Boxplot FHMM e CO
boxplot(freq4$time.train ~ freq4$algorithms, ylab = "Tempo (s)", xlab="Algoritmos", main = "Frequência 4 seg\n(Treinamento)", las=1)
boxplot(freq4$time.disag ~ freq4$algorithms, ylab = "Tempo (s)", xlab="Algoritmos", main = "Frequência 4 seg\n(Desagregação)", las=1)

summary(freq4$time.train[freq4$algorithms == 'FHMM'])
summary(freq4$time.disag[freq4$algorithms == 'FHMM'])
summary(freq4$time.train[freq4$algorithms == 'CO'])
summary(freq4$time.disag[freq4$algorithms == 'CO'])

#Função densidade de probabilidade (FDP)
par(mfrow = c(2,2))
normalidade.fdp(freq4$time.train[freq4$algorithms == 'FHMM'], "Frequencia 4 seg\n(Treinamento - FHMM)")
normalidade.fdp(freq4$time.disag[freq4$algorithms == 'FHMM'], "Frequencia 4 seg\n(Desagregacao - FHMM)")
normalidade.fdp(freq4$time.train[freq4$algorithms == 'CO'], "Frequencia 4 seg\n(Treinamento - CO)")
normalidade.fdp(freq4$time.disag[freq4$algorithms == 'CO'], "Frequencia 4 seg\n(Desagregacao - CO)")

#Métricas
#Skewness=0 e Kurtosis=3 (ideal)
#FHMM
skewness(freq4$time.train[freq4$algorithms == "FHMM"]) #0.06798352
skewness(freq4$time.disag[freq4$algorithms == "FHMM"]) #2.293204
kurtosis(freq4$time.train[freq4$algorithms == "FHMM"]) #2.49684
kurtosis(freq4$time.disag[freq4$algorithms == "FHMM"]) #7.931378
#CO
skewness(freq4$time.train[freq4$algorithms == "CO"]) #0.2301699
skewness(freq4$time.disag[freq4$algorithms == "CO"]) #0.6854648
kurtosis(freq4$time.train[freq4$algorithms == "CO"]) #1.798825
kurtosis(freq4$time.disag[freq4$algorithms == "CO"]) #2.475241
#Concluí-se que não há simetria nos dados.

#Testes
#teste de normalidade shapiro-wilk
#Considera-se (assume) uma normal de p-value >= 0.05(emp??rico) (5%)
#FHMM
shapiro.test(freq4$time.train[freq4$algorithms == "FHMM"]) #p-value = 0.7463
shapiro.test(freq4$time.disag[freq4$algorithms == "FHMM"]) #p-value = 2.002e-06
#CO
shapiro.test(freq4$time.train[freq4$algorithms == "CO"]) #p-value = 0.1011
shapiro.test(freq4$time.disag[freq4$algorithms == "CO"]) #p-value = 0.01458

#teste de normalidade Anderson Darling
#Considera-se (assume) uma normal de p-value >= 0.05(emp??rico) (5%)
#FHMM
ad.test(freq4$time.train[freq4$algorithms == "FHMM"]) #p-value = 0.5967
ad.test(freq4$time.disag[freq4$algorithms == "FHMM"]) #p-value = 1.558e-07
#CO
ad.test(freq4$time.train[freq4$algorithms == "CO"]) #p-value = 0.207
ad.test(freq4$time.disag[freq4$algorithms == "CO"]) #p-value = 0.02652

#Teste de hipótese para frequência de 4 segundos.
#H1-0: O tempo médio para treinamento é igual para os dois algoritmos.
#H2-0: O tempo médio para desagregação é igual para os dois algoritmos.

#Obs.: p-Value > 0.05, não rejeita a hipótese nula, ou seja, aceita H0.

#Não-paramétrico (Não segue uma distribuição normal)
wilcox.test(freq4$time.train[freq4$algorithms == "FHMM"], freq4$time.train[freq4$algorithms == "CO"], paired = T)
#V=465, p-value = 1.863e-09, rejeita H1-0. (<5%)
wilcox.test(freq4$time.disag[freq4$algorithms == "FHMM"], freq4$time.disag[freq4$algorithms == "CO"], paired = T)
#V=0, p-value = 1.863e-09, rejeita H2-0. (<5%)


#--------------------------------------
#Analise para frequencia de 8 segundos
#--------------------------------------
par(mfrow = c(3,2))
#QQPlot FHMM
qqplotChart("Frequencia 8 seg\n(Treinamento - FHMM)", freq8$time.train[freq8$algorithms == 'FHMM'])
qqplotChart("Frequencia 8 seg\n(Desagregacao - FHMM)", freq8$time.disag[freq8$algorithms == 'FHMM'])
#QQPlot CO
qqplotChart("Frequencia 8 seg\n(Treinamento - CO)", freq8$time.train[freq8$algorithms == 'CO'])
qqplotChart("Frequencia 8 seg\n(Desagregacao - CO)", freq8$time.disag[freq8$algorithms == 'CO'])

#Boxplot FHMM e CO
boxplot(freq8$time.train ~ freq8$algorithms, ylab = "Tempo (s)", xlab="Algoritmos", main = "Frequência 8 seg\n(Treinamento)", las=1)
boxplot(freq8$time.disag ~ freq8$algorithms, ylab = "Tempo (s)", xlab="Algoritmos", main = "Frequência 8 seg\n(Desagregação)", las=1)

summary(freq8$time.train[freq8$algorithms == 'FHMM'])
summary(freq8$time.disag[freq8$algorithms == 'FHMM'])
summary(freq8$time.train[freq8$algorithms == 'CO']) 
summary(freq8$time.disag[freq8$algorithms == 'CO'])

#Funcao densidade de probabilidade (FDP)
par(mfrow = c(2,2))
normalidade.fdp(freq8$time.train[freq8$algorithms == 'FHMM'], "Frequencia 8 seg\n(Treinamento - FHMM)")
normalidade.fdp(freq8$time.disag[freq8$algorithms == 'FHMM'], "Frequencia 8 seg\n(Desagregacao - FHMM)")
normalidade.fdp(freq8$time.train[freq8$algorithms == 'CO'], "Frequencia 8 seg\n(Treinamento - CO)")
normalidade.fdp(freq8$time.disag[freq8$algorithms == 'CO'], "Frequencia 8 seg\n(Desagregacao - CO)")

#Metricas
#Skewness=0 e Kurtosis=3 (ideal)
#FHMM
skewness(freq8$time.train[freq8$algorithms == "FHMM"]) #0.5781474
skewness(freq8$time.disag[freq8$algorithms == "FHMM"]) #0.8192514
kurtosis(freq8$time.train[freq8$algorithms == "FHMM"]) #2.613398
kurtosis(freq8$time.disag[freq8$algorithms == "FHMM"]) #3.0095
#CO
skewness(freq8$time.train[freq8$algorithms == "CO"]) #0.5130643
skewness(freq8$time.disag[freq8$algorithms == "CO"]) #1.194784
kurtosis(freq8$time.train[freq8$algorithms == "CO"]) #3.520688
kurtosis(freq8$time.disag[freq8$algorithms == "CO"]) #2.841933
#Conclui-se que nao ha simetria nos dados.

#Testes
#teste de normalidade shapiro-wilk
#Considera-se (assume) uma normal de p-value >= 0.05(empirico) (5%)
#FHMM
shapiro.test(freq8$time.train[freq8$algorithms == "FHMM"]) #p-value = 0.1171
shapiro.test(freq8$time.disag[freq8$algorithms == "FHMM"]) #p-value = 0.02127
#CO
shapiro.test(freq8$time.train[freq8$algorithms == "CO"]) #p-value = 0.4369
shapiro.test(freq8$time.disag[freq8$algorithms == "CO"]) #p-value = 1.154e-05

#teste de normalidade Anderson Darling
#Considera-se (assume) uma normal de p-value >= 0.05(empirico) (5%)
#FHMM
ad.test(freq8$time.train[freq8$algorithms == "FHMM"]) #p-value = 0.08648
ad.test(freq8$time.disag[freq8$algorithms == "FHMM"]) #p-value = 0.03428
#CO
ad.test(freq8$time.train[freq8$algorithms == "CO"]) #p-value = 0.6873
ad.test(freq8$time.disag[freq8$algorithms == "CO"]) #p-value = 4.52e-08

#Teste de hipotese para frequencia de 8 segundos.
#H1-0: O tempo medio para treinamento eh igual para os dois algoritmos.
#H2-0: O tempo medio para desagregacao eh igual para os dois algoritmos.

#Obs.: p-Value > 0.05, nao rejeita a hipotese nula, ou seja, aceita H0.

#Nao-parametrico (Nao segue uma distribuicao normal)
wilcox.test(freq8$time.train[freq8$algorithms == "FHMM"], freq8$time.train[freq8$algorithms == "CO"], paired = T)
#V=465, p-value = 1.863e-09, rejeita H1-0. (<5%)
wilcox.test(freq8$time.disag[freq8$algorithms == "FHMM"], freq8$time.disag[freq8$algorithms == "CO"], paired = T)
#V=0, p-value = 1.863e-09, rejeita H2-0. (<5%)


#--------------------------------------
#Analise para frequencia de 16 segundos
#--------------------------------------
par(mfrow = c(3,2))
#QQPlot FHMM
qqplotChart("Frequencia 16 seg\n(Treinamento - FHMM)", freq16$time.train[freq16$algorithms == 'FHMM'])
qqplotChart("Frequencia 16 seg\n(Desagregacao - FHMM)", freq16$time.disag[freq16$algorithms == 'FHMM'])
#QQPlot CO
qqplotChart("Frequencia 16 seg\n(Treinamento - CO)", freq16$time.train[freq16$algorithms == 'CO'])
qqplotChart("Frequencia 16 seg\n(Desagregacao - CO)", freq16$time.disag[freq16$algorithms == 'CO'])

#Boxplot FHMM e CO
boxplot(freq16$time.train ~ freq16$algorithms, ylab = "Tempo (s)", xlab="Algoritmos", main = "Frequência 16 seg\n(Treinamento)", las=1)
boxplot(freq16$time.disag ~ freq16$algorithms, ylab = "Tempo (s)", xlab="Algoritmos", main = "Frequência 16 seg\n(Desagregação)", las=1)

summary(freq16$time.train[freq16$algorithms == 'FHMM'])
summary(freq16$time.disag[freq16$algorithms == 'FHMM'])
summary(freq16$time.train[freq16$algorithms == 'CO']) 
summary(freq16$time.disag[freq16$algorithms == 'CO'])

#Funcao densidade de probabilidade (FDP)
par(mfrow = c(2,2))
normalidade.fdp(freq16$time.train[freq16$algorithms == 'FHMM'], "Frequencia 16 seg\n(Treinamento - FHMM)")
normalidade.fdp(freq16$time.disag[freq16$algorithms == 'FHMM'], "Frequencia 16 seg\n(Desagregacao - FHMM)")
normalidade.fdp(freq16$time.train[freq16$algorithms == 'CO'], "Frequencia 16 seg\n(Treinamento - CO)")
normalidade.fdp(freq16$time.disag[freq16$algorithms == 'CO'], "Frequencia 16 seg\n(Desagregacao - CO)")

#Metricas
#Skewness=0 e Kurtosis=3 (ideal)
#FHMM
skewness(freq16$time.train[freq16$algorithms == "FHMM"]) #0.2242617
skewness(freq16$time.disag[freq16$algorithms == "FHMM"]) #1.377532
kurtosis(freq16$time.train[freq16$algorithms == "FHMM"]) #3.005805
kurtosis(freq16$time.disag[freq16$algorithms == "FHMM"]) #5.211032
#CO
skewness(freq16$time.train[freq16$algorithms == "CO"]) #1.032121
skewness(freq16$time.disag[freq16$algorithms == "CO"]) #2.028411
kurtosis(freq16$time.train[freq16$algorithms == "CO"]) #3.287759
kurtosis(freq16$time.disag[freq16$algorithms == "CO"]) #7.923145
#Conclui-se que nao ha simetria nos dados.

#Testes
#teste de normalidade shapiro-wilk
#Considera-se (assume) uma normal de p-value >= 0.05(empirico) (5%)
#FHMM
shapiro.test(freq16$time.train[freq16$algorithms == "FHMM"]) #p-value = 0.9753
shapiro.test(freq16$time.disag[freq16$algorithms == "FHMM"]) #p-value = 0.003354
#CO
shapiro.test(freq16$time.train[freq16$algorithms == "CO"]) #p-value = 0.006578
shapiro.test(freq16$time.disag[freq16$algorithms == "CO"]) #p-value = 7.889e-05

#teste de normalidade Anderson Darling
#Considera-se (assume) uma normal de p-value >= 0.05(empirico) (5%)
#FHMM
ad.test(freq16$time.train[freq16$algorithms == "FHMM"]) #p-value = 0.8841
ad.test(freq16$time.disag[freq16$algorithms == "FHMM"]) #p-value = 0.02244
#CO
ad.test(freq16$time.train[freq16$algorithms == "CO"]) #p-value = 0.004381
ad.test(freq16$time.disag[freq16$algorithms == "CO"]) #p-value = 0.0004852

#Teste de hipotese para frequencia de 4 segundos.
#H1-0: O tempo medio para treinamento eh igual para os dois algoritmos.
#H2-0: O tempo medio para desagregacao eh igual para os dois algoritmos.

#Obs.: p-Value > 0.05, nao rejeita a hipotese nula, ou seja, aceita H0.

#Nao-parametrico (Nao segue uma distribuicao normal)
wilcox.test(freq16$time.train[freq16$algorithms == "FHMM"], freq16$time.train[freq16$algorithms == "CO"], paired = T)
#V=465, p-value = 1.863e-09, rejeita H1-0. (<5%)
wilcox.test(freq16$time.disag[freq16$algorithms == "FHMM"], freq16$time.disag[freq16$algorithms == "CO"], paired = T)
#V=0, p-value = 1.863e-09, rejeita H2-0. (<5%)


#--------------------------------------
#Analise para frequencia de 32 segundos
#--------------------------------------
par(mfrow = c(3,2))
#QQPlot FHMM
qqplotChart("Frequencia 32 seg\n(Treinamento - FHMM)", freq32$time.train[freq32$algorithms == 'FHMM'])
qqplotChart("Frequencia 32 seg\n(Desagregacao - FHMM)", freq32$time.disag[freq32$algorithms == 'FHMM'])
#QQPlot CO
qqplotChart("Frequencia 32 seg\n(Treinamento - CO)", freq32$time.train[freq32$algorithms == 'CO'])
qqplotChart("Frequencia 32 seg\n(Desagregacao - CO)", freq32$time.disag[freq32$algorithms == 'CO'])

#Boxplot FHMM e CO
boxplot(freq32$time.train ~ freq32$algorithms, ylab = "Tempo (s)", xlab="Algoritmos", main = "Frequência 32 seg\n(Treinamento)", las=1)
boxplot(freq32$time.disag ~ freq32$algorithms, ylab = "Tempo (s)", xlab="Algoritmos", main = "Frequência 32 seg\n(Desagregação)", las=1)

summary(freq32$time.train[freq32$algorithms == 'FHMM'])
summary(freq32$time.disag[freq32$algorithms == 'FHMM'])
summary(freq32$time.train[freq32$algorithms == 'CO']) 
summary(freq32$time.disag[freq32$algorithms == 'CO'])

#Funcao densidade de probabilidade (FDP)
par(mfrow = c(2,2))
normalidade.fdp(freq32$time.train[freq32$algorithms == 'FHMM'], "Frequencia 32 seg\n(Treinamento - FHMM)")
normalidade.fdp(freq32$time.disag[freq32$algorithms == 'FHMM'], "Frequencia 32 seg\n(Desagregacao - FHMM)")
normalidade.fdp(freq32$time.train[freq32$algorithms == 'CO'], "Frequencia 32 seg\n(Treinamento - CO)")
normalidade.fdp(freq32$time.disag[freq32$algorithms == 'CO'], "Frequencia 32 seg\n(Desagregacao - CO)")

#Metricas
#Skewness=0 e Kurtosis=3 (ideal)
#FHMM
skewness(freq32$time.train[freq32$algorithms == "FHMM"]) #0.8382197
skewness(freq32$time.disag[freq32$algorithms == "FHMM"]) #0.4609451
kurtosis(freq32$time.train[freq32$algorithms == "FHMM"]) #3.433281
kurtosis(freq32$time.disag[freq32$algorithms == "FHMM"]) #2.559926
#CO
skewness(freq32$time.train[freq32$algorithms == "CO"]) #1.484006
skewness(freq32$time.disag[freq32$algorithms == "CO"]) #0.2959036
kurtosis(freq32$time.train[freq32$algorithms == "CO"]) #4.635219
kurtosis(freq32$time.disag[freq32$algorithms == "CO"]) #2.790799
#Conclui-se que não há simetria nos dados.

#Testes
#teste de normalidade shapiro-wilk
#Considera-se (assume) uma normal de p-value >= 0.05(empirico) (5%)
#FHMM
shapiro.test(freq32$time.train[freq32$algorithms == "FHMM"]) #p-value = 0.09551
shapiro.test(freq32$time.disag[freq32$algorithms == "FHMM"]) #p-value = 0.5554
#CO
shapiro.test(freq32$time.train[freq32$algorithms == "CO"]) #p-value = 0.0001895
shapiro.test(freq32$time.disag[freq32$algorithms == "CO"]) #p-value = 0.4588

#teste de normalidade Anderson Darling
#Considera-se (assume) uma normal de p-value >= 0.05(empirico) (5%)
#FHMM
ad.test(freq32$time.train[freq32$algorithms == "FHMM"]) #p-value = 0.1467
ad.test(freq32$time.disag[freq32$algorithms == "FHMM"]) #p-value = 0.5962
#CO
ad.test(freq32$time.train[freq32$algorithms == "CO"]) #p-value = 0.0001171
ad.test(freq32$time.disag[freq32$algorithms == "CO"]) #p-value = 0.3256

#Teste de hipotese para frequencia de 4 segundos.
#H1-0: O tempo medio para treinamento eh igual para os dois algoritmos.
#H2-0: O tempo medio para desagregacao eh igual para os dois algoritmos.

#Obs.: p-Value > 0.05, não rejeita a hipótese nula, ou seja, aceita H0.

#Nao-parametrico (Nao segue uma distribuicao normal)
wilcox.test(freq32$time.train[freq32$algorithms == "FHMM"], freq32$time.train[freq32$algorithms == "CO"], paired = T)
#V=465, p-value = 1.863e-09, rejeita H1-0. (<5%)
wilcox.test(freq32$time.disag[freq32$algorithms == "FHMM"], freq32$time.disag[freq32$algorithms == "CO"], paired = T)
#V=0, p-value = 1.863e-09, rejeita H2-0. (<5%)





