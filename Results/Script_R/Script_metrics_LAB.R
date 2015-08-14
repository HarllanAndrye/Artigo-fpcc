#Obter os dados das métricas dos algoritmos
metrics = read.table("metrics_equip.txt", header = TRUE, sep = ",")
timeAlg = read.table("time_algorithms.txt", header = TRUE, sep = ",")

fridge = subset(metrics, appliance=='Fridge')

plot(fridge$frequency[fridge$algorithms == 'FHMM'], fridge$f.score[fridge$algorithms == 'FHMM'])

#instalando um pacote com paleta de cores
install.packages("RColorBrewer")
require("RColorBrewer")

par(mfrow = c(1,2))
#FHMM - F-Score
barplot(fridge$f.score[fridge$algorithms == 'FHMM'], main="F-Score para geladeira (FHMM)", xlab="Frequência", las=1, 
        col=brewer.pal(6, "Blues"), names.arg = fridge$frequency[fridge$algorithms == 'FHMM'])

#CO - F-Score
barplot(fridge$f.score[fridge$algorithms == 'CO'], main="F-Score para geladeira (CO)", xlab="Frequência", las=1, 
        col=brewer.pal(6, "Blues"), names.arg = fridge$frequency[fridge$algorithms == 'CO'])


#---------------------------------------------------
#Métricas
fhmm = subset(metrics, algorithms=='FHMM')
freq1_fhmm = subset(fhmm, frequency==1)
freq2_fhmm = subset(fhmm, frequency==2)
freq4_fhmm = subset(fhmm, frequency==4)
freq8_fhmm = subset(fhmm, frequency==8)
freq16_fhmm = subset(fhmm, frequency==16)
freq32_fhmm = subset(fhmm, frequency==32)
#Tempo
fhmmTime = subset(timeAlg, algorithms=='FHMM')
freq1_fhmmTime = subset(fhmmTime, frequency==1)
freq2_fhmmTime = subset(fhmmTime, frequency==2)
freq4_fhmmTime = subset(fhmmTime, frequency==4)
freq8_fhmmTime = subset(fhmmTime, frequency==8)
freq16_fhmmTime = subset(fhmmTime, frequency==16)
freq32_fhmmTime = subset(fhmmTime, frequency==32)


df = data.frame(frequency=c('1', '2', '4', '8', '16','32'),
                time.train=c(mean(freq1_fhmmTime$time.train), mean(freq2_fhmmTime$time.train), mean(freq4_fhmmTime$time.train), mean(freq8_fhmmTime$time.train),
                             mean(freq16_fhmmTime$time.train), mean(freq32_fhmmTime$time.train)),
                time.disag=c(mean(freq1_fhmmTime$time.disag), mean(freq2_fhmmTime$time.disag), mean(freq4_fhmmTime$time.disag), mean(freq8_fhmmTime$time.disag),
                             mean(freq16_fhmmTime$time.disag), mean(freq32_fhmmTime$time.disag)),
                f.score=c(mean(freq1_fhmm$f.score), mean(freq2_fhmm$f.score), mean(freq4_fhmm$f.score), mean(freq8_fhmm$f.score),
                          mean(freq16_fhmm$f.score), mean(freq32_fhmm$f.score)),
                mne=c(mean(freq1_fhmm$mne), mean(freq2_fhmm$mne), mean(freq4_fhmm$mne), mean(freq8_fhmm$mne),
                      mean(freq16_fhmm$mne), mean(freq32_fhmm$mne)),
                rms=c(mean(freq1_fhmm$rms), mean(freq2_fhmm$rms), mean(freq4_fhmm$rms), mean(freq8_fhmm$rms),
                      mean(freq16_fhmm$rms), mean(freq32_fhmm$rms))
                )

barplot(df$f.score, main="F-Score (FHMM)", xlab="Frequência", las=1, 
        col=brewer.pal(6, "Blues"), names.arg = df$frequency)


#Métricas
co = subset(metrics, algorithms=='CO')
freq1_co = subset(co, frequency==1)
freq2_co = subset(co, frequency==2)
freq4_co = subset(co, frequency==4)
freq8_co = subset(co, frequency==8)
freq16_co = subset(co, frequency==16)
freq32_co = subset(co, frequency==32)
#Tempo
coTime = subset(timeAlg, algorithms=='CO')
freq1_coTime = subset(coTime, frequency==1)
freq2_coTime = subset(coTime, frequency==2)
freq4_coTime = subset(coTime, frequency==4)
freq8_coTime = subset(coTime, frequency==8)
freq16_coTime = subset(coTime, frequency==16)
freq32_coTime = subset(coTime, frequency==32)

df_ = data.frame(frequency=c('1', '2', '4', '8', '16','32'),
                 time.train=c(mean(freq1_coTime$time.train), mean(freq2_coTime$time.train), mean(freq4_coTime$time.train), mean(freq8_coTime$time.train),
                              mean(freq16_coTime$time.train), mean(freq32_coTime$time.train)),
                 time.disag=c(mean(freq1_coTime$time.disag), mean(freq2_coTime$time.disag), mean(freq4_coTime$time.disag), mean(freq8_coTime$time.disag),
                              mean(freq16_coTime$time.disag), mean(freq32_coTime$time.disag)),
                 f.score=c(mean(freq1_co$f.score), mean(freq2_co$f.score), mean(freq4_co$f.score), mean(freq8_co$f.score),
                              mean(freq16_co$f.score), mean(freq32_co$f.score)),
                 mne=c(mean(freq1_co$mne), mean(freq2_co$mne), mean(freq4_co$mne), mean(freq8_co$mne),
                              mean(freq16_co$mne), mean(freq32_co$mne)),
                 rms=c(mean(freq1_co$rms), mean(freq2_co$rms), mean(freq4_co$rms), mean(freq8_co$rms),
                              mean(freq16_co$rms), mean(freq32_co$rms))
)

summary(freq1_co$f.score)
summary(freq2_co$f.score)
summary(freq4_co$f.score)
summary(freq8_co$f.score)
summary(freq16_co$f.score)
summary(freq32_co$f.score)

barplot(df_$f.score, main="F-Score (CO)", xlab="Frequência", las=1, 
        col=brewer.pal(6, "Blues"), names.arg = df_$frequency)





