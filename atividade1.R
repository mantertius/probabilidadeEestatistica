library(readxl) 
dados <- read_excel("C:/Users/Manoel/Downloads/ENEM_AL_EXCEL_AJUS_OKSNZ.xlsx") 

#QUESTAO 1 - LETRA A
quartis.cut <- cut(dados$NOTA_ENEN, breaks=quantile(dados$NOTA_ENEN),include.lowest = TRUE) #pega os quatro
summary(quantile(dados$NOTA_ENEN))
boxplot(dados$NOTA_ENEN ~ quartis.cut, 
        data = dados, 
        xlab="Quartis", 
        ylab="Notas", 
        main="Notas por Quartil", 
        col="orange",
        border="brown")##https://www.datamentor.io/r-programming/box-plot/


summary(quartis.cut)
classes_label <- c("[318.44,455.20]", "(455.20,497.22]", "(497.22,553.04]", "(553.04,796.14]")

Freq = table(quartis.cut)
FreqAc = cumsum(Freq)
FreqRel = prop.table(Freq)
FreqRelAc = cumsum(FreqRel)

TabResult = cbind(Freq,FreqAc,FreqRel=round(FreqRel*100,digits=2),FreqRelAc=round(FreqRelAc*100,digits=2))
TabResult

#QUESTAO 1 - LETRA B
histograma = hist(dados$NOTA_ENEN, breaks=quantile(dados$NOTA_ENEN),
                  ylab="Frequencias Absolutas",
                  xlab="Notas",
                  main = "Histograma 1",
                  col="burlywood1",)
lines(c(min(histograma$breaks), histograma$mids, max(histograma$breaks)), 
      c(0,histograma$counts, 0), type = "l")

histograma_correto = hist(dados$NOTA_ENEN,
                  ylab="Frequencias Absolutas",
                  xlab="Notas",
                  main = "Histograma 2",
                  col="burlywood1",)                  
lines(c(min(histograma_correto$breaks), histograma_correto$mids, max(histograma_correto$breaks)), 
      c(0,histograma_correto$counts, 0), type = "l")
df = data.frame(dados$NOTA_ENEN,dados$TP_SEXO)
df$Quartile <- cut(df$dados.NOTA_ENEN,quantile(df$dados.NOTA_ENEN),include.lowest = TRUE,labels = FALSE)

tb2 = table(quartis.cut,df$dados.NOTA_ENEN)

barplot(tb2, col=c("red","orange","yellow","green"), main= "Notas",
        xlab = "Quantiles", ylab = "Frequência de Notas",
        beside = TRUE, legend = TRUE) #quantas vezes cada nota apareceu

library(tidyverse)
library(scales)

#QUESTAO 1 - LETRA C 
df2 = data.frame(dados$NOTA_ENEN,dados$TP_SEXO,df$Quartile)
plotzao = ggplot(data=df2, aes(x=df$Quartile,y=dados.NOTA_ENEN,fill=dados$TP_SEXO)) + 
  geom_bar(stat='identity')+
  ggtitle("Notas por Quartil")+
  xlab("Quartil")+
  ylab("Nota do ENEM")+
  scale_fill_discrete(name="Sexo")
print(plotzao)


#QUESTAO 1 - LETRA D 
df4 = data.frame(dados$TP_COR_RACA,dados$SG_UF_NASCIMENTO)
tb4 = table(df4)
barplot(tb4,legend=TRUE)

#QUESTAO 1 - LETRA E
df3 = data.frame(dados$NOTA_ENEN,dados$NU_IDADE)
notaIdade= ggplot(df3,aes(dados$NOTA_ENEN,dados$NU_IDADE))+
  geom_jitter(height=2,width=2,color="darkseagreen")+
  geom_density2d(color="black")+
  scale_fill_brewer(palette="Blues")+
  ggtitle("Distribuição de Notas do ENEM por Idade")+
  xlab("Nota do ENEM")+
  ylab("Idade")
print(notaIdade)

#QUESTAO 1 - LETRA F
tenCut <- cut(dados$NOTA_ENEN, breaks=10,include.lowest = TRUE) 
boxplot(dados$NOTA_ENEN ~ tenCut, 
        data = dados, 
        xlab="Classes", 
        ylab="Notas", 
        main="Notas por Classe", 
        col="orange",
        border="brown")

Freq = table(tenCut)
FreqAc = cumsum(Freq)
FreqRel = prop.table(Freq)
FreqRelAc = cumsum(FreqRel)

TabResult = cbind(Freq,FreqAc,FreqRel=round(FreqRel*100,digits=2),FreqRelAc=round(FreqRelAc*100,digits=2))
TabResult

histograma_correto2 = hist(dados$NOTA_ENEN,
                          ylab="Frequencias Absolutas",
                          xlab="Notas",
                          main = "Histograma 3",
                          col="burlywood1",
                          breaks=10)                  
lines(c(min(histograma_correto$breaks), histograma_correto$mids, max(histograma_correto$breaks)), 
      c(0,histograma_correto$counts, 0), type = "l")


#QUESTAO 2
penedo = dados[dados$NO_MUNICIPIO_PROVA == 'Penedo',]
View(penedo)
histograma_correto2 = hist(penedo$NOTA_ENEN,
                           ylab="Frequencias Absolutas",
                           xlab="Notas",
                           main = "Histograma Penedo",
                           col="#105000",) 

dfP = data.frame(penedo$NOTA_ENEN,penedo$NO_MUNICIPIO_RESIDENCIA)
notaMunicipio.penedo = ggplot(dfP,aes(penedo$NOTA_ENEN,penedo$NO_MUNICIPIO_RESIDENCIA), fill=penedo$NO_MUNICIPIO_RESIDENCIA)+
  geom_boxplot(alpha=0.8)+
  ggtitle("Distribuição de Notas do ENEM em Penedo por Município de Residência")+
  ylab("Cidade")+
  xlab("Nota do Enem")+
  theme(legend.position="none")+
  scale_fill_brewer(palette="BuPu")
print(notaMunicipio.penedo)

