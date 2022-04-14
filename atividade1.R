library(readxl) 
dados <- read_excel("C:/Users/Manoel/Downloads/ENEM_AL_EXCEL_AJUS_OKSNZ.xlsx") 

quartis.cut <- cut(dados$NOTA_ENEN, breaks=quantile(dados$NOTA_ENEN),include.lowest = TRUE) #pega os quatro
summary(quantile(dados$NOTA_ENEN))
boxplot(dados$NOTA_ENEN ~ quartis.cut, 
        data = dados, 
        xlab="Quartis", 
        ylab="Notas", 
        main="Notas por Quartil", 
        col="orange",
        border="brown")
##https://www.datamentor.io/r-programming/box-plot/

summary(quartis.cut)
classes_label <- c("[318.44,455.20]", "(455.20,497.22]", "(497.22,553.04]", "(553.04,796.14]")

Freq = table(quartis.cut)
FreqAc = cumsum(Freq)
FreqRel = prop.table(Freq)
FreqRelAc = cumsum(FreqRel)

TabResult = cbind(Freq,FreqAc,FreqRel=round(FreqRel*100,digits=2),FreqRelAc=round(FreqRelAc*100,digits=2))
TabResult


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
#View(df)
tb2 = table(quartis.cut,df$dados.NOTA_ENEN)
#View(tb2)
barplot(tb2, col=c("red","orange","yellow","green"), main= "Notas",
        xlab = "Quantiles", ylab = "Frequência de Notas",
        beside = TRUE, legend = TRUE) #quantas vezes cada nota apareceu
library(tidyverse)
library(scales)
df2 = data.frame(dados$NOTA_ENEN,dados$TP_SEXO,df$Quartile)
plotzao = ggplot(data=df2, aes(x=df$Quartile,y=dados.NOTA_ENEN,fill=dados$TP_SEXO)) + 
  geom_bar(stat='identity')+
  ggtitle("Notas por Quartil")+
  xlab("Quartil")+
  ylab("Nota do ENEM")+
  scale_fill_discrete(name="Sexo")
  
print(plotzao)

#tb3 = table(df$dados.TP_SEXO,df$Quartile)

#barplot(tb3, col=c("coral3","cyan4"), main="Distribuição de Sexo por Quartil",
        #xlab = "Quartil",
        #ylab = "Número de Pessoas",
        #legend=TRUE)

##df3 = data.frame(dados$NOTA_ENEN,dados$NU_IDADE)
##hmm = ggplot(df3,aes(dados$NOTA_ENEN,dados$NU_IDADE))+
 ## geom_jitter(height=2,width=2,color="darkseagreen")+
 ## geom_density2d(color="black")+
 ## scale_fill_brewer(palette="Blues")+
 ## ggtitle("Distribuição de Notas do ENEM por Idade")+
 ## xlab("Nota do ENEM")+
  ##ylab("Idade")
##print(hmm)

df4 = data.frame(dados$TP_COR_RACA,dados$SG_UF_NASCIMENTO)
tb4 = table(df4)
barplot(tb4,legend=TRUE)



df5 = data.frame(dados$TP_SEXO,dados$NOTA_ENEN)
sexismo = ggplot(df5,aes(dados$TP_SEXO,dados$NOTA_ENEN))+
  geom_point(size=2,shape=23)+
  coord_flip()
print(sexismo)
