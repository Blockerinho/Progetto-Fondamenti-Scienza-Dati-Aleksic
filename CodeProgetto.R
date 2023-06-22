
#-------------------------------------- inizializzazione ------------------------------------

#importo le librerie 
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

#leggo i dataset
nobel = read_csv("nobel_latest.csv",show_col_types = FALSE)
invst = read_csv("invstTotale.csv",show_col_types = FALSE)

#---------------------------------------------------------------------------------------------

#------------------------------------ PARTE I: DATASET PREMI NOBEL ---------------------------

#per ogni paese che ho scelto di trattare vado a scegliere solo il paese di nascita del vincitore
#e conto quante volte appare un determinato paese di nascita, in tal modo avrò il numero totale 
#dei vincitori che vengono da qual dato paese

#lo faccio per:

#italia
nobelItalia = 
  nobel %>% 
  filter(Birth_Country=="Italy")%>%
  select(Birth_Country)
it = as.integer(count(nobelItalia))

#francia
nobelFrancia = 
  nobel %>% 
  filter(Birth_Country=="France")%>%
  select(Birth_Country)
fr = as.integer(count(nobelFrancia))

#germania
nobelGermania = 
  nobel %>% 
  filter(Birth_Country=="Germany")%>%
  select(Birth_Country)
ger = as.integer(count(nobelGermania))

#svizzera
nobelSvizzera = 
  nobel %>% 
  filter(Birth_Country=="Switzerland")%>%
  select(Birth_Country)
swi = as.integer(count(nobelSvizzera))

#svezia
nobelSvezia = 
  nobel %>% 
  filter(Birth_Country=="Sweden")%>%
  select(Birth_Country)
swe = as.integer(count(nobelSvezia))

#russia
nobelRussia = 
  nobel %>% 
  filter(Birth_Country=="Russia")%>%
  select(Birth_Country)
ru = as.integer(count(nobelRussia))

#Stati Uniti
nobelus = 
  nobel %>% 
  filter(Birth_Country=="USA")%>%
  select(Birth_Country)
us = as.integer(count(nobelus))

#Giappone
nobelgp = 
  nobel %>% 
  filter(Birth_Country=="Japan")%>%
  select(Birth_Country)
gp = as.integer(count(nobelgp))

#Norvegia
nobelnw = 
  nobel %>% 
  filter(Birth_Country=="Norway")%>%
  select(Birth_Country)
nw = as.integer(count(nobelnw))

#Danimarca
nobeldk = 
  nobel %>% 
  filter(Birth_Country=="Denmark")%>%
  select(Birth_Country)
dk = as.integer(count(nobeldk))

Country = c("IT","FR","GE","SV","SWE","RU","US","JP","NW","DK")

Nobels = c(it,fr,swi,swe,ger,ru,us,gp,nw,dk)

#creo un data frame per contenere le informazioni che mi servono
NobelPerNazione = data.frame(Country=Country,Nobels = Nobels)

#creo il grafico a barre specificando:
# - altezza del grafico = max numero di nobel vinti
# - nessuno bordo alle barre
# - definisco il titolo 
# - il nome degli argomenti satà quello dei paesi che ho definito nel vettore country
# - definisco il nome degli assi
# - definisco il colore delle barre 

nobelgraph <- barplot( height = NobelPerNazione$Nobels, border = F,main="Numero di Nobel per paese", 
                       names.arg=Country,
                       ylab="Nobels",xlab="Paese",
                       col= c("black","black","black","black","black","black","gold","black","grey","grey"))

#siccome voglio il grafico più leggibile definisco una leggenda:
# - la posiziono in alto a destra
# - il suo contenuto sarà un vettore di due elementi 
# - così anche il suo colore
# - definisco una serie di parametri per eliminare il bordo della leggenda e per mettere i cerchi al posto dei quadratini 

legend("topright", legend = c("Maggior numero", "Minor numero"), 
       col = c("gold", "grey"), 
       bty = "n", pch=20 , pt.cex = 2, cex = 0.8, horiz = FALSE, inset = c(0.05, 0.05))

#siccome dall'asse y non si legge bene definisco un testo in cima a ogni barra 
# - uso l'if else per un semplice motivo, definisco il testo normalmente sopra le barre tranne quello degli usa 
# - finisce fuori dal grafico quindi se la barra è quella degli usa metto il testo dentro la barra mentre per gli altri lo metto fuori 
# - l'uso di Nobel-9 per quanto strano definisce semplicemente la distanza del testo dalla barra 

text(nobelgraph, ifelse(Country == "US", Nobels-9, Nobels+9), paste( Nobels))

#creo un secondo grafico per raccogliere le informazioni riguardo alle vincite per ogni categoria 

nobelUSA = 
  nobel %>% 
  filter(Birth_Country=="USA") %>%
  select(Category)

topic <- data.frame(table(nobelUSA))

#per la creazione del grafico mi comporto praticamente nello stesso modo in cui mi sono comportato sopra

catpie <- pie(topic$Freq,labels = c("57","53","10","79","19","71"),main="Categorie di vincita dei Nobel (USA)", sub="*I numeri indicano il numero di vincite per categoria")

legend("topright", legend = c("Chimica","Economia","Letteratura","Medicina","Pace","Fisica"), 
       fill = c("white","lightblue","#FFE4E1","#E0FFFF","#D8BFD8","#FFFACD"),border="black",
       bty = "n", pt.cex = 2,  horiz = FALSE)

#------------------------ PARTE II: TOTALE VINCITORI DEL NOBEL IN AMERICA NEGLI ULTIMI 21 ANNI ------------------------------------------------

#in questa parte mi interessa andare capire quanti americani hanno vinto il nobel negli ultimi 21 anni, partendo dal 
#2000 e fino al 2021, l'intendo è quello di trovare una correlazione tra la spesa effetuata e i premi nobel vinti 

#calcolo il numero di vincitori del nobel negli usa negli ultimi 21 anni 

#prendo solo le colonne che mi interessano
nbl = 
  nobel%>%
  filter((between(Year,2000,2021)) & (Birth_Country=="USA"))%>%
  select(Year)

conteggio <- count(nbl,Year)

numVincitori <- data.frame(conteggio)

#creo un grafico a barre a riguardo
barplot(numVincitori$n, border = F,main="Numero di Nobel USA negli ultimi 21 anni", las = 2,
        names.arg = numVincitori$Year,
        ylab="Nobel vinti",xlab="anno")

#anche per questa parte "ristretta" vado a vedere quali sono state le categorie più vittoriose
#creo il grafico nello stesso modo in cui l'ho fatto prima

catRistretta = 
  nobel %>% 
  filter((Birth_Country=="USA") & (between(Year,2000,2021))) %>% 
  select(Category)

topic <- data.frame(table(catRistretta))

catpie <- pie(topic$Freq,labels = c("22","31","2","22","3","20"),main="Categorie di vincita nel periodo 2000-2021",  sub="*I numeri indicano il numero di vincite per categoria")

legend("topright", legend = c("Chimica","Economia","Letteratura","Medicina","Pace","Fisica"), 
       fill = c("white","lightblue","#FFE4E1","#E0FFFF","#D8BFD8","#FFFACD"),border="black",
       bty = "n", pt.cex = 2,  horiz = FALSE)

#------------------------ PARTE III: TOTALE SPESA DEI PAESI IN RICERCA ------------------------------------------------

#mi interessa ora andare a vedere quanto ha speso ogni paese in ricerca negli ultimi 21 anni per capire se anche qua gli
#usa stra vincono sugli altri paesi, uso tutti i paesi di prima ma escludo la svizzera sulla quale non ho dati :(

invstUs = 
  invst %>%
  filter((LOCATION =="USA") & (between(TIME,2000,2021))) %>%
  mutate(Value = Value / 10^3) %>%
  select(TIME,Value)

iUs = sum(invstUs[,2])

invstIt = 
  invst %>%
  filter((LOCATION =="ITA") & (between(TIME,2000,2021))) %>%
  mutate(Value = Value / 10^3) %>%
  select(TIME,Value)
iIt = sum(invstIt[,2])

invstSwe = 
  invst %>%
  filter((LOCATION =="SWE") & (between(TIME,2000,2021))) %>%
  mutate(Value = Value / 10^3) %>%
  select(TIME,Value)
iSw = sum(invstSwe[,2])

invstJp = 
  invst %>%
  filter ((LOCATION =="JPN") & (between(TIME,2000,2021))) %>%
  mutate(Value = Value / 10^3) %>%
  select(TIME,Value)
iJp = sum(invstJp[,2])

invstDk = 
  invst %>%
  filter( (LOCATION =="DNK")  & (between(TIME,2000,2021))) %>%
  mutate(Value = Value / 10^3) %>%
  select(TIME,Value)

iDk = sum(invstDk[,2])

invstFr = 
  invst %>%
  filter( (LOCATION =="FRA") &  (between(TIME,2000,2021)))%>%
  mutate(Value = Value / 10^3) %>%
  select(TIME,Value)
iFr = sum(invstFr[,2])

invstGe = 
  invst %>%
  filter((LOCATION =="DEU") & (between(TIME,2000,2021))) %>%
  mutate(Value = Value / 10^3) %>%
  select(TIME,Value)
iGe = sum(invstGe[,2])

invstSv = 
  invst %>%
  filter ( (LOCATION =="SVN") & (between(TIME,2000,2021)))%>%
  mutate(Value = Value / 10^3) %>%
  select(TIME,Value)
iSv = sum(invstSv[,2])

invstRu = 
  invst %>%
  filter((LOCATION =="RUS") & (between(TIME,2000,2021))) %>%
  mutate(Value = Value / 10^3) %>%
  select(TIME,Value)
iRu = sum(invstRu[,2])

invstNw = 
  invst %>%
  filter((LOCATION =="NOR") & (between(TIME,2000,2021))) %>%
  mutate(Value = Value / 10^3) %>%
  select(TIME,Value)
iNw = sum(invstNw[,2])

#creo un altro grafico seguendo il procedimento che ho fatto prima

Ctr = c("IT","FR","GE","SW","RU","US","JP","NW","DK")

#mentre un altro vettore conterrà il numero di nobel vinti da ogni paese
Spesa = c(iIt,iFr,iGe,iSw,iRu,iUs,iJp,iNw,iDk)

spesameglio <- round((Spesa/10^2),0)

spesagraph <- barplot( height = spesameglio, horiz = TRUE,border = F,main="Totale spesa in ricerca negli ultimi 21 anni (2000-2021)", 
                       names.arg=Ctr, cex.names = 0.7,
                       xlab="Spesa (in miliardi di $)",ylab="Paese",
                       col= c("black","black","black","black"))


#lo modifico per scegliere solo le colonne che mi interessano cioè solo quella degli usa
invstUs = 
  invst %>%
  filter((LOCATION =="USA") & (between(TIME,2000,2021))) %>%
  mutate(Value = Value / 10^3) %>%
  select(TIME,Value)


#creo il grafico che mostra la tendenza della spesa degli usa

ggplot(invstUs,aes(TIME,Value)) +
  geom_point(data=invstUs) +
  geom_smooth(color="gold",formula = y ~ x, method = "loess") + 
  labs(
    title = "Spesa totale in ricerca scientifica degli USA ",
    subtitle = "Il periodo d'interesse va dal 2000 al 2021",
    x = "Anno",
    y = "Totale speso (in miliardi di $)",
  )


#creo lo stesso prototipo di grafico per andare a vedere se c'è un minimo di correlazione tra spesa e numero
#di vincitori (sempre usa)

nnn = 
  nobel%>%
  filter((between(Year,1981,2021)) & (Birth_Country=="USA")) %>%
  select(Year)

#conto il numero di volte che un anno si ripete (vuol dire che quel anno c'è 
#più di una vincita)
conteggio2 <- count(nnn,Year)

#prendo in considerazione tutti i vincitori dal 81 al 2021
winners <- data.frame(conteggio2)

#considero tutti gli inestimenti fatti dal 81 al 2021
invstUs2 = 
  invst %>%
  filter(LOCATION =="USA") %>%
  mutate(Value = Value / 10^3) %>%
  select(TIME,Value)

invstUtile = subset(invstUs2, TIME!=1999 & TIME!=1991)

conteggio2 <- count(nnn,Year)

correlazione <- data.frame(invstUtile[,2], conteggio2[,])

corrLast <- subset(correlazione, select = -c(Year))

corrLast$Value <- round(corrLast$Value,0)

ggplot(corrLast,aes(Value,n)) +
  geom_point(data=corrLast) +
  geom_smooth(color="gold",formula = y ~ x, method = "loess") + 
  labs(
    title = "Correlazione tra spesa totale e numero di vincitori del Nobel (USA) ",
    subtitle = "Il totale speso è relativo a ogni anno dal 1981 al 2021, così anche i vincitori",
    x = "Totale speso (in miliardi di $)",
    y = "Numero di vincitori",
  )

##------------------------ PARTE IV: REGRESSIONE LINEARE E PREDIZIONE ------------------------------------------------

#provo ora a creare un modello di regressione lineare e a predire il vincitore del 2022

#creo un modello di regressione lineare
prova <- lm(n ~ Value, data = corrLast )

cor(corrLast$Value,corrLast$n)
prova$coefficients
summary(prova)$r.squared


#creo il grafico di tale modello, vedo che c'è un po' di regressione
ggplot(corrLast, aes(x = Value, y = n)) + 
  geom_point() + 
  labs(
    title = 'Correlazione tra spesa e numero di vincitori',
    x = 'Totale speso in miliardi di $',
    y = 'Numero di vincite',
    subtitle = 'Con regressione lineare'
  ) +
  geom_abline(intercept = prova$coefficients[1], 
              slope = prova$coefficients[2], 
              color = "red") 


#predico considerando che secondo la OECD gli usa nel 2023 hanno speso 703 miliardi di $ in ricerca e sviluppo
predict(object = prova, newdata = subset(corrLast, Value = 703))

