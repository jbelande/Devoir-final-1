
library(readxl)
library(httr)
library(tidyverse)
library(tseries)
library(lmtest)
########importation des données

lienurl<-"https://www.brh.ht/wp-content/uploads/agregatsmon.xls"
GET(lienurl,
    write_disk(tf<-tempfile(fileext="agregatsmon.xls")))
agregat<-read_excel(tf,1L)
agregat

agregat<-data.frame(agregat)

colnames(agregat)<-agregat[2,]  #####utilisation d'une ligne comme nom des autres lignes


travail<-agregat[26:520,c(1,4,12,14)]
colnames(travail)<-c("Date","M3_M.G.","Base_Monetaire_M.G.","Monnaie_en_Circulation_M.G.")

str(travail)


travail$Date <- as.numeric(travail$Date)
travail$Base_Monetaire_M.G. <- as.numeric(travail$Base_Monetaire_M.G.)
travail$Monnaie_en_Circulation_M.G. <- as.numeric(travail$Monnaie_en_Circulation_M.G.)
travail$M3_M.G. <- as.numeric(travail$M3_M.G.)

str(travail)

travail$Date <- as.Date(travail$Date,origin = "1899-12-30")
str(travail)

travail$Date[309] <- "2004-07-01"
travail$Date[310] <- "2004-08-01"
travail$Date[311] <- "2004-09-01"
travail$Date[312] <- "2004-10-01"
travail$Date[313] <- "2004-11-01"
travail$Date[314] <- "2004-12-01"

travail$Date[315] <- "2005-01-01"
travail$Date[316] <- "2005-02-01"
travail$Date[317] <- "2005-03-01"
travail$Date[318] <- "2005-04-01"
travail$Date[319] <- "2005-05-01"
travail$Date[320] <- "2005-06-01"

travail$Date[360] <- "2008-10-01"
travail$Date[361] <- "2008-11-01"
travail$Date[362] <- "2008-12-01"

travail$Date[363] <- "2009-01-01"
travail$Date[364] <- "2009-02-01"


str(travail)

ggplot(travail, aes(x = Date, y = Base_Monetaire_M.G.))+
  geom_line()+
  labs(title = " Base monetaire en million de gourdes")

adf.test(travail$Base_Monetaire_M.G.)


ggplot(travail, aes(x = Date, y = Monnaie_en_Circulation_M.G.))+
  geom_line()+
  labs(title = "Monnaie en Circulation en Million de Gourdes")

adf.test(travail$Monnaie_en_Circulation_M.G.)

ggplot(travail, aes(x = Date, y = M3_M.G.))+
  geom_line()+
  labs(title = " Aggregat monetaire M3 en million de gourdes")

adf.test(travail$M3_M.G.)


g1<- diff(travail$Base_Monetaire_M.G.)
plot(g1, ylab = "Base Monetaire en million de gourdes.")
adf.test(g1, k=2)

g2<- diff(travail$Monnaie_en_Circulation_M.G.)
plot(g2, ylab = "Monnaie en Circulation en million de gourdes")
adf.test(g2, k=2)

g3<- diff(travail$M3_M.G.)
plot(g3, ylab = "Agregat monetaire M3 en million de gourdes")
adf.test(g3, k=2)


grangertest(M3_M.G. ~ Base_Monetaire_M.G., data = travail, order = 1)
grangertest(M3_M.G. ~ Base_Monetaire_M.G., data = travail, order = 2)

grangertest(Monnaie_en_Circulation_M.G. ~ Base_Monetaire_M.G., data = travail, order = 1)
grangertest(Monnaie_en_Circulation_M.G. ~ Base_Monetaire_M.G., data = travail, order = 2)

grangertest(Base_Monetaire_M.G. ~ M3_M.G., data = travail, order = 1)
grangertest(Base_Monetaire_M.G. ~ M3_M.G., data = travail, order = 2)

grangertest(Base_Monetaire_M.G. ~ Monnaie_en_Circulation_M.G., data = travail, order = 1)
grangertest(Base_Monetaire_M.G. ~ Monnaie_en_Circulation_M.G., data = travail, order = 2)

estimation <- lm(M3_M.G. ~ Base_Monetaire_M.G., Monnaie_en_Circulation_M.G., data = travail)
summary(estimation)$coef
