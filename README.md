# H.albuquerqueiVSM.domestica
Codes to data analysis 
We test the following hypothesis: 
(i) larval and pupal survival of M. domestica decreases as increases the density and stage of H. albuquerquei; 
(ii) as well larval and pupal development rate accelerates and; 
(iii) the predatory capacity increase as decreases the predator density and larval stage increases. 
(iv) At the same time, if the predator is also a better competitor, we expect that their development time will tend to prolongate as conspecific larval 
density increases. This delay should be caused primarily by energy costs related to predation activity.

library(readxl)
Tabela_Hydrotaea_vc_MuscaR <- read_excel("D:/Biblio mestrado/projeto/analises/Hydrotea vs Musca/Tabela  Hydrotaea vc MuscaR.xlsx")
View(Tabela_Hydrotaea_vc_MuscaR)

head(Tabela_Hydrotaea_vc_MuscaR)

#criar tabela apenas com encontros
dados <-   filter(Tabela_Hydrotaea_vc_MuscaR ,Encontro=='H3M2'|
                    Encontro=='H3M1'| Encontro=='H2M1'| Encontro=='H1M1')

library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(car)
library(DescTools)
library(ggpubr)
library(rstatix)

# convertendo as coluna de densidade para fator
Tabela_Hydrotaea_vc_MuscaR$DensInMu <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$DensInMu )
Tabela_Hydrotaea_vc_MuscaR$`%Surv.p.Md` <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$`%Surv.p.Md` )
Tabela_Hydrotaea_vc_MuscaR$Encontro <- factor(Tabela_Hydrotaea_vc_MuscaR$Encontro )
Tabela_Hydrotaea_vc_MuscaR$NsurLarvaMd <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$NsurLarvaMd)
Tabela_Hydrotaea_vc_MuscaR$Prop <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$Prop)

glimpse(dados)

#Plot da sobrevivencia larval
Sur.larv.Md <- ggplot(Tabela_Hydrotaea_vc_MuscaR, aes(x=DensInMu, y=`%surv.larva.Md` , group=DensInMu))+ 
  geom_boxplot(aes(fill=DensInMu))
Sur.larv.Md <- Sur.larv.Md + facet_wrap(~ Encontro)
Sur.larv.Md <- Sur.larv.Md + scale_y_log10()

#Plot da sobrevivencia pupal
Sur.pup.Md <- ggplot(Tabela_Hydrotaea_vc_MuscaR, aes(x=DensInMu, y=`%Surv.p.Md` , group=DensInMu))+ 
  geom_boxplot(aes(fill=DensInMu))
Sur.pup.Md <- Sur.pup.Md + facet_wrap(~ Encontro)

#Plot da capacidade predatoria
Pred.cap.Hyd <- ggplot(Tabela_Hydrotaea_vc_MuscaR, aes(x=DensInMu, y= `Ndeath.larva.Md/N.Surv.Larv.Ha` , group=DensInMu))+ 
  geom_boxplot(aes(fill=DensInMu))
Pred.cap.Hyd <- Pred.cap.Hyd + facet_wrap(~ Encontro)

#### Recorte geral do Grupo controle 
control       <- slice(Tabela_Hydrotaea_vc_MuscaR, 61:69)

#teste de homogeneidade de variancias para sobrevivencia larval
hom.var.surv.larv.Md <- bartlett.test(`%surv.larva.Md` ~ DensInMu, data = Tabela_Hydrotaea_vc_MuscaR)
#teste de homogeneidade de variancias para sobrevivencia larval
Levene.surv.larv.Md <- leveneTest(`%surv.larva.Md` ~ DensInMu, data = Tabela_Hydrotaea_vc_MuscaR)
#P<0,05 = ao menos dois grupos possuem diferen?as 
#significativas nas variancias

#analise de outliers na variavel sobrevivencia larval
require(mfx)
logitor((NsurLarvaMd/DensInMu)~DensInMu+Encontro ,data=dados)
#criar tabela para compara??o entre grupos
#control1 + h1m1
Sur.larv.Md.control1  <- Tabela_Hydrotaea_vc_MuscaR[Tabela_Hydrotaea_vc_MuscaR$Encontro == "Control1", ]
Sur.larv.h1m1.Md <-   Tabela_Hydrotaea_vc_MuscaR[Tabela_Hydrotaea_vc_MuscaR$Encontro == "H1M1", ]
Sur.larv.h1m1.Md <- bind_rows (Sur.larv.Md.control1 , Sur.larv.h1m1.Md)

#control1 + h2m1
Sur.larv.h2m1.Md <-   Tabela_Hydrotaea_vc_MuscaR[Tabela_Hydrotaea_vc_MuscaR$Encontro == "H2M1", ]
Sur.larv.h2m1.Md <- bind_rows (Sur.larv.Md.control1 , Sur.larv.h2m1.Md)

#control1 + h3m1
Sur.larv.h3m1.Md <-   Tabela_Hydrotaea_vc_MuscaR[Tabela_Hydrotaea_vc_MuscaR$Encontro == "H3M1", ]
Sur.larv.h3m1.Md <- bind_rows (Sur.larv.Md.control1 , Sur.larv.h3m1.Md)

#control2 + h3m2
Sur.larv.Md.control2  <- Tabela_Hydrotaea_vc_MuscaR[Tabela_Hydrotaea_vc_MuscaR$Encontro == "Control2", ]
Sur.larv.h3m2.Md <-   Tabela_Hydrotaea_vc_MuscaR[Tabela_Hydrotaea_vc_MuscaR$Encontro == "H3M2", ]
Sur.larv.h3m2.Md <- bind_rows (Sur.larv.Md.control2 , Sur.larv.h3m2.Md)
