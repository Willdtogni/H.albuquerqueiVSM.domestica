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

# convertendo as colunas para formato numerico
Tabela_Hydrotaea_vc_MuscaR$DensInMu <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$DensInMu )
Tabela_Hydrotaea_vc_MuscaR$`%Surv.p.Md` <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$`%Surv.p.Md` )
Tabela_Hydrotaea_vc_MuscaR$Encontro <- factor(Tabela_Hydrotaea_vc_MuscaR$Encontro )
Tabela_Hydrotaea_vc_MuscaR$NsurLarvaMd <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$NsurLarvaMd)
Tabela_Hydrotaea_vc_MuscaR$Prop <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$Prop)
Tabela_Hydrotaea_vc_MuscaR$NsurPupaMd <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$NsurPupaMd)
Tabela_Hydrotaea_vc_MuscaR$NsurLarvaHa <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$NsurLarvaHa)
Tabela_Hydrotaea_vc_MuscaR$NsurPupaHa <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$NsurPupaHa)
Tabela_Hydrotaea_vc_MuscaR$taxa.p.Md <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$taxa.p.Md)
Tabela_Hydrotaea_vc_MuscaR$taxa.p.Ha <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$taxa.p.Ha)
Tabela_Hydrotaea_vc_MuscaR$taxa.larv.Md <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$taxa.larv.Md)
Tabela_Hydrotaea_vc_MuscaR$taxa.larv.Ha <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$taxa.larv.Ha)
Tabela_Hydrotaea_vc_MuscaR$time.ciclo.Md <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$time.ciclo.Md)
Tabela_Hydrotaea_vc_MuscaR$taxa.ciclo.Md <- as.numeric(Tabela_Hydrotaea_vc_MuscaR$taxa.ciclo.Md)

#criar tabela apenas com encontros
dados <-   filter(Tabela_Hydrotaea_vc_MuscaR ,Encontro=='H3M2'|
                    Encontro=='H3M1'| Encontro=='H2M1'| Encontro=='H1M1')
   
