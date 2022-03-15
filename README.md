
#criar tabela apenas com encontros
dados1 <-  filter(Tabela_Hydrotaea_vc_MuscaR ,Encontro=='H3M2'|
                    Encontro=='H3M1'| Encontro=='H2M1'| Encontro=='H1M1')

#Mount a collumn with species sci names
Specie = c()
for (i in 1:60)
  Specie[[i]] = "Musca domestica"
for (i in 61:120)
  Specie[[i]] = "Hydrotaea albuquerquei"

#Modifies the list
Species <- unlist(Specie) 
#create data.frame 
Specie <-as.data.frame(Species)

dados3 <-data.frame(Survival = c( dados$NsurLarvaMd, dados$NsurLarvaHa), Density=
                       c(dados$DensInMu, dados$DensInMu),Encounter =
                       c(dados$Encontro, dados$Encontro), Species ) 

dados4 <- data.frame(Survival = NULL)
dados5 <-data.frame(DevRate = (c( dados$taxa.larv.Md , dados$taxa.larv.Ha)), Density=
                      c(dados$DensInMu, dados$DensInMu),Encounter =
                      c(dados$Encontro, dados$Encontro), Species ) 
dados6 <- data.frame(DevRate = (c( dados$taxa.p.Md , dados$taxa.p.Ha )), Density=
                       c(dados$DensInMu, dados$DensInMu),Encounter =
                       c(dados$Encontro, dados$Encontro), Species ) 

PSurvival <-data.frame(Psurvival = c(dados$NsurPupaMd/dados$NsurLarvaMd, dados$NsurPupaHa/dados$
               NsurLarvaHa), Density=c(dados$DensInMu, dados$DensInMu),Encounter =
                       c(dados$Encontro, dados$Encontro), Species)

DevRate <-  c( dados$taxa.larv.Md , dados$taxa.larv.Ha)
Encounter <- unlist(c(dados$Encontro, dados$Encontro))
Density <- unlist(c(dados$DensInMu, dados$DensInMu))

Psurvival <- unlist(c(dados$NsurPupaMd/dados$NsurLarvaMd, dados$
                 NsurPupaHa/dados$NsurLarvaHa))

dados4 <- data.frame(Survival = Psurvival, Encounter=Encounter, Density=
                       Density, Species)
rm(dados4)
dados4$Survival=Psurvival

#save file
write.csv(dados4, "dados4.csv")
#find directory
getwd()
#check data
glimpse(dados4)

Density<-data.frame(Density=c(dados$DensInMu, dados$DensInMu))

Encounter<-data.frame(Encounter =c(dados$Encontro, dados$Encontro))

#adicionar ao df
Specie <- dados(Specie)

# M. domestica larval survival  model
model1 <- glm((NsurLarvaMd/DensInMu)~log(Prop)*Encontro, family
              =binomial, weights = DensInMu,data=dados)
summary(model1)


#overdispersion

model.surv.larv.Md <- glm((NsurLarvaMd/DensInMu)~log(DensInMu)*Encontro, family
              =quasibinomial, weights = DensInMu, data=dados)
summary(model.surv.larv.Md)
anova.surv.md <-anova(model.surv.larv.Md, test="Chi")

# what I usually do for regression outputs:
library(stargazer)
stargazer(model.surv.larv.Md, model2P, model3PHa, model2Ha ,model2.cp,  type = "html", out ="survival.html")

stargazer( model.tax.des.Md.quas.poi.log, model1.rate.ha,  type = "html", out ="development.html")

stargazer(anova.surv.md,  type = "html", out ="surv Md.html")

model2P <- glm((NsurPupaMd/NsurLarvaMd)~log(DensInMu)*Encontro, family=
                 quasibinomial, weights =
                 DensInMu, data=dados)
summary(model2P)
anova(model2P, test="Chi")

hnp(model2, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')

PCT.p.Md <- as.data.frame (exp(cbind(OR = coef(model2), confint(model2))))

PCT.p.Md$interp = (PCT.p.Md$OR -1) *100
