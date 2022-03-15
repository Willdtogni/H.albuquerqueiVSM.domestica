
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

              # M. domestica larval survival  model
model1 <- glm((NsurLarvaMd/DensInMu)~log(Prop)*Encontro, family
              =binomial, weights = DensInMu,data=dados)
summary(model1)


#overdispersion

model.surv.larv.Md <- glm((NsurLarvaMd/DensInMu)~log(DensInMu)*Encontro, family
              =quasibinomial, weights = DensInMu, data=dados)
summary(model.surv.larv.Md)
anova.surv.md <-anova(model.surv.larv.Md, test="Chi")

model2P <- glm((NsurPupaMd/NsurLarvaMd)~log(DensInMu)*Encontro, family=
                 quasibinomial, weights =
                 DensInMu, data=dados)
summary(model2P)
anova(model2P, test="Chi")
              
              ##Hydrotaea albuquerquei larval survival model

model.surv.larv.Ha <- glm((NsurLarvaHa/DensInMu)~log(DensInMu)*Encontro, family=
                binomial, weights = DensInMu, data=dados)
summary(model.surv.larv.Ha)

#overdispersion

model2Ha <- glm((NsurLarvaHa/DensInMu)~log(DensInMu)*Encontro, family=
                 quasibinomial, weights = DensInMu, data=dados)
summary(model2Ha)

model3PHa <- glm(NsurPupaHa/NsurLarvaHa ~log(DensInMu)*Encontro, family=
                   quasibinomial, weights= NsurLarvaHa, data=dados)
summary(model3PHa)

#Capacidade predat?ria

model1.cp <- glm( `Ndeath.larva.Md/N.Surv.Larv.Ha`~Prop*Encontro, family=poisson)
summary(model1.cp)

#over

model2.cp <- glm(`Ndeath.larva.Md/N.Surv.Larv.Ha`~log(DensInMu)*
                   Encontro, data=dados, family=quasipoisson)
summary(model2.cp)
anova(model2.cp, test="Chi")

model3.cp <- glm(`Ndeath.larva.Md/N.Surv.Larv.Ha`~DensInMu, data=dados, family=quasipoisson)
summary(model3.cp)
anova(model3.cp, test="Chi")
anova(model2.cp, model3.cp,test="Chi")

#mean
predcap <- Tabela_Hydrotaea_vc_MuscaR %>% group_by (DensInMu) %>% 
  summarise ( predcap = mean(`Ndeath.larva.Md/N.Surv.Larv.Ha`))
  
  #"Larval development rate of Musca domestica (1/days)"
#desenvolvimento das pupas de Musca domestica

model1.rate.p <- lm(taxa.p.Md~log(DensInMu)*Encontro, data = dados)
summary(model1.rate.p)

anova(model1.rate.p, test="F")

 kmodel1.rate.p2 <- lm(taxa.p.Md~Prop+Encontro)
summary(model1.rate.p)

anova(model1.rate.p2, test="F")

model1.rate <- lm(taxa.larv.Md~log(DensInMu)*Encontro, data = dados)
summary(model1.rate)

anova(model1.rate, test="F")

model2.rate <- lm(taxa.larv.Md~Prop+Encontro, data = dados)
summary(model2.rate)

anova(model2.rate, test="F")
