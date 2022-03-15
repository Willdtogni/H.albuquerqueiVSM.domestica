
#criar tabela apenas com encontros
dados1 <-  filter(Tabela_Hydrotaea_vc_MuscaR ,Encontro=='H3M2'|
                    Encontro=='H3M1'| Encontro=='H2M1'| Encontro=='H1M1')

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
