####RAnkeamento de valor p para verificar tendencia entre os grupos
#criar tabela para compara??o entre grupos
#control1 + h1m1
Sur.larv.control1  <- Tabela_Hydrotaea_vc_MuscaR[Tabela_Hydrotaea_vc_MuscaR$Encontro == "Control1", ]
Sur.larv.h1m1 <-   Tabela_Hydrotaea_vc_MuscaR[Tabela_Hydrotaea_vc_MuscaR$Encontro == "H1M1", ]
Sur.larv.h1m1.control <- bind_rows (Sur.larv.control1 , Sur.larv.h1m1)

#Comparison of H. albuquerquei larval survival at H1M1  
Sur.larv.h1m1.Ha.model <- glm(NsurLarvaHa/DensInMu~log(DensInMu),
                              family=quasibinomial, weights =
                                DensInMu, data=Sur.larv.h1m1.control)
summary(Sur.larv.h1m1.Ha.model)
anova(Sur.larv.h1m1.Ha.model, test="Chi")

#Comparison of H. albuquerquei pupal survival at H1M1  
Sur.pup.h1m1.Ha.model <- glm(NsurPupaHa/NsurLarvaHa~log(DensInMu),
                             family=quasibinomial, weights =
                               DensInMu, data=Sur.larv.h1m1.control)
summary(Sur.pup.h1m1.Ha.model)
anova(Sur.pup.h1m1.Ha.model, test="Chi")

#Comparison of M. domestica larval survival at H1M1  
Sur.larv.h1m1.Md.model <- glm(NsurLarvaMd/DensInMu~log(DensInMu),
                              family=quasibinomial, weights = 
                                DensInMu, data=Sur.larv.h1m1)
summary(Sur.larv.h1m1.Md.model)
anova(Sur.larv.h1m1.Md.model, test="Chi")

#Comparison of M. domestica pupal survival at H1M1  
Sur.pup.h1m1.Md.model <- glm(NsurPupaMd/NsurLarvaMd~log(DensInMu),
                             family=quasibinomial, weights = 
                               DensInMu, data=Sur.larv.h1m1.control)
summary(Sur.pup.h1m1.Md.model)
anova(Sur.pup.h1m1.Md.model, test="Chi")

#Save table H1M1
stargazer(Sur.larv.h1m1.Md.model, Sur.larv.h1m1.Ha.model, Sur.pup.h1m1.Md.model, 
          Sur.pup.h1m1.Ha.model,
          type = "html", out ="Survival of M. domestica and H. albuquerquei-H1M1.html")

#control1 + h2m1
Sur.h2m1.Md <-   Tabela_Hydrotaea_vc_MuscaR[Tabela_Hydrotaea_vc_MuscaR$Encontro == "H2M1", ]
Sur.h2m1.control <- bind_rows (Sur.larv.control1 , Sur.h2m1.Md)

#M.domestica larval  survival at H2M1
Sur.larv.h2m1.Md.model <- glm(NsurLarvaMd/DensInMu~log(Prop),
                              family=binomial, weights =
                                DensInMu, data=Sur.h2m1.control)
summary(Sur.larv.h2m1.Md.model)
anova(Sur.larv.h2m1.Md.model, test="Chi")
#M.domestica pupal  survival at H2M1
Sur.larv.h2m1.P.model <- glm((NsurPupaMd/NsurLarvaMd)~log(Prop)*Encontro, family=
                               quasibinomial, weights =
                 DensInMu, data=Sur.h2m1.control)
summary(Sur.larv.h2m1.P.model)
anova(Sur.larv.h2m1.P.model)
#M.domestica pupal  survival at H2M1
Sur.pup.h2m1.Md.model <- glm(NsurPupaMd/NsurLarvaMd~log(Prop)*Encontro,
                              family=binomial, weights = DensInMu, data=Sur.h2m1.control)
summary(Sur.pup.h2m1.Md.model)
anova(Sur.pup.h2m1.Md.model, test="Chi")

#Comparison of H. albuquerquei larval survival at H2M1  
Sur.larv.h2m1.Ha.model <- glm(NsurLarvaHa/DensInMu~log(DensInMu),
                              family=quasibinomial, weights =
                                DensInMu, data=Sur.h2m1.Md)
summary(Sur.larv.h2m1.Ha.model)
anova(Sur.larv.h2m1.Ha.model, test="Chi")

#Comparison of H. albuquerquei pupal survival at H2M1  
Sur.pup.h2m1.Ha.model <- glm(NsurPupaHa/NsurLarvaHa~as.factor(DensInMu),
                             family=quasibinomial, weights =
                               DensInMu, data=Sur.h2m1.Md)
summary(Sur.pup.h2m1.Ha.model)
anova(Sur.pup.h2m1.Ha.model, test="Chi")

#teste anova para capacidade predat?ria
cap.pred.h2m1.Md <- glm(`Ndeath.larva.Md/N.Surv.Larv.Ha`~ as.factor(DensInMu)*Encontro,
                        family=quasipoisson, weights =
                          DensInMu, data=dados)
summary(cap.pred.h2m1.Md)
anova(cap.pred.h2m1.Md, test="Chi")

#control1 + h3m1
Sur.larv.h3m1 <-   Tabela_Hydrotaea_vc_MuscaR[Tabela_Hydrotaea_vc_MuscaR$Encontro == "H3M1", ]
Sur.larv.h3m1.control <- bind_rows (Sur.larv.control1 , Sur.larv.h3m1)


Sur.larv.h3m1.Md.model <- glm(NsurLarvaMd/DensInMu~log(DensInMu),
                              family=quasibinomial, weights = 
                                DensInMu, data=Sur.larv.h3m1.control)
summary(Sur.larv.h3m1.Md.model)
anova(Sur.larv.h3m1.Md.model, test="Chi")

#M.domestica pupal  survival at H3M1
Sur.pup.h3m1.Md.model <- glm(NsurPupaMd/NsurLarvaMd~log(DensInMu),
                             family=quasibinomial, weights =
                               DensInMu, data=Sur.larv.h3m1.control)
summary(Sur.pup.h3m1.Md.model)
anova(Sur.pup.h3m1.Md.model, test="Chi")

#Comparison of H. albuquerquei larval survival at H3M1  
Sur.larv.h3m1.Ha.model <- glm(NsurLarvaHa/DensInMu~as.factor(DensInMu)*Encontro,
                              family=quasibinomial, weights =
                                DensInMu, data=Sur.larv.h3m1.control)
summary(Sur.larv.h3m1.Ha.model)
anova(Sur.larv.h3m1.Ha.model, test="Chi")

#Comparison of H. albuquerquei pupal survival at H3M1  
Sur.pup.h3m1.Ha.model <- glm(NsurPupaHa/NsurLarvaHa~log(DensInMu),
                             family=quasibinomial, weights =
                               DensInMu, data=Sur.larv.h3m1)
summary(Sur.pup.h3m1.Ha.model)
anova(Sur.pup.h3m1.Ha.model, test="Chi")

#teste anova para capacidade predat?ria
cap.pred.h3m1.Md <- glm(`Ndeath.larva.Md/N.Surv.Larv.Ha`~ log(DensInMu),
                        family=quasipoisson, weights =
                          DensInMu, data=Sur.larv.h3m1)
summary(cap.pred.h3m1.Md)
anova(cap.pred.h3m1.Md, test="Chi")

#control2 + h3m2
Sur.larv.control2  <- Tabela_Hydrotaea_vc_MuscaR [Tabela_Hydrotaea_vc_MuscaR$Encontro == "Control2", ]
Sur.larv.h3m2 <-   Tabela_Hydrotaea_vc_MuscaR[Tabela_Hydrotaea_vc_MuscaR$Encontro == "H3M2", ]
Sur.h3m2 <- bind_rows (Sur.larv.control2 , Sur.larv.h3m2)

#Comparision larval survival M. domestica H3M2
Sur.larv.h3m2.Md.model <- glm(NsurLarvaMd/DensInMu~as.factor(Prop),
                              family=quasibinomial, weights =
                                DensInMu, data=Sur.h3m2)
summary(Sur.larv.h3m2.Md.model)

#M.domestica pupal  survival at H3M2
Sur.pup.h3m2.Md.model <- glm(NsurPupaMd/NsurLarvaMd~log(DensInMu)*Encontro,
                             family=quasibinomial, weights =
                               DensInMu, data=Sur.h3m2)
summary(Sur.pup.h3m2.Md.model)
anova(Sur.pup.h3m2.Md.model, test="Chi")

#Comparison of H. albuquerquei larval survival at H3M2 
Sur.larv.h3m2.Ha.model <- glm(NsurLarvaHa/DensInMu~log(DensInMu),
                              family=quasibinomial, weights =
                                DensInMu, data=Sur.h3m2)
summary(Sur.larv.h3m2.Ha.model)
anova(Sur.larv.h3m2.Ha.model, test="Chi")

#Comparison of H. albuquerquei pupal survival at H3M2  
Sur.pup.h3m2.Ha.model <- glm(NsurPupaHa/NsurLarvaHa~log(DensInMu)*Encontro,
                             family=quasibinomial, weights =
                               DensInMu, data=Sur.h3m2)
summary(Sur.pup.h3m2.Ha.model)
anova(Sur.pup.h3m2.Ha.model, test="Chi")
#teste anova 
anova.Sur.larv.h3m2.Md <- aov(Sur.larv.h3m2$`%surv.larva.Md` ~ Sur.larv.h3m2$
                                DensInMu)
summary(anova.Sur.larv.h3m2.Md)

#teste anova para capacidade predat?ria
cap.pred.h3m2.Md <- glm(`Ndeath.larva.Md/N.Surv.Larv.Ha`~ log(DensInMu),
                        family=quasipoisson, weights =
                          DensInMu, data=Sur.larv.h3m2)
summary(cap.pred.h3m2.Md)
anova(cap.pred.h3m2.Md, test="Chi")

mSur.larv.h2m1.Md <- Sur.larv.h2m1.Md %>% group_by (DensInMu) %>% 
  summarise ( (larval_survival.Md = mean(`%surv.larva.Md`)))
