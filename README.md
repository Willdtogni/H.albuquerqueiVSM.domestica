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

# chamando as libs necessÃ¡rias
pacman::p_load(tidyverse, magrittr, lubridate, iNEXT, vegan, RAM, MASS, hnp)

dados2 <- dados %>% dplyr::select(NsurLarvaMd, Encontro, DensInMu, Prop) %>%
   mutate(surv = NsurLarvaMd/DensInMu, 
         Prop = log(Prop)) %>% dplyr::select(-`NsurLarvaMd`)
#verificar esrutura das variáveis
glimpse(dados2)

#Montar modelos
#quasiBinomial
modelsurv.Md.quas.bin <- glm(surv~Prop*Encontro,
                             family=quasibinomial, weights = DensInMu, data=dados2)
summary(modelsurv.Md.quas.bin)
anova(modelsurv.Md.quas.bin, test="Chi")

hnp(modelsurv.Md.quas.bin, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')

#Gamma
modelsurv.Md.gam <- glm((NsurLarvaMd/DensInMu)~log(Prop)*Encontro,
                        family=Gamma, weights = DensInMu, data=dados)
summary(modelsurv.Md.gam)
anova(modelsurv.Md.gam, test="Chi")

#Avaliação do modelo predict
#passo1 realizando a predição com o modelo quasibinomial
dados2$predict <- predict(modelsurv.Md.quas.bin, dados2, type="response")

#passo2 transformando os valores de sobrevivência para 0 e 1
dados2 <- dados2 %>% mutate(surv = case_when(surv < 0.5 ~ 0, surv >= 0.5 ~ 1) )

#passo3 tranformando o valor do predict para 0 e 1 
dados2 <- dados2 %>% mutate(predict = case_when(predict < 0.5 ~ 0, predict >= 0.5 ~ 1) )

library(pROC)
pROC_obj <- roc(dados2$surv,dados2$predict,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


library(caret)
confusionMatrix(data= as.factor(dados2$predict), as.factor (dados2$surv))
