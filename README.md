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

#criar tabela apenas com encontros
dados <-   filter(Tabela_Hydrotaea_vc_MuscaR ,Encontro=='H3M2'|
                    Encontro=='H3M1'| Encontro=='H2M1'| Encontro=='H1M1')
   
   # chamando as libs necessÃ¡rias
pacman::p_load(tidyverse, magrittr, lubridate, iNEXT, vegan, RAM, MASS, hnp)

dados2 <- dados %>% dplyr::select(NsurLarvaMd, Encontro, DensInMu, Prop) %>%
   mutate(surv = NsurLarvaMd/DensInMu, 
         Prop = log(Prop)) %>% dplyr::select(-`NsurLarvaMd`)

#verificar esrutura das variáveis
glimpse(dados2)

#unir colunas para montar gráfico

Survival.ha.md <- bind_rows(dados$NsurLarvaMd, dados$NsurLarvaHa)

#Montar modelos
#Larval survival of M. domestica - quasiBinomial
modelsurv.Md.quas.bin <- glm(surv~Prop*Encontro,
                             family=quasibinomial, weights = DensInMu, data=dados2)
summary(modelsurv.Md.quas.bin)
anova(modelsurv.Md.quas.bin, test="Chi")

#PREDICAO DE SOBRECIVENCIA EM FUNÇAO DA DENSIDADE

dados.predict <-  dados %>% dplyr::select(Encontro, DenInHa, NsurLarvaMd, DensInMu) %>%
  mutate(surv = NsurLarvaMd/DensInMu) %>% dplyr::select(-NsurLarvaMd, -DensInMu)
                               
dados.predict$DenInHa <-  as.factor(dados.predict$DenInHa )
glimpse(dados.predict)

library(rpart)
library (caret)
library(rpart.plot)
Model <- train(DenInHa ~ ., data = dados.predict,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"))

fit <- rpart(DenInHa~., data = dados.predict, method = 'class')

rpart.plot(fit, extra = 106)

dados.predict$predict <- predict(fit, dados.predict, type = 'class')

table_mat <- table(dados.predict$DenInHa, predict)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

#model random forest
library(randomForest)
modelrand <- randomForest(DenInHa - surv, data = dados.predict, ntree = 1000, mtry =100)


PCTMd <- as.data.frame (exp(cbind(OR = coef(modelsurv.Md.quas.bin), confint(modelsurv.Md.quas.bin))))

PCTMd$interp = (PCT$OR -1) *100

pup <- dados$NsurPupaMd/ dados$DensInMu

pup[is.na(pup)] <- 0


#Pupal survival of M. domestica - quasiBinomial
dadosPupMd <- dados %>% dplyr::select(NsurPupaMd, Encontro, DensInMu, Prop) %>%
  mutate(survpMd = pup, 
         Prop = log(Prop)) %>% dplyr::select(-`NsurPupaMd`)

modelsurv.Md.P.quas.bin <- glm(survpMd~(Prop),
                             family=quasibinomial, weights = DensInMu, data
                             =dadosPupMd)
summary(modelsurv.Md.P.quas.bin)
anova(modelsurv.Md.P.quas.bin, test="Chi")

hnp(modelsurv.Md.P.quas.bin, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')

PCT.p.Md <- as.data.frame (exp(cbind(OR = coef(modelsurv.Md.P.quas.bin), confint(modelsurv.Md.P.quas.bin))))

PCT.p.Md$interp = (PCT.p.Md$OR -1) *100

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


#pupal survival fo h. albuquerquei - quasiBinomial
dadosPupHa <- dados %>% dplyr::select(NsurPupaHa, Encontro, NsurLarvaHa, Prop) %>%
  mutate(survPupHa = NsurPupaHa/NsurLarvaHa, 
         Prop = log(Prop)) %>% dplyr::select(-`NsurPupaHa`)
glimpse(dadosPupHa)
modelsurv.Pup.Ha.quas.bin <- glm((NsurPupaHa/NsurLarvaHa)~Prop*Encontro,
                             family=binomial, weights = NsurPupaHa, data=dados)
summary(modelsurv.Pup.Ha.quas.bin)
anova(modelsurv.Pup.Ha.quas.bin, test="Chi")

hnp(modelsurv.Pup.Ha.quas.bin, xlab = 'Percentil da N(0,1)', ylab 
    = 'Resíduos', main = 'Gráfico Normal de Probabilidades')

PCTPupHa <- as.data.frame (exp(cbind(OR = coef(modelsurv.Pup.Ha.quas.bin), confint
                                     (modelsurv.Pup.Ha.quas.bin))))

PCTPupHa$interp = (PCTPupHa$OR -1) *100

#Larval survival for h. albuquerquei - quasiBinomial
dadosLarHa <- dados %>% dplyr::select(NsurLarvaHa, Encontro, DensInMu, Prop) %>%
  mutate(survLarHa = NsurLarvaHa/DensInMu, 
         Prop = log(Prop)) %>% dplyr::select(-`NsurLarvaHa`)
modelsurv.Ha.quas.bin <- glm(survLarHa~log(Prop)*Encontro,
                             family=quasibinomial, weights = DensInMu, data=dadosLarHa)
summary(modelsurv.Ha.quas.bin)
anova(modelsurv.Ha.quas.bin, test="Chi")

hnp(modelsurv.Ha.quas.bin, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')

PCTHa <- as.data.frame (exp(cbind(OR = coef(modelsurv.Ha.quas.bin), confint(modelsurv.Ha.quas.bin))))

PCTHa$interp = (PCTHa$OR -1) *100
   
