#Plot - survival percentage larval M. domestica
Sur.larv.Md <- ggplot(Tabela_Hydrotaea_vc_MuscaR, aes(x=DensInMu, y=`%surv.larva.Md` , group=DensInMu))+ 
  geom_boxplot(aes(fill=DensInMu))
Sur.larv.Md <- Sur.larv.Md + facet_wrap(~ Encontro)  +
  labs(subtitle = ,
       x = "Density",
       y = "Musca larval Survival (%)")
Sur.larv.Md <- Sur.larv.Md + scale_y_log10()
ggsave("M. domestica larval survival.pdf", units="in", width=5, height=4, dpi=300)

#Plot da sobrevivencia pupal Md
Sur.pup.Md <- ggplot(dados, aes(x=log(DensInMu), y=`%Surv.p.Md` , group=DensInMu))+ 
  geom_boxplot(aes(fill=DensInMu))
Sur.pup.Md <- Sur.pup.Md + facet_wrap(~ Encontro) +
  labs(subtitle = ,
       x = "Density",
       y = "Musca pupal Survival (%)")
ggsave("M. domestica pupal survival.pdf", units="in", width=5, height=4, dpi=300)

#Plot da capacidade predatoria
Pred.cap.Hyd <- ggplot(Tabela_Hydrotaea_vc_MuscaR, aes(x=log(DensInMu), y= `Ndeath.larva.Md/N.Surv.Larv.Ha` , group=DensInMu))+ 
  geom_boxplot(aes(fill=DensInMu))
Pred.cap.Hyd <- Pred.cap.Hyd + facet_wrap(~ Encontro)

#Plot da sobrevivencia pupal de H. albuquerquei
Sur.pup.Ha <- ggplot(Tabela_Hydrotaea_vc_MuscaR, aes(x=DensInMu, y= `%SHaPupal`, group=DensInMu))+ 
  geom_boxplot(aes(fill=DensInMu))
Sur.pup.Ha <- Sur.pup.Ha + facet_wrap(~ Encontro) +
  labs(subtitle = ,
       x = "Density",
       y = "Hydrotaea pupal Survival (%)")
ggsave("H. albuquerquei pupal survival.pdf", units="in", width=5, height=4, dpi=300)

#Plot da sobrevivencia larval de H. albuquerquei
Sur.larva.Ha <- ggplot(Tabela_Hydrotaea_vc_MuscaR, aes(x=DensInMu, y=`%surv.larva.Ha`, group=DensInMu))+ 
  geom_boxplot(aes(fill=DensInMu))
Sur.larva.Ha <- Sur.larva.Ha + facet_wrap(~ Encontro) +
  labs(subtitle = ,
       x = "Density",
       y = "Hydrotaea larval Survival (%)")
ggsave("H. albuquerquei larval survival.pdf", units="in", width=5, height=4, dpi=300)

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
                       
dados4$Survival=Psurvival

Density<-data.frame(Density=c(dados$DensInMu, dados$DensInMu))

Encounter<-data.frame(Encounter =c(dados$Encontro, dados$Encontro))

#adicionar ao df
Specie <- dados(Specie)

hnp(model2, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')

PCT.p.Md <- as.data.frame (exp(cbind(OR = coef(model2), confint(model2))))

PCT.p.Md$interp = (PCT.p.Md$OR -1) *100

#Larval survival
paleta2 <- c("indianred", "darkorange", "gold", "royalblue")
graph.1 <- ggplot(data=dados3, aes(x=as.factor(log(Density)), y=Survival/Density, fill=Species)) +
  geom_point(size=3, pch=21) +
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        title =element_text(size=12, hjust = 1))+
  scale_color_manual(values = paleta2) +
  scale_fill_manual(values = paleta2) +
  facet_wrap(Encounter ~., nrow=2) +
  labs(subtitle =  ,
       x = "Density (log)",
       y = "Survival rate") +
  geom_smooth(method="glm", aes(color = Species),
              method.args = list(family = "quasibinomial")) +
  theme(legend.position='bottom',text=element_text(size=12))
ggsave("Larval survival.pdf", units="in", width=5, height=4, dpi=300)

graph.1 + scale_x_discrete(labels=c("100" = "1:1", "160" = "1:4",
                          "180" = "1:9", "190" = "1:19", "195" = "1:39"))
# caption = "Statistical significances: Stage: p=0.0021; Density: p=0.0095); 
#H1M1: Encounter between 1º stage f predator and 1º of prey;
#H2M1: Encounter between 2º stage of predator and 1º of prey;
#H3M1: Encounter between 3º stage of predator and 1º of prey;
#H3M2: Encounter between 3º stage of predator and 2º of prey."

#Pupal survival
library(ggplot2)
paleta2 <- c("indianred", "darkorange", "gold", "royalblue")
ggplot(data=dados4, aes(x=log(Density), y=Survival, fill=Species)) +
  geom_point(size=3, pch=21) + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        title =element_text(size=12, hjust = 1))+
  scale_color_manual(values = paleta2) +
  scale_fill_manual(values = paleta2) +
    facet_wrap(Encounter ~., nrow=2) +
  labs(subtitle = ,
       x = "Density (log)",
       y = "Survival rate") +
  geom_smooth(method="glm", aes(color = Species), 
              method.args = list(family = "quasibinomial"))+  
  theme(legend.position='bottom',text=element_text(size=12))
ggsave("Pupal survival.pdf", units="in", width=5, height=4, dpi=300)

# "Figure 2. Pupal survival of Hydrotaea albuquerquei and Musca Domestica at all encounters with respective densities")


#Predatory capacity
ggplot(data=dados, aes(x=log(DensInMu), y=`Ndeath.larva.Md/N.Surv.Larv.Ha`)) +
  geom_point()  +
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        title =element_text(size=12, hjust = 1))+
  scale_color_manual(values = paleta2) +
  scale_fill_manual(values = paleta2) +
  facet_wrap(Encontro ~., nrow=2) +
  labs(x = "Density (log)",
       y = "Predatory Capacity") +
  geom_smooth(method="glm", aes(color = DensInMu),
              method.args = list(family = "quasipoisson")) +
  theme(legend.position='top',text=element_text(size=12))
ggsave("Predatory capacity.pdf", units="in", width=5, height=4, dpi=300)

#"Figure 3. Numbers of Musca domestica larval deaths to each living larvae of Hydrotaea albuquerquei"
#desenvolvimento das larvas de Musca domestica



#larval Development rate
ggplot(data=dados5, aes(x=log(Density), y=DevRate, fill=Species)) +
  geom_point(size=3, pch=21)  +
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        title =element_text(size=12, hjust = .5))+
  scale_color_manual(values = paleta2) +
  scale_fill_manual(values = paleta2) +
  facet_wrap(Encounter ~., nrow=2) +
  labs(x = "Density (log)",
       y = "Development rate") +
  geom_smooth(method="glm", aes(color = Species),
              method.args = list(family = "gaussian")) +
  theme(legend.position='bottom',text=element_text(size=12))
ggsave("Larval development rate.pdf", units="in", width=5, height=4, dpi=300)

#graph
#Pupal Development rate
ggplot(data=dados6, aes(x=log(Density), y=DevRate, fill=Species)) +
  geom_point(size=3, pch=21)  +
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        title =element_text(size=12, hjust = .5))+
  scale_color_manual(values = paleta2) +
  scale_fill_manual(values = paleta2) +
  facet_wrap(Encounter ~., nrow=2) +
  labs(x = "Density (log)",
       y = "Pupal Development rate") +
  geom_smooth(method="glm", aes(color = Species),
              method.args = list(family = "gaussian")) +
  theme(legend.position='bottom',text=element_text(size=12))
ggsave("Pupal development rate.pdf", units="in", width=5, height=4, dpi=300)
#Larval development rate (1/days) of Musca domestica

# larval development rate of Hydrotaea
#graph
library(ggplot2)
paleta2 <- c("indianred", "darkorange", "gold", "royalblue")
ggplot(data=dados, aes(x=log(Prop), y=taxa.larv.Ha, fill=Encontro)) +
  geom_point(size=5, pch=21) +
  scale_color_manual(values = paleta2) +
  scale_fill_manual(values = paleta2) +
  facet_wrap(Encontro ~., nrow=2) +
  labs(subtitle = "Taxa de desenvolvimento (1/dias) das larvas de Hydrotaea albuquerquei",
       x = "log da Densidade de larvas",
       y = "Taxa de desenvolvimento (1/dias)") +
  geom_smooth(method="glm", aes(color = Encontro),
              method.args = list(family = "quasipoisson"))

#pupal development rate of Hydrotaea

#graph
ggplot(data=dados, aes(x=log(Prop), y=taxa.p.Ha, fill=Encontro)) +
  geom_point(size=5, pch=21) +
  scale_color_manual(values = paleta2) +
  scale_fill_manual(values = paleta2) +
  facet_wrap(Encontro ~., nrow=2) +
  labs(subtitle = "Taxa de desenvolvimento (1/dias) das larvas de Hydrotaea albuquerquei",
       x = "log da Densidade de larvas",
       y = "Taxa de desenvolvimento (1/dias)") +
  geom_smooth(method="glm", aes(color = Encontro),
              method.args = list(family = "quasipoisson"))
              


