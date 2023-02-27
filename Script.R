
#ler banco de dados
data<-readxl::read_excel("data.xlsx", sheet = "publicData")
#estrutura do banco
str(data)

#transformar covariaveis em fator e idade em valor 
data<-cbind(lapply(data[,1:10],as.factor), (data[,11:13]))
data$idade<-round(data$age10*10,0)

str(data)

#Apenas pacientes em hemodialise
data<-subset(data,dialysis == 1 )


#Objeto surival
y <- survival::Surv(data$Time_to_Event, data$dead)

#Análise exploratória
# Tabela 1


tab1<- tableone::CreateTableOne(vars = c("Gender","DM","hypert",
                                            "heart","" ,"neoplasia",
                                            "liver", "vascular", "COPD",
                                         "idade"), 
                                   strata = "dead",test = TRUE, data = data)

tabela1<-print (tab1, showAllLevels = TRUE,nonnormal = "idade")


# Estimando as curvas de sobrevivência pelo Kaplan-Meier para diversas variáveis
KMgender <- survival::survfit(y ~ Gender, data = data)
KMDM <- survival::survfit(y ~ DM, data = data)
KMhypert <- survival::survfit(y ~ hypert, data = data)
KMheart <- survival::survfit(y ~ heart, data = data)
KMneo <- survival::survfit(y ~ neoplasia, data = data)
KMliver <- survival::survfit(y ~ liver, data = data)
KMvascular <- survival::survfit(y ~ vascular, data = data)
KMCOPD <- survival::survfit(y ~ COPD, data = data)


# Gráfico das curvas de Kaplan-Meier
par(mfrow = c(2, 2))
#SEXO
plot(KMgender, main = "Sexo", col = 1:2)
legend (100, 0.2, c("F", "M"), col=1:2,lty=1)
# DM
plot(KMDM, main = "DM", col = 1:2)
legend(100,0.2, c("Não", "Sim"), col=1:2, lty=1)
# HIPERTENSAO
plot(KMhypert, main = "Hipertensão", col = 1:2)
legend (100, 0.2, c("Não", "Sim"), col=1:2, lty=1)
# HEART
plot(KMheart, main = "Coração", col = 1:3)
legend (500, 1.1, c("Não", "Sim"), col=1:2, lty=1)

par(mfrow = c(2, 2))
#Neoplasia
plot(KMneo, main = "Neolasia", col = 1:2)
legend (100, 0.2, c("Não", "Sim"), col=1:2,lty=1)
# Liver
plot(KMliver, main = "Fígado", col = 1:2)
legend(100,0.2, c("Não", "Sim"), col=1:2, lty=1)
# Vascular
plot(KMvascular, main = "Vascular", col = 1:2)
legend (100, 0.2, c("Não", "Sim"), col=1:2, lty=1)
# DPOC
plot(KMheart, main = "DPOC", col = 1:3)
legend (500, 1.1, c("Não", "Sim"), col=1:2, lty=1)

