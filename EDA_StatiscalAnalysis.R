#importando os dados
setwd("/Users/talita.shiguemoto/Desktop/monografia")
df <- read.csv("dataset/IBM_HR_data.csv")
head(df)

#verificando as features
summary(df)

#importando bibliotecas
library(ggplot2)
library(naniar)
library(maps)
require(gridExtra)
library(epiR)

#checando missing values na df
vis_miss(df, show_perc_col=F) + 
  xlab("Vari�veis") + 
  ylab("Observa��es")

#criando paleta de cores
palette<- c(   '#ea2a58','#06598b','#6e1668', '#69c1b8', '#f6dd10','#78b343')


#plotando chart de frequencia de Attrition
ggplot(df, aes(x=Attrition))+
  geom_bar( aes(y=..prop.., group = 1),  stat="count", fill="#06598b", width =0.4) + 
  geom_text(aes(label = scales::percent(..prop..), group = 1, y=..prop..), stat = "count", vjust = -.5) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete( expand = c(0.2, 0.2)) +
  ylab("Frequ�ncia relativa das observa��es\n") +
  theme_minimal() + 
  theme(text = element_text(size=16) )

# Nomeando os nivels de educa��o
df$Educational_Levels <-  ifelse(df$Education == 1, "1. Bellow College",
                                 ifelse(df$Education == 2 , "2. College",
                                        ifelse(df$Education == 3, "3. Bachelor",
                                               ifelse(df$Education == 4, "4. Master", "5. Doctor"))))



#plotando chart
ggplot(df, aes(x=Educational_Levels, stat = 'count', fill = EducationField)) +
  geom_bar(position='fill') +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = palette)+
  ylab("Frequ�ncia") +
  xlab("\n�rea de Educa��o")+
  labs(fill = "Escolaridade") +
  theme_minimal() + 
  theme(text = element_text(size=14) )


#plotando chart
ggplot(df, aes(x=EnvironmentSatisfaction, stat = 'count', fill = Attrition)) +
  geom_bar(position='fill') +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values =  c('#06598b', '#ea2a58'))+
  ylab("Frequ�ncia") +
  xlab("\nSatisfa��o com o ambiente")+
  labs(fill = "Rotatividade") +
  theme_minimal() + 
  theme(text = element_text(size=14),  plot.title = element_text(face="bold", hjust = 0.5))+
  ggtitle("Frequ�nca de rotatividade vs Satisfa��o com o ambiente")

#plotando chart
ggplot(df, aes(x=RelationshipSatisfaction, stat = 'count', fill = Attrition)) +
  geom_bar(position='fill') +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values =  c(  '#06598b', '#ea2a58'))+
  ylab("Frequ�ncia") +
  xlab("\nSatisfa��o com os relacionamentos")+
  labs(fill = "Rotatividade") +
  theme_minimal() + 
  theme(text = element_text(size=14),  plot.title = element_text(face="bold", hjust = 0.5))+
  ggtitle("Frequ�nca de rotatividade vs Satisfa��o com os relacionamentos")

#plotando chart
ggplot(df, aes(x=JobSatisfaction, stat = 'count', fill = Attrition)) +
  geom_bar(position='fill') +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values =  c(  '#06598b', '#ea2a58'))+
  ylab("Frequ�ncia") +
  xlab("\nSatisfa��o com o trabalho")+
  labs(fill = "Rotatividade") +
  theme_minimal() + 
  theme(text = element_text(size=14),  plot.title = element_text(face="bold", hjust = 0.5))+
  ggtitle("Frequ�nca de rotatividade vs Satisfa��o com o trabalho")

#trasnformando joblevel em categorica
df$JobLevel <- as.factor(df$JobLevel)
#rename colunn
colnames(df)[colnames(df)=="�..Age"] <- "Age"

#plot scatterplot
ggplot(df, aes(x=Age, y=MonthlyIncome, color=JobLevel)) +
  geom_point() +
  theme_minimal() +
  scale_color_manual(values = palette)+
  ylab("Renda mensal") +
  xlab("\nIdade")

#boxplot
boxplot(df$DailyRate,
        horizontal = T,
        main = "Boxplot Di�ria",
        ylab = "Di�ria",
        col = "#cfedff",
        border = "#06598b"
        )

#boxplot
boxplot(df$DistanceFromHome,
        horizontal = T,
        main = "Boxplot Dist�ncia de casa",
        ylab = "Dist�ncia de casa",
        col = "#cfedff",
        border = "#06598b"
)

#boxplot
boxplot(df$MonthlyRate,
        horizontal = T,
        main = "Boxplot Subs�dios",
        ylab = "Subs�dios",
        col = "#cfedff",
        border = "#06598b"
)

#boxplot
boxplot(df$NumCompaniesWorked,
        horizontal = T,
        main = "Boxplot Empresas trabalhadas",
        ylab = "N�mero de Empresas trabalhadas",
        col = "#cfedff",
        border = "#06598b"
)

#boxplot
boxplot(df$PercentSalaryHike,
        horizontal = T,
        main = "Boxplot Percentual de aumento de sal�rio",
        ylab = "Percentual de aumento de sal�rio",
        col = "#cfedff",
        border = "#06598b"
)

#boxplot
boxplot(df$PercentSalaryHike~df$PerformanceRating,
        xlab = "N�vel de Performance",
        ylab = "Percentual de aumento de sal�rio",
        col = "#cfedff",
        border = "#06598b"
)


#boxplot
boxplot(df$DistanceFromHome~df$Attrition,
        xlab = "Rotatividade",
        ylab = "Dist�ncia do trabalho at� a casa",
        col = "#cfedff",
        border = "#06598b"
)

#boxplot
boxplot(df$NumCompaniesWorked~df$Attrition,
        xlab = "Rotatividade",
        ylab = "N� de empresas trabalhadas",
        col = "#cfedff",
        border = "#06598b"
)


#boxplot
boxplot(df$MonthlyIncome~df$Attrition,
        xlab = "Rotatividade",
        ylab = "Renda mensal",
        col = "#cfedff",
        border = "#06598b"
)



#plotando chart
ggplot(df, aes(x=Attrition, stat = 'count', fill = OverTime)) +
  geom_bar(position='fill') +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values =  c(  '#06598b', '#ea2a58'))+
  ylab("Frequ�ncia") +
  xlab("\nRotatividade")+
  labs(fill = "Hora Extra") +
  theme_minimal() + 
  theme(text = element_text(size=14))



#testando normalidade da distribui��o das vari�veis
par(mfrow=c(3,2))
qqnorm(df$DistanceFromHome,
       col = "#06598b")
qqline(df$DistanceFromHome,
       col = "#ea2a58")
hist(df$DistanceFromHome,
  xlab = "Dist�ncia de casa",
  main= "Histograma",
  col = "#cfedff",
  border = "#06598b")


#testando normalidade da distribui��o das vari�veis
qqnorm(df$NumCompaniesWorked,
       col = "#06598b")
qqline(df$NumCompaniesWorked,
       col = "#ea2a58")
hist(df$NumCompaniesWorked,
     xlab = "N� Empresas anteriores",
     main= "Histograma",
     col = "#cfedff",
     border = "#06598b")

#testando normalidade da distribui��o das vari�veis
qqnorm(df$MonthlyIncome,
       col = "#06598b")
qqline(df$MonthlyIncome,
       col = "#ea2a58")
hist(df$MonthlyIncome,
     xlab = "Renda Mensal",
     main= "Histograma",
     col = "#cfedff",
     border = "#06598b")




#testando normalidade da distribui��o das vari�veis
par(mfrow=c(3,2))
qqnorm(df$EnvironmentSatisfaction,
       col = "#06598b")
qqline(df$EnvironmentSatisfaction,
       col = "#ea2a58")
hist(df$EnvironmentSatisfaction,
     xlab = "Satisfa��o com o ambiente",
     main= "Histograma",
     col = "#cfedff",
     border = "#06598b")

qqnorm(df$JobSatisfaction,
       col = "#06598b")
qqline(df$JobSatisfaction,
       col = "#ea2a58")
hist(df$JobSatisfaction,
     xlab = "Satisfa��o o trabalho",
     main= "Histograma",
     col = "#cfedff",
     border = "#06598b")

qqnorm(df$RelationshipSatisfaction,
       col = "#06598b")
qqline(df$RelationshipSatisfaction,
       col = "#ea2a58")
hist(df$RelationshipSatisfaction,
     xlab = "Satisfa��o com relacionamento",
     main= "Histograma",
     col = "#cfedff",
     border = "#06598b")



#TRANSFORMANDO OS DADOS EM LOG ou LOG(X+1)

#transformando em log
df$log_JobSatisfaction <- log(df$JobSatisfaction)
df$log_EnvironmentSatisfaction <- log(df$EnvironmentSatisfaction)
df$log_RelationshipSatisfaction <- log(df$RelationshipSatisfaction)

#plot da transforma��o
#testando normalidade da distribui��o das vari�veis
par(mfrow=c(3,2))
qqnorm(df$log_EnvironmentSatisfaction,
       col = "#06598b")
qqline(df$log_EnvironmentSatisfaction,
       col = "#ea2a58")
hist(df$log_EnvironmentSatisfaction,
     xlab = "Satisfa��o com o ambiente em log",
     main= "Histograma",
     col = "#cfedff",
     border = "#06598b")

qqnorm(df$log_JobSatisfaction,
       col = "#06598b")
qqline(df$log_JobSatisfaction,
       col = "#ea2a58")
hist(df$log_JobSatisfaction,
     xlab = "Satisfa��o o trabalho em log",
     main= "Histograma",
     col = "#cfedff",
     border = "#06598b")

qqnorm(df$log_RelationshipSatisfaction,
       col = "#06598b")
qqline(df$log_RelationshipSatisfaction,
       col = "#ea2a58")
hist(df$log_RelationshipSatisfaction,
     xlab = "Satisfa��o com relacionamento em log",
     main= "Histograma",
     col = "#cfedff",
     border = "#06598b")


#transformando em log(x+1)
df$log_DistanceFromHome<- log(df$DistanceFromHome+1)
df$log_NumCompaniesWorked <- log(df$NumCompaniesWorked+1)
df$log_MonthlyIncome <- log(df$MonthlyIncome+1)

#plot da transforma��o
#testando normalidade da distribui��o das vari�veis
par(mfrow=c(3,2))
qqnorm(df$log_DistanceFromHome,
       col = "#06598b")
qqline(df$log_DistanceFromHome,
       col = "#ea2a58")
hist(df$log_DistanceFromHome,
     xlab = "Dist�ncia de casa em log",
     main= "Histograma",
     col = "#cfedff",
     border = "#06598b")

qqnorm(df$log_NumCompaniesWorked,
       col = "#06598b")
qqline(df$log_NumCompaniesWorked,
       col = "#ea2a58")
hist(df$log_NumCompaniesWorked,
     xlab = "N� Empresas anteriores em log",
     main= "Histograma",
     col = "#cfedff",
     border = "#06598b")

qqnorm(df$log_MonthlyIncome,
       col = "#06598b")
qqline(df$log_MonthlyIncome,
       col = "#ea2a58")
hist(df$log_MonthlyIncome,
     xlab = "Renda Mensal em log",
     main= "Histograma",
     col = "#cfedff",
     border = "#06598b")


#Teste de Shapiro-Wilk
shapiro.test(df$RelationshipSatisfaction)
shapiro.test(df$JobSatisfaction)
shapiro.test(df$EnvironmentSatisfaction)
shapiro.test(df$DistanceFromHome)
shapiro.test(df$NumCompaniesWorked)
shapiro.test(df$MonthlyIncome)


#Teste de Shapiro-Wilk em logo
shapiro.test(df$log_RelationshipSatisfaction)
shapiro.test(df$log_JobSatisfaction)
shapiro.test(df$log_EnvironmentSatisfaction)
shapiro.test(df$log_DistanceFromHome)
shapiro.test(df$log_NumCompaniesWorked)
shapiro.test(df$log_MonthlyIncome)

#histograma para verificar se por classe possui mesma forma
h1 <- ggplot(df, aes(x = EnvironmentSatisfaction)) +
  geom_histogram(aes( fill = Attrition),
                 position = "identity") +
 scale_fill_manual(values = c('#ea2a58','#06598b'))+
  ylab("Contagem\n") +
  xlab("Satisfa��o com o ambiente\n")+
  labs(fill = "Rotatividade") +
  theme_minimal() + 
  theme(text = element_text(size=12),  plot.title = element_text(face="bold", hjust = 0.5))+
  ggtitle("Histograma da Satisfa��o com o ambiente pela Rotatividade")

h2 <- ggplot(df, aes(x = JobSatisfaction)) +
  geom_histogram(aes( fill = Attrition),
                 position = "identity") +
  scale_fill_manual(values = c('#ea2a58','#06598b'))+
  ylab("Contagem\n") +
  xlab("Satisfa��o com o trabalho\n")+
  labs(fill = "Rotatividade") +
  theme_minimal() + 
  theme(text = element_text(size=12),  plot.title = element_text(face="bold", hjust = 0.5))+
  ggtitle("Histograma da Satisfa��o com o trabalho pela Rotatividade")

h3 <- ggplot(df, aes(x = RelationshipSatisfaction)) +
  geom_histogram(aes( fill = Attrition),
                 position = "identity") +
  scale_fill_manual(values = c('#ea2a58','#06598b'))+
  ylab("Contagem\n") +
  xlab("Satisfa��o com o relacionamento\n")+
  labs(fill = "Rotatividade") +
  theme_minimal() + 
  theme(text = element_text(size=12),  plot.title = element_text(face="bold", hjust = 0.5))+
  ggtitle("Histograma da Satisfa��o com o relacionamento pela Rotatividade")

grid.arrange(h1, h2, h3, nrow=3)


#outras vari�vies
h1 <- ggplot(df, aes(x = DistanceFromHome)) +
  geom_histogram(aes( fill = Attrition),
                 position = "identity") +
  scale_fill_manual(values = c('#ea2a58','#06598b'))+
  ylab("Contagem\n") +
  xlab("Dist�ncia de casa\n")+
  labs(fill = "Rotatividade") +
  theme_minimal() + 
  theme(text = element_text(size=12),  plot.title = element_text(face="bold", hjust = 0.5))+
  ggtitle("Histograma da Dist�ncia de casa pela Rotatividade")

h2 <- ggplot(df, aes(x = NumCompaniesWorked)) +
  geom_histogram(aes( fill = Attrition),
                 position = "identity") +
  scale_fill_manual(values = c('#ea2a58','#06598b'))+
  ylab("Contagem\n") +
  xlab("N� empresas trabalhadas\n")+
  labs(fill = "Rotatividade") +
  theme_minimal() + 
  theme(text = element_text(size=12),  plot.title = element_text(face="bold", hjust = 0.5))+
  ggtitle("Histograma do N� empresas trabalhadas pela Rotatividade")

h3 <- ggplot(df, aes(x = MonthlyIncome)) +
  geom_histogram(aes( fill = Attrition),
                 position = "identity") +
  scale_fill_manual(values = c('#ea2a58','#06598b'))+
  ylab("Contagem\n") +
  xlab("Renda mensal")+
  labs(fill = "Rotatividade") +
  theme_minimal() + 
  theme(text = element_text(size=12),  plot.title = element_text(face="bold", hjust = 0.5))+
  ggtitle("Histograma da Renda mensal pela Rotatividade")

grid.arrange(h1, h2, h3, nrow=3)


#Aplicando o teste Mann Whitney U / Wilcoxon Rank-Sum
wilcox.test(df$EnvironmentSatisfaction ~ df$Attrition, mu=0, alt="two.sided",
            conf.int=T, conf.level=0.95, paired=F, exact=F, correct=T)

wilcox.test(df$JobSatisfaction ~ df$Attrition, mu=0, alt="two.sided",
            conf.int=T, conf.level=0.95, paired=F, exact=F, correct=T)


wilcox.test(df$RelationshipSatisfaction ~ df$Attrition, mu=0, alt="two.sided",
            conf.int=T, conf.level=0.95, paired=F, exact=F, correct=T)


wilcox.test(df$DistanceFromHome ~ df$Attrition, mu=0, alt="two.sided",
            conf.int=T, conf.level=0.95, paired=F, exact=F, correct=T)


wilcox.test(df$NumCompaniesWorked ~ df$Attrition, mu=0, alt="two.sided",
            conf.int=T, conf.level=0.95, paired=F, exact=F, correct=T)


wilcox.test(df$MonthlyIncome ~ df$Attrition, mu=0, alt="two.sided",
            conf.int=T, conf.level=0.95, paired=F, exact=F, correct=T)

#chi square Test 
tab <- table(df$OverTime, df$Attrition)
par(mfrow=c(1,1))
barplot(tab, beside = T, legend=T,
        args.legend=list(title="Rotatividade"),
        xlab = "Overtime",
        ylab = "Contagem",
        col = c(  '#06598b', '#ea2a58')
        )

chi <- chisq.test(tab, correct=T)
chi
chi$expected
tab

epi.2by2(tab, conf.level = 0.95)

#grafico bar com m�dia entre grupos para mann-whitney u teste
par(mfrow=c(1,2))
JobSatisfaction.m <- t(tapply(df$JobSatisfaction, df$Attrition, mean))
b1 <- barplot(JobSatisfaction.m, 
        col=c('#06598b', '#ea2a58'), 
        beside=T, 
        legend=rownames(JobSatisfaction.m),
        main="M�dia de Satisfa��o com trabalho",
        xlab = "Rotavidade", border=F, ylim=c(0,5)
        )
text(b1, JobSatisfaction.m+0.2, paste(round(JobSatisfaction.m,2)) )

EnvironmentSatisfaction.m <- t(tapply(df$EnvironmentSatisfaction, df$Attrition, mean))
b1 <- barplot(EnvironmentSatisfaction.m, 
              col=c('#06598b', '#ea2a58'), 
              beside=T, 
              legend=rownames(EnvironmentSatisfaction.m),
              main="M�dia de Satisfa��o com ambiente",
              xlab = "Rotavidade", border=F, ylim=c(0,5)
)
text(b1, EnvironmentSatisfaction.m+0.2, paste(round(EnvironmentSatisfaction.m,2)) ) 


#outro grupo de plot
par(mfrow=c(1,2))
DistanceFromHome.m <- t(tapply(df$DistanceFromHome, df$Attrition, mean))
b1 <- barplot(DistanceFromHome.m, 
              col=c('#06598b', '#ea2a58'), 
              beside=T, 
              legend=rownames(DistanceFromHome.m),
              main="M�dia da dist�ncia de casa",
              xlab = "Rotavidade", border=F, ylim=c(0,max(DistanceFromHome.m)+2)
)
text(b1, DistanceFromHome.m+0.3, paste(round(DistanceFromHome.m,2)) )

MonthlyIncome.m <- t(tapply(df$MonthlyIncome, df$Attrition, mean))
b1 <- barplot(MonthlyIncome.m, 
              col=c('#06598b', '#ea2a58'), 
              beside=T, 
              legend=rownames(MonthlyIncome.m),
              main="M�dia da renda mensal",
              xlab = "Rotavidade", border=F, ylim=c(0,max(MonthlyIncome.m )+1500)
)
text(b1, MonthlyIncome.m+200, paste(round(MonthlyIncome.m,2)) ) 