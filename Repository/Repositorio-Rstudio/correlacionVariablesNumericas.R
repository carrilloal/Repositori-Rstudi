library(dplyr)
library(ggplot2)

setwd("/")

setwd("Users/carri/Desktop/Analisis Seminario/")
survey <- read.csv("Tesis_cleaned.csv",sep = ",",header = T)


str(survey)


# en nuestro caso nuestra encuesta solo tiene una variable de tipo numerica 
# Horas_diarias_estudio 
#la cual la correlacionaremos con la variable de tipo categorica
#Reprobacion


str(survey)
summary(survey$Horas_.diarias_.estudio)
table(survey$Horas_.diarias_.estudio)
prop.table(table(survey$Reprobaci.n))


#descriptivo
qqnorm(survey$Horas_.diarias_.estudio)
qqline(survey$Horas_.diarias_.estudio)

#comprobamos la normalidad

shapiro.test(survey$Horas_.diarias_.estudio)


boxplot(survey$Horas_.diarias_.estudio)
survey <- read.csv("Tesis_cleaned.csv",sep = ",",header = T)

survey[survey$Horas_.diarias_.estudio > 4 , "Horas_.diarias_.estudio"] <- median(survey$Horas_.diarias_.estudio)
summary(survey$Horas_.diarias_.estudio)
qqnorm(survey$Horas_.diarias_.estudio)
qqline(survey$Horas_.diarias_.estudio)
boxplot(survey$Horas_.diarias_.estudio)


shapiro.test(survey$Horas_.diarias_.estudio)

#H_O nuestra distribucion es normal
#H_A nuestra distribucion no es normal
table(survey$Horas_.diarias_.estudio,survey$Reprobaci.n)


Si_Reprobo <- survey %>% filter(Reprobaci.n == "Si") %>% select(Horas_.diarias_.estudio)
No_Reprobo <- survey %>% filter(Reprobaci.n == "No") %>% select(Horas_.diarias_.estudio)


# Si_reprobo
qqnorm(Si_Reprobo$Horas_.diarias_.estudio)
qqline(Si_Reprobo$Horas_.diarias_.estudio)
boxplot(Si_Reprobo$Horas_.diarias_.estudio)


shapiro.test(Si_Reprobo$Horas_.diarias_.estudio)


# Ni_reprobo
qqnorm(No_Reprobo$Horas_.diarias_.estudio)
qqline(No_Reprobo$Horas_.diarias_.estudio)
boxplot(No_Reprobo$Horas_.diarias_.estudio)


shapiro.test(No_Reprobo$Horas_.diarias_.estudio)

