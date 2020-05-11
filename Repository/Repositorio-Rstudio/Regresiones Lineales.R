library(dplyr)
library(ggplot2)
library(caret)

setwd("/")

setwd("Users/carri/Desktop/Analisis Seminario/")
survey <- read.csv("Tesis_cleaned.csv",sep = ",",header = T)


table(survey$Computadora_.permanente)

survey$Cambio._LMS 

chisq.test(table(survey$Sobrepoblación,survey$Rango_.promedio_.clases))
str(survey)

table(survey$Sobrepoblaci.n,survey$Frecuencia_.de_.uso)


table(survey$Demora)
prop.table(table(survey$Horas_.diarias_.estudio)) 


features <- c(
  "Computadora_.permanente",
  "Conexión_.permanente",
  "Rango_.promedio_.clases",
  "Horas_.diarias_.estudio",
  "Disciplina",
  "Autodidacta",
  "Plataforma",
  "Frecuencia_.de_.uso", 
  "Recursos",
  "Uso_.lms"
)

survey$Frecuencia_.de_.uso
set <- survey[, names(survey) %in% features ]

set$Uso_.lms <- as.factor(set$Uso_.lms)

model <- glm(Uso_.lms ~ ., data = set, family = "binomial") 
 
model   

Importancia <- varImp(model)

Importancia$col <- row.names(Importancia)

Importancia <- Importancia %>% arrange(-Overall)  

Importancia
survey$Computadora_.permanente


ggplot(survey)+
  aes(x= Computadora_.permanente, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c("#999999","#E69F00"))


ggplot(survey)+
  aes(x= Disciplina, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c("#999999","#E69F00"))



ggplot(survey)+
  aes(x= Rango_.promedio_.clases, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c("#999999","#E69F00"))
  

ggplot(survey)+
  aes(x= Recursos, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c("#999999","#E69F00"))


ggplot(survey)+
  aes(x= Autodidacta, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c("#999999","#E69F00"))

survey[survey$Horas_.diarias_.estudio > 7 , "Horas_.diarias_.estudio"] <- median(survey$Horas_.diarias_.estudio)


ggplot(survey)+
  aes(x= Horas_.diarias_.estudio, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c("#999999","#E69F00"))  
  

ggplot(survey)+
  aes(x=  Exelencia_academica, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c("#999999","#E69F00"))  


ggplot(survey)+
  aes(x=   Plataforma, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c("#999999","#E69F00"))  



ggplot(survey)+
  aes(x=  Disciplina, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c("#999999","#E69F00"))  




# en caso de que la variable fuese sobrepoblacion
features2 <- c(
  "Computadora_.permanente",
  "Conexión_.permanente",
  "Rango_.promedio_.clases",
  "Disciplina",
  "Autodidacta",
  "Plataforma",
  "Recursos",
  "Exelencia_academica",
  "Horas_.diarias_.estudio",
  "Sobrepoblaci.n"
)

set2 <- survey[, names(survey) %in% features2 ]

set2$Sobrepoblaci.n <- as.factor(set2$Sobrepoblaci.n)

model2 <- glm(Sobrepoblaci.n ~ ., data = set2, family = "binomial") 

model2   

Importancia2 <- varImp(model2)

Importancia2$col <- row.names(Importancia2)

Importancia2 <- Importancia2 %>% arrange(-Overall)  

Importancia2


