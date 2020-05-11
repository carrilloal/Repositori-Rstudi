library(dplyr)
library(ggplot2)
library(caret)

setwd("/")

setwd("Users/carri/Desktop/Analisis Seminario/")
survey <- read.csv("Tesis_cleaned.csv",sep = ",",header = T)





table(survey$Recursos)
prop.table(table(survey$Recursos)) 


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


set <- survey[, names(survey) %in% features ]

set$Uso_.lms <- as.factor(set$Uso_.lms)

model <- glm(Uso_.lms ~ ., data = set, family = "binomial") 

model   

Importancia <- varImp(model)

Importancia

Importancia$col <- row.names(Importancia)

Importancia <- Importancia %>% arrange(-Overall)  

Importancia


ggplot(survey)+
  aes(x= Recursos, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c("#E69F00","#999999"))

survey[survey$Horas_.diarias_.estudio > 6 , "Horas_.diarias_.estudio"] <- median(survey$Horas_.diarias_.estudio)


ggplot(survey)+
  aes(x= Horas_.diarias_.estudio, fill= Uso_.lms)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c("#E69F00","#999999"))



survey$Computadora_.permanente