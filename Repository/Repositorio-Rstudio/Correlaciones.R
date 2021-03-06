library(dplyr)
library(ggplot2)

setwd("/")

setwd("Users/carri/Desktop/Analisis Seminario/")
survey <- read.csv("Tesis_cleaned.csv",sep = ",",header = T)

summary(str(survey))
table(survey$Sobrepoblaci.n)
table(survey$Aumento._de._la.poblaci.n)

table(survey$Computadora_.permanente)
table(survey$Conexi.n_.permanente)
table(summary(survey))
table(survey$Procedencia)
str(survey$Procedencia)
names(survey)

##LAS PERSONAS QUE POSEEN UNA COMPUTADORA TIENEN UNA CONEXION PERMANENTE

prop.table(table(survey$Computadora_.permanente,survey$Conexi.n_.permanente),2)

ggplot(survey)+
  aes(x= Computadora_.permanente, fill= Conexi.n_.permanente)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey)+
  aes(x= Computadora_.permanente, fill= Conexi.n_.permanente)+
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 45))




chisq.test(table(survey$Computadora_.permanente,survey$Conexi.n_.permanente))

#H_O LAS CATEGORIAS COMPUTADORA PERMANENTE Y CONEXION PERMANENTE SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE RECHASAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON DEPENDIENTES






##EL AUMENTO DE POBLACION TIENE UN EFECTO ENE EL INDICE ACADEMICO

prop.table(table(survey$Rango_.acad.mico,survey$Aumento._de._la.poblaci.n),2)

ggplot(survey)+
  aes(x= Rango_.acad.mico, fill= Aumento._de._la.poblaci.n)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(survey$Rango_.acad.mico,survey$Aumento._de._la.poblaci.n))

#H_O LAS CATEGORIAS RANGO ACADEMICO Y AUMENTO EN LA POBLACION SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE RECHASAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON DEPENDIENTES

##CONSIDERA QUE EL USO DE LAS PLATAFORMAS SON DE IMPORTANCIA VS FRECUENCIA DE USO
table(survey$Plataforma)
table(survey$Frecuencia_.de_.uso)

prop.table(table(survey$Frecuencia_.de_.uso,survey$Plataforma),2)

ggplot(survey)+
  aes(x= Frecuencia_.de_.uso, fill= Plataforma)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))


chisq.test(table(survey$Frecuencia_.de_.uso,survey$Plataforma))

#H_O LAS CATEGORIAS FRECUENCIA DE USO Y PLATAFORMA SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE RECHASAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON DEPENDIENTES




prop.table(table(survey$Aumento_.de_.uso,survey$Plataforma),2)

ggplot(survey)+
  aes(x= Aumento_.de_.uso, fill= Plataforma)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))


chisq.test(table(survey$Aumento_.de_.uso,survey$Plataforma))

#H_O LAS CATEGORIAS AUMENTO DE USO Y PLATAFORMA SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE RECHASAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON DEPENDIENTES




## LA DICIPLINA DEL ESTUDIANTE MEJORA SU INDICE 

prop.table(table(survey$Disciplina,survey$Rango_.acad.mico),2)

ggplot(survey)+
  aes(x= Disciplina, fill= Rango_.acad.mico)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(survey$Disciplina,survey$Rango_.acad.mico))

#H_O LAS CATEGORIAS DICIPLINA Y RANGO ACADEMICO SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE RECHASAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON DEPENDIENTES


## LOS QUE SON AUTO DIDACTAS TIENEN UN BUEN INDICE ACADEMICO
prop.table(table(survey$Autodidacta,survey$Rango_.acad.mico),2)

ggplot(survey)+
  aes(x= Autodidacta, fill= Rango_.acad.mico)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(survey$Autodidacta,survey$Rango_.acad.mico))

#H_O LAS CATEGORIAS AUTODIDACTA Y RANGO ACADEMICO SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE RECHASAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON DEPENDIENTES


## ESTIMACION DE HORAS LIBRES CON HORAS LIBRES
table(survey$Horas_.libres)
table(survey$Rango_.acad.mico)

prop.table(table(survey$Rango_.acad.mico,survey$Horas_.libres),2)

ggplot(survey)+
  aes(x= Rango_.acad.mico, fill= Horas_.libres)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(survey$Rango_.acad.mico,survey$Horas_.libres))

#H_O LAS CATEGORIAS  RANGO INDICE Y HORAS LIBRES ACADEMICO SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE RECHASAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON DEPENDIENTES


## ESTIMACION DE HORAS LIBRES CON HORAS LIBRES
table(survey$Rango_.acad.mico)
table(survey$Estimaci.n_.horas_.libres)

prop.table(table(survey$Rango_.acad.mico,survey$Estimaci.n_.horas_.libres),2)

ggplot(survey)+
  aes(x= Rango_.acad.mico, fill= Estimaci.n_.horas_.libres)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(survey$Estimaci.n_.horas_.libres,survey$Horas_.libres))

#H_O LAS CATEGORIAS  RANGO INDICE Y HORAS LIBRES ACADEMICO SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE RECHASAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON DEPENDIENTES





## LAS ERSONAS QUE ESTAN EN LISTA DE ESPERA CONCIDERAN LA APERTURA DE NUEVOS CUPOS
table(survey$Lista_.espera)
table(survey$Mas_.cupos)

prop.table(table(survey$Lista_.espera,survey$Mas_.cupos),2)

ggplot(survey)+
  aes(x= Lista_.espera, fill= Mas_.cupos)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(survey$Lista_.espera,survey$Mas_.cupos))

#H_O LAS CATEGORIAS  LISTA ESPERA Y MAS CUPOS SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE RECHASAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON DEPENDIENTES



## LA SOBRECARGA LABORAL GENERA UN MENOR RENDIMIENTO ACADEMICO
table(survey$Sobrecarga_.laboral)
table(survey$Rango_.acad.mico)

prop.table(table(survey$Rango_.acad.mico,survey$Sobrecarga_.laboral),2)

ggplot(survey)+
  aes(x= Rango_.acad.mico, fill= Sobrecarga_.laboral)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(survey$Rango_.acad.mico,survey$Sobrecarga_.laboral))

#H_O LAS CATEGORIAS  RANGO INDICE Y HORAS LIBRES ACADEMICO SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE RECHASAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON DEPENDIENTES


## LAS CONDICIONES DE SOBREPOBLACION GENERAN PROBLEMAS DE RETRAZO EN LA CARRERA
table(survey$Demora)
table(survey$Sobrepoblaci.n)

prop.table(table(survey$Demora,survey$Sobrepoblaci.n),2)

ggplot(survey)+
  aes(x= Demora, fill= Sobrepoblaci.n)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(survey$Demora,survey$Sobrepoblaci.n))

#H_O LAS CATEGORIAS  DEMORA Y SOBREPOBLACION SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE RECHASAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON DEPENDIENTES


## LOS ESTUDIANTES QUE HAN USADO LMS HAN SUFRIDO UN AUMENTO EN SU USO DEBIDO A LA PROBLEMATICA ACTUAL
table(survey$Uso_.lms)
table(survey$Aumento_.de_.uso)

prop.table(table(survey$Aumento_.de_.uso,survey$Uso_.lms),2)

ggplot(survey)+
  aes(x= Aumento_.de_.uso , fill= Uso_.lms)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(survey$Uso_.lms,survey$Aumento_.de_.uso))

#H_O LAS CATEGORIAS  USO LMS Y AUMENTO USO PLATAFORMA SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE HACEPTAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON INDEPENDIENTES



## COMO SE SIENTE EL ESTUDIANTE EN LA ACTUALIDAD CON LA CARRERA Y SI HA PENSADO EN AVANDONAR
table(survey$Seguimiento)
table(survey$Deserci.n)

prop.table(table(survey$Seguimiento,survey$Deserci.n),2)

ggplot(survey)+
  aes(x= Seguimiento, fill= Deserci.n)+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(survey$Seguimiento,survey$Deserci.n))

#H_O LAS CATEGORIAS  SEGUIMIENTO Y DESERCION SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE HACEPTAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON INDEPENDIENTES

## COMO SE SIENTE EL ESTUDIANTE EN LA ACTUALIDAD CON LA CARRERA Y QUE LOSMITIVO A ELEGIRLA
table(survey$Seguimiento)
table(survey$Deserci.n)

prop.table(table(survey$Motivo_.estudio,survey$Seguimiento),2)

ggplot(survey)+
  aes(x= Motivo_.estudio, fill=Seguimiento )+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(survey$Seguimiento,survey$Motivo_.estudio))

#H_O LAS CATEGORIAS  MOTIVO ESTUDIO Y SEGUIMIENTO SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE RECHASAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON DEPENDIENTES


table(survey$Exelencia_academica)



## los alumnos que si han tenido excelencia academica ver el indice en el que se encuentran
table(survey$Exelencia_academica)
table(survey$Rango_.acad.mico)

prop.table(table(survey$Rango_.acad.mico,survey$Exelencia_academica),1)

ggplot(survey)+
  aes(x= Rango_.acad.mico, fill=Exelencia_academica )+
  geom_bar(position = "stack")+
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(survey$Rango_.acad.mico,survey$Exelencia_academica))

#H_O LAS CATEGORIAS  MOTIVO ESTUDIO Y SEGUIMIENTO SON INDEPENDIENTES
#H_A LAS CATEGORIAS SON DEPENDIENTES

# ACEPTAMOS NUESTRA HIPOTESIS NULA CUANDO EL P_VALUE DE NUESTRA PRUEBA ES MENOR A 0.05

#CONCLUSION: SEGUN NUESTRO P_VALUE RECHASAMOS NUESTRA HIPOTESIS NULA , POR LO TANTO NUESTAS VARIABLES SON DEPENDIENTES


table(survey$Estudio_.secundaria)




table(survey$Cambio._LMS....)



