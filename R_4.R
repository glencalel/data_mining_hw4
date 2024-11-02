install.packages("ggplot2")
install.packages("genero")

library(arules)
library(ggplot2)
library(genero)

# Cargar todos los datos contenidos en el csv a la variable data.
data <- read.csv('C:\\MIGRACION_BDP.csv', sep=",")

colnames(data)[colnames(data) == "PEI3"] <- "SEXO"
colnames(data)[colnames(data) == "PEI4"] <- "EDAD"
colnames(data)[colnames(data) == "PEI5"] <- "ANIO"


data <- data[, !(names(data) %in% c("NUM_VIVIENDA"))]
data <- data[, !(names(data) %in% c("ZONA"))]


data_apriori <- data

cluster <- kmeans(data_apriori, centers=4)



ggplot(data_apriori, aes(x = EDAD, y = MUNICIPIO, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), aes(x=EDAD, y = MUNICIPIO), color = "black", size=4, shape=17)+
  labs(title = "Edad vs Municipio")+
  theme_minimal()


ggplot(data_apriori, aes(x = EDAD, y = DEPARTAMENTO, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), aes(x=EDAD, y = DEPARTAMENTO), color = "black", size=4, shape=17)+
  labs(title = "Edad vs Departamento")+
  theme_minimal()

datau <- subset(data_apriori, AREA==1)
clusteru <- kmeans(datau, centers=4)

ggplot(datau, aes(x = AREA, y = EDAD, color = as.factor(clusteru$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(clusteru$centers), aes(x=AREA, y = EDAD), color = "black", size=4, shape=17)+
  labs(title = "AREA URBANA vs EDAD")+
  theme_minimal()

datar <- subset(data_apriori, AREA==2)
clusterr <- kmeans(datar, centers=4)

ggplot(datar, aes(x = AREA, y = EDAD, color = as.factor(clusterr$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(clusterr$centers), aes(x=AREA, y = EDAD), color = "black", size=4, shape=17)+
  labs(title = "AREA URBANA vs EDAD")+
  theme_minimal()


