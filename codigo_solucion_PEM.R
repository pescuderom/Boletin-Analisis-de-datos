################################################################################
# # Prueba Tecnica Analista de datos-ADRES 
# # R Versions: R version 4.3.1
# #
# # Author(s): Patricia Escudero Montañez
# #
# # Prueba Técnica	
# # Description: Función creada para caracterizar a la población mediante consultas de SQL.
# #
# # File history:
# #   20240624: Creation
# #   
# # ###########################################################################

##Borrar memoria
rm(list=ls())
##Instalar la librería RSQLite y readxl
install.packages("readxl")
install.packages("DBI")
install.packages("RSQLite")
install.packages("openxlsx")

##Cargar la librería
library(RSQLite)
library(readxl)
library(DBI)
library(gdata)
library(openxlsx)
library(data.table)
#options(encoding = "UTF-8")

## Cargar en R los archivos de extension .xlsx 
#Cuenta con 1118 Municipios
ruta_munic <- read_excel("C:\\Users\\57311\\Desktop\\Patricia\\Proyectos\\ADRES\\Municipios\\Municipios.xlsx")
nrow(ruta_munic)
#Cuenta con 60946 registro
ruta_presta <- read_excel("C:\\Users\\57311\\Desktop\\Patricia\\Proyectos\\ADRES\\Prestadores\\Prestadores.xlsx")
nrow(ruta_presta)

##Limpiar la base Municipio
#Se carga la base ETC_MUNI (Fuente DANE) que tiene la informacion de los Municipios y los departamentos
#Se agrega el código Deprtamento Municipio 05030, ya que la base enviada no la contiene
#Se confirma completitud de bases
#1124 Municipios con sus departamentos
datos <- fread("C:/Users/57311/Desktop/Patricia/Proyectos/ADRES/ETC_MUNI.txt")  
#Quitar variables de la base de municipio y datos
municipio <- subset(ruta_munic, select = -c(Departamento, Municipio))
Datos <- subset(datos, select = -c(Dep, id_ente))

##Volver numerica la variable Depmun en la base de datos de municipio
municipio$Depmun<-as.integer(municipio$Depmun)

#Se hace un merge para limpiar caracteres de Muniicipio y Departamento de la base ruta_munic
Municipio <- merge(municipio, Datos, by = "Depmun")

#Cambiar nombre a la base ruta_presta
Prestadores <- ruta_presta
##Las bases limpias son Municipio y ruta_presta

##Conectar datos a la base de datos SQLite
municipio_sql <- dbConnect(RSQLite::SQLite(), dbname = "Municipio.db")
prestadores_sql <- dbConnect(RSQLite::SQLite(), dbname = "ruta_presta.db")

##Escribir el dataframe en la base de datos SQLite
dbWriteTable(municipio_sql, name = "Municipios", value = Municipio, overwrite = TRUE)
dbWriteTable(prestadores_sql, name = "Prestadores", value = Prestadores, overwrite = TRUE)

###OBJETIVO DEL ANALISIS

##EXPLORACION DE DATOS
##Iniciar consultas en RSQLite


#Se inicia mirando el contenido de cada una de las bases de datos desde SQL
#Se miran los seis primeros registros de las tablas
query_1 <- "SELECT * FROM Municipios LIMIT 6"
resultado_1 <- dbGetQuery(municipio_sql, query_1)

query_2 <- "SELECT * FROM Prestadores LIMIT 6"
resultado_2 <- dbGetQuery(prestadores_sql, query_2)

# Número de registros que tienen cada una de las bases de datos
#Se tienen 1118 municipios
query_3 <- "SELECT COUNT(*) FROM Municipios"
resultado_3 <- dbGetQuery(municipio_sql, query_3)

#Se tienen 60946 registros prestadores de servicios
query_4 <- "SELECT COUNT(*) FROM Prestadores"
resultado_4 <- dbGetQuery(prestadores_sql, query_4)

## Categorias unicas por region
query_5 <- "SELECT DISTINCT Region 
FROM Municipios"
resultado_5 <- dbGetQuery(municipio_sql, query_3)

#Numero de municipios que hacen parte de cada region
#Conteos de municipios que hacen parte del departamento de  "Region Caribe"
query_6 <- "SELECT * FROM Municipios WHERE Region = 'Región Caribe'"
resultado_6 <- dbGetQuery(municipio_sql, query_6)

#Conteos de municipios que hacen parte del departamento de  "Región Eje Cafetero"
query_7 <- "SELECT * FROM Municipios WHERE Region = 'Región Eje Cafetero'"
resultado_7 <- dbGetQuery(municipio_sql, query_7)

#Conteos de municipios que hacen parte del departamento de  "Región Centro Oriente"
query_8 <- "SELECT * FROM Municipios WHERE Region = 'Región Centro Oriente'"
resultado_8 <- dbGetQuery(municipio_sql, query_8)

#Conteos de municipios que hacen parte del departamento de  "Región Centro Sur"
query_9 <- "SELECT * FROM Municipios WHERE Region = 'Región Centro Sur'"
resultado_9 <- dbGetQuery(municipio_sql, query_9)

#Conteos de municipios que hacen parte del departamento de  "Región Centro Sur"
query_10 <- "SELECT * FROM Municipios WHERE Region = 'Región Centro Sur'"
resultado_10 <- dbGetQuery(municipio_sql, query_10)

#Conteos de municipios que hacen parte del departamento de  "Región Pacífico"
query_11 <- "SELECT * FROM Municipios WHERE Region = 'Región Pacífico'"
resultado_11 <- dbGetQuery(municipio_sql, query_11)

#Conteos de municipios que hacen parte del departamento de  "Región Llano"
query_12 <- "SELECT * FROM Municipios WHERE Region = 'Región Llano'"
resultado_12 <- dbGetQuery(municipio_sql, query_12)

#Región con menos de 100 municipios
query_13 <- "SELECT region, COUNT(*) AS Municipio
FROM Municipios
GROUP BY region
HAVING COUNT(*) < 100
ORDER BY Municipio"
resultado_13 <- dbGetQuery(municipio_sql, query_13)

#Región con mas de 300 municipios
query_14 <- "SELECT region, COUNT(*) AS Municipio
FROM Municipios
GROUP BY region
HAVING COUNT(*) > 300
ORDER BY Municipio"
resultado_14 <- dbGetQuery(municipio_sql, query_14)

#En qué región se agrupa la menor cantidad de poblacion
query_15<-"SELECT Region, MIN(Poblacion) AS Poblacion
FROM Municipios"
resultado_15 <- dbGetQuery(municipio_sql, query_15)

#En qué región se agrupa la mayor cantidad de poblacion
query_16<-"SELECT Region, MAX(Poblacion) AS Poblacion
FROM Municipios"
resultado_16 <- dbGetQuery(municipio_sql, query_16)

#Pormedio de población en los municipios
query_17<-"SELECT AVG(Poblacion) AS promedio_poblacion
FROM Municipios"
resultado_17 <- dbGetQuery(municipio_sql, query_17)

##Gráfico de Municipios por Region
ggplot(Municipio, aes(x = Region, y = Municipio, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Municipios por Región",
       x = "Región", y = "Municipios") +
  theme_minimal() 

##########################################################
##Consultas para el análsis de los prestadores de servicio de salud
## Categorias unicas por municicpio
query_1 <- "SELECT DISTINCT muni_nombre 
FROM Prestadores"
resultado_1 <- dbGetQuery(prestadores_sql, query_1)

#Agrupar por departamentos
query_2 <- "SELECT DISTINCT clpr_nombre
FROM Prestadores"
resultado_2 <- dbGetQuery(prestadores_sql, query_2)

#Conteos por categorias de la variable clpr_nombre
query_3 <- "SELECT clpr_nombre, COUNT(*) AS cantidad_clp
FROM Prestadores
GROUP BY clpr_nombre"
resultado_3 <- dbGetQuery(prestadores_sql, query_3)

#Conteos por categorias de la variable clpr_nombre
query_3 <- "SELECT clpr_nombre, COUNT(*) AS cantidad_clp
FROM Prestadores
GROUP BY clpr_nombre"
resultado_3 <- dbGetQuery(prestadores_sql, query_3)

#Conteos por categorias de la variable clase_persona
query_4 <- "SELECT clase_persona, COUNT(*) AS cantidad_clase
FROM Prestadores
GROUP BY clase_persona"
resultado_4 <- dbGetQuery(prestadores_sql, query_4)