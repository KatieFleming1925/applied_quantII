library(devtools)
library(muniSpain)


options(stringsAsFactors = FALSE)
Sys.setlocale("LC_CTYPE", "C")
library(muniSpain)

## RETRIEVING DATA AND PREPARING

# Download
url = "http://vitimas.nomesevoces.net/media/base-datos.csv"
file = "victims_galicia_raw.csv"
download.file(url, file)

data = read.csv("victims_galicia_raw.csv",
  col.names = c("nombre", "apellidos", "apodo", "tipo", "sexo",
  "edad", "profesion", "concello_nat", "comarca_nat", "prov_nat", "lugar",
  "concello_vecino", "comarca_vecino", "prov_vecino", "fecha", "info"))

data = adapt(data[, c("nombre", "apellidos", "edad",
  "concello_vecino", "prov_vecino", "tipo", "fecha")])

data$concello_vecino = tolower(data$concello_vecino)

data$prov_vecino = tolower(data$prov_vecino)

data$tipo = tolower(data$tipo)

nrow(data)
summary(data)
summary(data$fecha)

range(data$fecha)

unique(data$tipo)
mode(data$fecha)

library(dplyr)

##uploading election data


