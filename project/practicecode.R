library(devtools)
install_github("franvillamil/muniSpain")
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

summary(data)
unique(data$concello_nat)

data$Nome=adapt(data$Nome)

library(infoelectoral)
data
