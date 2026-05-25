## Independent Variable: repression level from Nomes e Voces

options(stringsAsFactors = FALSE)
Sys.setlocale("LC_CTYPE", "C")
library(muniSpain)

## RETRIEVING DATA AND PREPARING

# Download
url = "http://vitimas.nomesevoces.net/media/base-datos.csv"
file = "victims_galicia_raw.csv"
download.file(url, file)

# Load
data = read.csv("victims_galicia_raw.csv",
  col.names = c("nombre", "apellidos", "apodo", "tipo", "sexo",
  "edad", "profesion", "concello_nat", "comarca_nat", "prov_nat", "lugar",
  "concello_vecino", "comarca_vecino", "prov_vecino", "fecha", "info"))
data = adapt(data[, c("nombre", "apellidos", "edad",
  "concello_vecino", "prov_vecino", "tipo", "fecha")])
data$concello_vecino = tolower(data$concello_vecino)
data$prov_vecino = tolower(data$prov_vecino)
data$tipo = tolower(data$tipo)

## SUBSETTING

# Only victims with known death date
data = subset(data, fecha != "0000-00-00")
# NOTE: Data includes victims of non-fatal repression (did not die),
# that's why the high numbers sometimes.

# Transform to date class
data$fecha = as.Date(data$fecha, "%Y-%m-%d")

# Only 'paseos' and 'execucions'. Excluding deaths in prison and others.
data = subset(data, tipo %in% c("paseo", "execucion"))

# Exclude deaths after 1942 (0.2%)
data = subset(data, fecha <= "1942-12-31")

## ASSIGNING MUNICIPALITY CODES
# (Following place where victims lived)

# Exclude those without locality info
data$concello_vecino[data$concello_vecino %in% c("", "desconecido")] = NA
data = subset(data, !is.na(concello_vecino))

# Converting names
tmp = c("a estrada", "a pobra do caraminal", "o grove", "a coruna",
  "a caniza", "o porrino", "as neves", "a guarda", "o rosal", "o corgo",
  "a pontenova", "o barco de valdeorras", "o incio", "o savinao",
  "a pobra do brollon", "a fonsagrada", "as pontes de garcia rodriguez",
  "as somozas", "a mezquita", "a laracha", "o vicedo", "o pereiro de aguiar",
  "a arnoia", "a bana", "a rua", "a pobra de trives", "a gudina",
  "a veiga", "o carballino", "o irixo", "o bolo", "a pastoriza", "a lama")
data$concello_vecino[data$concello_vecino %in% tmp] = gsub("(a|o|as) (.*)", "\\2, \\1",
  data$concello_vecino[data$concello_vecino %in% tmp])
data$concello_vecino[data$concello_vecino == "a illa de arousa"] = "illa de arousa (a)"
data$concello_vecino[data$concello_vecino == "a merca"] = "merca, a"
# Missing province names
data$prov_vecino[data$concello_vecino == "corcubion" &
  data$prov_vecino == ""] = "a coruna"
data$prov_vecino[data$concello_vecino == "cee" &
  data$prov_vecino == ""] = "a coruna"
data$prov_vecino[data$concello_vecino == "mondariz-balneario" &
  data$prov_vecino == ""] = "pontevedra"
data$prov_vecino[data$concello_vecino == "fisterra" &
  data$prov_vecino == ""] = "a coruna"

# Assigning INE codes
data$muni_code = name_to_code(data$concello_vecino, prov = data$prov_vecino)

# Adapting to municipality changes between 1930 and 2011
data$muni_code = changes_newcode(data$muni_code, 1930, 2011)

write.csv(data, "victims_galicia.csv", row.names = FALSE)
victims=data

## AGGREGATING REPRESSION TO MUNICIPALITY LEVEL

# Loading datasets needed
census=read.csv("finalproject/data/INE_census.csv")

# Aggregating victim counts
repression=as.data.frame(table(victims$muni_code))
names(repression)=c("muni_code", "n_victims")
repression$muni_code=as.integer(as.character(repression$muni_code))
census$muni_code=as.integer(census$muni_code)

# Merge with 1930 census
repression=merge(repression, census[,c("muni_code", "c1930")], by="muni_code", all.x=TRUE)

# Scaling repression counts per 1000 inhabitants in municipality population
repression$repression_pc=repression$n_victims/repression$c1930*1000

write.csv(repression, "repression.csv", row.names=FALSE)

## DEPENDENT VARIABLE: Pro-Secessionist Party Voteshares in Regional Elections

## RETRIEVING RAW DATA FROM XUNTA DE GALICIA

base_url <- "https://abertos.xunta.gal/catalogo/administracion-publica/-/dataset"

# 2001-2016: single file per year
single_file_years <- list(
  "2001" = "0263",
  "2005" = "0261",
  "2009" = "0260",
  "2012" = "0048",
  "2016" = "0371"
)

for (yr in names(single_file_years)) {
  id  <- single_file_years[[yr]]
  url <- paste0(base_url, "/", id, "/eleccions-parlamento-galicia-resultados-", yr,
                "/001/descarga-directa-ficheiro.csv")
  download.file(url, paste0("elections_", yr, ".csv"))
}

# 2020 and 2024: split by province
urls <- list(
  "2020" = c(
    lugo       = paste0(base_url, "/0426/eleccions-parlamento-galicia-resultados-2020/005/descarga-directa-ficheiro.csv"),
    ourense    = paste0(base_url, "/0426/eleccions-parlamento-galicia-resultados-2020/009/descarga-directa-ficheiro.csv")
  ),
  "2024" = c(
    lugo       = paste0(base_url, "/0656/eleccions-parlamento-galicia-resultados-2024/005/descarga-directa-ficheiro.csv"),
    ourense    = paste0(base_url, "/0656/eleccions-parlamento-galicia-resultados-2024/009/descarga-directa-ficheiro.csv")
  )
)

for (yr in names(urls)) {
  for (prov in names(urls[[yr]])) {
    download.file(urls[[yr]][prov], paste0("elections_", yr, "_", prov, ".csv"))
  }
}

## BASELINE/PRE-TREATMENT: 1936 General Election Results

library(readxl)

## LUGO

# making raw vote counts in 1936 results into percentage
lugo_36=read.csv("finalproject/data/lugo1936.csv")
lugo_36$pct_left=lugo_36$izq/lugo_36$votos*100
lugo_36$pct_centleft=lugo_36$centroizq / lugo_36$votos * 100
lugo_36$pct_right=lugo_36$dcha/lugo_36$votos*100

# subsetting to keep only percentage columns for vote variable
lugo_36=lugo_36[, c("muni_code", "pct_left", "pct_centleft", "pct_right")]

## OURENSE
ourense_36=read_excel("finalproject/data/ourense_1936.xlsx", sheet="Data")

# subsetting to keep only percentage columns for vote variable
ourense_36=ourense_36[,c("v.code", "percentage_left_1936B", "percentage_center_left_1936B", "percentage_right_1936")]

# renaming columns to match Lugo
names(ourense_36)=c("muni_code", "pct_left", "pct_centleft", "pct_right")

## COMBINING LUGO AND OURENSE RESULTS
data_1936=rbind(lugo_36, ourense_36)


## BUILDING THE PANEL DATASET OF ELECTIONS 2001-2024
library(dplyr)

# function to help read the raw CSVs from the Xunta
read_xunta <- function(file) {
  lines <- readLines(file, encoding = "latin1")
  lines <- gsub("(\\d)\\.(\\d{3})", "\\1\\2", lines)
  df <- read.csv(text = lines, sep = ";", stringsAsFactors = FALSE,
                 check.names = FALSE)
  df[, names(df) != ""]
}

# another function to help clean CSVs
clean_numbers <- function(df) {
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      cleaned <- gsub("\\.", "", df[[col]])
      if (!any(is.na(suppressWarnings(as.numeric(cleaned[cleaned != ""]))))) {
        df[[col]] <- as.numeric(cleaned)
      }
    }
  }
  df
}

# elections: only including votes for BNG, EN MAREA, AGE, ANOVA, and NÓS as well as any coalitions of which they were a member

# 2001
e2001 <- read_xunta("elections_2001.csv")
names(e2001)[c(1,3,11,14)] <- c("prov_code","muni_code","valid_votes","bng")
e2001 <- e2001 |>
  filter(prov_code %in% c(27, 32)) |>
  mutate(muni_code = as.integer(prov_code) * 1000 + as.integer(muni_code),
         year = 2001,
         sec_share = as.numeric(bng) / as.numeric(valid_votes) * 100) |>
  select(muni_code, year, sec_share)


# 2005
e2005 <- read_xunta("elections_2005.csv")
names(e2005)[c(1,3,11,13,22)] <- c("prov_code","muni_code","valid_votes","bng","nos_up")
e2005 <- e2005 |>
  filter(prov_code %in% c(27, 32)) |>
  mutate(muni_code = as.integer(prov_code) * 1000 + as.integer(muni_code),
         year = 2005,
         sec_share = (as.numeric(bng) + as.numeric(nos_up)) / as.numeric(valid_votes) * 100) |>
  select(muni_code, year, sec_share)

# 2009
e2009 <- read_xunta("elections_2009.csv")
names(e2009)[c(1,3,11,14,20)] <- c("prov_code","muni_code","valid_votes","bng","nos_up")
e2009 <- e2009 |>
  filter(prov_code %in% c(27, 32)) |>
  mutate(muni_code = as.integer(prov_code) * 1000 + as.integer(muni_code),
         year = 2009,
         sec_share = (as.numeric(bng) + as.numeric(nos_up)) / as.numeric(valid_votes) * 100) |>
  select(muni_code, year, sec_share)

# 2012
e2012 <- read_xunta("elections_2012.csv")
names(e2012)[c(1,3,11,13,14)] <- c("prov_code","muni_code","valid_votes","age","bng")
e2012 <- e2012 |>
  filter(prov_code %in% c(27, 32)) |>
  mutate(muni_code = as.integer(prov_code) * 1000 + as.integer(muni_code),
         year = 2012,
         sec_share = (as.numeric(bng) + as.numeric(age)) / as.numeric(valid_votes) * 100) |>
  select(muni_code, year, sec_share)

# 2016
e2016 <- read_xunta("elections_2016.csv")
names(e2016)[c(1,3,11,18,28)] <- c("prov_code","muni_code","valid_votes","bng_nos","en_marea")
e2016 <- e2016 |>
  filter(prov_code %in% c(27, 32)) |>
  mutate(muni_code = as.integer(prov_code) * 1000 + as.integer(muni_code),
         year = 2016,
         sec_share = (as.numeric(bng_nos) + as.numeric(en_marea)) / as.numeric(valid_votes) * 100) |>
  select(muni_code, year, sec_share)

# 2020
e2020_lugo <- read_xunta("elections_2020_lugo.csv")
names(e2020_lugo)[c(1,2,11,13,18)] <- c("prov_code","muni_code","valid_votes","podemos_anova","bng")
e2020_lugo <- e2020_lugo |>
  filter(!is.na(as.integer(muni_code))) |>
  mutate(muni_code = as.integer(prov_code) * 1000 + as.integer(muni_code),
         year = 2020,
         sec_share = (as.numeric(bng) + as.numeric(podemos_anova)) / as.numeric(valid_votes) * 100) |>
  select(muni_code, year, sec_share)

e2020_ourense <- read_xunta("elections_2020_ourense.csv")
names(e2020_ourense)[c(1,2,11,18,21,25)] <- c("prov_code","muni_code","valid_votes","podemos_anova","bng","en_marea")
e2020_ourense <- e2020_ourense |>
  filter(!is.na(as.integer(muni_code))) |>
  mutate(muni_code = as.integer(prov_code) * 1000 + as.integer(muni_code),
         year = 2020,
         sec_share = (as.numeric(bng) + as.numeric(podemos_anova) + as.numeric(en_marea)) / as.numeric(valid_votes) * 100) |>
  select(muni_code, year, sec_share)

# 2024
e2024_lugo <- read_xunta("elections_2024_lugo.csv")
names(e2024_lugo)[c(1,2,11,13)] <- c("prov_code","muni_code","valid_votes","bng")
e2024_lugo <- e2024_lugo |>
  filter(!is.na(as.integer(muni_code))) |>
  mutate(muni_code = as.integer(prov_code) * 1000 + as.integer(muni_code),
         year = 2024,
         sec_share = as.numeric(bng) / as.numeric(valid_votes) * 100) |>
  select(muni_code, year, sec_share)

e2024_ourense <- read_xunta("elections_2024_ourense.csv")
names(e2024_ourense)[c(1,2,11,13)] <- c("prov_code","muni_code","valid_votes","bng")
e2024_ourense <- e2024_ourense |>
  filter(!is.na(as.integer(muni_code))) |>
  mutate(muni_code = as.integer(prov_code) * 1000 + as.integer(muni_code),
         year = 2024,
         sec_share = as.numeric(bng) / as.numeric(valid_votes) * 100) |>
  select(muni_code, year, sec_share)

# put into panel
panel_elections <- rbind(
  e2001, e2005, e2009, e2012, e2016,
  e2020_lugo, e2020_ourense,
  e2024_lugo, e2024_ourense
)
write.csv(panel_elections, "panel_elections.csv", row.names = FALSE)

# Start with panel as base
final_data <- panel_elections

# Merge repression (per capita victims)
final_data <- merge(final_data, repression[, c("muni_code", "repression_pc", "n_victims")],
                    by = "muni_code", all.x = TRUE)

# Merge 1936 baseline election results
final_data <- merge(final_data, data_1936,
                    by = "muni_code", all.x = TRUE)

# Municipalities with zero victims recorded as NA — replace with 0
final_data$repression_pc[is.na(final_data$repression_pc)] <- 0
final_data$n_victims[is.na(final_data$n_victims)] <- 0

# Drop municipalities with no 1936 data (non-existent by 2011 boundaries)
final_data <- final_data[!is.na(final_data$pct_left), ]

write.csv(final_data, "final_data.csv", row.names = FALSE)
