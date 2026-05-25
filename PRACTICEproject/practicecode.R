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

library(tidyverse)
library(readxl)
library(janitor)

data_dir <- "data/galiciadata"
output_dir <- "data/galiciadata/processed"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Party name -> clean abbreviation map
# ------------------------------------------------------------------
party_map <- c(
  # PP
  "PP (PARTIDO POPULAR)"                                                   = "pp",
  "PP(PARTIDO POPULAR)"                                                    = "pp",
  "PP"                                                                     = "pp",
  # PSdeG-PSOE
  "PSdeG-PSOE(PARTIDO DOS SOCIALISTAS DE GALICIA-PARTIDO SOCIALISTA OBRERO ESPAÑOL)" = "psoe",
  "PSdeG - PSOE"                                                           = "psoe",
  "PSdeG-PSOE"                                                             = "psoe",
  "PSdeG - PSOE (PARTIDO DOS SOCIALISTAS DE GALICIA -PSOE)"               = "psoe",
  # BNG
  "BNG(BLOQUE NACIONALISTA GALEGO)"                                        = "bng",
  "BNG"                                                                    = "bng",
  "B.N.G."                                                                 = "bng",
  "BNG-NOS (BNG-NOS CANDIDATURA GALEGA)"                                  = "bng",
  "BNG-NÓS (BNG-NÓS CANDIDATURA GALEGA)"                                  = "bng",
  # EU / IU left (pre-2012)
  "IU-EU (ESQUERDA UNIDA-IZQUIERDA UNIDA)"                                = "eu_iu",
  "EU-IU"                                                                  = "eu_iu",
  # AGE 2012
  "AGE (EU-ANOVA)(ALTERNATIVA GALEGA DE ESQUERDA (ESQUERDA UNIDA-ANOVA))" = "age_eu_anova",
  # PODEMOS 2020
  "PODEMOS-ESQUERDA UNIDA-ANOVA"                                           = "podemos_eu_anova",
  # PODEMOS 2024
  "PODEMOS-ALIANZA VERDE"                                                  = "podemos_av",
  # SUMAR 2024
  "SUMAR GALICIA"                                                          = "sumar",
  # EN MAREA
  "EN MAREA"                                                               = "en_marea",
  "EN MAREA-COMPROMISO POR GALICIA-PARTIDO GALEGUISTA"                     = "en_marea",
  # GANEMOS 2016
  "GAÑEMOS"                                                                = "ganemos",
  # VOX
  "VOX"                                                                    = "vox",
  # Ciudadanos
  "C's (CIUDADANOS - PARTIDO DE LA CIUDADANIA)"                           = "cs",
  "C's (CIUDADANOS - PARTIDO DE LA CIUDADANÍA)"                           = "cs",
  "Cs"                                                                     = "cs",
  # NOS-UP
  "NOS-UP"                                                                 = "nos_up",
  "NÓS-UP"                                                                 = "nos_up",
  # CxG
  "CxG(COMPROMISO POR GALICIA)"                                            = "cxg",
  "CxG (COMPROMISO POR GALICIA)"                                           = "cxg",
  # Democracia Ourensana
  "D.O."                                                                   = "do_ourense",
  "DO(DEMOCRACIA OURENSANA)"                                               = "do_ourense",
  "DO (DEMOCRACIA OURENSANA)"                                              = "do_ourense",
  "DO"                                                                     = "do_ourense",
  # PACMA
  "PACMA(PARTIDO ANIMALISTA CONTRA O MALTRATO ANIMAL)"                     = "pacma",
  "PACMA (PARTIDO ANIMALISTA CONTRA O MALTRATO ANIMAL)"                    = "pacma",
  "PACMA"                                                                  = "pacma",
  # UPyD
  "UPyD(UNION PROGRESO Y DEMOCRACIA)"                                      = "upyd",
  "UPyD(UNIÓN PROGRESO Y DEMOCRACIA)"                                      = "upyd",
  "UPYD"                                                                   = "upyd",
  # PUM+J
  "PUM+J(POR UN MUNDO MAS JUSTO)"                                         = "pum_j",
  "PUM+J(POR UN MUNDO MÁS JUSTO)"                                         = "pum_j",
  "PUM+J"                                                                  = "pum_j",
  # FPG
  "FPG (FRENTE POPULAR GALEGA)"                                            = "fpg",
  "FPG"                                                                    = "fpg",
  # FE de las JONS
  "FE de las JONS"                                                         = "fe_jons",
  "FE de las JONS(FALANGE ESPANOLA DE LAS J.O.N.S.)"                      = "fe_jons",
  "FE de las JONS(FALANGE ESPAÑOLA DE LAS J.O.N.S.)"                      = "fe_jons",
  # SDD
  "SDD(PARTIDO SOCIAL Y DEMOCRATICO DE DERECHO)"                          = "sdd",
  "SDD(PARTIDO SOCIAL Y DEMOCRÁTICO DE DERECHO)"                          = "sdd",
  "SDD"                                                                    = "sdd",
  # PH
  "PH(PARTIDO HUMANISTA)"                                                  = "ph",
  "PH"                                                                     = "ph",
  # AUTONOMO
  "AUTONOMO (PARTIDO DE LOS AUTONOMOS Y PROFESIONALES)"                   = "autonomo",
  "AUTONOMO (PARTIDO DE LOS AUTÓNOMOS Y PROFESIONALES)"                   = "autonomo",
  "AU.TO.NO.MO"                                                            = "autonomo",
  "CCD-AU.TO.NO.MO"                                                        = "autonomo",
  # DPG
  "DPG (DEMOCRACIA PROGRESISTA GALEGA)"                                    = "dpg",
  # EdeG
  "EdeG (ESQUERDA DE GALICIA)"                                             = "edeg",
  # IDEGA
  "IDEGA"                                                                  = "idega",
  # IR-ERG
  "IR-ERG"                                                                 = "ir_erg",
  # D.N.
  "D.N."                                                                   = "dn",
  # S.E.P.
  "S.E.P."                                                                 = "sep",
  # CDS
  "CDS"                                                                    = "cds",
  # +G
  "+G"                                                                     = "mas_g",
  # AVE
  "AVE"                                                                    = "ave",
  # GU
  "GU"                                                                     = "gu",
  # OV
  "OV"                                                                     = "ov",
  # SAIn
  "SAIn(SOLIDARIDAD Y AUTOGESTION INTERNACIONALISTA)"                     = "sain",
  "SAIn(SOLIDARIDAD Y AUTOGESTIÓN INTERNACIONALISTA)"                      = "sain",
  "SAIn"                                                                   = "sain",
  # SCD
  "SCD(SOCIEDAD CIVIL Y DEMOCRACIA)"                                       = "scd",
  # TEGA
  "TEGA"                                                                   = "tega",
  # UCL
  "UCL"                                                                    = "ucl",
  # ADCG
  "ADCG(ACCION DEMOCRATICA DE CENTRO DE GALICIA)"                         = "adcg",
  "ADCG(ACCIÓN DEMOCRÁTICA DE CENTRO DE GALICIA)"                         = "adcg",
  "ADCG (ACCION DEMOCRATICA CIUDADANOS DE GALICIA)"                       = "adcg",
  "ADCG (ACCIÓN DEMOCRÁTICA CIUDADANOS DE GALICIA)"                       = "adcg",
  "ADCG"                                                                   = "adcg",
  # CDL
  "CDL(CENTRO DEMOCRATICO LIBERAL)"                                        = "cdl",
  "CDL(CENTRO DEMOCRÁTICO LIBERAL)"                                        = "cdl",
  # Comunistas da Galiza / PCTG
  "COMUNISTAS DA GALIZA(COMUNISTAS DA GALIZA)"                             = "pctg",
  "PARTIDO COMUNISTA DOS POBOS DE ESPANA - COMUNISTAS DA GALIZA"          = "pctg",
  "PARTIDO COMUNISTA DOS POBOS DE ESPAÑA - COMUNISTAS DA GALIZA"          = "pctg",
  "PCTG"                                                                   = "pctg",
  # C21
  "C.XXI(CONVERXENCIA VINTEUM)"                                            = "c21",
  "C.XXI(CONVERXENCIA VINTEÚN)"                                            = "c21",
  "C 21 (CONVERXENCIA 21)"                                                 = "c21",
  "C 21"                                                                   = "c21",
  # DeC
  "DeC(DEMOS EL CAMBIO)"                                                   = "dec",
  # Escanos en Blanco
  "Eb(ESCANOS EN BLANCO)"                                                  = "eb",
  "EB (ESCANOS EN BRANCO)"                                                 = "eb",
  "EB"                                                                     = "eb",
  "ESCANOS EN BRANCO"                                                      = "eb",
  # HARTOS
  "HARTOS.org(HARTOS.org y Voto en Blanco)"                               = "hartos",
  # PT
  "PT(PARTIDO DA TERRA)"                                                   = "pt",
  # PYC
  "PYC(PARTIDO INTEGRACION COMUNITARIA)"                                   = "pyc",
  "PYC(PARTIDO INTEGRACIÓN COMUNITARIA)"                                   = "pyc",
  # PIRATA.GAL
  "PIRATA.GAL(PIRATAS DE GALICIA)"                                         = "pirata_gal",
  # UCE
  "UCE(UNIFICACION COMUNISTA DE ESPANA)"                                   = "uce",
  "UCE(UNIFICACIÓN COMUNISTA DE ESPAÑA)"                                   = "uce",
  # Union Corunesa
  "UNION CORUNESA(UNION CORUNESA)"                                         = "union_corunesa",
  "UNIÓN CORUÑESA(UNIÓN CORUÑESA)"                                         = "union_corunesa",
  # 2016
  "A.I.D.G (ALTERNATIVA INDEPENDENTE DE GALICIA)"                         = "aidg",
  "VN (VIA NOVA GALICIA)"                                                  = "via_nova",
  "VN (VÍA NOVA GALICIA)"                                                  = "via_nova",
  "RECORTES CERO-GRUPO VERDE"                                              = "recortes_cero",
  "PAYJ (PARTIDO ANTICORRUPCION Y JUSTICIA)"                               = "payj",
  "PAYJ (PARTIDO ANTICORRUPCIÓN Y JUSTICIA)"                               = "payj",
  "C-C (CIDADANS-CEMTRUM)"                                                 = "cc_cidadans",
  "C-C (CIDADÁNS-CEMTRUM)"                                                 = "cc_cidadans",
  "UNIDOS SI-DEF (UNIDOS POR EL FUTURO)"                                   = "unidos_si",
  "P-LIB (PARTIDO LIBERTARIO)"                                             = "p_lib",
  "P-LIB"                                                                  = "p_lib",
  # 2020
  "EQUO"                                                                   = "equo",
  "RECORTES CERO-ESCO-OV-M"                                               = "recortes_esco",
  "ESCO-RC-OV-M"                                                           = "recortes_esco",
  "mg"                                                                     = "mg",
  "CONTIGO"                                                                = "contigo",
  "UNIDOS SI-UDP-DEf"                                                      = "unidos_si",
  # 2024
  "ECG"                                                                    = "ecg"
)

# ------------------------------------------------------------------
# 2. Helper: identify metadata columns (not party columns)
# ------------------------------------------------------------------
is_meta <- function(col_names) {
  str_detect(col_names,
    regex(paste(
      "provincia", "municipio", "concello", "censo", "votos", "abstenci",
      "nulos", "brancos", "candidatura", "v.lid", "c.d\\.", "cir\\.",
      "con\\.", "certif", "unnamed",
      sep = "|"
    ), ignore_case = TRUE)
  )
}

# ------------------------------------------------------------------
# 3. Core reader function
# ------------------------------------------------------------------
read_election_file <- function(path, year, is_province_file = FALSE) {

  raw <- read_excel(path, .name_repair = "unique") |>
    mutate(across(where(is.character), str_squish))
  names(raw) <- str_squish(names(raw))
  names(raw) <- iconv(names(raw), from = "UTF-8", to = "ASCII//TRANSLIT")

  # Identify party columns
  party_cols <- names(raw)[!is_meta(names(raw)) & names(raw) != "Unnamed: 3"]

  # Warn on unmapped parties
  unmapped <- setdiff(party_cols, names(party_map))
  if (length(unmapped) > 0) {
    warning(sprintf("[%s | %s] Unmapped columns: %s",
                    year, basename(path), paste(unmapped, collapse = ", ")))
  }

  # Rename party columns to clean abbreviations
  mapped     <- party_cols[party_cols %in% names(party_map)]
  rename_vec <- setNames(mapped, party_map[mapped])
  raw        <- rename(raw, any_of(rename_vec))

  # Standardise metadata column names
  if (!is_province_file) {

    names(raw) <- names(raw) |>
      trimws() |>
      str_replace_all("^CÓD\\.? PROVINCIA$",    "cod_provincia") |>
      str_replace_all("^PROVINCIA$",            "provincia") |>
      str_replace_all("^MUNICIPIO$",            "municipio") |>
      str_replace_all("^CENSO$",                "censo") |>
      str_replace_all("^TOTAL VOTOS$",          "votos_totais") |>
      str_replace_all("^TOTAL ABSTECCI.N$",     "abstencions") |>
      str_replace_all("^VOTOS EN BRANCO$",      "votos_brancos") |>
      str_replace_all("^VOTOS NULOS$",          "votos_nulos") |>
      str_replace_all("^VOTOS A CANDIDATURAS$", "votos_candidaturas") |>
      str_replace_all("^VOTOS V.LIDOS$",        "votos_validos")

    mun_cols <- names(raw)[str_detect(names(raw),
                           regex("COD.? MUNICIPIO", ignore_case = TRUE))]
    names(raw)[names(raw) == mun_cols[[1]]] <- "cod_municipio"
    if (length(mun_cols) > 1) raw <- select(raw, -any_of(mun_cols[-1]))

  } else {

    raw <- raw |>
      rename_with(~ "cod_provincia",      any_of(c("Cód. Cir.", "Cód Cir"))) |>
      rename_with(~ "cod_municipio",      any_of(c("Cód. Con.", "Cód Con"))) |>
      rename_with(~ "municipio",          any_of(c("Concello"))) |>
      rename_with(~ "censo",              any_of(c("Censo Total"))) |>
      rename_with(~ "votos_totais",       any_of(c("Votos Totais"))) |>
      rename_with(~ "abstencions",        any_of(c("Abstención"))) |>
      rename_with(~ "votos_nulos",        any_of(c("Votos nulos"))) |>
      rename_with(~ "votos_brancos",      any_of(c("Votos brancos"))) |>
      rename_with(~ "votos_candidaturas", any_of(c("Votos Candidaturas"))) |>
      rename_with(~ "votos_validos",      any_of(c("Votos Válidos")))

    prov_label <- c("15" = "A Coruña", "27" = "Lugo",
                    "32" = "Ourense",  "36" = "Pontevedra")
    raw <- mutate(raw, provincia = prov_label[as.character(cod_provincia)])

  }

  # Drop empty Unnamed column if still present
  raw <- select(raw, -any_of("Unnamed: 3"))

  # Separate metadata block from party block
  fixed <- c("cod_provincia", "provincia", "cod_municipio", "municipio",
             "censo", "votos_totais", "abstencions", "votos_nulos",
             "votos_brancos", "votos_candidaturas", "votos_validos")
  party_abbrevs_present <- setdiff(names(raw), fixed)

  id_block <- raw |>
    transmute(
      year               = year,
      cod_provincia      = as.integer(cod_provincia),
      provincia,
      cod_municipio      = as.integer(cod_municipio),
      municipio,
      censo              = as.numeric(censo),
      votos_totais       = as.numeric(votos_totais),
      abstencions        = as.numeric(abstencions),
      votos_nulos        = as.numeric(votos_nulos),
      votos_brancos      = as.numeric(votos_brancos),
      votos_candidaturas = as.numeric(votos_candidaturas),
      votos_validos      = as.numeric(votos_validos)
    )

  party_block <- raw |>
    select(all_of(party_abbrevs_present)) |>
    mutate(across(everything(), as.numeric))

  bind_cols(id_block, party_block)
}

# ------------------------------------------------------------------
# 4. Load all files
# ------------------------------------------------------------------
panel_early <- bind_rows(
  read_election_file(file.path(data_dir, "2001munis.xls"),  2001),
  read_election_file(file.path(data_dir, "2005munis.xls"),  2005),
  read_election_file(file.path(data_dir, "2009munis.xls"),  2009),
  read_election_file(file.path(data_dir, "2012munis.xls"),  2012),
  read_election_file(file.path(data_dir, "2016munis.xlsx"), 2016)
)

panel_2020 <- bind_rows(
  read_election_file(file.path(data_dir, "2020acoruna.xlsx"),    2020, is_province_file = TRUE),
  read_election_file(file.path(data_dir, "2020lugo.xlsx"),       2020, is_province_file = TRUE),
  read_election_file(file.path(data_dir, "2020ourense.xlsx"),    2020, is_province_file = TRUE),
  read_election_file(file.path(data_dir, "2020pontevedra.xlsx"), 2020, is_province_file = TRUE)
)

panel_2024 <- bind_rows(
  read_election_file(file.path(data_dir, "2024acoruna.xlsx"),    2024, is_province_file = TRUE),
  read_election_file(file.path(data_dir, "2024lugo.xlsx"),       2024, is_province_file = TRUE),
  read_election_file(file.path(data_dir, "2024ourense.xlsx"),    2024, is_province_file = TRUE),
  read_election_file(file.path(data_dir, "2024pontevedra.xlsx"), 2024, is_province_file = TRUE)
)

panel_raw <- bind_rows(panel_early, panel_2020, panel_2024)

# ------------------------------------------------------------------
# 5. Build INE code, turnout, and vote-share columns
# ------------------------------------------------------------------
fixed_cols <- c("year", "cod_provincia", "provincia", "cod_municipio", "municipio",
                "censo", "votos_totais", "abstencions", "votos_nulos",
                "votos_brancos", "votos_candidaturas", "votos_validos")

party_abbrevs <- sort(setdiff(names(panel_raw), fixed_cols))

panel <- panel_raw |>
  mutate(
    cod_ine     = paste0(str_pad(cod_provincia, 2, "left", "0"),
                         str_pad(cod_municipio, 3, "left", "0")),
    turnout_pct = votos_totais / censo * 100
  ) |>
  mutate(across(
    all_of(party_abbrevs),
    list(pct = ~ . / votos_validos * 100),
    .names = "{.col}_pct"
  )) |>
  select(
    cod_ine, year,
    cod_provincia, provincia, cod_municipio, municipio,
    censo, votos_totais, abstencions, votos_nulos, votos_brancos,
    votos_candidaturas, votos_validos, turnout_pct,
    all_of(party_abbrevs),
    paste0(party_abbrevs, "_pct")
  ) |>
  arrange(cod_ine, year)

# ------------------------------------------------------------------
# 6. Sanity checks
# ------------------------------------------------------------------
cat("\n========================================\n")
cat(" Panel sanity checks\n")
cat("========================================\n\n")
cat("Dimensions :", nrow(panel), "rows x", ncol(panel), "cols\n")
cat("Years      :", paste(sort(unique(panel$year)), collapse = ", "), "\n")
cat("Unique munis:", n_distinct(panel$cod_ine), "\n\n")

cat("Rows per year:\n")
print(count(panel, year))

cat("\nYears each party appeared:\n")
panel |>
  select(year, all_of(party_abbrevs)) |>
  pivot_longer(-year, names_to = "party", values_to = "votes") |>
  filter(!is.na(votes)) |>
  distinct(party, year) |>
  arrange(party, year) |>
  group_by(party) |>
  summarise(years = paste(year, collapse = ", "), .groups = "drop") |>
  print(n = Inf)

cat("\nMismatched vote totals (gap > 5 votes):\n")
panel |>
  mutate(
    party_sum = rowSums(select(., all_of(party_abbrevs)), na.rm = TRUE),
    gap       = abs(party_sum - votos_candidaturas)
  ) |>
  filter(gap > 5) |>
  select(cod_ine, municipio, year, votos_candidaturas, party_sum, gap) |>
  print()

# ------------------------------------------------------------------
# 7. Export
# ------------------------------------------------------------------
write_csv(panel, file.path(output_dir, "galicia_panel.csv"))
saveRDS(panel,   file.path(output_dir, "galicia_panel.rds"))

cat("\n✓ Saved to", file.path(output_dir, "galicia_panel.csv"), "\n")
cat("  Columns:", ncol(panel), "| Rows:", nrow(panel), "\n")