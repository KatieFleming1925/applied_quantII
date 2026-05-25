# =============================================================================
# build_panel.R
# Galicia regional election results — municipal panel, 2001-2024
# =============================================================================

library(tidyverse)
library(readxl)
library(janitor)

data_dir   <- "data/galiciadata"
output_dir <- "data/galiciadata/processed"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Party name -> clean abbreviation map (accent-free keys)
# ------------------------------------------------------------------
party_map <- c(
  # PP
  "PP (PARTIDO POPULAR)"                                                   = "pp",
  "PP(PARTIDO POPULAR)"                                                    = "pp",
  "PP"                                                                     = "pp",
  # PSdeG-PSOE
  "PSdeG-PSOE(PARTIDO DOS SOCIALISTAS DE GALICIA-PARTIDO SOCIALISTA OBRERO ESPANOL)" = "psoe",
  "PSdeG - PSOE"                                                           = "psoe",
  "PSdeG-PSOE"                                                             = "psoe",
  "PSdeG - PSOE (PARTIDO DOS SOCIALISTAS DE GALICIA -PSOE)"               = "psoe",
  # BNG
  "BNG(BLOQUE NACIONALISTA GALEGO)"                                        = "bng",
  "BNG"                                                                    = "bng",
  "B.N.G."                                                                 = "bng",
  "BNG-NOS (BNG-NOS CANDIDATURA GALEGA)"                                  = "bng",
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
  "GANEMOS"                                                                = "ganemos",
  # VOX
  "VOX"                                                                    = "vox",
  # Ciudadanos
  "C's (CIUDADANOS - PARTIDO DE LA CIUDADANIA)"                           = "cs",
  "Cs"                                                                     = "cs",
  # NOS-UP
  "NOS-UP"                                                                 = "nos_up",
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
  "UPYD"                                                                   = "upyd",
  # PUM+J
  "PUM+J(POR UN MUNDO MAS JUSTO)"                                         = "pum_j",
  "PUM+J"                                                                  = "pum_j",
  # FPG
  "FPG (FRENTE POPULAR GALEGA)"                                            = "fpg",
  "FPG"                                                                    = "fpg",
  # FE de las JONS
  "FE de las JONS"                                                         = "fe_jons",
  "FE de las JONS(FALANGE ESPANOLA DE LAS J.O.N.S.)"                      = "fe_jons",
  # SDD
  "SDD(PARTIDO SOCIAL Y DEMOCRATICO DE DERECHO)"                          = "sdd",
  "SDD"                                                                    = "sdd",
  # PH
  "PH(PARTIDO HUMANISTA)"                                                  = "ph",
  "PH"                                                                     = "ph",
  # AUTONOMO
  "AUTONOMO (PARTIDO DE LOS AUTONOMOS Y PROFESIONALES)"                   = "autonomo",
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
  "SAIn"                                                                   = "sain",
  # SCD
  "SCD(SOCIEDAD CIVIL Y DEMOCRACIA)"                                       = "scd",
  # TEGA
  "TEGA"                                                                   = "tega",
  # UCL
  "UCL"                                                                    = "ucl",
  # ADCG
  "ADCG(ACCION DEMOCRATICA DE CENTRO DE GALICIA)"                         = "adcg",
  "ADCG (ACCION DEMOCRATICA CIUDADANOS DE GALICIA)"                       = "adcg",
  "ADCG"                                                                   = "adcg",
  # CDL
  "CDL(CENTRO DEMOCRATICO LIBERAL)"                                        = "cdl",
  # Comunistas da Galiza / PCTG
  "COMUNISTAS DA GALIZA(COMUNISTAS DA GALIZA)"                             = "pctg",
  "PARTIDO COMUNISTA DOS POBOS DE ESPANA - COMUNISTAS DA GALIZA"          = "pctg",
  "PCTG"                                                                   = "pctg",
  # C21
  "C.XXI(CONVERXENCIA VINTEUM)"                                            = "c21",
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
  # PIRATA.GAL
  "PIRATA.GAL(PIRATAS DE GALICIA)"                                         = "pirata_gal",
  # UCE
  "UCE(UNIFICACION COMUNISTA DE ESPANA)"                                   = "uce",
  # Union Corunesa
  "UNION CORUNESA(UNION CORUNESA)"                                         = "union_corunesa",
  # 2016
  "A.I.D.G (ALTERNATIVA INDEPENDENTE DE GALICIA)"                         = "aidg",
  "VN (VIA NOVA GALICIA)"                                                  = "via_nova",
  "RECORTES CERO-GRUPO VERDE"                                              = "recortes_cero",
  "PAYJ (PARTIDO ANTICORRUPCION Y JUSTICIA)"                               = "payj",
  "C-C (CIDADANS-CEMTRUM)"                                                 = "cc_cidadans",
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
  "ECG"                                                                    = "ecg",
  "UPyD"                          = "upyd",
  "C.XXI(CONVERXENCIA VINTEUN)"   = "c21"
)

# ------------------------------------------------------------------
# 2. Helper: identify metadata columns (not party columns)
# ------------------------------------------------------------------
is_meta <- function(col_names) {
  str_detect(col_names,
    regex(paste(
      "provincia", "municipio", "concello", "censo", "votos", "abstenci",
      "nulos", "brancos", "candidatura", "validos", "valids", "c\\.d\\.",
      "cir\\.", "con\\.", "certif", "unnamed",
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

  # Strip accents from column names for consistent matching
  names(raw) <- stringi::stri_trans_general(str_squish(names(raw)), "Latin-ASCII")

  # Identify party columns
  party_cols <- names(raw)[!is_meta(names(raw)) & !str_detect(names(raw), "Unnamed")]

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
      str_replace_all("^COD\\. PROVINCIA$",     "cod_provincia") |>
      str_replace_all("^PROVINCIA$",            "provincia") |>
      str_replace_all("^MUNICIPIO$",            "municipio") |>
      str_replace_all("^CENSO$",                "censo") |>
      str_replace_all("^TOTAL VOTOS$",          "votos_totais") |>
      str_replace_all("^TOTAL ABSTENCION$",     "abstencions") |>
      str_replace_all("^VOTOS EN BRANCO$",      "votos_brancos") |>
      str_replace_all("^VOTOS NULOS$",          "votos_nulos") |>
      str_replace_all("^VOTOS A CANDIDATURAS$", "votos_candidaturas") |>
      str_replace_all("^VOTOS V.LIDOS$",        "votos_validos")

    mun_cols <- names(raw)[str_detect(names(raw),
                       regex("COD\\. MUNICIPIO", ignore_case = TRUE))]
    if (length(mun_cols) > 0) {
      names(raw)[names(raw) == mun_cols[[1]]] <- "cod_municipio"
      if (length(mun_cols) > 1) raw <- select(raw, -any_of(mun_cols[-1]))
    }

} else {

     names(raw)[names(raw) == "Cod. Cir."]         <- "cod_provincia"
    names(raw)[names(raw) == "Cod Cir"]            <- "cod_provincia"
    names(raw)[names(raw) == "Cod. Con."]          <- "cod_municipio"
    names(raw)[names(raw) == "Cod Con"]            <- "cod_municipio"
    names(raw)[names(raw) == "Concello"]           <- "municipio"
    names(raw)[names(raw) == "Censo Total"]        <- "censo"
    names(raw)[names(raw) == "Votos Totais"]       <- "votos_totais"
    names(raw)[names(raw) == "Abstencion"]         <- "abstencions"
    names(raw)[names(raw) == "Votos nulos"]        <- "votos_nulos"
    names(raw)[names(raw) == "Votos brancos"]      <- "votos_brancos"
    names(raw)[names(raw) == "Votos Candidaturas"] <- "votos_candidaturas"
    names(raw)[names(raw) == "Votos Validos"]      <- "votos_validos"

 cat("Names before provincia mutate:\n")   # <- add here
    print(names(raw))                          # <- add here

    prov_label <- c("15" = "A Coruna", "27" = "Lugo",
                    "32" = "Ourense",  "36" = "Pontevedra")
    raw <- mutate(raw, provincia = prov_label[as.character(cod_provincia)])

  }

  # Drop empty Unnamed column if still present
raw <- raw |> select(-any_of(c(names(raw)[str_detect(names(raw), "^\\.\\.\\.")], "Certif. Alta", "Certif Alta")))

  # Separate metadata from party columns
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

# Step 1: build filtered panel without pct columns
panel <- panel_raw |>
  mutate(
    cod_ine     = paste0(str_pad(cod_provincia, 2, "left", "0"),
                         str_pad(cod_municipio, 3, "left", "0")),
    turnout_pct = votos_totais / censo * 100
  ) |>
  filter(cod_provincia %in% c(27, 32)) |>
  select(where(~ !all(is.na(.))))

# Step 2: redefine party_abbrevs based on what's actually in the filtered panel
party_abbrevs <- sort(setdiff(names(panel), c(fixed_cols, "cod_ine", "turnout_pct")))

# Step 3: add pct columns and final ordering
panel <- panel |>
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
    party_sum = rowSums(across(all_of(party_abbrevs)), na.rm = TRUE),
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
