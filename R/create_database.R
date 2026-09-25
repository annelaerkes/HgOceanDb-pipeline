### <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# Import datasets and create database ----
#
### <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#
# Compiles the mercury-in-seawater datasets in this repository into HgOceanDb_<date>.csv and
# Sources_<date>.csv, written to Database/ at the repo root. See README.md for how to run this
# script, dataset categories, and citation requirements before using the output.
#
# Created by: Anne L Soerensen, 2026
### <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

## Read libraries----
library(readr)
library(readxl)
library(tidyverse)
library(dplyr) # if select doesn't work:  |> dplyr::select(height)
library(stringr) # for working with strings (pattern matching)
library(lubridate) # for working with dates
library(ncdf4) # for reading the Kohler et al. 2022 NetCDF source files (RD_008)
library(httr) # for submitting the Marine Regions download form (basin shapefiles, below)
library(sf)
sf_use_s2(FALSE)

## First-run setup ----
# Data/Not_redistributed_data/ and Database/ are gitignored, so a fresh clone of this repo won't
# have them - creates the folders this script writes to, and checks up front for the handful of
# datasets that need manual placement (everything else downloads automatically), so you find out
# now rather than after a long download run.
dir.create("../Database", showWarnings = FALSE)
dir.create("../Data/Not_redistributed_data/downloaded", recursive = TRUE, showWarnings = FALSE)
dir.create("../Data/Not_redistributed_data/GEOTRACES", recursive = TRUE, showWarnings = FALSE)
dir.create("../Data/Not_redistributed_data/Bratkic et al 2016", recursive = TRUE, showWarnings = FALSE)
dir.create("../Data/Not_redistributed_data/Starr et al 2025", recursive = TRUE, showWarnings = FALSE)

manual_placement_needed <- c(
  "../Data/Not_redistributed_data/GEOTRACES/GEOTRACES_IDP2021_Seawater_Discrete_Sample_Data_v1.csv" =
    "GEOTRACES IDP2021v2 - BODC serves this through an interactive portal, not a direct URL. See the comment above RD_002.",
  "../Data/Not_redistributed_data/Bratkic et al 2016/JC068_Hg_submission.xlsx" =
    "Bratkic et al. 2016 - same BODC portal limitation. See the comment above RD_003.",
  "../Data/Not_redistributed_data/Munson et al 2015_gbc20277-sup-0002-2015gb005120ts01.xls" =
    "Munson et al. 2015 - publisher site is Cloudflare-blocked from automated downloads. See the comment above RD_004.",
  "../Data/Not_redistributed_data/Capo_Cayian 2022_es2c03784_si_002.xlsx" =
    "Capo & Cayian 2022 - same Cloudflare limitation. See the comment above RD_012.",
  "../Data/Not_redistributed_data/Starr et al 2025/RR1815_DOoR Dissolved and Particulate Hg.xlsx" =
    "Starr et al. 2025 (Leg 1) - not yet published to its repository. See the comment above RD_016.",
  "../Data/Not_redistributed_data/Starr et al 2025/RR1814_DOoR Dissolved and Particulate Hg.xlsx" =
    "Starr et al. 2025 (Leg 2) - not yet published to its repository. See the comment above RD_016.",
  "../Data/Not_redistributed_data/Tate_et_al_2025_Site_Information.csv" =
    "Tate et al. 2025 - ScienceBase is Cloudflare-blocked from automated downloads. See the comment above RD_017.",
  "../Data/Not_redistributed_data/Tate_et_al_2025_Hg_Concentrations_Water.csv" =
    "Tate et al. 2025 - same Cloudflare limitation. See the comment above RD_017."
)
missing_files <- names(manual_placement_needed)[!file.exists(names(manual_placement_needed))]
if (length(missing_files) > 0) {
  message(
    "NOTE: ", length(missing_files), " dataset file(s) require manual placement before this ",
    "script will complete - everything else downloads automatically. Missing:"
  )
  for (f in missing_files) message("  - ", f, "\n      (", manual_placement_needed[[f]], ")")
}

## Download datasets hosted on public repositories directly from source ----
# Fetches a file from `url` into `destfile` if it isn't already cached locally, then returns
# destfile so it can be piped straight into read_csv()/read_excel()/etc. Keeping a local cache
# (rather than re-downloading on every run) makes repeated runs fast while still keeping the
# actual data acquisition step - and its source URL - fully visible and versioned in this script.
# Delete a file under ../Data/Not_redistributed_data/downloaded/ to force a fresh re-download from source.
# method = "libcurl" is explicit here because RStudio otherwise substitutes its own internal
# downloader, which has a known intermittent "SSL connect error" bug with some hosts (confirmed
# Sep 2026: a host that fails this way is fully reachable and serves the file fine outside
# RStudio's downloader) - libcurl bypasses that substitution.
fetch_source <- function(url, destfile) {
  if (!file.exists(destfile)) {
    dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
    download.file(url, destfile, mode = "wb", quiet = TRUE, method = "libcurl")
  }
  destfile
}

# Unzips `zip_path` into `extract_dir` if that directory doesn't already hold files - companion to
# fetch_source() for sources distributed as zip archives (shapefiles, below).
unzip_if_needed <- function(zip_path, extract_dir) {
  if (!dir.exists(extract_dir) || length(list.files(extract_dir)) == 0) {
    dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
    unzip(zip_path, exdir = extract_dir)
  }
  extract_dir
}

## Download basin/region shapefiles hosted on public repositories directly from source ----
# GOaS and IHO Sea Areas (marineregions.org) don't offer a plain file URL - clicking "Shapefile"
# on their downloads page submits a registration form (name/organisation/email/country/user
# category/purpose) before the zip is served. This is a genuine data-collection step for VLIZ's
# usage statistics and update notifications (per Marine Regions' license terms, see
# https://www.marineregions.org/), not just a technical gate - so rather than fabricate answers,
# this prompts whoever runs the script for their own details the first time, caches the answers
# locally (gitignored, never committed, reused for both Marine Regions downloads), and submits
# that real info on their behalf.
get_marineregions_user_info <- function() {
  cache <- "../Data/Not_redistributed_data/downloaded/.marineregions_user_info.rds"
  if (file.exists(cache)) {
    return(readRDS(cache))
  }
  if (!interactive()) {
    stop(
      "Marine Regions (marineregions.org) requires a one-time registration (name/organisation/",
      "email/etc.) before downloading the GOaS/IHO shapefiles - this can't be done in a ",
      "non-interactive session. Run this script interactively once to be prompted (the answer ",
      "is cached for later runs), or place the pre-extracted shapefiles under ",
      "../Data/Not_redistributed_data/downloaded/GOaS_v1_20211214/ and ../Data/Not_redistributed_data/downloaded/World_Seas_IHO_v3/ manually."
    )
  }
  message(
    "Marine Regions (marineregions.org) asks for your details before serving their ",
    "shapefiles - used for their usage statistics/update notifications, per their license ",
    "terms. Asked once here and cached locally; never shared or committed to this repo."
  )
  info <- list(
    name = readline("Your name: "),
    organisation = readline("Your organisation: "),
    email = readline("Your email: "),
    country = readline("Your country (spelled in English, e.g. Sweden): "),
    user_category = readline("User category [academia/industry/government/civil society]: "),
    purpose_category = readline("Purpose [e.g. Research/GIS Analysis/Education & workshops/Other]: ")
  )
  dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(info, cache)
  info
}

# Downloads one Marine Regions shapefile bundle by submitting their registration form (see above),
# then unzips it. `filename` is the .zip name as used on marineregions.org/downloads.php.
fetch_marineregions_shapefile <- function(filename, extract_dir) {
  if (dir.exists(extract_dir) && length(list.files(extract_dir)) > 0) {
    return(extract_dir)
  }
  info <- get_marineregions_user_info()
  zip_path <- file.path("../Data/Not_redistributed_data/downloaded", filename)
  dir.create(dirname(zip_path), recursive = TRUE, showWarnings = FALSE)
  url <- paste0("https://www.marineregions.org/download_file.php?name=", filename)

  # The form carries a hidden, off-screen "firstname-<hash>" field (a spam honeypot - real users
  # never see or fill it) that a browser submits as an empty string along with everything else.
  # A POST that omits this key entirely gets rejected server-side with a generic "please fill in
  # the fields correctly" error, even when every visible field is valid - so GET the live form
  # first (also establishes a session/cookies via a shared handle) and extract whatever that
  # field is currently named, rather than hardcoding a value that could change.
  session <- httr::handle(url)
  form_page <- httr::GET(url, handle = session)
  honeypot_name <- stringr::str_extract(httr::content(form_page, as = "text", encoding = "UTF-8"), "firstname-[a-f0-9]+")

  body <- list(
    name = info$name, organisation = info$organisation, email = info$email,
    country = info$country, user_category = info$user_category,
    purpose_category = info$purpose_category, agree = "1"
  )
  if (!is.na(honeypot_name)) body[[honeypot_name]] <- ""

  httr::POST(url,
    handle = session, body = body, encode = "form",
    httr::write_disk(zip_path, overwrite = TRUE)
  )
  # The form endpoint re-serves the HTML form (not the zip) if a required field was rejected -
  # fail loudly rather than silently trying to unzip an HTML error page.
  if (!identical(readBin(zip_path, "raw", n = 2), as.raw(c(0x50, 0x4b)))) {
    stop(
      "Marine Regions did not return a zip file for '", filename, "' - the submission may have ",
      "been rejected, or their form fields may have changed since this was written. Inspect ",
      zip_path, " (likely an HTML page) and compare against the live form at ",
      "https://www.marineregions.org/downloads.php."
    )
  }
  unzip_if_needed(zip_path, extract_dir)
}

# Locates the .shp file within a directory tree. Marine Regions' own zips are inconsistent about
# whether the shapefile sits flat in the archive or inside a nested subfolder (confirmed: GOaS is
# flat, IHO v3 nests everything under an extra folder) - searching recursively avoids hardcoding a
# path that only works for one of them.
find_shapefile <- function(dir) {
  matches <- list.files(dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  if (length(matches) == 0) stop("No .shp file found under ", dir)
  matches[1]
}

SPECIES <- tribble(
  ~SPECIES_NAME, ~DEFINITION, ~UNIT,
  "THG", "Total Hg (unfiltered)", "pM",
  "THG_D", "Total dissolved Hg (filtered)", "pM",
  "THG_P", "Total particulate Hg", "pM",
  "DGM", "Dissolved gaseous mercury (HG0+DMHG)", "pM",
  "HG0", "Dissolved elemental Hg", "pM",
  "HG0_D", "Dissolved elemental Hg (filtered)", "pM",
  "HGII", "Total divalent Hg (unfiltered)", "pM",
  "HGII_D", "Divalent dissolved Hg (filtered)", "pM",
  "HGII_P", "Divalent particulate Hg", "pM",
  "MEHG", "Total methyl-Hg (MMHG+DMHG) (unfiltered)", "fM",
  "MEHG_D", "Total dissolved methyl-Hg (filtered)", "fM",
  "MEHG_P", "Total particulate methyl-Hg", "fM",
  "MMHG", "Total monomethyl-Hg (unfiltered)", "fM",
  "MMHG_D", "Dissolved monomethyl-Hg (filtered)", "fM",
  "MMHG_P", "Particulate monomethyl-Hg", "fM",
  "DMHG", "Dimethyl-Hg", "fM",
  "DMHG_D", "Dimethyl-Hg (filtered)", "fM"
)

## read shape basin file ----
# GOaS and IHO Sea Areas: Marine Regions (marineregions.org) - see fetch_marineregions_shapefile()
# above for why this needs a one-time interactive prompt rather than a plain download.
map_low_res <- st_read(find_shapefile(fetch_marineregions_shapefile("GOaS_v1_20211214.zip", "../Data/Not_redistributed_data/downloaded/GOaS_v1_20211214")))
map_high_res <- st_read(find_shapefile(fetch_marineregions_shapefile("World_Seas_IHO_v3.zip", "../Data/Not_redistributed_data/downloaded/World_Seas_IHO_v3")))
# IPCC reference regions: downloaded directly from the official IPCC-WG1/Atlas GitHub repo -
# verified Sep 2026, no registration/license gate.
IPCC_zip <- fetch_source(
  "https://raw.githubusercontent.com/IPCC-WG1/Atlas/main/reference-regions/IPCC-WGI-reference-regions-v4_shapefile.zip",
  "../Data/Not_redistributed_data/downloaded/IPCC-WGI-reference-regions-v4.zip"
)
map_ipcc <- st_read(find_shapefile(unzip_if_needed(IPCC_zip, "../Data/Not_redistributed_data/downloaded/IPCC-WGI-reference-regions-v4")))

## Create empty dataframe to hold imported data ----
columns <- c(
  "ID_DATASET", "NAME_DATASET", "CRUISE_NAME", "LATITUDE", "LONGITUDE", "DEPTH", "YEAR", "MONTH", "SPECIES_NAME", "SPECIES_CONC",
  "SALINITY_PSU", "TEMPERATURE_C", "OXYGEN_umol_kg", "CHLA_ug_L",
  "PUBLISHED_IN_PAPER", "DOI_PAPER_REFERENCE", "DATASET_PUBLISHED", "REPOSITORY", "DOI_DATASET", "ID_SAMPLE"
)

DATA_HEADER <- data.frame(matrix(nrow = 0, ncol = length(columns)))

colnames(DATA_HEADER) <- columns

DATA_HEADER <- DATA_HEADER |> mutate(
  ID_DATASET = as.character(ID_DATASET), NAME_DATASET = as.character(NAME_DATASET), CRUISE_NAME = as.character(CRUISE_NAME),
  LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE),
  DEPTH = as.numeric(DEPTH), YEAR = as.numeric(YEAR), MONTH = as.numeric(MONTH), SPECIES_NAME = as.character(SPECIES_NAME),
  SPECIES_CONC = as.numeric(SPECIES_CONC), SALINITY_PSU = as.numeric(SALINITY_PSU), TEMPERATURE_C = as.numeric(TEMPERATURE_C),
  OXYGEN_umol_kg = as.numeric(OXYGEN_umol_kg), CHLA_ug_L = as.numeric(CHLA_ug_L),
  PUBLISHED_IN_PAPER = as.character(PUBLISHED_IN_PAPER), DOI_PAPER_REFERENCE = as.character(DOI_PAPER_REFERENCE), DATASET_PUBLISHED = as.character(DATASET_PUBLISHED),
  REPOSITORY = as.character(REPOSITORY), DOI_DATASET = as.character(DOI_DATASET), ID_SAMPLE = as.character(ID_SAMPLE)
)


## Information needed for all datasets ----
# YEAR
# MONTH
# ID_DATASET
# NAME_DATASET
# ID_SAMPLE= paste("S",ID_DATASET,1:n(),sep='_')
# PUBLISHED_IN_PAPER
# DOI_PAPER_REFERENCE
# DOI_DATASET
# DATASET_PUBLISHED
# REPOSITORY
# COMMENT


## Import datasets ----
## Import datasets, grouped by category ----

### PT - extracted table in publication (manual transcription from a non-tabular source) ----

### PT-001 - Soerensen et al 2013 ----
### THG method detection limit
### Table extracted from the paper's SI (Table S1) - not a figshare/repository download (the
### figshare mirror previously used here was the wrong link for this dataset).
PT_001 <- read_excel("../Data/Extracted_table_in_publication/Soerensen et al 2013.xlsx", skip = 4) |>
  rename(
    LATITUDE = "Latitude", LONGITUDE = "Longitude", DEPTH = "Depth_m",
    THG = "THgU_pM", THG_D = "THgF_pM"
  ) |>
  mutate(MONTH = month(Date)) |>
  select(YEAR, MONTH, LATITUDE, LONGITUDE, DEPTH, THG, THG_D, SALINITY_PSU, TEMPERATURE_C) |>
  mutate(
    ID_DATASET = "PT-001",
    NAME_DATASET = "Soerensen et al. 2013",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_"),
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1021/es401354q",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
  ) |>
  pivot_longer(THG:THG_D, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.15 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "THG_D" & SPECIES_CONC <= 0.15 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### PT-002 - Ci et al 2016 ----
### LOD THg: 0.1 ng/L
PT_002 <- read_excel("../Data/Extracted_table_in_publication/Ci et al 2016.xlsx", skip = 2) |>
  rename(THG = "THG_ng_L") |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, THG, SALINITY_PSU, COMMENT) |>
  mutate(
    ID_DATASET = "PT-002",
    NAME_DATASET = "Ci et al. 2016",
    YEAR = 2015, # actual year of collection unclear
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_"),
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1021/acs.est.5b05372",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
    COMMENT = "year and month of collection not stated in paper"
  ) |>
  pivot_longer(THG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.1 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  )) |>
  mutate(SPECIES_CONC = SPECIES_CONC / 200 * 1000) # from ng/L to pM

### PT-003 - Hammerschmidt et al 2013 ----
### No LOD given
PT_003 <- read_excel("../Data/Extracted_table_in_publication/Hammerschmidt et al 2013.xlsx", skip = 2) |>
  dplyr::select(YEAR, MONTH, LATITUDE, LONGITUDE, DEPTH, MMHG_D) |>
  mutate(
    ID_DATASET = "PT-003",
    NAME_DATASET = "Hammerschmidt et al. 2013",
    DEPTH = 14,
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1021/es3048619",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_"),
    COMMENT = "Sample depth given as interval of 8-20 meters, no detection limit specified"
  ) |>
  pivot_longer(MMHG_D, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")

### PT-004 - Wang et al 2020 ----
### LOD: THg = 0.1 ng/L = 0.5 pM, DGM = 2.7 pg/L = 0.0135 pM
PT_004a <- read_excel("../Data/Extracted_table_in_publication/Wang et al 2020.xlsx", sheet = "Table_S3", skip = 2, col_types = c(c("text", "numeric", "numeric", "date"), (rep(c("numeric"), 9))))
PT_004 <- read_excel("../Data/Extracted_table_in_publication/Wang et al 2020.xlsx", sheet = "Table_S4", col_types = c(c("text", "numeric", "numeric", "date"), (rep(c("numeric"), 11)))) |>
  bind_rows(PT_004a) |>
  mutate(THG = THG / 200 * 1000, THG_D = THG_D / 200 * 1000, DGM = DGM / 200) |> # ng/l to pM, pg/l to pM
  mutate(YEAR = year(DATE), MONTH = month(DATE)) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, THG, THG_D, DGM, TEMPERATURE_C, YEAR, MONTH) |>
  mutate(
    ID_DATASET = "PT-004",
    NAME_DATASET = "Wang et al. 2020",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1016/j.envres.2019.109092",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:DGM, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG_D" & SPECIES_CONC <= 0.5 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.5 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DGM" & SPECIES_CONC <= 0.0135 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### PT-005 - Wang et al 2016 ----
### MDL: THg = 0.12 ng/L = 0.6 pM, DGM = 3.3 pg/L = 0.165 pM (in dataset: THg ng/L, DGM pg/L) 
PT_005 <- read_excel("../Data/Extracted_table_in_publication/Wang et al 2016.xlsx", skip = 2, col_types = c(c("text", "numeric", "numeric", "date"), (rep(c("numeric"), 8)))) |>
  mutate(THG = THG / 200 * 1000, DGM = DGM / 200) |> # ng/l to pM, pg/l to pM
  mutate(YEAR = year(DATE), MONTH = month(DATE), DEPTH = 0.3) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG, DGM, TEMPERATURE_C) |>
  mutate(
    ID_DATASET = "PT-005",
    NAME_DATASET = "Wang et al. 2016",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "http://dx.doi.org/10.1016/j.envpol.2016.03.016",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_"),
    COMMENT = "Seawater was manually collected at a depth of 10e50 cm below the sea surface"
  ) |>
  pivot_longer(THG:DGM, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.6 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DGM" & SPECIES_CONC <= 0.0165 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### PT-006 - Marumoto et al 2018 ----
### MDL: DGM = 3.4 pg/L, THgP = 12 pg/L, THG_D = 0.15 pM, MeHg_D = 1.5 pg/L (in dataset all pg/L) 
PT_006 <- read_excel("../Data/Extracted_table_in_publication/Marumoto et al 2018.xlsx", skip = 2) |>
  mutate(
    THG = THG / 200, THG_D = THG_D / 200, THG_P = THG_P / 200, DGM = DGM / 200, MEHG_D = abs(MEHG_D / 200 * 1000), # pg/l to pM
    DEPTH = 11
  ) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG, THG_D, THG_P, MEHG_D, DGM, TEMPERATURE_C, SALINITY_PSU) |>
  mutate(
    ID_DATASET = "PT-006",
    NAME_DATASET = "Marumoto et al. 2018",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "doi:10.2343/geochemj.2.0485",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_"),
    COMMENT = 'Obs < DL included as DL; sampling depth was only specified as "surface water" and a 1 m depth is assumed'
  ) |>
  pivot_longer(THG:DGM, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC < 0.15 ~ (-0.15), 
    SPECIES_NAME == "THG_D" & SPECIES_CONC < 0.15 ~ (-0.15), 
    SPECIES_NAME == "THG_P" & SPECIES_CONC < 0.06 ~ (-0.06), 
    SPECIES_NAME == "DGM" & SPECIES_CONC < 0.017 ~ (-0.017), 
    SPECIES_NAME == "MEHG_D" & SPECIES_CONC < 7.5 ~ (-7.5), 
    TRUE ~ SPECIES_CONC
  ))

### PT-007 - Perrot et al 2023 ----
### (in dataset all ng/L) 
PT_007 <- read_excel("../Data/Extracted_table_in_publication/Perrot et al 2023.xlsx", skip = 2) |>
  mutate(THG = THG / 200 * 1000, THG_D = THG_D / 200 * 1000, THG_P = THG_P / 200 * 1000) |> # ng/l to pM
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG, THG_D, THG_P, TEMPERATURE_C, SALINITY_PSU) |>
  mutate(
    ID_DATASET = "PT-007",
    NAME_DATASET = "Perrot et al. 2023",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "http://dx.doi.org/10.1016/j.scitotenv.2023.163019",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Table and Supporting Information",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_"),
    COMMENT = "Part of a larger dataset with estuarine data; No detection limits indicated"
  ) |>
  pivot_longer(THG:THG_P, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")

### PT-008 - Kirk et al 2008 ----
### DL: THG = 0.02 ngL, MMHG = 15 pgL, DMHG = 25 pgL, Hg0
PT_008 <- read_excel("../Data/Extracted_table_in_publication/Kirk et al 2008.xlsx", sheet = "Table S1", skip = 3) |>
  rename(THG = "THG_ngL", MMHG = "MMHG_pgL", DMHG = "DMHG_pgL", HG0 = "GEM_pgL") |>
  mutate(THG = THG / 200 * 1000, MMHG = MMHG / 200 * 1000, DMHG = DMHG / 200 * 1000, HG0 = as.numeric(HG0) / 200) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG, HG0, MMHG, DMHG) |>
  mutate(
    ID_DATASET = "PT-008",
    CRUISE_NAME = "CCGS Amundsen",
    NAME_DATASET = "Kirk et al. 2008",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://pubs.acs.org/doi/abs/10.1021/es801635m",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:DMHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.1 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MMHG" & SPECIES_CONC <= 74 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DMHG" & SPECIES_CONC <= 5.24 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "HG0" & SPECIES_CONC <= 0.005 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### PT-009 - Sharif et al 2014 ----
### DL: MeHg = 70 fM. HgII = 0.13 pM, DGM = 0.03 pM, THg set to sum of the three 0.17 pM
PT_009 <- read_excel("../Data/Extracted_table_in_publication/Sharif et al 2014.xlsx", skip = 2) |>
  mutate(MEHG_D = MEHG_D * 1000, MEHG = MEHG * 1000) |> # pM to fM
  filter(STATION != "IE_3") |>
  dplyr::select(
    LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG, HGII, HGII_D, DGM, MEHG, MEHG_D,
    TEMPERATURE_C, SALINITY_PSU
  ) |>
  mutate(
    ID_DATASET = "PT-009",
    NAME_DATASET = "Sharif et al. 2014",
    CRUISE_NAME = "Metadour_3",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "http://dx.doi.org/10.1016/j.scitotenv.2014.06.116",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Table",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MEHG_D, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.17 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "HGII" & SPECIES_CONC <= 0.13 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "HGII_D" & SPECIES_CONC <= 0.13 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DGM" & SPECIES_CONC <= 0.03 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 70 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG_D" & SPECIES_CONC <= 70 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### PT-010 - Lehnherr et al 2011 ----
### DL as described in Lehnherr et al. (2011): THg = 0.4 pM, DMHG and Hg(0) 0.005 pM, MeHg 3.5 fM
### Now reads the dedicated paper-table extraction (Table S1 for this paper specifically) instead
### of filtering the raw unpublished multi-year summary file used by AP_002 - same columns/layout,
### so the transformation logic below is unchanged; the STATION/YEAR filters are kept as harmless
### defensive checks in case this file isn't already fully pre-filtered to just this paper's data.
PT_010 <- read_excel("../Data/Extracted_table_in_publication/Lehnherr et al 2011.xlsx", sheet = "SI_Table_S1", skip = 2) |>
  rename(
    STATION = "Station ID", YEAR = "Year", DATE = "Date",
    DEPTH = "Depth (m)", THG = "THg", MEHG = "MeHg", DMHG = "DMHg", MMHG = "MMHg", HG0 = "Hg(0)"
  ) |>
  dplyr::select(-c("Lat (°N)", "Long (°W)")) |>
  drop_na(STATION) |>
  mutate(
    MONTH = month(DATE), LONGITUDE = as.numeric(LONGITUDE), THG = as.numeric(THG) / 200 * 1000, HG0 = as.numeric(HG0) / 200 * 1000, MEHG = as.numeric(MEHG) / 200 * 1000000,
    DMHG = as.numeric(DMHG) / 200 * 1000000, MMHG = as.numeric(MMHG) / 200 * 1000000 # pM to fM
  ) |> 
  filter(STATION != "IE_3") |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG, HG0, MEHG, MMHG, DMHG) |>
  mutate(
    ID_DATASET = "PT-010",
    NAME_DATASET = "Lehnherr et al. 2011",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://www.nature.com/articles/ngeo1134",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:DMHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.4 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "HG(0)" & SPECIES_CONC <= 0.005 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 3.5 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MMHG" & SPECIES_CONC <= 3.5 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DMHG" & SPECIES_CONC <= 5 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### PT-011 - Fu et al. 2010 ----
### DL: THg = 0.5 pM, MeHg = 45 fM, DGM = 15 fM
PT_011 <- read_excel("../Data/Extracted_table_in_publication/Fu et al 2010.xlsx", skip = 2) |>
  rename(THG = "THG_ngL", MEHG = "MEHG_ngL", DGM = "DGM_pgL") |>
  mutate(THG = THG / 200 * 1000, MEHG = MEHG / 200 * 1000000, DGM = DGM / 200) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, THG, DGM, MEHG) |>
  mutate(
    ID_DATASET = "PT-011",
    YEAR = 2007,
    MONTH = 8,
    NAME_DATASET = "Fu et al. 2010",
    CRUISE_NAME = "R/V Shiyan 3",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2009JD012958",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Table",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.5 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 45 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DGM" & SPECIES_CONC <= 0.015 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### PT-012 - Malcolm et al. 2010 ----
PT_012 <- read_excel("../Data/Extracted_table_in_publication/Malcolm et al 2010.xlsx", skip = 4) |>
  rename(MEHG = "MeHg (pM)") |>
  mutate(MEHG = MEHG * 1000) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, MEHG, SALINITY_PSU, TEMPERATURE_C, OXYGEN_umol_kg) |>
  mutate(
    ID_DATASET = "PT-012",
    YEAR = 2007,
    MONTH = 9,
    NAME_DATASET = "Malcolm et al. 2010",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://www.sciencedirect.com/science/article/pii/S0304420310000940",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Table",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")

### PT-013 - Bergamaschi et al 2012 ----
### DL: THg = 0.1 pM, MeHg = 50 fM
PT_013 <- read_excel("../Data/Extracted_table_in_publication/Bergamaschi et al 2012.xlsx", skip = 4) |>
  rename(THG_D = "FTHg_ngL", MEHG_D = "FMeHg_ngL", THG_P = "PTHg_ngL", MEHG_P = "PMeHg_ngL") |>
  mutate(THG_D = THG_D / 200 * 1000, THG_P = THG_P / 200 * 1000, MEHG_D = MEHG_D / 200 * 1000 * 1000, MEHG_P = as.numeric(MEHG_P) / 200 * 1000 * 1000) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG_D, MEHG_D, THG_P, MEHG_P, SALINITY_PSU) |>
  mutate(
    ID_DATASET = "PT-013",
    NAME_DATASET = "Bergamaschi et al. 2012",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://pubs.acs.org/doi/10.1021/es2029137",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG_D:MEHG_P, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG_D" & SPECIES_CONC <= 0.1 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG_D" & SPECIES_CONC <= 49 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "THG_P" & SPECIES_CONC <= 0.1 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG_P" & SPECIES_CONC <= 49 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### PT-014 - Wang et al 2009 ----
### DL: THg = 0.05 pM, MMHg = 25 fM
PT_014 <- read_excel("../Data/Extracted_table_in_publication/Wang et al 2009.xlsx", skip = 4) |>
  mutate(THG = THg_ngL / 200 * 1000, MMHG = MMHg_ngL / 200 * 1000 * 1000) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG, MMHG, SALINITY_PSU) |>
  mutate(
    ID_DATASET = "PT-014",
    NAME_DATASET = "Wang et al. 2009",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://www.sciencedirect.com/science/article/pii/S0883292709001474",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Table",
    COMMENT = "Coordinates read from figure 1 in paper",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MMHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.05 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MMHG" & SPECIES_CONC <= 25 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))


### PT-015 - Umhau et al 2024 ----
### DL: not given
PT_015 <- read_excel("../Data/Extracted_table_in_publication/Umhau et al 2024.xlsx", skip = 4) |>
  mutate(THG_P = THG_P / 1000) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG_P, MEHG_P) |>
  mutate(
    ID_DATASET = "PT-015",
    NAME_DATASET = "Umhau et al. 2024",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://www.sciencedirect.com/science/article/pii/S0304420324000847?via%3Dihub",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Table",
    COMMENT = "Data from table 1 and 3; particles <53 um, particles >53 very small fraction of total particle Hg",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG_P:MEHG_P, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")


### PT-016 - Coale et al 2018 ----
### DL: THg = 0.25 pM, MMHg = 11 fM
PT_016 <- read_excel("../Data/Extracted_table_in_publication/Coale et al 2018.xlsx", skip = 2) |>
  fill(LATITUDE, LONGITUDE, YEAR, MONTH) |>
  rename(
    DMHG = "DMHg (fM)", MMHG = "MMHg (fM)", HG0 = "Hg0 (pM)", THG = "Hgt (pM)",
    OXYGEN_umol_kg = "Oxygen (µM)"
  ) |>
  mutate(
    MEHG = NA,
    MEHG = case_when(
      !is.na(DMHG) & !is.na(MMHG) ~ DMHG + MMHG,
      TRUE ~ MEHG
    )
  ) |>
  dplyr::select(
    LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG, MMHG, DMHG, HG0, MEHG,
    SALINITY_PSU, TEMPERATURE_C, OXYGEN_umol_kg
  ) |>
  mutate(
    ID_DATASET = "PT-016",
    NAME_DATASET = "Coale et al. 2018",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://www.sciencedirect.com/science/article/pii/S0967064518301152",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
    COMMENT = "MeHg calculated as DMHg+MMHg",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.25 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MMHG" & SPECIES_CONC <= 11 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 11 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### PT-017 - Chakraborty et al 2019 ----
PT_017 <- read_excel("../Data/Extracted_table_in_publication/Chakraborty et al 2019.xlsx", skip = 3) |>
  dplyr::select(LATITUDE, LONGITUDE, YEAR, MONTH, THG, THG_D, SALINITY_PSU) |>
  mutate(
    ID_DATASET = "PT-017",
    DEPTH = 1,
    NAME_DATASET = "Chakraborty et al. 2019",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://www.sciencedirect.com/science/article/pii/S0048969718353737?via%3Dihub",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Table",
    COMMENT = "Surface measurement, depth of 1 m assumed",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:THG_D, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")

### PT-018 - Tesan-Onrubia et al 2026 ----
### LOQ 1.3 pg/L THg and 3.3 pg/L MeHg
PT_018 <- read_excel("../Data/Extracted_table_in_publication/Tesan-Onrubia et al 2026.xlsx", skip = 4) |>
  mutate(
    THG = THg_pgL / 200, MEHG = MeHg_pgL / 200 * 1000,
    LATITUDE = 43.2417, LONGITUDE = 5.291670,
    DEPTH = 15,
    YEAR = as.numeric(str_sub(Date, 7, 10)),
    MONTH = as.numeric(str_sub(Date, 4, 5))
  ) |>
  select(!c(THg_pgL, MeHg_pgL, Date)) |>
  mutate(
    ID_DATASET = "PT-018",
    NAME_DATASET = "Tesan-Onrubia et al. 2026",
    CRUISE_NAME = "",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://www.sciencedirect.com/science/article/pii/S0045653526000470",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
    COMMENT = "sample depth varied between 8-45 meters",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.0065 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 16.5 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### MD - monitoring data ----

### MD-001 - Ireland monitoring ----
MD_001a <- read_excel("../Data/Requested_monitoring_data/Marine_Institute_Ireland/DR-25-017 Mercury in Seawater WFD and SWD 2014-2024.xlsx") |>
  rename(
    LATITUDE = "Sample Latitude", LONGITUDE = "Sample Longitude", YEAR = "Monitoring Year",
    date = "Sample Date", THG = "Analytical Result", Station = "Station Name", labb = "Analytical Laboratory"
  ) |>
  filter(!labb == "ALS Scandinavia - Luleå") |>
  mutate(
    MONTH = month(date),
    DEPTH = 2,
    SPECIES_NAME = "THG",
    SPECIES_CONC = as.numeric((str_replace(THG, "^<", "-"))) * 1000 / 200
  ) |> # ,
  # SPECIES_CONC = ifelse(is.na(SPECIES_CONC),1,SPECIES_CONC)) |> # fill in LOD for Ireland lab
  dplyr::select(Station, YEAR, MONTH, DEPTH, LATITUDE, LONGITUDE, SPECIES_NAME, SPECIES_CONC) |>
  mutate(
    ID_DATASET = "MD-001",
    NAME_DATASET = "Ireland monitoring",
    PUBLISHED_IN_PAPER = "NO",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "data available on request from Marine Institue, Ireland (https://www.marine.ie/)",
    COMMENT = 'Citation: "Marine Institute, 2025", river, estuarine and closed bay stations excluded; no specific sample depth, approximated to 2 meter',
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  )

MD_001_stations <- MD_001a |> distinct(Station)

MD_001_sal <- read_excel("../Data/Requested_monitoring_data/Marine_Institute_Ireland/DR-25-017 Temperature and Salinity Profile Data WFD SWD 2014-2024.xlsx",
  sheet = "Sheet2"
) |>
  filter(Depth < 10) |>
  semi_join(MD_001_stations, by = "Station") |>
  mutate(MONTH = as.numeric(str_sub(date, 4, 5))) |>
  rename(YEAR = "myear", SALINITY_PSU = "salinity(PSU)", TEMPERATURE_C = "temperature(degC)") |>
  select(YEAR, MONTH, Station, SALINITY_PSU, TEMPERATURE_C) |>
  group_by(YEAR, MONTH, Station) |>
  summarise(SALINITY_PSU = mean(SALINITY_PSU), TEMPERATURE_C = mean(TEMPERATURE_C)) |>
  ungroup() |>
  mutate(YEAR = as.numeric(YEAR))

MD_001 <- MD_001a |>
  left_join(MD_001_sal, by = c("YEAR", "MONTH", "Station")) |>
  filter(!is.na(SPECIES_CONC)) |>
  filter(Station == "Waterford Harbour Stn 1" | Station == "Bruckless" | Station == "Dublin Bay Stn 2" |
    Station == "Dundalk Bay Inner" | Station == "Kilkieran Bay North" | Station == "Wexford Harbour Outer" |
    Station == "Roaringwater Bay Inner" | Station == "Baltimore Harbour / Sherkin" |
    Station == "Bantry Bay Inner" | Station == "League Point" | Station == "Bantry Bay South" |
    Station == "Adrigole Harbour" | Station == "Dunmanus Bay Inner" | Station == "Ballymacoda" |
    Station == "Dungarvan Bay" | Station == "Ballinakill Bay" | Station == "Mannin Bay" |
    Station == "Carrigaholt" | Station == "Rinevella" | Station == "Tralee Bay Inner" |
    Station == "Maharees" | Station == "Ballylongford" | Station == "Loughras Beg" |
    Station == "Dungloe Bay" | Station == "Donegal Bay" | Station == "Inver Bay" |
    Station == "Blacksod bay" | Station == "Killala Bay" | Station == "Clew Bay North" |
    Station == "Westport Bay" | Station == "Gweebarra Bay" | Station == "Galway Bay Outer / Indreabhan" |
    Station == "Castletownbere" | Station == "Broadhaven Bay" | Station == "Sligo Bay" |
    Station == "Northwestern Atlantic Seaboard - HAs 37/38 Stn 1" |
    Station == "Clew BAy South" | Station == "Dundalk Bay" | Station == "Malahide" |
    Station == "Corrib Estuary" | Station == "Cork HAbour" | Station == "Kenmare River Outer Stn 1" |
    Station == "Roaringwater Bay Outer Stn 1" | Station == "Dublin Bay Stn 1" |
    Station == "Dundalk Bay Outer" | Station == "Balbriggen - Skerris" | Station == "Gweebarra Bay SWD") |>
  select(!Station)

### AP - author-permitted (unpublished, with consent) ----

### AP-001 - Jonsson et al 2022 ----
### LOD: MeHg = 23 fM, DMHG = 1.6 fM, THG = 0.085 pM
### Not clear what unit the Oxygen data is in - not umol/kg and doesn't seem ug/kg (16 g/mol)
AP_001 <- read_delim("../Data/Data provided by authors/Jonsson et al 2022.txt", delim = "\t") |>
  rename(
    LONGITUDE = "Longitude ", LATITUDE = "Latitude", DEPTH = "Depth (m)", DATE = "Date...7",
    TEMPERATURE_C = "Temp (C)", SALINITY_PSU = "Salinity (PSU)", OXYGEN_umol_kg = "Oxygen",
    THG = "HgT (pM)", MMHG = "MMeHg (fM)", DMHG = "DMeHg (fM)", MEHG = "MeHgTOT (fM)"
  ) |>
  mutate(
    month = str_sub(DATE, 1, 3),
    MONTH = case_when(
      month == "Aug" ~ 8,
      month == "Sep" ~ 9
    )
  ) |>
  dplyr::select(LATITUDE, LONGITUDE, MONTH, DEPTH, THG, MEHG, MMHG, DMHG, SALINITY_PSU, TEMPERATURE_C) |> # , OXYGEN_umol_kg) |>
  mutate(
    ID_DATASET = "AP-001",
    NAME_DATASET = "Jonsson et al. 2022",
    YEAR = 2016,
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_"),
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1016/j.marchem.2022.104105",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission"
  ) |>
  pivot_longer(THG:DMHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.085 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 23 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DMHG" & SPECIES_CONC <= 1.6 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### AP-002 - Soerensen et al 2016 ----
#### 2006 and 2010 data, delete 2007 data as it is published in Lehnherr et al 2011 - Hg in pM 
### DL as described in Lehnherr et al. (2011): THg = 0.4 pM, DMHG and Hg(0) 0.005 pM, MeHg 3.5 fM
AP_002 <- read_excel("../Data/Data provided by authors/Soerensen et al 2016.xlsx", skip = 2) |>
  rename(
    STATION = "Station ID", YEAR = "Year", DATE = "Date",
    DEPTH = "Depth (m)", THG = "THg", MEHG = "MeHg", DMHG = "DMHg", MMHG = "MMHg", HG0 = "Hg(0)"
  ) |>
  dplyr::select(-c("Lat (°N)", "Long (°W)")) |>
  drop_na(STATION) |>
  mutate(
    MONTH = month(DATE), LONGITUDE = as.numeric(LONGITUDE), THG = as.numeric(THG) / 200 * 1000, HG0 = as.numeric(HG0) / 200 * 1000, MEHG = as.numeric(MEHG) / 200 * 1000000,
    DMHG = as.numeric(DMHG) / 200 * 1000000, MMHG = as.numeric(MMHG) / 200 * 1000000 # pM to fM
  ) |> 
  filter(STATION != "IE_3") |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG, HG0, MEHG, MMHG, DMHG) |>
  mutate(
    ID_DATASET = "AP-002",
    NAME_DATASET = "Soerensen et al. 2016",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2015GB005280",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:DMHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.4 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "HG(0)" & SPECIES_CONC <= 0.005 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 3.5 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MMHG" & SPECIES_CONC <= 3.5 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DMHG" & SPECIES_CONC <= 5 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### AP-003 - Kim et al. 2020 ----
AP_003 <- read_excel("../Data/Data provided by authors/Kim et al 2020.xlsx", skip = 2) |>
  rename(
    LATITUDE = "Latitude", LONGITUDE = "Longitude", DEPTH = "Depth (m)",
    THG = "THg (pM)", MEHG = "MeHg (pM)", SALINITY_PSU = "Salinity (psu)"
  ) |>
  mutate(
    YEAR = 2018,
    MONTH = 9,
    MEHG = MEHG * 1000,
    ID_DATASET = "AP-003",
    NAME_DATASET = "Kim et al. 2020",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_"),
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://dx.doi.org/10.1021/acs.est.0c00154",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission"
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.39 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 8.8 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### AP-004 - Kim et al. 2017 ----
AP_004a <- read_excel("../Data/Data provided by authors/Kim et al 2017.xlsx", sheet = "2012", skip = 2) |>
  rename(
    LATITUDE = "Latitude", LONGITUDE = "Longitude", DEPTH = "Depth (m)",
    THG = "THg (pM)", MEHG = "MeHg (pM)", SALINITY_PSU = "Salinity (psu)"
  ) |>
  mutate(
    YEAR = 2012,
    MONTH = 7,
    MEHG = if_else(MEHG == "< DL", "0.0055", MEHG), # detection limit
    MEHG = as.numeric(MEHG) * 1000,
    ID_DATASET = "AP-004",
    NAME_DATASET = "Kim et al. 2017",
    CRUISE_NAME = "Western Pacific Ocean (2012)",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_"),
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1021/acs.est.6b04238",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission. Detection limit MeHg 0.0055 pM, obs below DL included as DL"
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 5.5 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

AP_004b <- read_excel("../Data/Data provided by authors/Kim et al 2017.xlsx",
  sheet = "2014", skip = 2,
  col_types = c(rep("numeric", 4), c("text", "numeric"))
) |>
  rename(
    LATITUDE = "Latitude", LONGITUDE = "Longitude", DEPTH = "Depth (m)",
    THG = "THg (pM)", MEHG = "MeHg (pM)", SALINITY_PSU = "Salinity (psu)"
  ) |>
  mutate(
    YEAR = 2014,
    MONTH = 4,
    MEHG = if_else(MEHG == "< DL", "0.0055", MEHG),
    MEHG = as.numeric(MEHG) * 1000,
    ID_DATASET = "AP-004",
    NAME_DATASET = "Kim et al. 2017",
    CRUISE_NAME = "Western Pacific Ocean (2014)",
    ID_SAMPLE = paste("S", ID_DATASET, 500:(n() + 499), sep = "_"),
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1021/acs.est.6b04238",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission"
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 5.5 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

AP_004 <- bind_rows(AP_004a, AP_004b)

### AP-005 - Yang et al. 2017 ----
AP_005 <- read_excel("../Data/Data provided by authors/Yang et al 2017.xlsx", skip = 2) |>
  rename(
    LATITUDE = "Latitude", LONGITUDE = "Longitude", DEPTH = "Depth (m)",
    THG = "THg (pM)", MEHG = "MeHg (pM)", SALINITY_PSU = "Salinity (psu)"
  ) |>
  mutate(
    YEAR = 2014,
    MONTH = 4,
    MEHG = as.numeric(MEHG) * 1000,
    ID_DATASET = "AP-005",
    NAME_DATASET = "Yang et al. 2017",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_"),
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1016/j.dsr.2017.10.009",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission"
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.4 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 24 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### AP-007 - Gosnell et al. 2017 ----
### DL: THg = 0.091 pM, MeHg = 16 fM
AP_007 <- read_excel("../Data/Data provided by authors/Gosnell et al 2017.xlsx", sheet = "modified_als", skip = 4) |>
  drop_na(Date) |>
  rename(
    THG_D = "HgT_D (pM)", MEHG_D = "MeHg_D (pM)",
    THG_P = "HgT_P (pM)", MEHG_P = "MeHg_P (pM)", DEPTH = "Depth (m)"
  ) |>
  mutate(MEHG_D = as.numeric(MEHG_D) * 1000, MEHG_P = MEHG_P * 1000, THG_P = as.numeric(THG_P)) |>
  mutate(MONTH = as.numeric(format(Date, "%m")), YEAR = as.numeric(format(Date, "%y")) + 2000) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG_D, THG_P, MEHG_D, MEHG_P) |>
  mutate(
    ID_DATASET = "AP-007",
    NAME_DATASET = "Gosnell et al. 2017",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://aslopubs.onlinelibrary.wiley.com/doi/full/10.1002/lno.10490",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG_D:MEHG_P, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG_D" & SPECIES_CONC <= 0.091 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG_D" & SPECIES_CONC <= 16 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "THG_P" & SPECIES_CONC <= 0.091 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG_P" & SPECIES_CONC <= 16 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### AP-008 - Mastromonaco et al. 2017a ----
### DL: THG ~ 0.3-1.2 pM, DGM = 0.0015 pM, MeHg = 5-6.5 fM
AP_008a <- read_csv("../Data/Data provided by authors/Mastromonaco et al 2017a-1.csv", skip = 2) |>
  rename(
    STATION = "Station", DATE = "yyyy-mm-dd Thh:mm", LONGITUDE = "Longitude [degrees_east]", LATITUDE = "Latitude [degrees_north]",
    TEMPERATURE_C = "T090C", SALINITY_PSU = "Sal00", DEPTH = "Depth [m]",
    THG = "HgTot [ng L-1]", DGM = "DGM [pg L-1]", MEHG = "MeHg [pg L-1]"
  ) |>
  mutate(
    YEAR = year(DATE), MONTH = month(DATE),
    THG = THG * 1000 / 200, DGM = DGM / 200, MEHG = MEHG * 1000 / 200
  ) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG, DGM, MEHG, TEMPERATURE_C, SALINITY_PSU) |>
  mutate(
    ID_DATASET = "AP-008",
    NAME_DATASET = "Mastromonaco et al. 2017a",
    CRUISE_NAME = "OSO 1011",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1016/j.marchem.2017.03.001",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.75 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DGM" & SPECIES_CONC <= 0.0015 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 6.5 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

AP_008b <- read_csv("../Data/Data provided by authors/Mastromonaco et al 2017a-2.csv", skip = 2) |>
  rename(
    STATION = "Station", DATE = "yyyy-mm-dd Thh:mm", LONGITUDE = "Longitude [degrees_east]", LATITUDE = "Latitude [degrees_north]",
    TEMPERATURE_C = "Temperature [?C]", SALINITY_PSU = "Salinity [psu]", DEPTH = "Depth [m]",
    THG = "HgTot [ng L-1]", DGM = "DGM [pg L-1]", MEHG = "MeHg [pg L-1]"
  ) |>
  mutate(
    YEAR = year(DATE), MONTH = month(DATE),
    THG = THG * 1000 / 200, DGM = DGM / 200, MEHG = MEHG * 1000 / 200
  ) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG, DGM, MEHG, TEMPERATURE_C, SALINITY_PSU) |>
  mutate(
    ID_DATASET = "AP-008",
    NAME_DATASET = "Mastromonaco et al. 2017a",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1016/j.marchem.2017.03.001",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission",
    ID_SAMPLE = paste("S", ID_DATASET, 500:(n() + 499), sep = "_")
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.75 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DGM" & SPECIES_CONC <= 0.0015 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 6.5 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

AP_008c <- read_csv("../Data/Data provided by authors/Mastromonaco et al 2017a-3.csv", skip = 2) |>
  rename(
    STATION = "Station", DATE = "yyyy-mm-dd Thh:mm", LONGITUDE = "Longitude [degrees_east]", LATITUDE = "Latitude [degrees_north]",
    TEMPERATURE_C = "Temperature [?C]", SALINITY_PSU = "Salinity [psu]", DEPTH = "Depth [m]",
    THG = "HgTot [ng L-1]", DGM = "DGM [pg L-1]", MEHG = "MeHg [pg L-1]"
  ) |>
  mutate(
    YEAR = year(DATE), MONTH = month(DATE),
    THG = THG * 1000 / 200, DGM = DGM / 200, MEHG = MEHG * 1000 / 200
  ) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG, DGM, MEHG, TEMPERATURE_C, SALINITY_PSU) |>
  mutate(
    ID_DATASET = "AP-008",
    NAME_DATASET = "Mastromonaco et al. 2017a",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1016/j.marchem.2017.03.001",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission",
    ID_SAMPLE = paste("S", ID_DATASET, 1000:(n() + 999), sep = "_")
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.75 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DGM" & SPECIES_CONC <= 0.0015 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 6.5 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

AP_008 <- bind_rows(AP_008a, AP_008b, AP_008c)

### AP-009 - Mastromonaco et al. 2017b ----
AP_009a <- read_csv("../Data/Data provided by authors/Mastromonaco et al 2017b-1.csv", skip = 2) |>
  rename(
    STATION = "Station", DATE = "yyyy-mm-dd Thh:mm", LONGITUDE = "Longitude [degrees_east]", LATITUDE = "Latitude [degrees_north]",
    DEPTH = "Depth [m]", DGM = "DGM [pg L-1]"
  ) |>
  mutate(YEAR = year(DATE), MONTH = month(DATE), DGM = DGM / 200) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, DGM) |>
  mutate(
    ID_DATASET = "AP-009",
    NAME_DATASET = "Mastromonaco et al. 2017b",
    CRUISE_NAME = "Fenice 2011",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1016/j.marchem.2017.02.003",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(DGM, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")

AP_009b <- read_csv("../Data/Data provided by authors/Mastromonaco et al 2017b-2.csv", skip = 2) |>
  # Longitude/latitude columns are swapped in the source file (confirmed by plotting: as
  # labeled, points fall in the Red Sea; swapped, they fall in the western Mediterranean,
  # consistent with the Fenice 2011 cruise) - renamed the other way round to correct for this.
  rename(
    STATION = "Station", DATE = "yyyy-mm-dd Thh:mm", LATITUDE = "Longitude [degrees_east]", LONGITUDE = "Latitude [degrees_north]",
    DEPTH = "Depth [m]", DGM = "DGM [pg L-1]"
  ) |>
  mutate(YEAR = year(DATE), MONTH = month(DATE), DGM = DGM / 200) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, DGM) |>
  mutate(
    ID_DATASET = "AP-009",
    NAME_DATASET = "Mastromonaco et al. 2017b",
    CRUISE_NAME = "Fenice 2012",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1016/j.marchem.2017.02.003",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission",
    ID_SAMPLE = paste("S", ID_DATASET, 500:(n() + 499), sep = "_")
  ) |>
  pivot_longer(DGM, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")

AP_009 <- bind_rows(AP_009a, AP_009b)

### AP-011 - Gosnell et al. 2023 ----
### DL: THg = 0.2 pM
AP_011 <- read_excel("../Data/Data provided by authors/Gosnell et al 2023.xlsx", skip = 2) |>
  rename(DEPTH = "Depth [m]", LONGITUDE = "Longitude [degrees_east]", LATITUDE = "Latitude [degrees_north]", THG = "Hg [pM]") |>
  filter(Flagg == 0) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, THG) |>
  mutate(
    ID_DATASET = "AP-011",
    YEAR = 2018,
    MONTH = 9,
    NAME_DATASET = "Gosnell et al. 2023",
    CRUISE_NAME = "RV Alkor",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://www.sciencedirect.com/science/article/pii/S0045653523027923",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.2 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### AP-012 - Hammerschmidt and Bowman 2012 ----
### DL: THg = 0.05 pM, MMHg = 2 fM
AP_012i <- read_excel("../Data/Data provided by authors/Hammerschmidt and Bowman 2012.xlsx", sheet = "SaFe", skip = 2)
AP_012_MMHG <- AP_012i |> select("Depth...1", "MMHg") |> drop_na() |> rename(DEPTH = "Depth...1") |> mutate(DEPTH = round(DEPTH, 0))
AP_012_DMHG <- AP_012i |> select("Depth...6", "DMHg") |> drop_na() |> rename(DEPTH = "Depth...6") |> mutate(DEPTH = round(DEPTH, 0))
AP_012_MMHGP <- AP_012i |> select("PartMMHgz", "PartMMHg") |> drop_na() |> rename(DEPTH = "PartMMHgz") |> mutate(DEPTH = round(DEPTH, 0))
AP_012_THG <- AP_012i |> select("HgTZ", "HgT") |> drop_na() |> rename(DEPTH = "HgTZ") |> mutate(DEPTH = round(DEPTH, 0))
AP_012_SAL <- AP_012i |> select("PrDM", "Sal00") |> drop_na() |> rename(DEPTH = "PrDM") |> mutate(DEPTH = round(DEPTH, 0))
AP_012_TEMP <- AP_012i |> select("TempZ", "Temp") |> drop_na() |> rename(DEPTH = "TempZ") |> mutate(DEPTH = round(DEPTH, 0))

AP_012 <- AP_012_THG |>
  full_join(AP_012_MMHG, by = "DEPTH") |>
  full_join(AP_012_DMHG, by = "DEPTH") |>
  full_join(AP_012_MMHGP, by = "DEPTH") |>
  left_join(AP_012_SAL, by = "DEPTH") |>
  left_join(AP_012_TEMP, by = "DEPTH") |>
  rename(MMHG_D = "MMHg", DMHG_D = "DMHg", THG_D = "HgT", MMHG_P = "PartMMHg", SALINITY_PSU = "Sal00", TEMPERATURE_C = "Temp") |>
  mutate(LATITUDE = 30, LONGITUDE = -140) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, THG_D, MMHG_D, DMHG_D, MMHG_P, SALINITY_PSU, TEMPERATURE_C) |>
  mutate(
    ID_DATASET = "AP-012",
    YEAR = 2009,
    MONTH = 5,
    NAME_DATASET = "Hammerschmidt and Bowman 2012",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://www.sciencedirect.com/science/article/abs/pii/S0304420312000242",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG_D:MMHG_P, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG_D" & SPECIES_CONC <= 0.05 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MMHG_D" & SPECIES_CONC <= 2 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### AP-013 - Eom et al. 2025 ----
AP_013 <- read_excel("../Data/Data provided by authors/Eom et al 2025.xlsx", skip = 3) |>
  rename(
    DEPTH = "m", LONGITUDE = "...3", LATITUDE = "...4", TEMPERATURE_C = "C", SALINITY_PSU = "PSU", OXYGEN_umol_kg = "umol/kg...8",
    THG = "pM...11", MEHG = "pM...12"
  ) |>
  mutate(MEHG = MEHG * 100) |>
  dplyr::select(LATITUDE, LONGITUDE, THG, MEHG, SALINITY_PSU, TEMPERATURE_C, OXYGEN_umol_kg) |>
  mutate(
    ID_DATASET = "AP-013",
    YEAR = 2022,
    MONTH = 8,
    NAME_DATASET = "Eom et al. 2025",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://www.sciencedirect.com/science/article/pii/S026974912500627X",
    DATASET_PUBLISHED = "NO",
    REPOSITORY = "Unpublished dataset, included with author permission",
    COMMENT = "Unpublished dataset, included with author permission",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")

### RD - repository-downloaded (auto-fetched, or manually placed when auto-fetch isn't possible) ----

### RD-001 - Soerensen et al 2018 ----
### LOD
### Downloaded directly from the Bolin Centre Database (bolin.su.se) - verified Sep 2026.
RD_001_file <- fetch_source(
  "https://bolin.su.se/data/uploads/Soerensen-2018-5.xlsx",
  "../Data/Not_redistributed_data/downloaded/Soerensen_et_al_2018.xlsx"
)
RD_001 <- read_excel(RD_001_file, sheet = "Data") |>
  mutate(MONTH = case_when(
    Cruise == "SEP14-N" | Cruise == "SEP14-S" ~ 9,
    Cruise == "AUG15-N" | Cruise == "AUG16-N" ~ 8,
    Cruise == "JUL15-S" | Cruise == "JUL16-S" ~ 7
  )) |>
  rename(
    YEAR = "Year", LATITUDE = "Latitude [Degrees]", LONGITUDE = "Longitude [Degrees]", DEPTH = "Depth [m]",
    THG = "THg [pM]", HGII = "HgII [pM]", HG0 = "Hg0 [pM]", MEHG = "MeHg [fM]", MEHG_D = "MeHgdissolved [fM]",
    SALINITY_PSU = "Salinity", TEMPERATURE_C = "Temperature [℃]", OXYGEN_mL_L = "Oxygen [mL/L]", CHLA_ug_L = "Chlorophyl A [ug/L]"
  ) |>
  select(YEAR, MONTH, LATITUDE, LONGITUDE, DEPTH, THG, HGII, HG0, MEHG, MEHG_D, SALINITY_PSU, TEMPERATURE_C, OXYGEN_mL_L, CHLA_ug_L) |>
  mutate(
    ID_DATASET = "RD-001",
    NAME_DATASET = "Soerensen et al. 2018",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_"),
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1029/2018GB005942",
    DOI_DATASET = "https://doi.org/10.17043/soerensen-2018-mercury-1",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://bolin.su.se/data/",
  ) |>
  pivot_longer(THG:MEHG_D, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.2 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "HGII" & SPECIES_CONC <= 0.1 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "HG0" & SPECIES_CONC <= 0.1 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 13 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG_D" & SPECIES_CONC <= 13 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### RD-002 - GEOTRACES data ----
### NOTE ON REPRODUCIBILITY: this dataset (GEOTRACES IDP2021v2 discrete sample data) cannot be
### fetched with a scripted download.file() call - BODC serves it through an interactive
### Published Data Library / basket system (https://www.bodc.ac.uk/geotraces/data/idp2021/,
### DOI doi:10.5285/cf2d9ba9-d51d-3b7c-e053-8486abc0f5fd), not a stable direct file URL. There is
### no documented public API or FTP path that returns the bulk CSV directly (checked Sep 2026).
### To re-obtain: go to the IDP2021v2 page above -> BODC Published Data Library entry for this DOI
### -> request/download the discrete sample data in CSV-ASCII format (delivered as a bundle with
### metadata/documentation alongside the actual data file). An alternative is the interactive
### subsetting tool at https://geotraces.webodv.awi.de/.
### This bundle's data file (96,185 rows x 1589 columns, ~230MB) is large enough that reading it
### with base read.csv() (as before) is very slow - switched to readr::read_csv() with col_select
### so only the ~16 needed columns are actually parsed, which is dramatically faster. Column names
### below are the raw header text (readr doesn't mangle spaces/brackets like read.csv did).
GEOTRACES_FILE <- "../Data/Not_redistributed_data/GEOTRACES/GEOTRACES_IDP2021_Seawater_Discrete_Sample_Data_v1.csv"
# NOTE: this file was moved up from BODC's original deeply-nested bundle path
# (.../GEOTRACES/idp2021/GEOTRACES_IDP2021_v1/seawater/ascii/GEOTRACES_IDP2021_Seawater_Discrete_Sample_Data_v1/...)
# to this flat location - the original path exceeded Windows' 260-character MAX_PATH limit once
# combined with this project's full folder path, causing a "file does not exist" error even though
# the file was genuinely there. The rest of BODC's bundle (documentation, metadata PDFs) is left
# in its original nested location under GEOTRACES/idp2021/ for reference; only the data file itself
# needed moving.
### DL: I use Petrova as a guide for all datasets in this GEOTRACES download - this is an approximation; rows with zeros removed
###     for Agather GN01 bdl set as 0, while nd NA (not included), for Cossa et al 2018 THg = 0.07 pM,
###     for Petrova et al THg/DGM = 0.025 pM, MeHg/MMHg = 5 fM, HGP = 0.0001 pM
info_RD_002 <- data.frame(
  sub_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
  CRUISE_NAME = c("GA01", "GA03", "GA04N", "GApr09", "GIPY06", "GN01", "GN03", "GN04", "GN05", "GP12", "GP16"),
  PUBLISHED_IN_PAPER = c("YES", "UNKNOWN", "UNKNOWN", "UNKNOWN", "YES", "YES", "YES", "YES", "YES", "UNKNOWN", "UNKNOWN"),
  DOI_PAPER_REFERENCE = c(
    "https://doi.org/10.5194/bg-15-2309-2018", "", "", "",
    "https://www.sciencedirect.com/science/article/pii/S0016703711002614",
    "https://doi.org/10.1016/j.marchem.2019.103686",
    "https://www.nature.com/articles/s41598-018-32760-0",
    "https://dx.doi.org/10.1021/acsearthspacechem.0c00055",
    "https://doi.org/10.1016/j.marchem.2020.103855", "", ""
  ),
  COMMENT = c(
    "Cossa et al. 2018", "", "", "", "Cossa et al. 2011", "Agather et al. 2019", "Wang et al. 2018",
    "Tesan onrubi et al 2020/Petrova et al. 2020", "Petrova et al. 2020", "", ""
  )
)

# GN01 = Agather et al 2019, GA01 = Cossa et al 2018,GA04N = Black Sea?, GApr09 = ?, GIPY06 = Antarctic, GN03 = Arctic,
# GN04 = Tesan onrubi et al 2020/Petrova et al 2020, GN05 = Petrova et al 2020, GP12 = solomon sea
RD_002 <- read_csv(GEOTRACES_FILE,
  col_select = c(
    "Cruise", "yyyy-mm-ddThh:mm:ss.sss", "Longitude [degrees_east]", "Latitude [degrees_north]", "DEPTH [m]",
    "CTDTMP_T_VALUE_SENSOR [deg C]", "CTDSAL_D_CONC_SENSOR [pss-78]", "CTDOXY_D_CONC_SENSOR [umol/kg]",
    "Hg_0_D_CONC_BOTTLE [pmol/kg]", "Hg_D_CONC_BOTTLE [pmol/kg]", "Hg_D_CONC_FISH [pmol/kg]",
    "Hg_DM_D_CONC_BOTTLE [pmol/kg]", "Hg_Me_D_CONC_BOTTLE [pmol/kg]",
    "Hg_MM_D_CONC_BOTTLE [pmol/kg]", "Hg_T_CONC_BOTTLE [pmol/kg]", "Hg_Me_T_CONC_BOTTLE [pmol/kg]"
  ),
  col_types = cols(.default = "c")
) |>
  rename(
    CRUISE_NAME = "Cruise", DATE = "yyyy-mm-ddThh:mm:ss.sss", LONGITUDE = "Longitude [degrees_east]", LATITUDE = "Latitude [degrees_north]", DEPTH = "DEPTH [m]",
    SALINITY_PSU = "CTDSAL_D_CONC_SENSOR [pss-78]", TEMPERATURE_C = "CTDTMP_T_VALUE_SENSOR [deg C]", OXYGEN_umol_kg = "CTDOXY_D_CONC_SENSOR [umol/kg]",
    HG0_D = "Hg_0_D_CONC_BOTTLE [pmol/kg]", DMHG_D = "Hg_DM_D_CONC_BOTTLE [pmol/kg]", MEHG_D = "Hg_Me_D_CONC_BOTTLE [pmol/kg]",
    MMHG_D = "Hg_MM_D_CONC_BOTTLE [pmol/kg]", THG = "Hg_T_CONC_BOTTLE [pmol/kg]", MEHG = "Hg_Me_T_CONC_BOTTLE [pmol/kg]",
    THG_D = "Hg_D_CONC_BOTTLE [pmol/kg]", THG_D_2 = "Hg_D_CONC_FISH [pmol/kg]"
  ) |>
  mutate(across(c(
    LONGITUDE, LATITUDE, DEPTH, SALINITY_PSU, TEMPERATURE_C, OXYGEN_umol_kg,
    HG0_D, DMHG_D, MEHG_D, MMHG_D, THG, MEHG, THG_D, THG_D_2
  ), as.numeric)) |>
  mutate(DMHG_D = DMHG_D * 1000, MEHG_D = MEHG_D * 1000, MMHG_D = MMHG_D * 1000, MEHG = MEHG * 1000) |>
  mutate(YEAR = as.numeric(str_sub(DATE, 1, 4)), MONTH = as.numeric(str_sub(DATE, 6, 7))) |>
  mutate(LONGITUDE = if_else(LONGITUDE > 180, (LONGITUDE - 360), LONGITUDE)) |>
  mutate(LONGITUDE = if_else(LONGITUDE <= (-19.51) & LONGITUDE >= (-19.52), -19.3, LONGITUDE)) |> # change longitude on land
  select(CRUISE_NAME, YEAR, MONTH, LATITUDE, LONGITUDE, DEPTH, THG, THG_D, THG_D_2, HG0_D, MEHG, MEHG_D, MMHG_D, DMHG_D, SALINITY_PSU, TEMPERATURE_C, OXYGEN_umol_kg) |>
  left_join(info_RD_002, by = "CRUISE_NAME") |>
  mutate(
    ID_DATASET = "RD-002",
    NAME_DATASET = paste("GEOTRACES_IDP_2021", CRUISE_NAME),
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_"),
    DOI_DATASET = "doi:10.5285/cf2d9ba9-d51d-3b7c-e053-8486abc0f5fd",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://www.bodc.ac.uk/",
    COMMENT = paste(COMMENT, "downloaded from data repository, contains data from several GEOTRACES publications - consult GEOTRACES webpage for Fair Data Use;
                      Detection limits from Petrova et al (2020) used as an approximation for the GEOTRACES bundle", sep = "_")
  ) |>
  pivot_longer(THG:DMHG_D, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_NAME = if_else(SPECIES_NAME == "THG_D_2", "THG_D", SPECIES_NAME)) |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.025 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "THG_D" & SPECIES_CONC <= 0.025 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DGM" & SPECIES_CONC <= 0.025 ~ (-1 * SPECIES_CONC),
    # SPECIES_NAME == 'HG0' & SPECIES_CONC <= 0.025 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "HG0_D" & SPECIES_CONC <= 0.025 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 5 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG_D" & SPECIES_CONC <= 5 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MMHG_D" & SPECIES_CONC <= 5 ~ (-1 * SPECIES_CONC),
    # SPECIES_NAME == 'DMHG' & SPECIES_CONC <= ? ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  )) |>
  filter(SPECIES_CONC != 0) |>
  mutate(NAME_DATASET = case_when(
    CRUISE_NAME == "GA01" ~ "Cossa et al. 2018",
    CRUISE_NAME == "GN01" ~ "Agather et al. 2019",
    CRUISE_NAME == "GIPY06" ~ "Cossa et al. 2011",
    CRUISE_NAME == "GN03" ~ "Wang et al. 2018",
    TRUE ~ NAME_DATASET
  ))


### RD-003 - Bratkic et al 2016 ----
### method detection limit: DGM = 4 pg/L, THg = 0.2 pg/L (???? ng/L), MeHg = 6-23 pg/L, DMHg = 0.2 pg/L
### NOT AUTOMATED: BODC (DOI doi:10.5285/1dbc9294-4e65-6530-e053-6c86abc09fb2) serves this dataset
### through the same interactive Published Data Library / basket system as the GEOTRACES bundle
### (RD_002) - no stable direct-download URL exists (checked Sep 2026). Kept as a local file;
### re-obtain via https://www.bodc.ac.uk/data/published_data_library/catalogue/10.5285/1dbc9294-4e65-6530-e053-6c86abc09fb2/
### if it needs refreshing.
RD_003 <- read_excel("../Data/Not_redistributed_data/Bratkic et al 2016/JC068_Hg_submission.xlsx") |>
  rename(
    DATE = "mon/day/yr", LONGITUDE = "Lon(°E)", LATITUDE = "Lat (°N)", DEPTH = "Depth [m]",
    THG = "THg [ng/L]", HG0 = "DGM [pg/L]", MEHG = "MeHg [pg/L]", DMHG = "DMeHg [pg/L]"
  ) |>
  mutate(HG0 = HG0 / 1000, MONTH = if_else(YEAR == 2011, 12, 1)) |> # Hg0 should be in pM by the end
  dplyr::select(YEAR, MONTH, LATITUDE, LONGITUDE, DEPTH, THG, HG0, MEHG, DMHG) |>
  mutate(
    ID_DATASET = "RD-003",
    NAME_DATASET = "Bratkic et al. 2016",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_"),
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "doi:10.1002/2015GB005275",
    DOI_DATASET = "doi:10.5285/1dbc9294-4e65-6530-e053-6c86abc09fb2",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://www.bodc.ac.uk/",
    COMMENT = "GEOTRACES GA10 cruise, other publications on data Zivkovik et al. 2022, DMHg in GEOTRACES file does not reflect that of the paper, in this file it is 1 or 0 but in the paper range between 0-8, it has therefore been removed"
  ) |>
  pivot_longer(THG:DMHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.0002 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "Hg0" & SPECIES_CONC <= 0.004 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 23 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DMHG" & SPECIES_CONC <= 0.2 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  )) |>
  mutate(SPECIES_CONC = SPECIES_CONC / 200 * 1000) |> # from ng/L to pM and pg/L to fM
  filter(SPECIES_NAME != "DMHG")

### RD-004 - Munson et al 2015 ----
### LOD: MMHg = 5 fM, DMHg = 20 fM, THg ~ 0.1 pM, Hg0 ~ 0.03 pM (for THg and Hg0 read from figure)
### NOT AUTOMATED: agupubs.onlinelibrary.wiley.com is behind an active Cloudflare "verify you are
### human" challenge on every access path (checked Sep 2026) - not scriptable with a plain
### download.file()/curl call. Kept as a local file; re-obtain the SI file
### gbc20277-sup-0002-2015gb005120ts01.xls from https://doi.org/10.1002/2015GB005120 if needed.
RD_004 <- read_excel("../Data/Not_redistributed_data/Munson et al 2015_gbc20277-sup-0002-2015gb005120ts01.xls") |>
  rename(
    DEPTH = "DEPTH[m]", TEMPERATURE_C = "TEMPERATURE [C]", SALINITY_PSU = "SALINITY [PSS78]", OXYGEN_umol_kg = "Oxygen [umol/kg]",
    THG_D = "THg [pM]", HG0_D = "Hg0 [pM]", MMHG_D = "MMHg [fM]", DMHG_D = "DMHg [fM]", DATE = "mon/day/yr"
  ) |>
  mutate(
    LONGITUDE = if_else(LONGITUDE > 180, (LONGITUDE - 360), LONGITUDE),
    YEAR = year(DATE), MONTH = month(DATE)
  ) |>
  fill(LATITUDE, LONGITUDE, YEAR, MONTH) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG_D, HG0_D, MMHG_D, DMHG_D, SALINITY_PSU, TEMPERATURE_C, OXYGEN_umol_kg) |>
  mutate(
    ID_DATASET = "RD-004",
    NAME_DATASET = "Munson et al. 2015",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1002/2015GB005120",
    DOI_DATASET = "https://doi.org/10.1002/2015GB005120",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG_D:DMHG_D, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG_D" & SPECIES_CONC <= 0.1 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "HG0_D" & SPECIES_CONC <= 0.03 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MMHG_D" & SPECIES_CONC <= 5 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DMHG_D" & SPECIES_CONC <= 20 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### RD-005 - Bowman et al 2016 ----
### Method detection limits MMHg 20 fM, THg 0.03 pM, 0.01 pM for Hg0 and 2 fM for DMHg
### Downloaded directly from BCO-DMO dataset 643494 (verified Sep 2026); replaces the previously
### locally-downloaded flatfile of the same data (which had whitespace-padded column names).
RD_005_file <- fetch_source(
  "https://datadocs.bco-dmo.org/dataset/643494/file/M77XqYktk8WVpw/Hg_filtered_joined_fish_btl.csv",
  "../Data/Not_redistributed_data/downloaded/Bowman_et_al_2016_643494.csv"
)
RD_005 <- read_csv(RD_005_file) |>
  dplyr::select(
    "date", "lat", "lon", "depth",
    "Hg_TD_CONC_BOTTLE", "Hg0_D_CONC_BOTTLE", "MMHg_D_CONC_BOTTLE", "DMHg_D_CONC_BOTTLE", "Hg_TD_CONC_FISH"
  ) |>
  rename(
    DATE = "date", LONGITUDE = "lon", LATITUDE = "lat", DEPTH = "depth",
    HG0_D = "Hg0_D_CONC_BOTTLE", DMHG_D = "DMHg_D_CONC_BOTTLE",
    MMHG_D = "MMHg_D_CONC_BOTTLE", THG_D = "Hg_TD_CONC_FISH", THG_D_2 = "Hg_TD_CONC_BOTTLE"
  ) |>
  mutate(DATE = as.numeric(DATE), MONTH = str_sub(DATE, 5, 6)) |>
  select(!c("DATE"))
RD_005 <- as.data.frame(apply(RD_005, 2, function(x) gsub("\\s+", "", x))) |>
  mutate(
    ID_DATASET = "RD-005",
    NAME_DATASET = "Bowman et al. 2016",
    CRUISE_NAME = "TN303",
    YEAR = 2013,
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1016/j.marchem.2016.09.005",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://www.bco-dmo.org/dataset/643494/data",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG_D_2:THG_D, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(
    SPECIES_CONC = if_else(SPECIES_CONC == "nd", NA_character_, SPECIES_CONC),
    SPECIES_NAME = if_else(SPECIES_NAME == "THG_D_2", "THG_D", SPECIES_NAME)
  ) |>
  drop_na(SPECIES_CONC) |>
  pivot_wider(names_from = c(SPECIES_NAME), values_from = SPECIES_CONC) |>
  mutate(
    MMHG_D = if_else(MMHG_D == "lt_DL", "-0.02", MMHG_D),
    DMHG_D = if_else(DMHG_D == "lt_DL", "-0.002", DMHG_D),
    HG0_D = if_else(HG0_D == "lt_DL", "-0.01", HG0_D),
    THG_D = if_else(THG_D == "lt_DL", "-0.03", THG_D),
    MMHG_D = as.numeric(MMHG_D) * 1000, THG_D = as.numeric(THG_D), DMHG_D = as.numeric(DMHG_D) * 1000, HG0_D = as.numeric(HG0_D),
    LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE), DEPTH = as.numeric(DEPTH),
    MONTH = as.numeric(MONTH)
  ) |>
  pivot_longer(THG_D:DMHG_D, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")


### RD-006 - Bowman et al 2015 ----
### Method detection limits MMHg 2 fM, THg 0.02 pM, 0.01 pM for Hg0 and 2 fM for DMHg
### Downloaded directly from BCO-DMO dataset 3860 (verified Sep 2026) - a single combined file
### covering both cruises (KN199-04, KN204-01), distinguished by the cruise_id column, replacing
### the two separately-downloaded per-cruise flatfiles previously used here.
RD_006_file <- fetch_source(
  "https://datadocs.bco-dmo.org/dataset/3860/file/6YYMwOzTo3VEJ0/Hg_filt_joined.csv",
  "../Data/Not_redistributed_data/downloaded/Bowman_et_al_2015_3860.csv"
)
RD_006 <- read_csv(RD_006_file) |>
  dplyr::select("cruise_id", "lat", "lon", "depth", "date", "Hg_total", "Hg0", "MMHg", "DMHg") |>
  rename(
    LATITUDE = "lat", LONGITUDE = "lon", DEPTH = "depth", DATE = "date",
    HG0_D = "Hg0", DMHG_D = "DMHg", MMHG_D = "MMHg", THG_D = "Hg_total"
  ) |>
  mutate(
    YEAR = case_when(cruise_id == "KN199-04" ~ 2010, cruise_id == "KN204-01" ~ 2011, TRUE ~ NA_real_),
    LONGITUDE = as.numeric(LONGITUDE),
    DATE = as.numeric(DATE),
    MONTH = as.numeric(str_sub(DATE, 5, 6))
  ) |>
  select(-cruise_id, -DATE)
RD_006 <- as.data.frame(apply(RD_006, 2, function(x) gsub("\\s+", "", x))) |>
  mutate(
    ID_DATASET = "RD-006",
    NAME_DATASET = "Bowman et al. 2015",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1016/j.dsr2.2014.07.004",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://www.bco-dmo.org/dataset/3860/data",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG_D:DMHG_D, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = if_else(SPECIES_CONC == "nd", NA_character_, SPECIES_CONC)) |>
  drop_na(SPECIES_CONC) |>
  pivot_wider(names_from = c(SPECIES_NAME), values_from = SPECIES_CONC) |>
  mutate(
    MMHG_D = if_else(MMHG_D == "lt_DL", "-2", MMHG_D),
    DMHG_D = if_else(DMHG_D == "lt_DL", "-2", DMHG_D),
    HG0_D = if_else(HG0_D == "lt_DL", "-0.010", HG0_D),
    THG_D = if_else(THG_D == "lt_DL", "-0.02", THG_D),
    MMHG_D = as.numeric(MMHG_D), THG_D = as.numeric(THG_D), DMHG_D = as.numeric(DMHG_D), HG0_D = as.numeric(HG0_D),
    LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE), DEPTH = as.numeric(DEPTH),
    YEAR = as.numeric(YEAR), MONTH = as.numeric(MONTH)
  ) |>
  pivot_longer(HG0_D:DMHG_D, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")


### RD-007 - Yue et al 2023 ----
### LOD: MeHg = 0.002 ng/L = 10 fM
### Downloaded directly from Mendeley Data (DOI 10.17632/x7r6mprjb7.1) - verified Sep 2026. Mendeley's
### convenience "public-files" redirect link can intermittently return an error page instead of the
### file for non-browser requests, so this calls Mendeley's public-api endpoint directly to resolve
### the real (S3-hosted) file URL first, then downloads that.
RD_007_resolved_url <- httr::content(
  httr::GET("https://data.mendeley.com/public-api/datasets/x7r6mprjb7/files/c9fd4361-4f78-4d1b-bc4b-d37e307058dc/file_downloaded"),
  as = "parsed", type = "application/json"
)$url
RD_007_file <- fetch_source(
  RD_007_resolved_url,
  "../Data/Not_redistributed_data/downloaded/Yue_et_al_2023_MeHg_Data.xlsx"
)
RD_007 <- read_excel(RD_007_file) |>
  rename(
    LATITUDE = "Latitude [degrees_north]", LONGITUDE = "Longitide [degrees_east]", DEPTH = "Depth (m)",
    SALINITY_PSU = "Salinity", TEMPERATURE_C = "Water temperature (°C)", OXYGEN_umol_kg = "DO (μmol/L)",
    MEHG = "MeHg (pmol/L)"
  ) |>
  mutate(MEHG = MEHG * 1000, DEPTH = DEPTH * -1) |> # MeHg from pM to fM
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, MEHG, SALINITY_PSU, TEMPERATURE_C, OXYGEN_umol_kg) |>
  mutate(
    ID_DATASET = "RD-007",
    NAME_DATASET = "Yue et al. 2023",
    YEAR = 2020, # 11-30 January
    MONTH = 1,
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1016/j.scitotenv.2023.163646",
    DATASET_PUBLISHED = "YES",
    DOI_DATASET = "doi:10.17632/x7r6mprjb7.1",
    REPOSITORY = "https://data.mendeley.com/",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "MeHg" & SPECIES_CONC <= 10 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))


### RD-008 - Kohler et al 2022 ----
### Instrument DL: THg = 0.07 pM (0.003 pM for 40 mL seawater; see Cossa et al 2018), MeHg = 1 fM (??)
### Downloaded directly from the Norwegian Marine Data Centre (nmdc.no) - verified Sep 2026. NMDC
### hosts this as 18 separate per-station NetCDF files (no single bulk table) across two cruises:
###   "Cruise 2019706 Q3" (Aug 2019, DOI 10.21335/NMDC-416151559): stations P1-P7, P6ctd, P7ctd
###   "Cruise 2019711 Q4" (Dec 2019, DOI 10.21335/NMDC-1871554897): stations P1-P7, P6ctd, P7ctd
### served via OPeNDAP at a stable per-file URL pattern (confirmed byte-identical to a manually
### downloaded copy). Replaces the previously hand-built Kohler_2022.xlsx entirely.
### NetCDF variable names/units/fill-value confirmed by inspecting the files directly (classic
### NetCDF-3 format, CF-1.6 conventions): lat/lon/time/alt are coordinates (the depth variable is
### actually named "alt" - "depth" is only its standard_name attribute); the Hg variables are
### mol_concentration_of_total_mercury_in_seawater and
### ...total_methylated_mercury_in_seawater, both stored in mol/m3 with missing_value=-999. The
### files' own metadata states these were originally measured in pmol/L (=pM) before conversion to
### mol/m3, so mol/m3 -> pM is *1e9 (1 mol/m3 = 1e-3 mol/L = 1e9 pM); MeHg is then *1000 again to
### match this pipeline's fM convention for MeHg, same as every other dataset here.
read_kohler_nc <- function(file) {
  nc <- nc_open(file)
  on.exit(nc_close(nc))
  thg <- as.numeric(ncvar_get(nc, "mol_concentration_of_total_mercury_in_seawater"))
  mehg <- as.numeric(ncvar_get(nc, "mol_concentration_of_total_methylated_mercury_in_seawater"))
  thg[thg <= -998] <- NA
  mehg[mehg <= -998] <- NA
  tibble(
    LATITUDE = as.numeric(ncvar_get(nc, "lat")),
    LONGITUDE = as.numeric(ncvar_get(nc, "lon")),
    DEPTH = as.numeric(ncvar_get(nc, "alt")), # variable is named "alt" - "depth" is only its standard_name attribute
    DATE = as.Date(ncvar_get(nc, "time"), origin = "1970-01-01"),
    THG = thg * 1e9,
    MEHG = mehg * 1e9 * 1000
  )
}
RD_008_stations <- c(paste0("Q3P", c(1:7, "6ctd", "7ctd")), paste0("Q4P", c(1:7, "6ctd", "7ctd")))
RD_008_files <- map_chr(RD_008_stations, function(s) {
  fetch_source(
    paste0("https://opendap1.nodc.no/opendap/physics/point/cruise/nansen_legacy_ntnu/", s, ".nc"),
    paste0("../Data/Not_redistributed_data/downloaded/Kohler_et_al_2022_", s, ".nc")
  )
})
RD_008 <- map_dfr(RD_008_files, read_kohler_nc) |>
  mutate(YEAR = year(DATE), MONTH = month(DATE)) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG, MEHG) |>
  mutate(
    ID_DATASET = "RD-008",
    NAME_DATASET = "Kohler et al. 2022",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1038/s41561-022-00986-3",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://nmdc.no/",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(c(THG, MEHG), names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  drop_na(SPECIES_CONC) |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.07 ~ (-1 * SPECIES_CONC),
    # SPECIES_NAME == 'MEHG' & SPECIES_CONC <= 1 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### RD-009 - Torres-Rodriguez et al 2023 ----
### Instrument DL: THg = 0.03 pM (??), MeHg = 1 fM (??) - (all data in pM)
### Downloaded directly from figshare (item 24314527) - verified Sep 2026. That figshare item
### bundles 3 files (water-column profile, vent-fluid end-member chemistry, and rock geochemistry);
### confirmed Hermine_MasterSheet.xlsx (figshare file 42696556) is the one with the
### tHg_pM/MeHg_pM/Depth_m profile columns this script needs - the other two are NOT usable here.
RD_009_file <- fetch_source(
  "https://ndownloader.figshare.com/files/42696556",
  "../Data/Not_redistributed_data/downloaded/Torres-Rodriguez_et_al_2023_Hermine_MasterSheet.xlsx"
)
RD_009 <- read_excel(RD_009_file) |>
  mutate(THG = tHg_pM, MEHG = MeHg_pM * 1000, LONGITUDE = Longitude, LATITUDE = Latitude, DEPTH = Depth_m) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, THG, MEHG) |>
  mutate(
    ID_DATASET = "RD-009",
    CRUISE_NAME = "GEOTRACES GApr07 - HERMINE cruise",
    NAME_DATASET = "Torres-Rodriguez et al. 2023",
    YEAR = 2017,
    MONTH = 4,
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1038/s41561-023-01341-w",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://doi.org/10.6084/m9.figshare.24314527",
    COMMENT = "Hydrothermal vent fluid, cruise from 13th March to 28th April; DL uncertain",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.03 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 1 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### RD-010 - Lindeman et al 2024 ----
### DL: THg = 0.23 pM, MeHg = 15.6 fM
### Downloaded directly from Zenodo (record 7890489) - verified Sep 2026. The full file mixes
### offshore (rosette/niskin) and nearshore/river/lake (hand-sampled) stations; per confirmation,
### "offshore" = Cast 22, 23, 25, 26, 27 (all explicitly noted "offshore West..." in the raw file),
### matching the local file's curated 'offshore_data_for_DB' sheet this replaces. Coordinates in
### the raw file are degrees+decimal-minutes text (e.g. "65 33.999") rather than decimal degrees -
### converted here (Lon (W) also flipped to the negative/western convention used throughout this
### script; verified against the known Tasiilaq, Greenland field site).
dm_to_decimal <- function(x) {
  parts <- str_split_fixed(x, " ", 2)
  as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60
}
RD_010_file <- fetch_source(
  "https://zenodo.org/api/records/7890489/files/SF2021_Hg_data.csv/content",
  "../Data/Not_redistributed_data/downloaded/Lindeman_et_al_2023_SF2021_Hg_data.csv"
)
RD_010 <- read_csv(RD_010_file) |>
  filter(Cast %in% c("22", "23", "25", "26", "27")) |>
  rename(THG = "THg (pM)", MEHG = "MeHg (pM)", DEPTH = "Pressure (prog)") |>
  mutate(
    LATITUDE = dm_to_decimal(`Lat (N)`), LONGITUDE = -dm_to_decimal(`Lon (W)`),
    THG = as.numeric(THG), MEHG = as.numeric(MEHG) * 1000, DEPTH = as.numeric(DEPTH)
  ) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, THG, MEHG) |>
  mutate(
    ID_DATASET = "RD-010",
    NAME_DATASET = "Lindeman et al. 2023",
    YEAR = 2021,
    MONTH = 8,
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.21203/rs.3.rs-3289576/v1",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://zenodo.org/records/7890489",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.23 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 15.6 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### RD-011 -  Adams et al 2024 ----
### DL: THg = 0.22 pM, MMHg = 11.3 fM = 100 fM, DMHg = 2 fM, Hg0 = 40 fM
### Downloaded directly from BCO-DMO dataset 926873 (verified Sep 2026). The raw BCO-DMO export
### uses different column names than the PI's working file previously read locally here (e.g.
### THg_pM instead of '[THg] (pM)', no separate Date column - derived from ISO_DateTime_PDT
### instead) - mapping updated accordingly; unit handling (HG0 fM->pM, others kept in fM) unchanged.
RD_011_file <- fetch_source(
  "https://datadocs.bco-dmo.org/dataset/926873/file/N7GqOBoFj54g20/926873_v1_dissolved_hg_speciation_california_current_system.csv",
  "../Data/Not_redistributed_data/downloaded/Adams_et_al_2024_926873.csv"
)
RD_011 <- read_csv(RD_011_file) |>
  rename(
    THG = "THg_pM", MMHG = "MMHg_fM", DMHG = "DMHg_fM", HG0 = "Hg0_fM", LONGITUDE = "Longitude",
    LATITUDE = "Latitude", DEPTH = "Depth_m", SALINITY_PSU = "Salinity_PSU", TEMPERATURE_C = "Temperature_C",
    OXYGEN_UMOL_KG = "Oxygen_umol_kg"
  ) |>
  mutate(HG0 = HG0 / 1000) |>
  mutate(Date = as_datetime(ISO_DateTime_PDT)) |> # read_csv() may already parse this column as a datetime - as_datetime() handles both that and a raw ISO8601 string
  mutate(MONTH = as.numeric(format(Date, "%m")), YEAR = (as.numeric(format(Date, "%y"))) + 2000) |>
  mutate(
    MEHG = NA,
    MEHG = case_when(
      !is.na(DMHG) & !is.na(MMHG) ~ DMHG + MMHG,
      TRUE ~ MEHG
    )
  ) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG, MMHG, DMHG, HG0, MEHG) |>
  mutate(
    ID_DATASET = "RD-011",
    NAME_DATASET = "Adams et al. 2024",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://pubs.acs.org/doi/10.1021/acs.est.4c01112",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://www.bco-dmo.org/dataset/926873",
    COMMENT = "MeHg calculated as DMHg+MMHg",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.22 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MMHG" & SPECIES_CONC <= 11.3 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "DMHG" & SPECIES_CONC <= 2 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "HG0" & SPECIES_CONC <= 0.04 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 13.3 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### RD-012 - Capo and Cayian et al 2022 ----
### DL: THg = 0.25 pM, MeHg 49 fM - Hg in fM 
### NOT AUTOMATED: pubs.acs.org is behind an active Cloudflare "verify you are human" challenge on
### every access path (checked Sep 2026) - not scriptable with a plain download.file()/curl call.
### Kept as a local file; re-obtain SI file es2c03784_si_002.xlsx (sheet A) from
### https://doi.org/10.1021/acs.est.2c03784 if needed.
RD_012 <- read_excel("../Data/Not_redistributed_data/Capo_Cayian 2022_es2c03784_si_002.xlsx", sheet = "A") |>
  fill(Stations) |>
  slice(-c(1)) |>
  rename(
    DEPTH = "Depth", SALINITY_PSU = "Salinity", TEMPERATURE_C = "T", OXYGEN_mL_L = "O2",
    THG = "HgT", MEHG = "MeHg"
  ) |>
  mutate(
    THG = as.numeric(THG) / 1000, MEHG = as.numeric(MEHG), DEPTH = as.numeric(DEPTH),
    SALINITY_PSU = as.numeric(SALINITY_PSU), TEMPERATURE_C = as.numeric(TEMPERATURE_C), OXYGEN_mL_L = as.numeric(OXYGEN_mL_L),
    LATITUDE = case_when(
      Stations == "BY32" ~ 58.02,
      Stations == "BY15" ~ 57.33
    ),
    LONGITUDE = case_when(
      Stations == "BY32" ~ 17.98,
      Stations == "BY15" ~ 20.05
    )
  ) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, THG, MEHG, SALINITY_PSU, TEMPERATURE_C) |>
  mutate(
    ID_DATASET = "RD-012",
    YEAR = 2019,
    MONTH = 8,
    NAME_DATASET = "Capo and Cayian et al. 2022",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://pubs.acs.org/doi/10.1021/acs.est.2c03784",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.25 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 49 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### RD-013 - Heimburger et al 2015 ----
### DL: THg = 0.025 pM, MeHg = 1 fM
### Downloaded directly from PANGAEA (DOI 10.1594/PANGAEA.844492) - verified Sep 2026. The raw
### PANGAEA export has different column names/units than the reformatted local xlsx previously
### used here (e.g. 'Hg [pmol/l]' instead of 'tHg [pM]', no separate Month column - derived from
### Date/Time instead); mapped accordingly. skip=29 matches the live file's header block length as
### of this check - re-verify if PANGAEA re-versions the dataset.
RD_013_file <- fetch_source(
  "https://doi.pangaea.de/10.1594/PANGAEA.844492?format=textfile",
  "../Data/Not_redistributed_data/downloaded/Heimburger_et_al_2015_PANGAEA_844492.tab"
)
RD_013 <- read.table(RD_013_file, header = TRUE, sep = "\t", skip = 29) |>
  rename(
    THG = "Hg..pmol.l.", MEHG = "MeHg..pmol.l.",
    LATITUDE = "Latitude", LONGITUDE = "Longitude", DEPTH = "Depth.water..m.", Date = "Date.Time"
  ) |>
  mutate(
    MEHG = MEHG * 1000,
    MONTH = as.numeric(str_sub(Date, 6, 7))
  ) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, MONTH, THG, MEHG) |>
  mutate(
    ID_DATASET = "RD-013",
    YEAR = 2011, # all samples from the same 2011 cruise - matches Date/Time year in the raw file
    NAME_DATASET = "Heimburger et al. 2015",
    CRUISE_NAME = "TransArc ARK XXVI/3",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://www.nature.com/articles/srep10318",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://doi.pangaea.de/10.1594/PANGAEA.844492",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.025 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 1 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))


### RD-014 - Lim et al 2024 ----
### DL: THg = XX pM
### Downloaded directly from the paper's Nature Communications Source Data file (MOESM7_ESM.xlsx,
### confirmed to be the single combined Source Data file for this paper) - verified Sep 2026. Row
### [76:88]/column position references are unchanged from the local copy since this is the same
### file; re-verify the row/column indices still line up on first run.
RD_014_file <- fetch_source(
  "https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-024-51852-2/MediaObjects/41467_2024_51852_MOESM7_ESM.xlsx",
  "../Data/Not_redistributed_data/downloaded/Lim_et_al_2024_MOESM7_ESM.xlsx"
)
RD_014 <- read_excel(RD_014_file)[76:88, ] |>
  rename(LATITUDE = "...4", LONGITUDE = "...5", DEPTH = "...7", THG = "...10") |>
  mutate(
    LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE),
    DEPTH = as.numeric(DEPTH), THG = as.numeric(THG)
  ) |>
  dplyr::select(LATITUDE, LONGITUDE, DEPTH, THG) |>
  distinct() |>
  mutate(
    ID_DATASET = "RD-014",
    YEAR = 2022,
    MONTH = 8,
    NAME_DATASET = "Lim et al. 2024",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://www.nature.com/articles/s41467-024-51852-2",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")

### RD-015 - Adams et al 2025 ----
### LOD THg 0.17 pM
### Downloaded directly from BCO-DMO dataset 950021 (verified Sep 2026) - same raw export as the
### local file this replaces (identical filename/columns).
RD_015_file <- fetch_source(
  "https://datadocs.bco-dmo.org/dataset/950021/file/GwrDMjXf1nyRPJ/950021_v1_mercurytimeseries.csv",
  "../Data/Not_redistributed_data/downloaded/Adams_et_al_2025_950021.csv"
)
RD_015 <- read_csv(RD_015_file) |>
  pivot_longer(Surface_THg_Concentration:Deep_THg_Concentration, names_to = "name", values_to = "SPECIES_CONC") |>
  mutate(
    MONTH = month(Date), YEAR = year(Date),
    SPECIES_NAME = "THG",
    Deep_Sample_Depth = if_else(name == "Surface_THg_Concentration", 0.5, Deep_Sample_Depth),
    Deep_Sample_Depth = if_else(is.na(Deep_Sample_Depth), 6, Deep_Sample_Depth)
  ) |>
  rename(LATITUDE = "Latitude", LONGITUDE = "Longitude", DEPTH = "Deep_Sample_Depth") |>
  dplyr::select(YEAR, MONTH, DEPTH, LATITUDE, LONGITUDE, SPECIES_NAME, SPECIES_CONC) |>
  mutate(
    ID_DATASET = "RD-015",
    NAME_DATASET = "Adams et al. 2025",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.21203/rs.3.rs-5760721/v1; https://doi.org/10.1038/s43247-025-02263-8",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://www.bco-dmo.org/dataset/950021",
    DOI_DATASET = "DOI: 10.26008/1912/bco-dmo.950021.1",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.17 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### RD-016 - Starr et al 2025 ----
### Flag 9: NA, 3: probably bad (removed), 2: probably good (keep), 6: below detection (keep minus)
### NOT AUTOMATED: BCO-DMO dataset 950492 is still marked "Data not available... currently being
### processed" (checked Sep 2026) - genuinely nothing to download yet, not an access restriction.
### Kept as a local file; check https://www.bco-dmo.org/dataset/950492 (and the related Leg 2
### dataset at /dataset/950510) periodically before the paper's final submission.
RD_016a <- read_excel("../Data/Not_redistributed_data/Starr et al 2025/RR1815_DOoR Dissolved and Particulate Hg.xlsx",
  skip = 11,
  col_types = c(c("numeric", "numeric", "numeric", "text", "date", "date"), (rep(c("numeric"), 21)))
) |>
  rename(
    DEPTH = "Sample Depth*", LATITUDE = "Start Latitude*", LONGITUDE = "Start Longitude*", DATE = "Start Date (UTC)*",
    THG_D = "Hg_D_CONC_BOTTLE::9imt4n", MEHG_D = "Hg_Me_D_CONC_BOTTLE::n8ldsp",
    THG_P = "Hg_SPT_CONC_PUMP::ht6atl", MMHG_P = "Hg_MM_SPT_CONC_PUMP::blitbi",
    THG_D_FLAG = "Flag::Hg_D_CONC_BOTTLE::9imt4n", MEHG_D_FLAG = "Flag::Hg_Me_D_CONC_BOTTLE::n8ldsp",
    THG_P_FLAG = "Flag::Hg_SPT_CONC_PUMP::ht6atl", MMHG_P_FLAG = "Flag::Hg_MM_SPT_CONC_PUMP::blitbi"
  )

RD_016 <- read_excel("../Data/Not_redistributed_data/Starr et al 2025/RR1814_DOoR Dissolved and Particulate Hg.xlsx",
  skip = 11,
  col_types = c(c("numeric", "numeric", "numeric", "text", "date", "date"), (rep(c("numeric"), 21)))
) |>
  rename(
    DEPTH = "Sample Depth*", LATITUDE = "Start Latitude*", LONGITUDE = "Start Longitude*", DATE = "Start Date (UTC)*",
    THG_D = "Hg_D_CONC_BOTTLE::lvplou", MEHG_D = "Hg_Me_D_CONC_BOTTLE::tkk791",
    THG_P = "Hg_SPT_CONC_PUMP::rfckfs", MMHG_P = "Hg_MM_SPT_CONC_PUMP::iaiz13",
    THG_D_FLAG = "Flag::Hg_D_CONC_BOTTLE::lvplou", MEHG_D_FLAG = "Flag::Hg_Me_D_CONC_BOTTLE::tkk791",
    THG_P_FLAG = "Flag::Hg_SPT_CONC_PUMP::rfckfs", MMHG_P_FLAG = "Flag::Hg_MM_SPT_CONC_PUMP::iaiz13"
  ) |>
  bind_rows(RD_016a) |>
  filter(!is.na(DEPTH), DEPTH >= 0, LATITUDE >= 0, LONGITUDE >= -200) |>
  mutate(
    MONTH = month(DATE), YEAR = year(DATE),
    THG_D = if_else(THG_D_FLAG == 9, NA_real_, THG_D),
    THG_D = if_else(THG_D_FLAG == 3, NA_real_, THG_D),
    THG_D = if_else(THG_D < 0, NA_real_, THG_D),
    MEHG_D = if_else(MEHG_D_FLAG == 9, NA_real_, MEHG_D),
    MEHG_D = if_else(MEHG_D_FLAG == 3, NA_real_, MEHG_D),
    MEHG_D = if_else(MEHG_D < 0, NA_real_, MEHG_D),
    THG_P = if_else(THG_P < 0, NA_real_, THG_P),
    THG_P = if_else(THG_P_FLAG == 9, NA_real_, THG_P),
    THG_P = if_else(THG_P_FLAG == 6, THG_P * (-1), THG_P),
    MMHG_P = if_else(MMHG_P < 0, NA_real_, MMHG_P),
    MMHG_P = if_else(MMHG_P_FLAG == 9, NA_real_, MMHG_P),
    MMHG_P = if_else(MMHG_P_FLAG == 6, MMHG_P * (-1), MMHG_P),
    MEHG_D = MEHG_D * 1000, # from pM to fM
    MMHG_P = MMHG_P * 1000 # from pM to fM
  ) |>
  # select(-c('Column title','Gear ID* (if applicable)','End Date (UTC)* (if applicable)',
  #          'End Time (UTC)* (if applicable)','Event ID* (if applicable)','End Latitude* (if applicable)',
  #          'End Longitude* (if applicable)','Rosette Position* (if applicable)','Sample ID*',
  #          '1SD::Hg_D_CONC_BOTTLE::lvplou','1SD::Hg_Me_D_CONC_BOTTLE::tkk791','1SD::Hg_MM_SPT_CONC_PUMP::iaiz13',
  #          '1SD::Hg_SPT_CONC_PUMP::rfckfs','Start Time (UTC)*','Station ID*','DATE',
  #          'MMHG_P_FLAG','MEHG_D_FLAG','THG_D_FLAG','THG_P_FLAG')) |> # flag 1, 2, 3, 6, 9
  dplyr::select(YEAR, MONTH, DEPTH, LATITUDE, LONGITUDE, THG_D, MEHG_D, MMHG_P, THG_P) |>
  mutate(
    ID_DATASET = "RD-016",
    NAME_DATASET = "Starr et al. 2025",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2024JC021672",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://www.bco‐dmo.org/dataset/950492",
    DOI_DATASET = "https://10.26008/1912/bco‐dmo.950492.1",
    COMMENT = "Flags 9: NA, 3: probably bad (removed), 2: probably good (kept), 6: below detection (kept as minus)",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG_D:THG_P, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  filter(!is.na(SPECIES_CONC)) |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG_D" & SPECIES_CONC <= 0.1 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG_D" & SPECIES_CONC <= 20 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### RD-017 - Tate et al 2025 ----
### NOT AUTOMATED: USGS ScienceBase now sits behind a Cloudflare bot-check that blocks direct
### programmatic downloads (confirmed Sep 2026 - the file URL below 403s even from a real browser
### until its "Verify you are human" challenge is solved by hand). Note: the ScienceBase item ID
### used elsewhere in this script/tracking sheet (67605140d34e03058f2207342) has an extra trailing
### digit and 404s - the correct item, confirmed by resolving DOI 10.5066/P14KDQHN, is
### 67605140d34e03058f220734 (REPOSITORY field below corrected to match). Re-obtain both files via
### https://www.sciencebase.gov/catalog/item/67605140d34e03058f220734 if they need refreshing.
RD_017i <- read_csv("../Data/Not_redistributed_data/Tate_et_al_2025_Site_Information.csv")

RD_017 <- read_csv("../Data/Not_redistributed_data/Tate_et_al_2025_Hg_Concentrations_Water.csv") |>
  mutate(Sample_Date = mdy(Sample_Date)) |> # Convert to Date
  left_join(RD_017i, by = c("Site_Name", "Cruise_Section")) |>
  filter(Cruise_Section == "A16N" | Cruise_Section == "I05" | Cruise_Section == "S04P" | Cruise_Section == "P16N") |>
  filter(Replicate == 1) |>
  mutate(SPECIES_NAME = "THG", SPECIES_CONC = uTHg * 5, MONTH = month(Sample_Date), YEAR = year(Sample_Date)) |>
  rename(LATITUDE = "Latitude", LONGITUDE = "Longitude", CRUISE_NAME = "Cruise_Section", DEPTH = "Sample_Depth") |>
  select(-c("Replicate", "uTHg", "Ocean_Basin", "Site_Name", "Sample_Date")) |>
  mutate(
    ID_DATASET = "RD-017",
    NAME_DATASET = "Tate et al. 2025",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1021/acs.est.4c13434", # https://doi.org/10.5066/P14KDQHN
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://www.sciencebase.gov/catalog/item/67605140d34e03058f220734",
    DOI_DATASET = "https://doi.org/10.5066/P14KDQHN",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  )

### RD-018 - Biester et al 2026 ----
### Downloaded directly from PANGAEA (DOI 10.1594/PANGAEA.987320) - verified Sep 2026. The local
### file was already this exact raw PANGAEA export (skip=31 matches the live file's header block
### length as of this check; re-verify if PANGAEA re-versions the dataset).
RD_018_file <- fetch_source(
  "https://doi.pangaea.de/10.1594/PANGAEA.987320?format=textfile",
  "../Data/Not_redistributed_data/downloaded/Biester_et_al_2026_PANGAEA_987320.tab"
)
RD_018 <- read.table(RD_018_file, header = TRUE, sep = "\t", skip = 31) |>
  rename(
    LATITUDE = "Latitude", LONGITUDE = "Longitude", DEPTH = "Depth.water..m.",
    THG = "Hg..pmol.l.", MEHG = "MeHg..pmol.l.", Date = "Date.Time"
  ) |>
  mutate(
    MEHG = MEHG * 1000,
    YEAR = as.numeric(str_sub(Date, 1, 4)),
    MONTH = as.numeric(str_sub(Date, 6, 7))
  ) |>
  dplyr::select(YEAR, MONTH, DEPTH, LATITUDE, LONGITUDE, THG, MEHG) |>
  mutate(
    ID_DATASET = "RD-018",
    NAME_DATASET = "Biester et al. 2026",
    CRUISE_NAME = " Island Impact (PS133/1)",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1029/2025GB008767",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://doi.pangaea.de/10.1594/PANGAEA.987320",
    COMMENT = "Method 1631, Revision E: Mercury in Water by Oxidation, Purge and Trap, and Cold Vapor Atomic Absorption Fluorescence Spectrometry (2002). U.S. Environmental Protection Agency, Office of Water, 38 p.",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.03 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 4 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### RD-019 - Smith et al 2026 ----
### Note: BCO-DMO's own citation/contributor metadata names this "Smith" (Sophia Smith, lead
### citation author) - "Schmidt" in the tracking spreadsheet was a mix-up with co-PI Chad
### Hammerschmidt; corrected here and NAME_DATASET below already used "Smith" correctly.
### Downloaded directly from BCO-DMO dataset 990899 (verified Sep 2026) - same raw export as the
### local file this replaces (identical filename/columns).
RD_019_file <- fetch_source(
  "https://datadocs.bco-dmo.org/dataset/990899/file/LAoD3rQsQlPzqm/990899_v1_gulf_of_maine_hg.csv",
  "../Data/Not_redistributed_data/downloaded/Smith_et_al_2026_990899.csv"
)
RD_019 <- read_csv(RD_019_file) |>
  filter(Station_Num <= 8 | Station_Num == 14 | Station_Num == 15) |>
  rename(
    LATITUDE = "Latitude_N", LONGITUDE = "Longitude_W", SALINITY_PSU = "Salinity", TEMPERATURE_C = "Temp",
    OXYGEN_umol_kg = "Oxygen", THG = "Bulk_THg", MEHG = "Bulk_MeHg", DEPTH = "Sample_Depth_m",
    CHLA_ug_L = "Chl_a", THG_P = "pTHg", MEHG_P = "pMeHg"
  ) |>
  mutate(
    MONTH = month(Date_Time_UTC),
    YEAR = year(Date_Time_UTC),
    MEHG = MEHG * 1000,
    MEHG_P = MEHG_P * 1000
  ) |>
  select(
    YEAR, MONTH, DEPTH, LATITUDE, LONGITUDE, THG, MEHG, THG_P, MEHG_P, SALINITY_PSU, TEMPERATURE_C,
    OXYGEN_umol_kg, CHLA_ug_L
  ) |>
  mutate(
    ID_DATASET = "RD-019",
    NAME_DATASET = "Smith et al. 2026",
    CRUISE_NAME = " EN699",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://doi.org/10.1029/2025JG009303",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "https://www.bco-dmo.org/project/896019",
    COMMENT = "",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MEHG_P, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
  mutate(SPECIES_CONC = case_when(
    SPECIES_NAME == "THG" & SPECIES_CONC <= 0.4 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG" & SPECIES_CONC <= 60 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "THG_P" & SPECIES_CONC <= 0.04 ~ (-1 * SPECIES_CONC),
    SPECIES_NAME == "MEHG_P" & SPECIES_CONC <= 5 ~ (-1 * SPECIES_CONC),
    TRUE ~ SPECIES_CONC
  ))

### RD-020 - Jiskra et al 2021 ----
### Downloaded directly from the paper's Nature Source Data file (MOESM3_ESM.xlsx, confirmed to be
### specifically "Source Data Fig. 1" - the file this script already used) - verified Sep 2026.
RD_020_file <- fetch_source(
  "https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-021-03859-8/MediaObjects/41586_2021_3859_MOESM3_ESM.xlsx",
  "../Data/Not_redistributed_data/downloaded/Jiskra_et_al_2021_MOESM3_ESM.xlsx"
)
RD_020 <- read_excel(RD_020_file) |>
  mutate(
    THG = THg_Jun2017, MEHG = MeHg_Jun2017 * 1000, DEPTH = depth,
    YEAR = 2017, MONTH = 6
  ) |>
  filter(grepl("2017", Sample_ID)) |>
  select(c(THG, MEHG, YEAR, MONTH, DEPTH)) |>
  mutate(
    ID_DATASET = "RD-020",
    NAME_DATASET = "Jiskra et al. 2021",
    LATITUDE = 42.98,
    LONGITUDE = 5.410,
    CRUISE_NAME = "ICC2017",
    PUBLISHED_IN_PAPER = "YES",
    DOI_PAPER_REFERENCE = "https://www.nature.com/articles/s41586-021-03859-8",
    DATASET_PUBLISHED = "YES",
    REPOSITORY = "Paper Supporting Information",
    COMMENT = "",
    ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
  ) |>
  pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")


## Datasets awaiting author permission ----
# The following datasets are not included in the compiled database. Their read-in code is
# written below but fully commented out, since author consent to redistribute the data as part
# of this compilation has not yet been confirmed. Datasets marked (AUR) have a
# "data available upon request" statement in their publication - the data has already been
# obtained directly from the authors on that basis, but consent to redistribute it as part of
# this compiled database is still pending:
#   AP-006 Lamborg unp. - https://doi.org/10.1016/j.chemgeo.2018.05.040
#   AP-010 Chen et al. 2024 (AUR) - https://www.sciencedirect.com/science/article/pii/S0043135424006936
#   AP-014 Carrasco et al. 2024 (AUR) - https://www.sciencedirect.com/science/article/pii/S0048969723062708
#   AP-015 Yang et al. 2023 (AUR) - https://www.sciencedirect.com/science/article/pii/S0043135423005869
#   AP-016 Nascimento et al. 2025 (AUR) - https://www.sciencedirect.com/science/article/pii/S0013935125003809
# To reinstate a pending dataset: uncomment its block below, place the data file where its
# read_excel()/read_csv() call expects it, then add it to the bind_rows() call in "Bind data to
# fill data_header file" below.

### AP-006 - Lamborg unpublished ----
# AP_006 <- read_excel("../Data/Data provided by authors/Lamborg unpublished.xlsx") |>
#   rename(
#     STATION = "Station", DATE = "yyyy-mm-ddThh:mm:ss.sss", LONGITUDE = "Longitude [degrees_east]", LATITUDE = "Latitude [degrees_north]",
#     TEMPERATURE_C = "Temperature [oC]", SALINITY_PSU = "Salinity", OXYGEN_umol_kg = "CTDOXY [mol/kg]", DEPTH = "Depth [m]",
#     THG_D = "Total Dissolved Mercury [pmole/kg]"
#   ) |>
#   mutate(YEAR = year(DATE), MONTH = month(DATE), LONGITUDE = LONGITUDE - 360) |>
#   fill(LATITUDE, LONGITUDE, YEAR, MONTH) |>
#   dplyr::select(LATITUDE, LONGITUDE, DEPTH, YEAR, MONTH, THG_D, TEMPERATURE_C, SALINITY_PSU, OXYGEN_umol_kg) |>
#   mutate(
#     ID_DATASET = "AP-006",
#     NAME_DATASET = "Lamborg unp.",
#     CRUISE_NAME = "JC057 (GA02)",
#     PUBLISHED_IN_PAPER = "YES",
#     DOI_PAPER_REFERENCE = "https://doi.org/10.1016/j.chemgeo.2018.05.040",
#     DATASET_PUBLISHED = "NO",
#     COMMENT = "Part of GEOTRACES but not published; contact author before use",
#     ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
#   ) |>
#   pivot_longer(THG_D, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")

### AP-010 - Chen et al. 2024 ----
### DL: MeHg = 0.02 ng/L = 100 fM
# AP_010a <- read_excel("../Data/Data provided by authors/Chen et al 2024.xlsx", sheet = "2015.8-9 autumn") |>
#   pivot_wider(names_from = SPECIES_NAME, values_from = SPECIES_CONC) |>
#   rename(
#     THG = "THg_ngL", MEHG = "MeHg_ngL", LONGITUDE = "longitude_degrees", LATITUDE = "latitude_degrees",
#     DEPTH = "Sampling Depth_m"
#   ) |>
#   mutate(THG = THG / 200 * 1000, MEHG = MEHG / 200 * 1000 * 1000) |>
#   dplyr::select(LATITUDE, LONGITUDE, DEPTH, THG, MEHG) |>
#   mutate(
#     ID_DATASET = "AP-010",
#     NAME_DATASET = "Chen et al. 2024",
#     YEAR = 2015,
#     MONTH = 8,
#     PUBLISHED_IN_PAPER = "YES",
#     DOI_PAPER_REFERENCE = "https://www.sciencedirect.com/science/article/pii/S0043135424006936",
#     DATASET_PUBLISHED = "NO",
#     COMMENT = "Available upon request per the paper's data availability statement; already obtained from the authors, contact before use",
#     ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
#   ) |>
#   pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")
#
# AP_010b <- read_excel("../Data/Data provided by authors/Chen et al 2024.xlsx", sheet = "2016.6 summer") |>
#   pivot_wider(names_from = SPECIES_NAME, values_from = SPECIES_CONC) |>
#   rename(
#     THG = "THg_ngL", MEHG = "MeHg_ngL", LONGITUDE = "longitude_degrees", LATITUDE = "latitude_degrees",
#     DEPTH = "Sampling Depth_m"
#   ) |>
#   mutate(THG = THG / 200 * 1000, MEHG = MEHG / 200 * 1000 * 1000) |>
#   dplyr::select(LATITUDE, LONGITUDE, DEPTH, THG, MEHG) |>
#   mutate(
#     ID_DATASET = "AP-010",
#     NAME_DATASET = "Chen et al. 2024",
#     YEAR = 2016,
#     MONTH = 6,
#     PUBLISHED_IN_PAPER = "YES",
#     DOI_PAPER_REFERENCE = "https://www.sciencedirect.com/science/article/pii/S0043135424006936",
#     DATASET_PUBLISHED = "NO",
#     COMMENT = "Available upon request per the paper's data availability statement; already obtained from the authors, contact before use",
#     ID_SAMPLE = paste("S", ID_DATASET, 500:(n() + 499), sep = "_")
#   ) |>
#   pivot_longer(THG:MEHG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")
#
# AP_010 <- bind_rows(AP_010a, AP_010b) |>
#   mutate(SPECIES_CONC = case_when(
#     SPECIES_NAME == "MEHG" & SPECIES_CONC <= 100 ~ (-1 * SPECIES_CONC),
#     TRUE ~ SPECIES_CONC
#   ))


### AP-014 - Carrasco et al. 2024 ----
# AP_014 <- read_excel("../Data/Data provided by authors/Carrasco et al 2024.xlsx") |>
#   rename(THG = "Aqueous TotHg (ng/L)", THG_D = "Dissolved TotHg (ng/L)", THG_P = "PartTotHg (ng/L)") |>
#   mutate(
#     THG = THG * 1000 / 200, THG_D = as.numeric(THG_D) * 1000 / 200, THG_P = THG_P * 1000 / 200,
#     TEMPERATURE_C = as.numeric(TEMPERATURE_C),
#     MONTH = case_when(
#       Month == "April" ~ 4,
#       Month == "May" ~ 5,
#       Month == "June" ~ 6,
#       Month == "July" ~ 7,
#       Month == "August" ~ 8
#     )
#   ) |>
#   dplyr::select(MONTH, LATITUDE, LONGITUDE, THG, THG_D, THG_P, SALINITY_PSU, TEMPERATURE_C) |>
#   mutate(
#     ID_DATASET = "AP-014",
#     YEAR = 2018,
#     NAME_DATASET = "Carrasco et al. 2024",
#     PUBLISHED_IN_PAPER = "YES",
#     DOI_PAPER_REFERENCE = "https://www.sciencedirect.com/science/article/pii/S0048969723062708",
#     DATASET_PUBLISHED = "NO",
#     COMMENT = "Available upon request per the paper's data availability statement; already obtained from the authors, contact before use. Latitude/longitude read from a map in the paper.",
#     ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
#   ) |>
#   pivot_longer(THG_D:THG_P, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")

### AP-015 - Yang et al. 2023 ----
# AP_015 <- read_excel("../Data/Data provided by authors/Yang et al 2023.xlsx", sheet = "Data_Table_S1_S4") |>
#   filter(Seawater != "Sample") |>
#   filter(LATITUDE < 22.3) |>
#   mutate(
#     THG_D = as.numeric(THG) * 1000 / 200, MEHG_D = as.numeric(MEHG) * 1000 * 1000 / 200,
#     LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE)
#   ) |>
#   mutate(DEPTH = 1) |>
#   dplyr::select(YEAR, MONTH, DEPTH, LATITUDE, LONGITUDE, THG_D, MEHG_D) |>
#   mutate(
#     ID_DATASET = "AP-015",
#     NAME_DATASET = "Yang et al. 2023",
#     PUBLISHED_IN_PAPER = "YES",
#     DOI_PAPER_REFERENCE = "https://www.sciencedirect.com/science/article/pii/S0043135423005869",
#     DATASET_PUBLISHED = "NO", # obtained via direct author contact, not from the paper's Supporting Information as originally assumed
#     COMMENT = "Assumed to be surface samples. Contact author before use; not yet cleared for redistribution",
#     ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
#   ) |>
#   pivot_longer(THG_D:MEHG_D, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC")

### AP-016 - Nascimento et al. 2025 ----
# AP_016 <- read_excel("../Data/Data provided by authors/Nascimento et al 2025.xlsx") |>
#   rename(LATITUDE = "Latitude", LONGITUDE = "Longitude", SALINITY_PSU = "Salinity") |>
#   mutate(DEPTH = 2, THG = as.numeric(Hg_pmolL)) |>
#   pivot_longer(THG, names_to = "SPECIES_NAME", values_to = "SPECIES_CONC") |>
#   dplyr::select(DEPTH, LATITUDE, LONGITUDE, SALINITY_PSU, SPECIES_NAME, SPECIES_CONC) |>
#   mutate(
#     ID_DATASET = "AP-016",
#     YEAR = 2018,
#     MONTH = 5,
#     NAME_DATASET = "Nascimento et al. 2025",
#     CRUISE_NAME = "R/V Meteor M147 cruise",
#     PUBLISHED_IN_PAPER = "YES",
#     DOI_PAPER_REFERENCE = "https://www.sciencedirect.com/science/article/pii/S0013935125003809",
#     DATASET_PUBLISHED = "NO",
#     COMMENT = "Available upon request per the paper's data availability statement; already obtained from the authors, contact before use",
#     ID_SAMPLE = paste("S", ID_DATASET, 1:n(), sep = "_")
#   ) |>
#   mutate(SPECIES_CONC = case_when(
#     SPECIES_NAME == "THG" & SPECIES_CONC <= 0.1 ~ (-1 * SPECIES_CONC),
#     TRUE ~ SPECIES_CONC
#   ))


## Bind data to fill data_header file ----

DATA_COMBINED <- bind_rows(
  DATA_HEADER,
  # PT - extracted table in publication
  PT_001, PT_002, PT_003, PT_004, PT_005, PT_006, PT_007, PT_008, PT_009,
  PT_010, PT_011, PT_012, PT_013, PT_014, PT_015, PT_016, PT_017, PT_018,
  # MD - monitoring data
  MD_001,
  # AP - author-permitted (unpublished, with consent)
  AP_001, AP_002, AP_003, AP_004, AP_005, AP_007, AP_008, AP_009, AP_011, AP_012,
  AP_013,
  # RD - repository-downloaded
  RD_001, RD_002, RD_003, RD_004, RD_005, RD_006, RD_007, RD_008, RD_009, RD_010,
  RD_011, RD_012, RD_013, RD_014, RD_015, RD_016, RD_017, RD_018, RD_019, RD_020
) |>
  drop_na(SPECIES_CONC) |>
  left_join(SPECIES, by = "SPECIES_NAME")


### Add basin names - and potentially drop geometry----
DATA_WITH_REGIONS <- DATA_COMBINED |>
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(map_low_res), remove = FALSE)
DATA_WITH_REGIONS <- st_join(DATA_WITH_REGIONS, map_low_res) |>
  select(-c("latitude", "longitude", "min_Y", "min_X", "max_Y", "max_X")) |>
  rename(BASIN = "name", BASIN_AREA_KM2 = "area_km2")
DATA_WITH_REGIONS <- st_join(DATA_WITH_REGIONS, map_ipcc) |>
  rename(IPCC_CONTINENT = "Continent", IPCC_TYPE = "Type", IPCC_NAME = "Name")
DATA_WITH_REGIONS <- st_join(DATA_WITH_REGIONS, map_high_res) |>
  select(-c("Latitude", "Longitude", "min_Y", "min_X", "max_Y", "max_X", "ID", "MRGID", "geometry")) |>
  rename(BASIN_REGION = "NAME", BASIN_REGION_AREA_KM2 = "area") |>
  st_drop_geometry()


HgOceanDb <- DATA_WITH_REGIONS |>
  filter(ID_SAMPLE != "S_PT-011_23") |> # remove duplicate for sample on border between two regions (was S_37_23 = Fu et al. 2010, now PT-011)
  select(c(
    "ID_DATASET", "NAME_DATASET", "BASIN", "BASIN_REGION", "BASIN_AREA_KM2", "BASIN_REGION_AREA_KM2",
    "IPCC_CONTINENT", "IPCC_TYPE", "IPCC_NAME",
    "LATITUDE", "LONGITUDE", "DEPTH", "YEAR", "MONTH",
    "SPECIES_NAME", "SPECIES_CONC", "UNIT",
    "SALINITY_PSU", "TEMPERATURE_C", "OXYGEN_umol_kg", "CHLA_ug_L", "ID_SAMPLE", "CRUISE_NAME", "PUBLISHED_IN_PAPER", "DOI_PAPER_REFERENCE",
    "DATASET_PUBLISHED", "REPOSITORY", "DOI_DATASET", "DEFINITION", "COMMENT"
  ))

## Create individual files for database

SOURCES <- HgOceanDb |>
  dplyr::select(
    "ID_DATASET", "NAME_DATASET", "CRUISE_NAME", "PUBLISHED_IN_PAPER", "DOI_PAPER_REFERENCE",
    "DATASET_PUBLISHED", "REPOSITORY", "DOI_DATASET", "COMMENT"
  ) |>
  distinct(ID_DATASET, NAME_DATASET, CRUISE_NAME, .keep_all = TRUE)

# add current date to written csv version
currentDate <- Sys.Date()

## Write database files to csv ----
write_csv(HgOceanDb, paste("../Database/HgOceanDb_", currentDate, ".csv", sep = ""), na = "")
write_csv(SOURCES, paste("../Database/Sources_", currentDate, ".csv", sep = ""), na = "")
