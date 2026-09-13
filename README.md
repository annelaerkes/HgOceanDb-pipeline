# HgOceanDb Pipeline

An R pipeline that compiles mercury-in-seawater datasets from published papers, public repositories, and monitoring programs into a single standardized, long-format database (`HgOceanDb`), along with a companion `Sources` table listing the citation, repository, and data-sharing status of every contributing dataset.

Rather than redistributing other researchers' data directly, this repository distributes the *code* needed to reconstruct the compiled database wherever possible. Running `R/create_database.R` downloads each source dataset directly from its original repository (where possible) or reads it from a local copy you provide, applies unit conversions and quality flags as documented in the script, and writes out `HgOceanDb_<date>.csv` and `Sources_<date>.csv` to a `Database/` folder at the repo root.

The exception is a handful of datasets that were only available as a table within a published paper's PDF or Word document, with no separate machine-readable file to download. These were manually transcribed and are included directly in this repository as their own Excel/CSV files (see `Data/Extracted_table_in_publication/`), since redistributing a transcription of content that's already published and publicly readable raises no additional concerns. The same applies to a small number of datasets shared directly by their authors, or obtained via a data request, where explicit permission for inclusion in this compilation has already been confirmed.

## Getting started

Requirements: R, with the following packages installed - `tidyverse` (includes `readr`, `dplyr`, `stringr`, `tibble`), `readxl`, `lubridate`, `ncdf4`, `httr`, and `sf`.

1. Clone this repository.
2. Open `R/HgOceanDb.Rproj` in RStudio (this sets the working directory to `R/`, which the script's relative paths assume) or otherwise set your working directory there yourself.
3. Run `create_database.R`. On first run it prints a checklist of any datasets that need a file placed manually (because the host doesn't support direct download links) - place those, then re-run.
4. `HgOceanDb_<date>.csv` and `Sources_<date>.csv` are written to `Database/` at the repo root.

## Citation

This database is presented and analyzed in:

> Soerensen, A.L., Schartup, A.T., Adams, H.M., Bieser, J., Dastoor, A. (2026). *Integrating coastal and open ocean observations to explore mercury spatio-temporal variability in the global surface ocean.* Submitted to *Scientific Reports*.

This is a preliminary reference to a manuscript currently in review; it will be updated with the final citation once available.

If you use this pipeline, or a database generated from it, please cite the paper above **and** the individual contributing datasets you used, per their entries in `Sources.csv`.

## Before you use a downloaded dataset

Several datasets in this pipeline are fetched automatically at runtime from their original public repositories. This script only automates the download step — it is the user's responsibility to check the terms of use, license, or data-sharing agreement on each source website before using a downloaded dataset beyond the scope of this compilation.

## Dataset categories

Each dataset in the script is identified by a category-prefixed code (e.g. `PT-001`, `RD-014`) rather than a plain sequential number, so new datasets can always be added without renumbering existing ones. The categories are:

- **PT** - extracted table in publication: manually transcribed from a table in a paper (PDF/Word), because the underlying data isn't available as a separate machine-readable file.
- **MD** - monitoring data: obtained from an ongoing monitoring program via a data request.
- **AP** - author-permitted: unpublished data obtained directly from the authors, included with their consent. Datasets awaiting that consent are kept in this script as inactive, fully commented-out code under this same prefix, ready to activate once consent is confirmed (see the comments starting "Datasets awaiting author permission" in `create_database.R`).
- **RD** - repository-downloaded: sourced from a public repository or supplementary data file that's already machine-tabular. Most of these download automatically when the script runs; a few require manual placement first (the script prints a checklist of these on first run) because the host doesn't support direct download links.

## Dataset identifiers: `ID_DATASET`, `NAME_DATASET`, `CRUISE_NAME`

These three fields serve different purposes and don't always map one-to-one:

- **`ID_DATASET`** is the technical/reproducibility unit - the code block in this script that reads in one file or one download.
- **`NAME_DATASET`** is the citable unit - normally one publication, but a single `ID_DATASET` can correspond to *multiple* `NAME_DATASET` values when the source repository bundles data from several publications or cruises together (for example, the GEOTRACES Intermediate Data Product, which spans many independently-published cruises).
- **`CRUISE_NAME`** is the finest sampling-event scope, when known.

`Sources.csv` lists one row per unique citable dataset - deduplicated on `ID_DATASET`, `NAME_DATASET`, and `CRUISE_NAME` together - so it will generally have more rows than there are dataset-reading code blocks in the script.

## Repository structure

```
R/create_database.R    - the pipeline script
Data/                   - source data, organized by category (see script comments)
Database/               - compiled output (gitignored, regenerated on each run)
```

`Data/Not_redistributed_data/` isn't included in this repository - it's created automatically the first time you run the script, and holds datasets that download automatically or need manual placement (per the first-run checklist), as described above.

## Contributing

If you notice an issue, know of a dataset that should be added, or have a question about a dataset's status, please open an [issue](../../issues) on this repository.

## License

This repository is licensed under [CC BY 4.0](LICENSE). This covers the pipeline code and the datasets included directly within it (see above for which datasets that includes); it does not extend to datasets the script merely downloads or references from their own independently-hosted repositories, which remain subject to their own licenses and terms of use.
