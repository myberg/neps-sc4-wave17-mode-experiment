# Replication code

## Participation and Selectivity in Web-Based Cognitive Testing and Surveying:
## Evidence from a Randomized Mode Experiment

Authors: Michael Bergrab, Timo Gnambs, and Christian Aßmann 

This repository contains the analysis code for the manuscript.
It is a cleaned replication release derived from the internal working repository.
Manuscript drafts, notes, obsolete exploratory scripts, and restricted microdata are not included.

## Data availability

The analyses use data from NEPS Starting Cohort 4, including Wave 17 fieldwork information that had not yet been released as Scientific Use File (SUF) 17.0.0 at the time of submission.
Individual-level NEPS data are not redistributed in this repository.
The Wave 17 fieldwork and related variables used in the analyses are expected to be available in the official SC4 SUF 17.0.0.
Because that release was not yet available when this archive was prepared, the final mapping from official SUF 17.0.0 files to the analysis dataset cannot yet be implemented without guessing future file names or variable locations.

The submission-version repository therefore provides the complete analysis code and documents the expected analysis-data interface.
After release of SC4 SUF 17.0.0, the data-preparation layer will be updated to use the official SUF files directly.

No participant-level data, IDs, passwords, tokens, or institutional server
paths are included in this repository.

## Repository structure

```text
.
├── 00_run_analysis.R
├── R/
│   ├── 00_setup.R
│   ├── 00_functions.R
│   ├── 01_data_preparation.R
│   ├── 02_descriptives.R
│   ├── 03_binary_models.R
│   └── 04_multinomial_models.R
├── data/
│   └── README.md
├── environment/
│   └── README.md
├── output/
│   └── .gitkeep
├── REVIEW_BEFORE_PUBLICATION.md
├── CITATION.cff
└── .gitignore
```

## Running the submission-version analysis

The analysis scripts expect the prepared restricted analysis object at

```text
data/derived/AuswertungB160.RData
```

with an R object named `AuswertungB160`.

With that file available in an authorised environment, run:

```r
source("00_run_analysis.R")
```

The master script is intentionally short.
`R/00_setup.R` loads packages, functions, and the prepared analysis data.
The analysis then runs in the following order:

1. descriptive analyses;
2. stage-specific binary logit models, heterogeneity analyses, equivalence
   analyses, and sensitivity checks;
3. multinomial models of realised testing-interview states.

`R/01_data_preparation.R` documents the status of the data-preparation layer.
It will be connected directly to the official SC4 SUF 17.0.0 after that release becomes available.

## Internal coding and manuscript labels

The underlying fieldwork data use historical internal mode labels.
In the manuscript these correspond to:

| Internal coding | Manuscript label |
|---|---|
| `CAPI` | CBT+CAPI |
| `CALVI` | VBT+CAVI |
| `CAWI-TBT+CATI` | WBT+CATI |

The analyses consistently interpret these as **randomised initial fieldwork assignments**.
Subsequently realised modes, fallback routing, and mode switches are post-assignment fieldwork processes and are not treated as randomised exposures.

## Reproducibility environment

Before the repository is archived, generate an `renv.lock` file and a `sessionInfo.txt` from the exact environment used for the final manuscript.
Instructions are in `environment/README.md`.

## Variable naming

The public replication code consistently uses the SUF-derived covariate names `migBG_suf` and `ISCED_suf`.
Legacy development variables with the same content are not used in the public workflow.

## Files intentionally excluded from the replication release

Duplicate execution scripts, multiple-imputation development code, obsolete appendix/probit scripts, exploratory notes, manuscript drafts, and restricted microdata are not part of the public replication workflow.

## Citation

A machine-readable citation file is provided in `CITATION.cff`.
Add the Zenodo DOI after the first archived release is created.
