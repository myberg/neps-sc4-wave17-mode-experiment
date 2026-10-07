# Data availability

No individual-level NEPS data are distributed in this repository.
The manuscript was analysed using NEPS Starting Cohort 4 data including Wave 17 fieldwork information that had not yet been released as Scientific Use File (SUF) 17.0.0 at the time this replication release was prepared.

The Wave 17 fieldwork, testing, interview, and related variables used in the analyses are expected to be available through the official SC4 SUF 17.0.0.
As the official file structure and final variable mapping were not yet available at submission, `R/01_data_preparation.R` is intentionally not implemented against guessed file names or variable locations.

## Submission release

For the submission-version code, the analysis scripts expect a prepared R object at

```text
data/derived/AuswertungB160.RData
```

containing an object named `AuswertungB160`.
This file contains restricted individual-level data and is therefore excluded from the public repository.

## Planned update after SUF 17.0.0 release

After SC4 SUF 17.0.0 becomes available, `R/01_data_preparation.R` will be updated to read the official SUF files directly and construct the analysis data without relying on non-public project-specific source objects.
A new archived release can then provide the SUF-based replication workflow for authorised NEPS users.
