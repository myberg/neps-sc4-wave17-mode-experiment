# R environment

Do not invent package versions for the public archive. Capture the exact
environment used for the final manuscript.

From the final analysis environment, run for example:

```r
dir.create("environment", showWarnings = FALSE)

capture.output(
  sessionInfo(),
  file = file.path("environment", "sessionInfo.txt")
)

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

renv::init(bare = TRUE)
renv::snapshot(prompt = FALSE)
```

After checking the generated `renv.lock`, commit it together with
`environment/sessionInfo.txt` before creating the Zenodo release.
