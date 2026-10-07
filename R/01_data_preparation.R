# ===
# Data preparation for the public replication workflow
# ===
#
# The manuscript was analysed before NEPS SC4 Scientific Use File (SUF) 17.0.0
# was publicly released. The final public data-preparation script therefore
# cannot yet be tied to the official SUF 17.0.0 file structure and variable
# names.
#
# At submission, the analysis scripts operate on the prepared restricted object
# `data/derived/AuswertungB160.RData`, which cannot redistributed.
#
# After release of SC4 SUF 17.0.0, this script will be updated to construct the
# analysis data directly from the official SUF files. The intended endpoint is
# an object named `AuswertungB160` containing the variables required by the
# analysis scripts, followed by:
#
#   save(
#     AuswertungB160,
#     file = file.path("data", "derived", "AuswertungB160.RData")
#   )
#
# The substantive recoding and modelling code is already contained in this
# repository. The future update will replace the pre-release input layer with
# the official SUF 17.0.0 input layer; it will not rely on non-releasable MYCP
# data.
# ===

stop(
  "SC4 SUF 17.0.0 was not yet publicly available when this replication ",
  "release was prepared. See data/README.md for the current data-access ",
  "statement."
)
