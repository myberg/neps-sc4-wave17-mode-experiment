# ===
# Stage-specific binary logit models
# Participation and Selectivity in Web-Based Cognitive Testing and Surveying:
# Evidence from a Randomized Mode Experiment
#
# Requires `00_run_analysis.R` and `R/00_functions.R`.
# The analytical commands are preserved; only paths, obsolete diagnostics,
# and presentation-level wording were cleaned.
# ===

# Überblick ------------------------------------------------------------------


# Datensatz definieren

# aber die wenigen unknown Fälle in ISCED bzw. CASMIN führen zu sehr hohen Schätzergebnissen,
# daher
dat_analysis_main <- AuswertungB160.deskriptiv %>% 
  filter(ISCED_suf != "unbekannt")

# names(dat_analysis)

# unter 4. Sensitivity wird u.a. mit dem full dataset gearbeitet.
# weitere siehe 4. direkt

# Datenaufbereitung -------------------------------------------------------

dat_analysis_main$Erwerbstätigkeit_aktuell <- factor(dat_analysis_main$Erwerbstätigkeit_aktuell,
                                                  levels = c(levels(dat_analysis_main$Erwerbstätigkeit_aktuell), "unbekannt"))
dat_analysis_main$Erwerbstätigkeit_aktuell[is.na(dat_analysis_main$Erwerbstätigkeit_aktuell)] <- "unbekannt"

dat_analysis_main$Berufliche_Stellung <- factor(dat_analysis_main$Berufliche_Stellung,
                                                  levels = c(levels(dat_analysis_main$Berufliche_Stellung), "unbekannt"))
dat_analysis_main$Berufliche_Stellung[is.na(dat_analysis_main$Berufliche_Stellung)] <- "unbekannt"



# Switcher bleiben drinnen, weil sie erst post-stage also nach der randomisierten Zuteilung entstanden sind (z.B. technische oder persönliche Belange)
#[dat_analysis_main$mode_switch %nin% TRUE, ]
#dat_analysis_main[is.na(dat_analysis_main$SWITCH), ]


# Common adjusted analysis sample: exclude unknown birth cohort / ISCED
# (six cases in the analysis used for the manuscript).
dat_analysis_main <- dat_analysis_main %>% 
  filter(
    Alter_suf != "unbekannt",
    ISCED_suf != "unbekannt"
  ) %>% 
  droplevels()


# Modelle definieren ------------------------------------------------------

rhs_specs <- list(
  # M0_Null = character(0),
  M1_Assignment = c(
    "STARTMODE"
    ),
  M2_Panel_history = c(
    "STARTMODE", 
    "status.16",
    "part_hist"
    ),
  M3_panel_engagement_interaction = c(
    "STARTMODE", 
    "status.16",
    "STARTMODE:status.16",
    "part_hist"
    ) ,
  M4_Demography = c(
    "STARTMODE", 
    "status.16",
    "STARTMODE:status.16",
    "part_hist",
    "Gender_suf",
    "Alter_suf",
    "Region_suf",
    "migBG_suf"
  ),
  M5_SocioEco = c(
    "STARTMODE", 
    "status.16",
    "STARTMODE:status.16",
    "part_hist",
    "Gender_suf",
    "Alter_suf",
    "Region_suf",
    "migBG_suf",
    "ISCED_suf",
    "Erwerbstätigkeit_aktuell"
  ),
  M6_full_pre_fieldwork = c(
    "STARTMODE", 
    "status.16",
    "STARTMODE:status.16",
    "part_hist",
    "Gender_suf",
    "Alter_suf",
    "Region_suf",
    "migBG_suf",
    "ISCED_suf",
    "Erwerbstätigkeit_aktuell",
    "valid_comp_w10",
    "valid_comp_w14"
  )
)


rhs_specs_heterogeneity <- list(
  H1_panel_engagement = c(
    "STARTMODE", 
    "status.16",
    "STARTMODE:status.16",
    "part_hist",
    "Gender_suf",
    "Alter_suf",
    "Region_suf",
    "migBG_suf",
    "ISCED_suf",
    "Erwerbstätigkeit_aktuell",
    "valid_comp_w10",
    "valid_comp_w14"
  ),
  
  H2_competence_history = c(
    "STARTMODE", 
    "valid_comp_w14",
    "STARTMODE:valid_comp_w14",
    "part_hist",
    "Gender_suf",
    "Alter_suf",
    "Region_suf",
    "migBG_suf",
    "ISCED_suf",
    "Erwerbstätigkeit_aktuell",
    "valid_comp_w10",
    "status.16"
  ),
  
  H3_education = c(
    "STARTMODE", 
    "ISCED_suf",
    "STARTMODE:ISCED_suf",
    "part_hist",
    "Gender_suf",
    "Alter_suf",
    "Region_suf",
    "migBG_suf",
    "Erwerbstätigkeit_aktuell",
    "valid_comp_w10",
    "status.16",
    "valid_comp_w14"
  ),
  
  H4_gender = c(
    "STARTMODE", 
    "Gender_suf",
    "STARTMODE:Gender_suf",
    "part_hist",
    "Alter_suf",
    "Region_suf",
    "migBG_suf",
    "Erwerbstätigkeit_aktuell",
    "valid_comp_w10",
    "status.16",
    "valid_comp_w14",
    "ISCED_suf"
  )
  
)


outcomes <- c("Y_TBT", "Y_Befragung", "Y_CAWI_nachgelagert")


# Modellvergleich
lr_sequence <- tibble::tribble(
  ~restricted, ~full,
  "M0_NULL", "M1_Assignment",
  "M1_Assignment", "M2_Panel_history",
  "M2_Panel_history", "M3_panel_engagement_interaction",
  "M3_panel_engagement_interaction", "M4_Demography",
  "M4_Demography", "M5_SocioEco",
  "M5_SocioEco", "M6_full_pre_fieldwork"
)


# 1. Logits ------------------------------------------------------------------


formula_env <- globalenv()


# Modelle für Y_1 definieren
formulas_y1 <- lapply(rhs_specs, function(vars) {
  f <- make_formula("Y_TBT", vars)
  environment(f) <- .GlobalEnv
  f
})
formulas_y1[["M0_NULL"]] <- c("Y_TBT ~ 1")


# Modelle für Y_2 definieren
formulas_y2 <- lapply(rhs_specs, function(vars) {
  f <- make_formula("Y_Befragung", vars)
  environment(f) <- .GlobalEnv
  f
})
formulas_y2[["M0_NULL"]] <- c("Y_Befragung ~ 1")


# Modelle für Y_3 definieren
formulas_y3 <- lapply(rhs_specs, function(vars) {
  f <- make_formula("Y_CAWI_nachgelagert", vars)
  environment(f) <- .GlobalEnv
  f
})
formulas_y3[["M0_NULL"]] <- c("Y_CAWI_nachgelagert ~ 1")


# 
fit_logit <- function(formula, data) {
  stats::glm(
    formula = formula,
    data = data,
    family = stats::binomial(link = "logit")
  )
}



# logits für Moderationsmodelle
# für Heterogeneity analyses

formulas_y1_heterogeneity <- lapply(rhs_specs_heterogeneity, function(vars) {
  f <- make_formula("Y_TBT", vars)
  environment(f) <- .GlobalEnv
  f
})

formulas_y2_heterogeneity <- lapply(rhs_specs_heterogeneity, function(vars) {
  f <- make_formula("Y_Befragung", vars)
  environment(f) <- .GlobalEnv
  f
})

formulas_y3_heterogeneity <- lapply(rhs_specs_heterogeneity, function(vars) {
  f <- make_formula("Y_CAWI_nachgelagert", vars)
  environment(f) <- .GlobalEnv
  f
})


# 1.2 Estimates -----------------------------------------------------------

models_y1 <- lapply(
  formulas_y1,
  FUN = fit_logit,
  data = dat_analysis_main
)

models_y2 <- lapply(
  formulas_y2,
  FUN = fit_logit,
  data = dat_analysis_main
)


# LOGIT CAWI follow-up
dat_analysis_interview <- dat_analysis_main %>% 
  filter(Y_Befragung == 1) %>% 
  droplevels()

models_y3 <- lapply(
  formulas_y3,
  FUN = fit_logit,
  data = dat_analysis_interview
)



coef_Y_TBT <- coef_table_models(models_y1, "Valid competence test")
coef_Y_Befragung <- coef_table_models(models_y2, "Interview started")
coef_Y_CAWI_nachgelagert <- coef_table_models(models_y3, "Follow-up CAWI | interview starters")

coef_all_logit <- bind_rows(coef_Y_TBT,
                      coef_Y_Befragung,
                      coef_Y_CAWI_nachgelagert)

# Output: Table
# Title: Logit coefficient estimates for the fully adjusted stage-specific models (M6) 
# LaTeX label: tab:main_logit_coefficients
write.csv(coef_all_logit, 
          file.path(path.save, "supplement_logit_coefficients.csv"), 
          row.names = FALSE)


modelsummary::modelsummary(
  models_y1,
  vcov = function(x) sandwich::vcovHC(x, type = "HC2"),
  # exponentiate = TRUE,
  stars = TRUE,
  output = "markdown",
  title = "Logit models for valid competence testing"
)



# Heterogeneity estimate
models_y1_heterogeneity <- lapply(
  formulas_y1_heterogeneity,
  FUN = fit_logit,
  data = dat_analysis_main
)

models_y2_heterogeneity <- lapply(
  formulas_y2_heterogeneity,
  FUN = fit_logit,
  data = dat_analysis_main
)

models_y3_heterogeneity <- lapply(
  formulas_y3_heterogeneity,
  FUN = fit_logit,
  data = dat_analysis_interview
)

# zusammenfassen
coef_Y_TBT_heterogeneity <- coef_table_models(models_y1_heterogeneity, "Valid competence test")
coef_Y_Befragung_heterogeneity <- coef_table_models(models_y2_heterogeneity, "Interview started")
coef_Y_CAWI_nachgelagert_heterogeneity <- coef_table_models(models_y3_heterogeneity, "Follow-up CAWI | interview starters")

coef_all_logit_heterogeneity <- bind_rows(coef_Y_TBT_heterogeneity,
                            coef_Y_Befragung_heterogeneity,
                            coef_Y_CAWI_nachgelagert_heterogeneity)

# Output: Table
# Title: Logit coefficient estimates for heterogneity (M2 -- M4) 
# LaTeX label: tab:coef_h2_comp_history
# LaTeX label: tab:coef_h3_education
# LaTeX label: tab:coef_h4_gender
write.csv(coef_all_logit_heterogeneity, 
          file.path(path.save, "supplement_logit_coefficients_heterogeneity.csv"), 
          row.names = FALSE)



# table appendix
# M6 = H1
modelsummary::modelsummary(
  models_y1_heterogeneity$H1_panel_engagement,
  vcov = function(x) sandwich::vcovHC(x, type = "HC2"),
  stars = TRUE,
  output = "markdown",
  title = "M6 | valid competence test"
)
modelsummary::modelsummary(
  models_y2_heterogeneity$H1_panel_engagement,
  # vcov = function(x) sandwich::vcovHC(x, type = "HC2"),
  stars = TRUE,
  output = "markdown",
  title = "M6 | Interview start"
)
modelsummary::modelsummary(
  models_y3_heterogeneity$H1_panel_engagement,
  # vcov = function(x) sandwich::vcovHC(x, type = "HC2"),
  stars = TRUE,
  output = "markdown",
  title = "M6 | Follow-up CAWI"
)


# H2
modelsummary::modelsummary(
  models_y1_heterogeneity$H2_competence_history,
  vcov = function(x) sandwich::vcovHC(x, type = "HC2"),
  stars = TRUE,
  output = "markdown",
  title = "H2 | valid competence test"
)
modelsummary::modelsummary(
  models_y2_heterogeneity$H2_competence_history,
  # vcov = function(x) sandwich::vcovHC(x, type = "HC2"),
  stars = TRUE,
  output = "markdown",
  title = "H2 | Interview start"
)
modelsummary::modelsummary(
  models_y3_heterogeneity$H2_competence_history,
  # vcov = function(x) sandwich::vcovHC(x, type = "HC2"),
  stars = TRUE,
  output = "markdown",
  title = "H2 | Follow-up CAWI"
)


# H3
modelsummary::modelsummary(
  models_y1_heterogeneity$H3_education,
  vcov = function(x) sandwich::vcovHC(x, type = "HC2"),
  stars = TRUE,
  output = "markdown",
  title = "H3 | valid competence test"
)
modelsummary::modelsummary(
  models_y2_heterogeneity$H3_education,
  # vcov = function(x) sandwich::vcovHC(x, type = "HC2"),
  stars = TRUE,
  output = "markdown",
  title = "H3 | Interview start"
)
modelsummary::modelsummary(
  models_y3_heterogeneity$H3_education,
  # vcov = function(x) sandwich::vcovHC(x, type = "HC2"),
  stars = TRUE,
  output = "markdown",
  title = "H3 | Follow-up CAWI"
)


# H4
modelsummary::modelsummary(
  models_y1_heterogeneity$H4_gender,
  vcov = function(x) sandwich::vcovHC(x, type = "HC2"),
  stars = TRUE,
  output = "markdown",
  title = "H4 | valid competence test"
)
modelsummary::modelsummary(
  models_y2_heterogeneity$H4_gender,
  # vcov = function(x) sandwich::vcovHC(x, type = "HC2"),
  stars = TRUE,
  output = "markdown",
  title = "H4 | Interview start"
)
modelsummary::modelsummary(
  models_y3_heterogeneity$H4_gender,
  # vcov = function(x) sandwich::vcovHC(x, type = "HC2"),
  stars = TRUE,
  output = "markdown",
  title = "H4 | Follow-up CAWI"
)

# 1.2 Information criteria ------------------------------------------------

ic_Y_TBT <- info_criteria(models_y1, "Valid competence test")
ic_Y_Befragung <- info_criteria(models_y2, "Interview started")
ic_Y_CAWI_nachgelagert <- info_criteria(models_y3, "Follow-up CAWI | interview starters")

ic_all_logit <- dplyr::bind_rows(ic_Y_TBT, ic_Y_Befragung, ic_Y_CAWI_nachgelagert)

ic_all_logit %>% 
  dplyr::arrange(outcome, BIC)

ic_table_logit  <- ic_all_logit %>% 
  dplyr::mutate(
    logLik = round(logLik, 1),
    AIC = round(AIC, 1),
    BIC = round(BIC, 1),
    delta_AIC = round(delta_AIC, 1),
    delta_BIC = round(delta_BIC, 1)
  )

# Output: Table
# Title: Information criteria for the nested stage-specific logit models
# LaTeX label: tab:stage_model_information_criteria
# abspeichern für SUPPLEMENT
readr::write_csv(ic_table_logit, 
                 file.path(path.save, "model_comparison_information_criteria.csv"))

ic_table_logit %>% 
  kable(
    # format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    caption = "Pooled Logit coefficient estimates for valid Befragung",
    label = "tab:coef_Befragung_models",
    align = "llccccccccc"
  ) %>% 
  kable_styling(latex_options = c("hold_position", "scale_down"))




# 1.3 Likelihood ratio test -----------------------------------------------

lr_Y_TBT <- lr_tests(models_y1, "Valid competence test", lr_sequence)
lr_Y_Befragung <- lr_tests(models_y2, "Interview started", lr_sequence)
lr_Y_CAWI_nachgelagert <- lr_tests(models_y3, "Follow-up CAWI | interview starters", lr_sequence)

lr_all_logit <- dplyr::bind_rows(lr_Y_TBT, lr_Y_Befragung, lr_Y_CAWI_nachgelagert)
lr_table_logit <- lr_all_logit %>% 
  dplyr::mutate(
    LR_statistic = round(LR_statistic, 3),
    p_value = signif(p_value, 3)
  )

# Output: Table
# Title: Sequential likelihood-ratio tests for the nested stage-specific logit models.
# LaTeX label: tab:stage_model_lr_tests
# abspeichern für SUPPLEMENT
readr::write_csv(lr_table_logit, 
                 file.path(path.save, "model_comparison_LikelihoodRatioTest.csv"))





# Preferred Models definieren ---------------------------------------------

# Preferred models:
m_pref_Y_TBT <- models_y1[["M6_full_pre_fieldwork"]]
m_pref_Y_Befragung <- models_y2[["M6_full_pre_fieldwork"]]
m_pref_Y_CAWI_nachgelagert <- models_y3[["M6_full_pre_fieldwork"]]



# 1.4 Predicted probabilities ---------------------------------------------

pred_pref <- dplyr::bind_rows(
  get_mode_predictions(
    m_pref_Y_TBT,
    "M6_full_pre_fieldwork",
    "Valid competence test"
  ),
  get_mode_predictions(
    m_pref_Y_Befragung,
    "M6_full_pre_fieldwork",
    "Interview started"
  ),
  get_mode_predictions(
    m_pref_Y_CAWI_nachgelagert,
    "M6_full_pre_fieldwork",
    "Follow-up CAWI | interview starters"
  )
)

pred_pref_table <- pred_pref %>% 
  dplyr::mutate(
    estimate_pct = 100 * estimate,
    conf.low_pct = 100 * conf.low,
    conf.high_pct = 100 * conf.high,
    cell = paste0(
      sprintf("%.1f", estimate_pct),
      " [",
      sprintf("%.1f", conf.low_pct),
      ", ",
      sprintf("%.1f", conf.high_pct),
      "]"
    )
  ) %>% 
  dplyr::select(outcome, STARTMODE, cell) %>% 
  tidyr::pivot_wider(
    names_from = STARTMODE,
    values_from = cell
  )

pred_pref_table %>% 
  kable(# format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    caption = "",
    label = "tab:pred-pref-table-logit",
    align = "lccc"
  ) %>% 
  kable_styling(latex_options = c("hold_position"))

# Output: Table
# Title: adjusted predicted probabilities
# LaTeX label: tab:stage_predprob



# Kontraste zu Prediction -------------------------------------------------

### Output
# tab:stage_prediction

contr_pref <- dplyr::bind_rows(
  get_all_pairwise_contrasts(
    m_pref_Y_TBT,
    conf_level = 0.95,
    "Valid competence test"),
  get_all_pairwise_contrasts(
    m_pref_Y_Befragung,
    conf_level = 0.95,
    "Interview started"),
  get_all_pairwise_contrasts(
    m_pref_Y_CAWI_nachgelagert,
    conf_level = 0.95,
    "Follow-up CAWI | interview starters")
)


contr_pref %>% 
  dplyr::select(outcome, contrast, estimate, conf.low, conf.high) %>% 
  dplyr::mutate(
    estimate_pct = 100 * estimate,
    conf.low_pct = 100 * conf.low,
    conf.high_pct = 100 * conf.high,
    cell = paste0(
      sprintf("%.1f", estimate_pct),
      " [",
      sprintf("%.1f", conf.low_pct),
      ", ",
      sprintf("%.1f", conf.high_pct),
      "]"
    )
  ) %>% 
  dplyr::select(outcome, contrast, cell) %>% 
  tidyr::pivot_wider(
    names_from = contrast,
    values_from = cell
  ) %>% 
  kable(# format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    caption = "",
    label = "tab:pred-pref-table-logit",
    align = "lccc"
  ) %>% 
  kable_styling(latex_options = c("hold_position"))

# In TABELLE tab:stage_predprob
# Output: Table
# Title: adjusted predicted probabilities
# LaTeX label: tab:stage_predprob


contr_pref %>% 
  dplyr::select(outcome, contrast, equivalence_result) %>% 
  tidyr::pivot_wider(
    names_from = contrast,
    values_from = equivalence_result
  ) %>% 
  kable(# format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    caption = "",
    label = "tab:pred-pref-table-logit",
    align = "lccc"
  ) %>% 
  kable_styling(latex_options = c("hold_position"))

# In TABELLE stage_predprob
# dritte Spalte
# Output: Table
# Title: adjusted predicted probabilities
# LaTeX label: tab:stage_predprob

# N für y3 für die Fußnote in der Tabelle stage_predprob
n_y3 <- stats::nobs(
  models_y3$M6_full_pre_fieldwork
)


# Sensitivity -------------------------------------------------------------

contr_pref_eq3 <- dplyr::bind_rows(
  get_all_pairwise_contrasts(
    equivalence_margin_pp = 3,
    m_pref_Y_TBT, 
    conf_level = 0.90,
    "Valid competence test"),
  get_all_pairwise_contrasts(
    equivalence_margin_pp = 3,
    m_pref_Y_Befragung,
    conf_level = 0.90,
    "Interview started"),
  get_all_pairwise_contrasts(
    equivalence_margin_pp = 3,
    m_pref_Y_CAWI_nachgelagert,
    conf_level = 0.90,
    "Follow-up CAWI | interview starters")
)

contr_pref_eq3 %>% 
  dplyr::select(outcome, contrast, estimate, conf.low, conf.high, equivalence_result) %>% 
  mutate(estimate = 100*estimate)

contr_pref_eq75 <- dplyr::bind_rows(
  get_all_pairwise_contrasts(
    equivalence_margin_pp = 7.5,
    m_pref_Y_TBT,
    conf_level = 0.90,
    "Valid competence test"),
  get_all_pairwise_contrasts(
    equivalence_margin_pp = 7.5,
    m_pref_Y_Befragung,
    conf_level = 0.90,
    "Interview started"),
  get_all_pairwise_contrasts(
    equivalence_margin_pp = 7.5,
    m_pref_Y_CAWI_nachgelagert,
    conf_level = 0.90,
    "Follow-up CAWI | interview starters")
)


contr_pref_eq75 %>% 
  dplyr::select(outcome, contrast, estimate, conf.low, conf.high, equivalence_result) %>% 
  mutate(
    estimate = 100*estimate,
    conf.low= 100*conf.low,
    conf.high = 100*conf.high)


# In TABELLE Appendix
# label: tab:equivalence_margin_sensitivity
# Output: Table
# Title: Sensitivity of equivalence conclusions to alternative equivalence margins
# LaTeX label: tab:equivalence_margin_sensitivity




# 2. Heterogeneity --------------------------------------------------------

# 2.1 heterogene prediction status.16 -------------------------------------
# Heterogenität - panel history

# 2.1.1 Heterogeniät status.16 - grafik -----------------------------------


pred_group <- dplyr::bind_rows(
  get_cf_predictions(
    m_pref_Y_TBT,
    "Valid competence test",
    assignment_var = "STARTMODE",
    group_var = "status.16"
  ),
  get_cf_predictions(
    m_pref_Y_Befragung,
    "Interview started",
    assignment_var = "STARTMODE",
    group_var = "status.16"
  ),
  get_cf_predictions(
    m_pref_Y_CAWI_nachgelagert,
    "Follow-up CAWI (conditional)",
    assignment_var = "STARTMODE",
    group_var = "status.16"
  )
)

# plotten:

pred_group$outcome <- factor(pred_group$outcome,
                             levels = c("Valid competence test",
                                        "Interview started",
                                        "Follow-up CAWI (conditional)"))

pred_group$STARTMODE <- factor(pred_group$STARTMODE,
                               levels = c("CAPI",
                                        "CALVI",
                                        "CAWI-TBT+CATI"))



pred_group <- pred_group %>%
  mutate(
    status.16 = factor(
      status.16, 
      levels = c(
        "Part", "TDO"
        ),
      labels = c(
        "Repeat respondents",
        "Temporary dropouts"
      ))
  )

mode_labels <- c(
  "CAPI" = "CBT+\nCAPI",
  "CALVI" = "VBT+\nCAVI",
  "CAWI-TBT+CATI" = "WBT+\nCATI"
)

outcome_labels <- c(
  "Valid competence test" = "Valid competence-test data",
  "Interview started" = "Interview start",
  "Follow-up CAWI (conditional)" = "Follow-up CAWI\namong interview starters"
)

status_levels <- c(
  "Repeat respondents",
  "Temporary dropouts"
)

position_status <- position_dodge(width = 0.10)

p_prior_panel <- ggplot2::ggplot(
  pred_group,
  ggplot2::aes(
    x = STARTMODE,
    y = estimate,
    color = status.16,
    group = status.16,
    shape = status.16,
    linetype = status.16
  )
) +
  geom_line(
    linewidth = 0.45,
    position = position_status
  ) +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.07,
    linewidth = 0.40,
    position = position_status
  ) +
  geom_point(
    size = 2.15,
    stroke = 0.45,
    position = position_status
  ) +
  facet_wrap(
    ~ outcome,
    nrow = 1,
    labeller = as_labeller(outcome_labels)
  ) +
  scale_x_discrete(
    labels = mode_labels
  ) + 
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.015))
  ) +
  scale_shape_manual(
    name = NULL,
    breaks = status_levels,
    values = c(
      "Repeat respondents" = 16,
      "Temporary dropouts" = 17
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    breaks = status_levels,
    values = c(
      "Repeat respondents" = "solid",
      "Temporary dropouts" = "dashed"
    )
  ) +
  scale_colour_manual(
    name = NULL,
    breaks = status_levels,
    values = c(
      "Repeat respondents" = "#D55E00",
      "Temporary dropouts" = "#0072B2"
    )
  ) +
  labs(
    x = "Randomised initial assignment",
    y = "Adjusted predicted probability"
  ) +
  guides(
    colour = guide_legend(nrow = 1),
    shape = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  ) +
  theme_article()

p_prior_panel

### Output
# abspeichern
ggplot2::ggsave(
  plot  = p_prior_panel,
  filename = file.path(path.save, "Predicted probabilities prior panel history.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 105,
  units = "mm",
  bg = "white",
  dpi = 300
)



# 2.1.2 Heterogenität status.16 - Tabelle ---------------------------------

mode_pairs <- list(
  "VBT-CAVI $-$ CBT-CAPI" = c("CAPI", "CALVI"),
  "WBT+CATI $-$ VBT-CAVI" = c("CALVI", "CAWI-TBT+CATI"),
  "WBT+CATI $-$ CBT+CAPI" = c("CAPI", "CAWI-TBT+CATI")
)


panel_status_pred_results <- dplyr::bind_rows(
  get_binary_group_table(
    model = m_pref_Y_TBT,
    outcome_label = "Valid competence test",
    mode_pairs = mode_pairs,
    group_var = "status.16",
    reference_group = "Part",
    comparison_group = "TDO"
  ),
  get_binary_group_table(
    model = m_pref_Y_Befragung,
    outcome_label = "Interview started",
    mode_pairs = mode_pairs,
    group_var = "status.16",
    reference_group = "Part",
    comparison_group = "TDO"
  ),
  get_binary_group_table(
    model = m_pref_Y_CAWI_nachgelagert,
    outcome_label = "Follow-up CAWI (conditional)",
    mode_pairs = mode_pairs,
    group_var = "status.16",
    reference_group = "Part",
    comparison_group = "TDO"
  )
)

panel_status_pred_results %>% print(n = Inf, width = Inf)


panel_status_table <- panel_status_pred_results %>% 
  dplyr::mutate(
    repeat_respondents = format_ci(reference_estimate_pp, reference_conf_low_pp, reference_conf_high_pp),
    temporary_dropouts = format_ci(comparison_estimate_pp, comparison_conf_low_pp, comparison_conf_high_pp),
    heterogeneity_contrast = format_ci(difference_estimate_pp, difference_conf_low_pp, difference_conf_high_pp),
    heterogeneity_p = format_p_value(difference_p_value)
  ) %>% 
  dplyr::select(
    outcome,
    contrast,
    repeat_respondents,
    temporary_dropouts,
    heterogeneity_contrast,
    heterogeneity_p
  )

### Output
# tab:panel_status_contrasts
# gesamte Tabelle
panel_status_table %>% 
  print(
    n = Inf, width = Inf
  )

### Output
# Teil 1 für
# tab:heterogeneity_summary
# Spalte
# TDO - repeat
panel_status_pred_results %>%
  select(outcome, moderator, contrast, difference_estimate_pp, difference_conf_low_pp, difference_conf_high_pp) %>% 
  mutate(
    difference_estimate_pp = round(difference_estimate_pp, 1),
    difference_conf_low_pp = round(difference_conf_low_pp, 1),
    difference_conf_high_pp = round(difference_conf_high_pp, 1)
  )



# 2.2 heterogene prediction prior competence -------------------------------------
# Heterogenität - panel history


# 2.2.1 Heterogeniät prior competence - grafik -----------------------------------


pred_group_prior_comp <- dplyr::bind_rows(
  get_cf_predictions(
    models_y1_heterogeneity$H2_competence_history,
    "Valid competence test",
    assignment_var = "STARTMODE",
    group_var = "valid_comp_w14"
  ),
  get_cf_predictions(
    models_y2_heterogeneity$H2_competence_history,
    "Interview started",
    assignment_var = "STARTMODE",
    group_var = "valid_comp_w14"
  ),
  get_cf_predictions(
    models_y3_heterogeneity$H2_competence_history,
    "Follow-up CAWI (conditional)",
    assignment_var = "STARTMODE",
    group_var = "valid_comp_w14"
  )
)

# plotten:
pred_group_prior_comp$outcome <- factor(pred_group_prior_comp$outcome,
                             levels = c("Valid competence test",
                                        "Interview started",
                                        "Follow-up CAWI (conditional)"))

pred_group_prior_comp$STARTMODE <- factor(pred_group_prior_comp$STARTMODE,
                               levels = c("CAPI",
                                          "CALVI",
                                          "CAWI-TBT+CATI"))



pred_group_prior_comp <- pred_group_prior_comp %>%
  mutate(
    valid_comp_w14 = factor(
      valid_comp_w14, 
      levels = c(
        "0", "1"
      ),
      labels = c(
        "No valid wave 14 competence data",
        "Valid wave 14 competence data"
      ))
  )

mode_labels <- c(
  "CAPI" = "CBT+\nCAPI",
  "CALVI" = "VBT+\nCAVI",
  "CAWI-TBT+CATI" = "WBT+\nCATI"
)

outcome_labels <- c(
  "Valid competence test" = "Valid competence-test data",
  "Interview started" = "Interview start",
  "Follow-up CAWI (conditional)" = "Follow-up CAWI\namong interview starters"
)

status_levels <- c(
  "No valid wave 14 competence data",
  "Valid wave 14 competence data"
)

position_status <- position_dodge(width = 0.10)

p_prior_comp <- ggplot2::ggplot(
  pred_group_prior_comp,
  ggplot2::aes(
    x = STARTMODE,
    y = estimate,
    color = valid_comp_w14,
    group = valid_comp_w14,
    shape = valid_comp_w14,
    linetype = valid_comp_w14
  )
) +
  geom_line(
    linewidth = 0.45,
    position = position_status
  ) +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.07,
    linewidth = 0.40,
    position = position_status
  ) +
  geom_point(
    size = 2.15,
    stroke = 0.45,
    position = position_status
  ) +
  facet_wrap(
    ~ outcome,
    nrow = 1,
    labeller = as_labeller(outcome_labels)
  ) +
  scale_x_discrete(
    labels = mode_labels
  ) + 
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.015))
  ) +
  scale_shape_manual(
    name = NULL,
    breaks = status_levels,
    values = c(
      "No valid wave 14 competence data" = 16,
      "Valid wave 14 competence data" = 17
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    breaks = status_levels,
    values = c(
      "No valid wave 14 competence data" = "solid",
      "Valid wave 14 competence data" = "dashed"
    )
  ) +
  scale_colour_manual(
    name = NULL,
    breaks = status_levels,
    values = c(
      "No valid wave 14 competence data" = "#D55E00",
      "Valid wave 14 competence data" = "#0072B2"
    )
  ) +
  labs(
    x = "Randomised initial assignment",
    y = "Adjusted predicted probability"
  ) +
  guides(
    colour = guide_legend(nrow = 1),
    shape = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  ) +
  theme_article()

p_prior_comp

### Output
# abspeichern
ggplot2::ggsave(
  plot  = p_prior_comp,
  filename = file.path(path.save, "Predicted probabilities prior competence test w14.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 105,
  units = "mm",
  bg = "white",
  dpi = 300
)



# 2.2.2 Heterogenität prior competence - Tabelle ---------------------------------

mode_pairs <- list(
  "VBT-CAVI $-$ CBT-CAPI" = c("CAPI", "CALVI"),
  "WBT+CATI $-$ VBT-CAVI" = c("CALVI", "CAWI-TBT+CATI"),
  "WBT+CATI $-$ CBT+CAPI" = c("CAPI", "CAWI-TBT+CATI")
)

pred_results_prior_comp <- dplyr::bind_rows(
  get_binary_group_table(
    model = models_y1_heterogeneity$H2_competence_history,
    outcome_label = "Valid competence test",
    mode_pairs = mode_pairs,
    group_var = "valid_comp_w14",
    reference_group = "0",
    comparison_group = "1"
  ),
  get_binary_group_table(
    model = models_y2_heterogeneity$H2_competence_history,
    outcome_label = "Interview started",
    mode_pairs = mode_pairs,
    group_var = "valid_comp_w14",
    reference_group = "0",
    comparison_group = "1"
  ),
  get_binary_group_table(
    model = models_y3_heterogeneity$H2_competence_history,
    outcome_label = "Follow-up CAWI (conditional)",
    mode_pairs = mode_pairs,
    group_var = "valid_comp_w14",
    reference_group = "0",
    comparison_group = "1"
  )
)

pred_results_prior_comp %>% print(n = Inf)


pred_results_prior_comp_table <- pred_results_prior_comp %>% 
  dplyr::mutate(
    no_valid = format_ci(reference_estimate_pp, reference_conf_low_pp, reference_conf_high_pp),
    valid = format_ci(comparison_estimate_pp, comparison_conf_low_pp, comparison_conf_high_pp),
    heterogeneity_contrast = format_ci(difference_estimate_pp, difference_conf_low_pp, difference_conf_high_pp),
    heterogeneity_p = format_p_value(difference_p_value)
  ) %>% 
  dplyr::select(
    outcome,
    contrast,
    no_valid,
    valid,
    heterogeneity_contrast,
    heterogeneity_p
  )


### Output
# tab:
# competence data valid in Wave 14
pred_results_prior_comp_table %>% 
  print(
    n = Inf, width = Inf
  )

### Output
# Teil 2 für
# tab:heterogeneity_summary
# Spalte
# Valid W14 data - no valid W14 data
pred_results_prior_comp %>%
  select(outcome, moderator, contrast, difference_estimate_pp, difference_conf_low_pp, difference_conf_high_pp) %>% 
  mutate(
    difference_estimate_pp = round(difference_estimate_pp, 1),
    difference_conf_low_pp = round(difference_conf_low_pp, 1),
    difference_conf_high_pp = round(difference_conf_high_pp, 1)
  )


# 2.3 heterogene prediction ISCED -------------------------------------
# Heterogenität - panel history


# 2.3.1 Heterogeniät ISCED - grafik -----------------------------------

dat_analysis_main$ISCED_suf

dat_analysis_main <- dat_analysis_main %>% 
  dplyr::mutate(
    ISCED_suf = factor(
      ISCED_suf,
      levels = c("0-2", "3-4", "5-6")
    )
)

levels(droplevels(dat_analysis_main$ISCED_suf))

pred_group_ISCED <- dplyr::bind_rows(
  get_cf_predictions(
    models_y1_heterogeneity$H3_education,
    "Valid competence test",
    assignment_var = "STARTMODE",
    group_var = "ISCED_suf"
  ),
  get_cf_predictions(
    models_y2_heterogeneity$H3_education,
    "Interview started",
    assignment_var = "STARTMODE",
    group_var = "ISCED_suf"
  ),
  get_cf_predictions(
    models_y3_heterogeneity$H3_education,
    "Follow-up CAWI (conditional)",
    assignment_var = "STARTMODE",
    group_var = "ISCED_suf"
  )
)

# plotten:
pred_group_ISCED$outcome <- factor(pred_group_ISCED$outcome,
                                        levels = c("Valid competence test",
                                                   "Interview started",
                                                   "Follow-up CAWI (conditional)"))

pred_group_ISCED$STARTMODE <- factor(pred_group_ISCED$STARTMODE,
                                          levels = c("CAPI",
                                                     "CALVI",
                                                     "CAWI-TBT+CATI"))



pred_group_ISCED <- pred_group_ISCED %>%
  mutate(
    ISCED_suf = factor(
      ISCED_suf, 
      levels = c(
        "0-2", "3-4", "5-6"
      ),
      labels = c(
        "Low ISCED",
        "Medium ISCED", 
        "High ISCED"
      ))
  )

mode_labels <- c(
  "CAPI" = "CBT+\nCAPI",
  "CALVI" = "VBT+\nCAVI",
  "CAWI-TBT+CATI" = "WBT+\nCATI"
)

outcome_labels <- c(
  "Valid competence test" = "Valid competence-test data",
  "Interview started" = "Interview start",
  "Follow-up CAWI (conditional)" = "Follow-up CAWI\namong interview starters"
)

status_levels <- c(
  "Low ISCED",
  "Medium ISCED",
  "High ISCED"
)

position_status <- position_dodge(width = 0.10)

p_ISCED <- ggplot2::ggplot(
  pred_group_ISCED,
  ggplot2::aes(
    x = STARTMODE,
    y = estimate,
    color = ISCED_suf,
    group = ISCED_suf,
    shape = ISCED_suf,
    linetype = ISCED_suf
  )
) +
  geom_line(
    linewidth = 0.45,
    position = position_status
  ) +
  
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.07,
    linewidth = 0.40,
    position = position_status
  ) +
  
  geom_point(
    size = 2.15,
    stroke = 0.45,
    position = position_status
  ) +
  facet_wrap(
    ~ outcome,
    nrow = 1,
    labeller = as_labeller(outcome_labels)
  ) +
  scale_x_discrete(
    labels = mode_labels
  ) + 
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.015))
  ) +
  scale_shape_manual(
    name = NULL,
    breaks = status_levels,
    values = c(
      "Low ISCED" = 16,
      "Medium ISCED" = 17,
      "High ISCED" = 18
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    breaks = status_levels,
    values = c(
      "Low ISCED" = "solid",
      "Medium ISCED" = "dashed",
      "High ISCED" = "dotted"
    )
  ) +
  scale_colour_manual(
    name = NULL,
    breaks = status_levels,
    values = c(
      "Low ISCED" = "#D55E00",
      "Medium ISCED" = "#0072B2",
      "High ISCED" = "#157536"
    )
  ) +
  labs(
    x = "Randomised initial assignment",
    y = "Adjusted predicted probability"
  ) +
  guides(
    colour = guide_legend(nrow = 1),
    shape = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  ) +
  theme_article()

p_ISCED

### Output
# abspeichern
ggplot2::ggsave(
  plot  = p_ISCED,
  filename = file.path(path.save, "Predicted probabilities ISCED.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 105,
  units = "mm",
  bg = "white",
  dpi = 300
)



# 2.2.2 Heterogenität prior competence - Tabelle ---------------------------------

mode_pairs <- list(
  "VBT-CAVI $-$ CBT-CAPI" = c("CAPI", "CALVI"),
  "WBT+CATI $-$ VBT-CAVI" = c("CALVI", "CAWI-TBT+CATI"),
  "WBT+CATI $-$ CBT+CAPI" = c("CAPI", "CAWI-TBT+CATI")
)


pred_group_ISCED

isced_levels <- c(
  "0-2", "3-4", "5-6"
)

levels(model.frame(models_y1_heterogeneity$H3_education)$ISCED_suf)

pred_isced <- dplyr::bind_rows(
  get_multigroup_table(
    model = models_y1_heterogeneity$H3_education,
    outcome_label = "Valid competence test",
    mode_pairs = mode_pairs,
    group_var = "ISCED_suf",
    group_levels = isced_levels
  ),
  get_multigroup_table(
    model = models_y2_heterogeneity$H3_education,
    outcome_label = "Interview started",
    mode_pairs = mode_pairs,
    group_var = "ISCED_suf",
    group_levels = isced_levels
  ),
  get_multigroup_table(
    model = models_y3_heterogeneity$H3_education,
    outcome_label = "Follow-up CAWI (conditional)",
    mode_pairs = mode_pairs,
    group_var = "ISCED_suf",
    group_levels = isced_levels
  )
)

pred_isced %>% print(n = Inf)


### Output
# table 
# tab:isced_full_contrasts

pred_isced_table <- pred_isced %>% 
  dplyr::mutate(
    low_ISCED = format_ci(low_estimate_pp, low_conf_low_pp, low_conf_high_pp),
    medium_ISCED = format_ci(medium_estimate_pp, medium_conf_low_pp, medium_conf_high_pp),
    high_ISCED = format_ci(high_estimate_pp, high_conf_low_pp, high_conf_high_pp),
    
    medium_minus_low = paste0(
      format_ci(medium_minus_low_pp, medium_minus_low_conf_low_pp, medium_minus_low_conf_high_pp),
      "; ",
      format_p_value(medium_minus_low_p_value)
    ),
    
    high_minus_low = paste0(
      format_ci(high_minus_low_pp, high_minus_low_conf_low_pp, high_minus_low_conf_high_pp),
      "; ",
      format_p_value(high_minus_low_p_value)
    ),
    
    high_minus_medium = paste0(
      format_ci(high_minus_medium_pp, high_minus_medium_conf_low_pp, high_minus_medium_conf_high_pp),
      "; ",
      format_p_value(high_minus_medium_p_value)
    )
  ) %>% 
  dplyr::select(
    outcome,
    contrast,
    low_ISCED,
    medium_ISCED,
    high_ISCED,
    medium_minus_low,
    high_minus_low,
    high_minus_medium
  )

pred_isced_table %>% 
  print(
    n = Inf, width = Inf
  )


# 2.4 heterogene prediction gender -------------------------------------
# Heterogenität - panel history


# 2.4.1 Heterogeniät gender - grafik -----------------------------------


pred_group_gender <- dplyr::bind_rows(
  get_cf_predictions(
    models_y1_heterogeneity$H4_gender,
    "Valid competence test",
    assignment_var = "STARTMODE",
    group_var = "Gender_suf"
  ),
  get_cf_predictions(
    models_y2_heterogeneity$H4_gender,
    "Interview started",
    assignment_var = "STARTMODE",
    group_var = "Gender_suf"
  ),
  get_cf_predictions(
    models_y3_heterogeneity$H4_gender,
    "Follow-up CAWI (conditional)",
    assignment_var = "STARTMODE",
    group_var = "Gender_suf"
  )
)

# plotten:
pred_group_gender$outcome <- factor(pred_group_gender$outcome,
                                        levels = c("Valid competence test",
                                                   "Interview started",
                                                   "Follow-up CAWI (conditional)"))

pred_group_gender$STARTMODE <- factor(pred_group_gender$STARTMODE,
                                          levels = c("CAPI",
                                                     "CALVI",
                                                     "CAWI-TBT+CATI"))



pred_group_gender <- pred_group_gender %>%
  mutate(
    Gender_suf = factor(
      Gender_suf, 
      levels = c(
        "männlich", "weiblich"
      ),
      labels = c(
        "Male",
        "Female"
      ))
  )

mode_labels <- c(
  "CAPI" = "CBT+\nCAPI",
  "CALVI" = "VBT+\nCAVI",
  "CAWI-TBT+CATI" = "WBT+\nCATI"
)

outcome_labels <- c(
  "Valid competence test" = "Valid competence-test data",
  "Interview started" = "Interview start",
  "Follow-up CAWI (conditional)" = "Follow-up CAWI\namong interview starters"
)

status_levels <- c(
  "Male",
  "Female"
)

position_status <- position_dodge(width = 0.10)

p_prior_gender <- ggplot2::ggplot(
  pred_group_gender,
  ggplot2::aes(
    x = STARTMODE,
    y = estimate,
    color = Gender_suf,
    group = Gender_suf,
    shape = Gender_suf,
    linetype = Gender_suf
  )
) +
  geom_line(
    linewidth = 0.45,
    position = position_status
  ) +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.07,
    linewidth = 0.40,
    position = position_status
  ) +
  geom_point(
    size = 2.15,
    stroke = 0.45,
    position = position_status
  ) +
  facet_wrap(
    ~ outcome,
    nrow = 1,
    labeller = as_labeller(outcome_labels)
  ) +
  scale_x_discrete(
    labels = mode_labels
  ) + 
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.015))
  ) +
  scale_shape_manual(
    name = NULL,
    breaks = status_levels,
    values = c(
      "Male" = 16,
      "Female" = 17
    )
  ) +
  scale_linetype_manual(
    name = NULL,
    breaks = status_levels,
    values = c(
      "Male" = "solid",
      "Female" = "dashed"
    )
  ) +
  scale_colour_manual(
    name = NULL,
    breaks = status_levels,
    values = c(
      "Male" = "#D55E00",
      "Female" = "#0072B2"
    )
  ) +
  labs(
    x = "Randomised initial assignment",
    y = "Adjusted predicted probability"
  ) +
  guides(
    colour = guide_legend(nrow = 1),
    shape = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  ) +
  theme_article()

p_prior_gender

### Output
# abspeichern
ggplot2::ggsave(
  plot  = p_prior_gender,
  filename = file.path(path.save, "Predicted probabilities gender.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 105,
  units = "mm",
  bg = "white",
  dpi = 300
)



# 2.4.2 Heterogenität gender - Tabelle ---------------------------------

mode_pairs <- list(
  "VBT-CAVI $-$ CBT-CAPI" = c("CAPI", "CALVI"),
  "WBT+CATI $-$ VBT-CAVI" = c("CALVI", "CAWI-TBT+CATI"),
  "WBT+CATI $-$ CBT+CAPI" = c("CAPI", "CAWI-TBT+CATI")
)


pred_results_gender <- dplyr::bind_rows(
  get_binary_group_table(
    model = models_y1_heterogeneity$H4_gender,
    outcome_label = "Valid competence test",
    mode_pairs = mode_pairs,
    group_var = "Gender_suf",
    reference_group = "männlich",
    comparison_group = "weiblich"
  ),
  get_binary_group_table(
    model = models_y2_heterogeneity$H4_gender,
    outcome_label = "Interview started",
    mode_pairs = mode_pairs,
    group_var = "Gender_suf",
    reference_group = "männlich",
    comparison_group = "weiblich"
  ),
  get_binary_group_table(
    model = models_y3_heterogeneity$H4_gender,
    outcome_label = "Follow-up CAWI (conditional)",
    mode_pairs = mode_pairs,
    group_var = "Gender_suf",
    reference_group = "männlich",
    comparison_group = "weiblich"
  )
)

pred_results_gender %>% print(n = Inf)




pred_results_gender_table <- pred_results_gender %>% 
  dplyr::mutate(
    male = format_ci(reference_estimate_pp, reference_conf_low_pp, reference_conf_high_pp),
    female = format_ci(comparison_estimate_pp, comparison_conf_low_pp, comparison_conf_high_pp),
    heterogeneity_contrast = format_ci(difference_estimate_pp, difference_conf_low_pp, difference_conf_high_pp),
    heterogeneity_p = format_p_value(difference_p_value)
  ) %>% 
  dplyr::select(
    outcome,
    contrast,
    male,
    female,
    heterogeneity_contrast,
    heterogeneity_p
  )


### Output
# table
# group-specific assignment effects
# tab:gender_full_contrasts

pred_results_gender_table %>% 
  print(
    n = Inf, width = Inf
  )



# 2.5 Summaries models ----------------------------------------------------

options(
  modelsummary_factory_latex = "kableExtra",
  modelsummary_factory_latex_tabular = "kableExtra",
  modelsummary_format_numeric_latex = "plain"
)


models_h1_panel <- list(
  "Valid competence-test data" = 
    models_y1_heterogeneity$H1_panel_engagement,
  "Interview start" = 
    models_y2_heterogeneity$H1_panel_engagement,
  "Follow-up CAWI" = 
    models_y3_heterogeneity$H1_panel_engagement
)

models_h2_comp <- list(
  "Valid competence-test data" = 
    models_y1_heterogeneity$H2_competence_history,
  "Interview start" = 
    models_y2_heterogeneity$H2_competence_history,
  "Follow-up CAWI" = 
    models_y3_heterogeneity$H2_competence_history
)

models_h3_isced <- list(
  "Valid competence-test data" = 
    models_y1_heterogeneity$H3_education,
  "Interview start" = 
    models_y2_heterogeneity$H3_education,
  "Follow-up CAWI" = 
    models_y3_heterogeneity$H3_education
)

models_h4_gender <- list(
  "Valid competence-test data" = 
    models_y1_heterogeneity$H4_gender,
  "Interview start" = 
    models_y2_heterogeneity$H4_gender,
  "Follow-up CAWI" = 
    models_y3_heterogeneity$H4_gender
)


coef_labels <- c(
  "(Intercept)" =
    "Intercept",
  "STARTMODECALVI" = 
    "VBT+CAVI",
  "STARTMODECAWI-TBT+CATI" =
    "WBT-CATI",
  "status.16TDO" =
    "Temporary dropout",
  "valid_comp_w14" =
    "Valid competence data in Wave 14",
  "ISCED3-4" = 
    "Medium ISCED",
  "ISCED5-6" =
    "Higher ISCED",
  "Genderweiblich" =
    "Female",
  "part_hist13-14" =
    "Cumulative panel participation: 13--14 waves",
  "part_hist15-16" =
    "Cumulative panel participation: 15--16 waves",
  "Alter1994 und früher" =
    "Born in 1994 or earlier",
  "Alter1996 und später" =
    "Born in 1996 or later",
  "RegionWest" =
    "Western Germany",
  "migBGmissing" =
    "Migration background: unknown",
  "migBGyes" =
    "Migration background: yes", 
  "Erwerbstätigkeit_aktuellabgeschlossene Erwerbstätigkeit" =
    "Pre-fieldwork employment: previously employed",
  "Erwerbstätigkeit_aktuellunbekannt" =
    "Pre-fieldwork employment: unknown",
  "valid_comp_w10" =
    "Valid competence data in Wave 10",
  
  # H1
  "STARTMODECALVI:status.16TDO" =
    "VBT+CAVI $\\times$ temporary dropout",
  "STARTMODECAWI-TBT+CATI:status.16TDO" =
    "WBT+CATI $\\times$ temporary dropout",
  
  # H2
  "STARTMODECALVI:valid_comp_w14" =
    "VBT+CAVI $\\times$ valid competence data in Wave 14",
  "STARTMODECAWI-TBT+CATI:valid_comp_w14" =
    "WBT+CATI $\\times$ valid competence data in Wave 14",
  
  # H3
  "STARTMODECALVI:ISCED3-4" =
    "VBT+CAVI $\\times$ medium ISCED",
  "STARTMODECALVI:ISCED5-6" =
    "VBT+CAVI $\\times$ higher ISCED",
  "STARTMODECAWI-TBT+CATI:ISCED3-4" =
    "WBT+CATI $\\times$ medium ISCED",
  "STARTMODECAWI-TBT+CATI:ISCED5-6" =
    "WBT+CATI $\\times$ higher ISCED",
  
  # H4
  "STARTMODECALVI:Genderweiblich" =
    "VBT+CAVI $\\times$ female",
  "STARTMODECAWI-TBT+CATI:Genderweiblich" =
    "WBT+CATI $\\times$ female"
)



order_h1 <- c(
  "(Intercept)",
  "STARTMODECALVI",
  "STARTMODECAWI-TBT+CATI",
  "status.16TDO",
  "STARTMODECALVI:status.16TDO",
  "STARTMODECAWI-TBT+CATI:status.16TDO",
  "part_hist13-14",
  "part_hist15-16",
  "Genderweiblich",
  "Alter1994 und früher",
  "Alter1996 und später",
  "RegionWest",
  "migBGmissing",
  "migBGyes",
  "ISCED3-4",
  "ISCED5-6",
  "Erwerbstätigkeit_aktuellabgeschlossene Erwerbstätigkeit",
  "Erwerbstätigkeit_aktuellunbekannt",
  "valid_comp_w10",
  "valid_comp_w14"
)

coef_map_h1 <- coef_labels[order_h1]


order_h2 <- c(
  "(Intercept)",
  "STARTMODECALVI",
  "STARTMODECAWI-TBT+CATI",
  "valid_comp_w14",
  "STARTMODECALVI:valid_comp_w14",
  "STARTMODECAWI-TBT+CATI:valid_comp_w14",
  "status.16TDO",
  "part_hist13-14",
  "part_hist15-16",
  "Genderweiblich",
  "Alter1994 und früher",
  "Alter1996 und später",
  "RegionWest",
  "migBGmissing",
  "migBGyes",
  "ISCED3-4",
  "ISCED5-6",
  "Erwerbstätigkeit_aktuellabgeschlossene Erwerbstätigkeit",
  "Erwerbstätigkeit_aktuellunbekannt",
  "valid_comp_w10"
)

coef_map_h2 <- coef_labels[order_h2]


order_h3 <- c(
  "(Intercept)",
  "STARTMODECALVI",
  "STARTMODECAWI-TBT+CATI",
  "ISCED3-4",
  "ISCED5-6",
  "STARTMODECALVI:ISCED3-4",
  "STARTMODECALVI:ISCED5-6",
  "STARTMODECAWI-TBT+CATI:ISCED3-4",
  "STARTMODECAWI-TBT+CATI:ISCED5-6",
  "status.16TDO",
  "part_hist13-14",
  "part_hist15-16",
  "Genderweiblich",
  "Alter1994 und früher",
  "Alter1996 und später",
  "RegionWest",
  "migBGmissing",
  "migBGyes",
  "Erwerbstätigkeit_aktuellabgeschlossene Erwerbstätigkeit",
  "Erwerbstätigkeit_aktuellunbekannt",
  "valid_comp_w10",
  "valid_comp_w14"
)

coef_map_h3 <- coef_labels[order_h3]


order_h4 <- c(
  "(Intercept)",
  "STARTMODECALVI",
  "STARTMODECAWI-TBT+CATI",
  "Genderweiblich",
  "STARTMODECALVI:Genderweiblich",
  "STARTMODECAWI-TBT+CATI:Genderweiblich",
  "status.16TDO",
  "part_hist13-14",
  "part_hist15-16",
  "Alter1994 und früher",
  "Alter1996 und später",
  "RegionWest",
  "migBGmissing",
  "migBGyes",
  "ISCED3-4",
  "ISCED5-6",
  "Erwerbstätigkeit_aktuellabgeschlossene Erwerbstätigkeit",
  "Erwerbstätigkeit_aktuellunbekannt",
  "valid_comp_w10",
  "valid_comp_w14"
)

coef_map_h4 <- coef_labels[order_h4]


# Tabellen erzeugen
make_heterogeneity_coef_table(
  models = models_h1_panel,
  coef_map = coef_map_h1,
  caption = paste0(
    "Logit coefficients estimates for assignment-by-",
    "prior-panel-status models"
  ),
  label = "tab:coef_h1_panel",
  moderator_reference = "repeat respondents",
  output_file = file.path(path.save, "tab_coef_h1_panel.tex")
)

make_heterogeneity_coef_table(
  models = models_h2_comp,
  coef_map = coef_map_h2,
  caption = paste0(
    "Logit coefficients estimates for assignment-by-",
    "prior-panel-status models"
  ),
  label = "tab:coef_h2_comp_history",
  moderator_reference = "respondents without valid competence data in Wave~14",
  output_file = file.path(path.save, "tab_coef_h2_comp_history.tex")
)

make_heterogeneity_coef_table(
  models = models_h3_isced,
  coef_map = coef_map_h3,
  caption = paste0(
    "Logit coefficients estimates for assignment-by-",
    "prior-panel-status models"
  ),
  label = "tab:coef_h3_education",
  moderator_reference = "the lower ISCED group",
  output_file = file.path(path.save, "tab_coef_h3_education.tex")
)


make_heterogeneity_coef_table(
  models = models_h4_gender,
  coef_map = coef_map_h4,
  caption = paste0(
    "Logit coefficients estimates for assignment-by-",
    "gender"
  ),
  label = "tab:coef_h4_gender",
  moderator_reference = "male respondents",
  output_file = file.path(path.save, "tab_coef_h4_gender.tex")
)



# 3. Stability of assignment ----------------------------------------------

model_stability <- list(
  "Valid competence-test data" = list(
    M1 = models_y1$M1_Assignment,
    M3 = models_y1$M3_panel_engagement_interaction,
    M6 = models_y1$M6_full_pre_fieldwork
  ),
  "Interview start" = list(
    M1 = models_y2$M1_Assignment,
    M3 = models_y2$M3_panel_engagement_interaction,
    M6 = models_y2$M6_full_pre_fieldwork
  ),
  "Follow-up CAWI" = list(
    M1 = models_y3$M1_Assignment,
    M3 = models_y3$M3_panel_engagement_interaction,
    M6 = models_y3$M6_full_pre_fieldwork
  )
)

# across berechnen
stability_results <- purrr::imap_dfr(
  model_stability,
  function(outcome_models, outcome_name) {
    purrr::imap_dfr(
      outcome_models,
      function(model, specification_name) {
        get_assignment_contrasts_across(
          model = model,
          outcome_label = outcome_name,
          specification_label = specification_name,
          mode_pairs = mode_pairs
        )
      }
    )
  }
)

### Output
# table: stability of pairwise assignment effects across nested models
# tab:assignment_stability

stability_results %>% 
  mutate(
    estimate_pp = round(estimate_pp, 1),
    conf_low_pp = round(conf_low_pp, 1),
    conf_high_pp = round(conf_high_pp, 1)
  ) %>% 
  print(n = Inf, width = Inf)


# überprüfen Gesamt-N
stability_results %>% 
  dplyr::distinct(
    outcome,
    specification,
    n
  )
# passt


# als Tabelle ausgeben
stability_results_long <- stability_results %>% 
  dplyr::mutate(
    effect = format_ci(
      estimate_pp,
      conf_low_pp,
      conf_high_pp
    ),
    specification = factor(
      specification,
      levels = c("M1", "M3", "M6")
    )
  ) %>% 
  dplyr::arrange(
    outcome,
    specification,
    contrast
  )

stability_table <- stability_results_long %>% 
  dplyr::select(
    outcome,
    specification,
    contrast,
    effect
  ) %>% 
  tidyr::pivot_wider(
    names_from = contrast,
    values_from = effect
  )

stability_table %>% 
  print(n = Inf, width = Inf)
# CAVEAT: arrange verändert auch die Reihenfolge der Outcome Variable
# und auch Spalten sind vertauscht


# Veränderungen gegenüber M1 berechnen
stability_chance <- stability_results %>% 
  dplyr::select(
    outcome,
    specification,
    contrast,
    estimate_pp
  ) %>% 
  tidyr::pivot_wider(
    names_from = specification,
    values_from = estimate_pp
  ) %>% 
  dplyr::mutate(
    change_M3_vs_M1 = M3 - M1,
    change_M6_vs_M1 = M6 - M1
  )

stability_chance %>% 
  print(n = Inf, width = Inf)


### Output
# table: stability of pairwise assignment effects across nested models
# tab:assignment_stability
# footnote

stability_chance %>% 
  summarise(
    max_abs_change_M3_vs_M1 = 
      max(abs(change_M3_vs_M1), na.rm = TRUE),
    
    max_abs_change_M6_vs_M1 = 
      max(abs(change_M6_vs_M1), na.rm = TRUE)
  )




# 4. Sensitivity ----------------------------------------------------------


# 4.1 NA Fälle ------------------------------------------------------------

# M1_Assignment auf den gesamten Datensatz anwenden

m1_y1_full <- fit_logit(
  formula = formulas_y1[["M1_Assignment"]],
  data = AuswertungB160.deskriptiv
)

m1_y2_full <- fit_logit(
  formula = formulas_y2[["M1_Assignment"]],
  data = AuswertungB160.deskriptiv
)

m1_y3_full <- fit_logit(
  formula = formulas_y3[["M1_Assignment"]],
  data = AuswertungB160.deskriptiv %>% filter(Y_Befragung == 1)
)

m1_y1_main <- models_y1[["M1_Assignment"]]
m1_y2_main <- models_y2[["M1_Assignment"]]
m1_y3_main <- models_y3[["M1_Assignment"]]

# Kontraste berechnen
sens_y1_full <- get_all_pairwise_contrasts(
  m1_y1_full,
  equivalence_margin_pp = 5,
  outcome_label = "Valid competence test"
)

sens_y1_main <- get_all_pairwise_contrasts(
  m1_y1_main,
  equivalence_margin_pp = 5,
  outcome_label = "Valid competence test"
)

sens_y2_full <- get_all_pairwise_contrasts(
  m1_y2_full,
  equivalence_margin_pp = 5,
  outcome_label = "Interview started"
)

sens_y2_main <- get_all_pairwise_contrasts(
  m1_y2_main,
  equivalence_margin_pp = 5,
  outcome_label = "Interview started"
)


sens_y3_full <- get_all_pairwise_contrasts(
  m1_y3_full,
  equivalence_margin_pp = 5,
  outcome_label = "Follow-up CAWI"
)

sens_y3_main <- get_all_pairwise_contrasts(
  m1_y3_main,
  equivalence_margin_pp = 5,
  outcome_label = "Follow-up CAWI"
)

# direkter Vergleich
sens_sample_restriction <- dplyr::bind_rows(
  
  sens_y1_full %>% 
    transmute(
      outcome,
      contrast,
      full_sample_pp = estimate_pp
    ) %>% 
    left_join(
      sens_y1_main %>% 
        transmute(
          outcome,
          contrast,
          main_sample_pp = estimate_pp
        ),
      by = c("outcome", "contrast")
    ),
  
  sens_y2_full %>% 
    transmute(
      outcome,
      contrast,
      full_sample_pp = estimate_pp
    ) %>% 
    left_join(
      sens_y2_main %>% 
        transmute(
          outcome,
          contrast,
          main_sample_pp = estimate_pp
        ),
      by = c("outcome", "contrast")
    ),
  
  sens_y3_full %>% 
    transmute(
      outcome,
      contrast,
      full_sample_pp = estimate_pp
    ) %>% 
    left_join(
      sens_y3_main %>% 
        transmute(
          outcome,
          contrast,
          main_sample_pp = estimate_pp
        ),
      by = c("outcome", "contrast")
    )
  
) %>% 
  mutate(
    difference_pp = main_sample_pp - full_sample_pp
  )


### Output
# table sensitivity appendix

sens_sample_restriction %>% print()
max(abs(sens_sample_restriction$difference_pp))




# 4.1 CASMIN vs. ISCED M6/H1 ----------------------------------------------------

dat_edu_common <- dat_analysis_main %>% 
  filter(
    !is.na(CASMIN_suf),
    CASMIN_suf != "unbekannt"
  )

nrow(dat_analysis_main)
nrow(dat_edu_common)

dat_analysis_main %>% 
  count(CASMIN_suf, .drop = FALSE)

dat_analysis_main %>% 
  filter(
    is.na(CASMIN_suf) | CASMIN_suf == "unbekannt"
  ) %>% 
  select(ID_t, ISCED_suf, CASMIN_suf)

# common sample: N= 4,290

# primary M6 formulas
f_y1_isced <- formulas_y1[["M6_full_pre_fieldwork"]]
f_y2_isced <- formulas_y2[["M6_full_pre_fieldwork"]]
f_y3_isced <- formulas_y3[["M6_full_pre_fieldwork"]]

# replace ISCED_suf durch CASMIN_suf
f_y1_casmin <- update(f_y1_isced, . ~ . - ISCED_suf + CASMIN_suf)
f_y2_casmin <- update(f_y2_isced, . ~ . - ISCED_suf + CASMIN_suf)
f_y3_casmin <- update(f_y3_isced, . ~ . - ISCED_suf + CASMIN_suf)

# ISCED models on common sample
m6_y1_isced_common <- fit_logit(
  f_y1_isced,
  data = dat_edu_common
)

m6_y2_isced_common <- fit_logit(
  f_y2_isced,
  data = dat_edu_common
)

# ISCED models on common sample
m6_y1_casmin_common <- fit_logit(
  f_y1_casmin,
  data = dat_edu_common
)

m6_y2_casmin_common <- fit_logit(
  f_y2_casmin,
  data = dat_edu_common
)

# Y_3 - Follow-up CAWI
m6_y3_isced_common <- fit_logit(
  f_y3_isced,
  data = dat_edu_common %>% filter(Y_Befragung == 1)
)

m6_y3_casmin_common <- fit_logit(
  f_y3_casmin,
  data = dat_edu_common %>% filter(Y_Befragung == 1)
)

# Check: Fallzahlen
c(
  nobs(m6_y1_isced_common),
  nobs(m6_y2_isced_common),
  nobs(m6_y3_isced_common),
  nobs(m6_y1_casmin_common),
  nobs(m6_y2_casmin_common),
  nobs(m6_y3_casmin_common)
)
# passt


# Assignment AMEs vergleiche
isced_y1 <- get_all_pairwise_contrasts(
  m6_y1_isced_common,
  equivalence_margin_pp = 5,
  outcome_label = "Valid competence-test data"
)

casmin_y1 <- get_all_pairwise_contrasts(
  m6_y1_casmin_common,
  equivalence_margin_pp = 5,
  outcome_label = "Valid competence-test data"
)


isced_y2 <- get_all_pairwise_contrasts(
  m6_y2_isced_common,
  equivalence_margin_pp = 5,
  outcome_label = "Interview started"
)

casmin_y2 <- get_all_pairwise_contrasts(
  m6_y2_casmin_common,
  equivalence_margin_pp = 5,
  outcome_label = "Interview started"
)


isced_y3 <- get_all_pairwise_contrasts(
  m6_y3_isced_common,
  equivalence_margin_pp = 5,
  outcome_label = "Follow-up CAWI"
)

casmin_y3 <- get_all_pairwise_contrasts(
  m6_y3_casmin_common,
  equivalence_margin_pp = 5,
  outcome_label = "Follow-up CAWI"
)

# Vergleichsfunktion
compare_education_coding <- function(isced, casmin) {
  isced %>% 
    select(outcome, contrast, estimate_pp) %>% 
    rename(ISCED_pp = estimate_pp) %>% 
    left_join(
      casmin %>% 
        select(outcome, contrast, estimate_pp) %>% 
        rename(CASMIN_pp = estimate_pp),
      by = c("outcome", "contrast")
    ) %>% 
    mutate(
      difference_pp = CASMIN_pp - ISCED_pp,
      abs_difference_pp = abs(difference_pp)
    )
}

sens_education <- bind_rows(
  compare_education_coding(isced_y1, casmin_y1),
  compare_education_coding(isced_y2, casmin_y2),
  compare_education_coding(isced_y3, casmin_y3)
)

### Output
# table sensitivity
sens_education %>% 
  group_by(outcome) %>%
  summarise(
    max_abs_difference_pp = max(abs_difference_pp), .groups = "drop"
  )



# 4.1 CASMIN vs. ISCED H3 ----------------------------------------------------

# primary M6 formulas
f_y1_H3_isced <- formulas_y1_heterogeneity[["H3_education"]]
f_y2_H3_isced <- formulas_y2_heterogeneity[["H3_education"]]
f_y3_H3_isced <- formulas_y3_heterogeneity[["H3_education"]]

# replace ISCED_suf durch CASMIN_suf
f_y1_H3_casmin <- update(f_y1_H3_isced, . ~ . - ISCED_suf + CASMIN_suf - STARTMODE:ISCED_suf + STARTMODE:CASMIN_suf)
f_y2_H3_casmin <- update(f_y2_H3_isced, . ~ . - ISCED_suf + CASMIN_suf - STARTMODE:ISCED_suf + STARTMODE:CASMIN_suf)
f_y3_H3_casmin <- update(f_y3_H3_isced, . ~ . - ISCED_suf + CASMIN_suf - STARTMODE:ISCED_suf + STARTMODE:CASMIN_suf)

# ISCED models on common sample
H3_y1_isced_common <- fit_logit(
  f_y1_H3_isced,
  data = dat_edu_common
)

H3_y2_isced_common <- fit_logit(
  f_y2_H3_isced,
  data = dat_edu_common
)

# ISCED models on common sample
H3_y1_casmin_common <- fit_logit(
  f_y1_H3_casmin,
  data = dat_edu_common
)

H3_y2_casmin_common <- fit_logit(
  f_y2_H3_casmin,
  data = dat_edu_common
)

# Y_3 - Follow-up CAWI
H3_y3_isced_common <- fit_logit(
  f_y3_H3_isced,
  data = dat_edu_common %>% filter(Y_Befragung == 1)
)

H3_y3_casmin_common <- fit_logit(
  f_y3_H3_casmin,
  data = dat_edu_common %>% filter(Y_Befragung == 1)
)

# Check: Fallzahlen
c(
  nobs(H3_y1_isced_common),
  nobs(H3_y2_isced_common),
  nobs(H3_y3_isced_common),
  nobs(H3_y1_casmin_common),
  nobs(H3_y2_casmin_common),
  nobs(H3_y3_casmin_common)
)
# passt


# Assignment AMEs vergleiche
isced_H3_y1 <- get_all_pairwise_contrasts(
  H3_y1_isced_common,
  equivalence_margin_pp = 5,
  outcome_label = "Valid competence-test data"
)

casmin_H3_y1 <- get_all_pairwise_contrasts(
  H3_y1_casmin_common,
  equivalence_margin_pp = 5,
  outcome_label = "Valid competence-test data"
)


isced_H3_y2 <- get_all_pairwise_contrasts(
  H3_y2_isced_common,
  equivalence_margin_pp = 5,
  outcome_label = "Interview started"
)

casmin_H3_y2 <- get_all_pairwise_contrasts(
  H3_y2_casmin_common,
  equivalence_margin_pp = 5,
  outcome_label = "Interview started"
)


isced_H3_y3 <- get_all_pairwise_contrasts(
  H3_y3_isced_common,
  equivalence_margin_pp = 5,
  outcome_label = "Follow-up CAWI"
)

casmin_H3_y3 <- get_all_pairwise_contrasts(
  H3_y3_casmin_common,
  equivalence_margin_pp = 5,
  outcome_label = "Follow-up CAWI"
)


sens_H3_education <- bind_rows(
  compare_education_coding(isced_H3_y1, casmin_H3_y1),
  compare_education_coding(isced_H3_y2, casmin_H3_y2),
  compare_education_coding(isced_H3_y3, casmin_H3_y3)
)

### Output
# table sensitivity
sens_H3_education %>% 
  group_by(outcome) %>%
  summarise(
    max_abs_difference_pp = max(abs_difference_pp), .groups = "drop"
  )


# 4.3 Birth year vs centering birth year -----------------------------------

dat_analysis_main <- dat_analysis_main %>% 
  mutate(
    Alter_suf_c = Alter_suf_year - mean(Alter_suf_year, na.rm = TRUE)
  )

# primary M6 formulas
f_y1_cohort <- formulas_y1[["M6_full_pre_fieldwork"]]
f_y2_cohort <- formulas_y2[["M6_full_pre_fieldwork"]]
f_y3_cohort <- formulas_y3[["M6_full_pre_fieldwork"]]

# Alternative specifications.
# birth-cohort facotr raus, centred birth year rein
f_y1_birthyear <- update(f_y1_cohort, . ~ . - Alter_suf + Alter_suf_c)
f_y2_birthyear <- update(f_y2_cohort, . ~ . - Alter_suf + Alter_suf_c)
f_y3_birthyear <- update(f_y3_cohort, . ~ . - Alter_suf + Alter_suf_c)

# fitten
m6_y1_cohort <- fit_logit(
  f_y1_cohort,
  data = dat_analysis_main
)

m6_y1_birthyear <- fit_logit(
  f_y1_birthyear,
  data = dat_analysis_main
)


m6_y2_cohort <- fit_logit(
  f_y2_cohort,
  data = dat_analysis_main
)

m6_y2_birthyear <- fit_logit(
  f_y2_birthyear,
  data = dat_analysis_main
)


m6_y3_cohort <- fit_logit(
  f_y3_cohort,
  data = dat_analysis_main %>% filter(Y_Befragung == 1)
)

m6_y3_birthyear <- fit_logit(
  f_y3_birthyear,
  data = dat_analysis_main %>% filter(Y_Befragung == 1)
)


# Kontraste vergleichen
cohort_y1 <- get_all_pairwise_contrasts(
  m6_y1_cohort,
  equivalence_margin_pp = 5,
  outcome_label = "Valid competence-test data"
)

birthyear_y1 <- get_all_pairwise_contrasts(
  m6_y1_birthyear,
  equivalence_margin_pp = 5,
  outcome_label = "Valid competence-test data"
)


cohort_y2 <- get_all_pairwise_contrasts(
  m6_y2_cohort,
  equivalence_margin_pp = 5,
  outcome_label = "Interview started"
)

birthyear_y2 <- get_all_pairwise_contrasts(
  m6_y2_birthyear,
  equivalence_margin_pp = 5,
  outcome_label = "Interview started"
)


cohort_y3 <- get_all_pairwise_contrasts(
  m6_y3_cohort,
  equivalence_margin_pp = 5,
  outcome_label = "Follow-up CAWI"
)

birthyear_y3 <- get_all_pairwise_contrasts(
  m6_y3_birthyear,
  equivalence_margin_pp = 5,
  outcome_label = "Follow-up CAWI"
)


sens_birthyear <- bind_rows(
  compare_education_coding(cohort_y1, birthyear_y1),
  compare_education_coding(cohort_y2, birthyear_y2),
  compare_education_coding(cohort_y3, birthyear_y3)
)

### Output
# table sensitivity
sens_birthyear %>% 
  group_by(outcome) %>%
  summarise(
    max_abs_difference_pp = max(abs_difference_pp), .groups = "drop"
  )




# 4.4 Probit vs. Logit ----------------------------------------------------

fit_probit <- function(formula, data) {
  stats::glm(
    formula = formula,
    data = data,
    family = stats::binomial(link = "probit")
  )
}

# primary M6 formulas
f_y1_m6 <- formulas_y1[["M6_full_pre_fieldwork"]]
f_y2_m6 <- formulas_y2[["M6_full_pre_fieldwork"]]
f_y3_m6 <- formulas_y3[["M6_full_pre_fieldwork"]]

# fitten
m6_y1_probit <- fit_probit(
  f_y1_m6,
  data = dat_analysis_main
)


m6_y2_probit <- fit_probit(
  f_y2_m6,
  data = dat_analysis_main
)


m6_y3_probit <- fit_probit(
  f_y3_m6,
  data = dat_analysis_main %>% filter(Y_Befragung == 1)
)

# logit results
m6_y1_logit <- models_y1[["M6_full_pre_fieldwork"]]
m6_y2_logit <- models_y2[["M6_full_pre_fieldwork"]]
m6_y3_logit <- models_y3[["M6_full_pre_fieldwork"]]

c(
  nobs(m6_y1_probit),
  nobs(m6_y2_probit),
  nobs(m6_y3_probit),
  nobs(m6_y1_logit),
  nobs(m6_y2_logit),
  nobs(m6_y3_logit)
)


# Kontraste vergleichen
logit_y1 <- get_all_pairwise_contrasts(
  m6_y1_logit,
  equivalence_margin_pp = 5,
  outcome_label = "Valid competence-test data"
)

probit_y1 <- get_all_pairwise_contrasts(
  m6_y1_probit,
  equivalence_margin_pp = 5,
  outcome_label = "Valid competence-test data"
)


logit_y2 <- get_all_pairwise_contrasts(
  m6_y2_logit,
  equivalence_margin_pp = 5,
  outcome_label = "Interview started"
)

probit_y2 <- get_all_pairwise_contrasts(
  m6_y2_probit,
  equivalence_margin_pp = 5,
  outcome_label = "Interview started"
)


logit_y3 <- get_all_pairwise_contrasts(
  m6_y3_logit,
  equivalence_margin_pp = 5,
  outcome_label = "Follow-up CAWI"
)

probit_y3 <- get_all_pairwise_contrasts(
  m6_y3_probit,
  equivalence_margin_pp = 5,
  outcome_label = "Follow-up CAWI"
)


sens_probit <- bind_rows(
  compare_education_coding(probit_y1, logit_y1),
  compare_education_coding(probit_y2, logit_y2),
  compare_education_coding(probit_y3, logit_y3)
)

### Output
# table sensitivity
sens_probit %>% 
  group_by(outcome) %>%
  summarise(
    max_abs_difference_pp = max(abs_difference_pp), .groups = "drop"
  )


# Check
sens_probit %>% 
  rename(
    probit_pp = ISCED_pp,
    logit_pp = CASMIN_pp
  )


# End -------------------
# Next step:
# multinomial model
