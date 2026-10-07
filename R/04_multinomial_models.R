# ===
# Multinomial models for realised testing-interview states
# Participation and Selectivity in Web-Based Cognitive Testing and Surveying:
# Evidence from a Randomized Mode Experiment
#
# Requires `00_run_analysis.R`, `R/00_functions.R`, and the common analysis
# sample created in `R/03_binary_models.R`.
# ===

# Daten aufbereiten -------------------------------------------------------

dat_state <- dat_analysis_main %>% 
  mutate(
    fieldwork_state = case_when(
      Y_TBT == 0 & Y_Befragung == 0 ~ "No valid test / no interview",
      Y_TBT == 0 & Y_Befragung == 1 ~ "Interview only",
      Y_TBT == 1 & Y_Befragung == 1 ~ "Valid test + interview",
      TRUE ~ NA_character_
    ),
    fieldwork_state = factor(
      fieldwork_state,
      levels = c(
        "No valid test / no interview",
        "Interview only",
        "Valid test + interview"
      )
    )
  )

dat_state_analysis <- dat_state


# Modelle definieren ------------------------------------------------------

rhs_specs <- list(
  # M0_Null = 0L,
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


outcomes <- c(
  "No valid test / no interview",
  "Interview only",
  "Valid test + interview"
)


# Modelle formulieren -----------------------------------------------------


formulas.multinom <- lapply(rhs_specs, function(vars) {
  f <- make_formula("fieldwork_state", vars)
  environment(f) <- .GlobalEnv
  f  
})

formulas.multinom[["M0_NULL"]] <- "fieldwork_state ~ 1"

used_variables <- get_all_vars(formulas.multinom$M6_full_pre_fieldwork, dat_state) %>% names()




# Multinomial Models ------------------------------------------------------

# 1. Modelle schätzen -----------------------------------------------------


fit_multinomial_logit <- function(formula, data) {
  nnet::multinom(
    formula = formula,
    data = data,
    trace = FALSE,
    Hess = TRUE,
    model = TRUE
  )
}

m_state <- lapply(
  formulas.multinom,
  FUN = fit_multinomial_logit,
  data = dat_state_analysis
)


# some checks
summary(m_state$M6_full_pre_fieldwork)

# design matrix full rank
X <- model.matrix(
  formulas.multinom$M6_full_pre_fieldwork,
  data = dat_state_analysis
)

c(
  rank = qr(X)$rank,
  parameters = ncol(X)
)

# zellbesetzung interaktion
with(
  dat_state_analysis,
  table(STARTMODE, status.16, fieldwork_state)
)

# Convergence
m_state$M6_full_pre_fieldwork$convergence

is.null(m_state$M6_full_pre_fieldwork$model)
is.null(m_state$M6_full_pre_fieldwork$Hessian)
# beide FALSE? perfect

nrow(m_state$M6_full_pre_fieldwork$model)
sapply(
  m_state,
  nobs
)
# Expected common analysis sample: N = 4,292.



# 1.1 Robust Covariance matrix --------------------------------------------

mod_M6 <- m_state$M6_full_pre_fieldwork
V_robust_M6 <- sandwich::sandwich(mod_M6)
dim(V_robust_M6)
all(is.finite(V_robust_M6))
dim(sandwich::estfun(mod_M6))
dim(sandwich::bread(mod_M6))



# 1.3 Heterogeneity models ------------------------------------------------


formulas.multinom_heterogeneity <- lapply(rhs_specs_heterogeneity, function(vars) {
  f <- make_formula("fieldwork_state", vars)
  environment(f) <- .GlobalEnv
  f  
})

used_variables_heterogeneity_H2 <- get_all_vars(formulas.multinom_heterogeneity$H2_competence_history, dat_state) %>% names()

h_state <- lapply(
  formulas.multinom_heterogeneity,
  FUN = fit_multinomial_logit,
  data = dat_state_analysis
)


# some checks
summary(h_state$H2_competence_history)

# design matrix full rank
X <- model.matrix(
  formulas.multinom_heterogeneity$H2_competence_history,
  data = dat_state_analysis
)

c(
  rank = qr(X)$rank,
  parameters = ncol(X)
)

# zellbesetzung interaktion
with(
  dat_state_analysis,
  table(STARTMODE, status.16, fieldwork_state)
)

# Convergence
h_state$H2_competence_history$convergence

is.null(h_state$H2_competence_history$model)
is.null(h_state$H2_competence_history$Hessian)
# beide FALSE? perfect

nrow(h_state$H2_competence_history$model)
sapply(
  h_state,
  nobs
)
# Expected common analysis sample: N = 4,292.


V_robust_H2 <- sandwich::sandwich(h_state$H2_competence_history)
dim(V_robust_H2)
all(is.finite(V_robust_H2))
dim(sandwich::estfun(h_state$H2_competence_history))
dim(sandwich::bread(h_state$H2_competence_history))


V_robust_H3 <- sandwich::sandwich(h_state$H3_education)
dim(V_robust_H3)
all(is.finite(V_robust_H3))
dim(sandwich::estfun(h_state$H3_education))
dim(sandwich::bread(h_state$H3_education))

V_robust_H4 <- sandwich::sandwich(h_state$H4_gender)
dim(V_robust_H4)
all(is.finite(V_robust_H4))
dim(sandwich::estfun(h_state$H4_gender))
dim(sandwich::bread(h_state$H4_gender))

# 2. Prediction -----------------------------------------------------------

# 2.1 overall assignment effects ------------------------------------------

# für alle Modelle
models_state <- m_state[c(formulas.multinom %>% names())[-length(m_state)]]
# außer M0, weil keine Covariates

res_state_all <- purrr::imap(
  models_state,
  ~ get_multinomial_results(
    mod = .x,
    model_name = .y
  )
)


# prediction zusammenführen
pred_state_all <- purrr::map_dfr(
  res_state_all,
  "predictions"
) %>% 
  dplyr::mutate(
    estimate_pct = 100 * estimate,
    conf.low_pct = 100 * conf.low,
    conf.high_pct = 100 * conf.high
  )


### Output
# table: adjusted probabilities and assignment effects for realised testing-interview states
# tab:mutlinomial_state
# Panel A: adjusted predicted state probabilities

pred_state_all %>% 
  filter(specification == "M6_full_pre_fieldwork") %>% 
  select(group, STARTMODE, ends_with("pct")) %>% 
  mutate(
    estimate_pct = round(estimate_pct, 1),
    conf.low_pct = round(conf.low_pct, 1),
    conf.high_pct = round(conf.high_pct, 1)
  )



# all contrasts
contrast_state_all <- purrr::map_dfr(
  res_state_all,
  "contrasts"
) %>% 
  dplyr::mutate(
    estimate_pct = 100 * estimate,
    conf.low_pct = 100 * conf.low,
    conf.high_pct = 100 * conf.high
  )


### Output
# table: adjusted probabilities and assignment effects for realised testing-interview states
# tab:mutlinomial_state
# Panel B: pairwise average marginal assignment effects

contrast_state_all %>% 
  filter(specification == "M6_full_pre_fieldwork") %>% 
  select(group, contrast, ends_with("pct")) %>% 
  mutate(
    estimate_pct = round(estimate_pct, 1),
    conf.low_pct = round(conf.low_pct, 1),
    conf.high_pct = round(conf.high_pct, 1)
  )



# some checks
check_probabilities <- pred_state_all %>% 
  dplyr::group_by(
    specification,
    STARTMODE
  ) %>% 
  dplyr::summarise(
    total_probabilities = sum(estimate),
    .groups = "drop"
  )
check_probabilities
# passt; alles 1

check_contrasts <- contrast_state_all %>% 
  dplyr::group_by(
    specification,
    contrast
  ) %>% 
  dplyr::summarise(
    total_probabilities = sum(estimate),
    .groups = "drop"
  )
check_contrasts
# passt; alle fast ~0



# 2.2 Assignment times status.16 state probabilities ----------------------------

# A. Average predicted state probabilities 
# by assignment times status.16

pred_state_status.16_M6 <- marginaleffects::avg_predictions(
  mod_M6,
  newdata = mod_M6$model,
  variables = "STARTMODE",
  by = c("status.16", "STARTMODE"),
  type = "probs",
  vcov = V_robust_M6,
  conf_level = 0.95
) %>% 
  as.data.frame() %>% 
  dplyr::mutate(
    estimate_pct = 100 * estimate,
    conf.low_pct = 100 * conf.low,
    conf.high_pct = 100 * conf.high
  )

pred_state_status.16_M6

# Summencheck
pred_state_status.16_M6 %>%
  dplyr::group_by(
    status.16,
    STARTMODE
  ) %>% 
  dplyr::summarise(
    total_probability = sum(estimate),
    .groups = "drop"
  )
# passt != 1


# B. assignment contrasts for status.16

contrast_state_status.16_obj <- marginaleffects::avg_comparisons(
  mod_M6,
  newdata = mod_M6$model,
  variables = list(STARTMODE = "pairwise"),
  comparison = "difference",
  by = "status.16",
  type = "probs",
  vcov = V_robust_M6,
  conf_level = 0.95
)

contrast_state_status.16_M6 <- contrast_state_status.16_obj %>% 
  as.data.frame() %>% 
  dplyr::mutate(
    estimate_pp = 100 * estimate,
    conf.low_pp = 100 * conf.low,
    conf.high_pp = 100 * conf.high
  )

contrast_state_status.16_M6

# checks
contrast_state_status.16_M6 %>% 
  dplyr::group_by(
    status.16
  ) %>% 
  dplyr::summarise(
    sum_contrast = sum(estimate),
    .groups = "drop"
  )
# passt. ==0


# C. Probability-scale heterogeneity contrast
# status.16 assignment effect

nd_M6 <- make_counterfactual_assignment_data(
  mod = m_state$M6_full_pre_fieldwork,
  assignment = "STARTMODE"
)

table(nd_M6$.assignment_cf)
# Expected counterfactual rows per assignment level: N = 4,292.



pred_state_status.16_M6_obj <- marginaleffects::avg_predictions(
  mod_M6,
  newdata = mod_M6$model,
  variables = "STARTMODE",
  by = c("status.16", "STARTMODE"),
  type = "probs",
  vcov = V_robust_M6,
  conf_level = 0.95
)



V_pred_status.16_M6 <- vcov(
  pred_state_status.16_M6_obj
)

dim(as.data.frame(
  pred_state_status.16_M6_obj
))
dim(V_pred_status.16_M6)
all(is.finite(V_pred_status.16_M6))


# für status.16
pairs_STARTMODE <- list(
  c("CALVI", "CAPI"),
  c("CAWI-TBT+CATI", "CALVI"),
  c("CAWI-TBT+CATI", "CAPI")
)

heterogeneity_state_status.16_M6 <- 
  multinom_heterogeneity_contrasts(
    pred_obj = pred_state_status.16_M6_obj,
    moderator = "status.16",
    level1 = "TDO",
    level0 = "Part",
    assignment = "STARTMODE",
    assignment_pairs = pairs_STARTMODE,
    conf_level = 0.95
  )

# check
heterogeneity_state_status.16_M6 %>% 
  dplyr::group_by(contrast) %>% 
  dplyr::summarise(
    sum_H = sum(estimate),
    sum_H_pp = sum(estimate_pp),
    .groups = "drop"
  )

all(is.finite(vcov(pred_state_status.16_M6_obj)))




# 2.3 H2 valid competence data wave14 ----------------------------------------

# A. Average predicted state probabilities 
# by assignment times valid_comp_w14

pred_state_H2_obj <- marginaleffects::avg_predictions(
  model = h_state$H2_competence_history,
  newdata = h_state$H2_competence_history$model,
  variables = "STARTMODE",
  by = c("valid_comp_w14", "STARTMODE"),
  type = "probs",
  vcov = V_robust_H2,
  conf_level = 0.95
) 

pred_state_H2 <- as.data.frame(
  pred_state_H2_obj
) %>% 
  dplyr::mutate(
    estimate_pct = 100 * estimate,
    conf.low_pct = 100 * conf.low,
    conf.high_pct = 100 * conf.high
  )

pred_state_H2

# Summencheck
pred_state_H2 %>%
  dplyr::group_by(
    valid_comp_w14,
    STARTMODE
  ) %>% 
  dplyr::summarise(
    total_probability = sum(estimate),
    .groups = "drop"
  )
# passt != 1


# B. assignment contrasts for valid_comp_w14

contrast_state_obj <- marginaleffects::avg_comparisons(
  model = h_state$H2_competence_history,
  newdata = h_state$H2_competence_history$model,
  variables = list(STARTMODE = "pairwise"),
  comparison = "difference",
  by = "valid_comp_w14",
  type = "probs",
  vcov = V_robust_H2,
  conf_level = 0.95
) 

contrast_state_H2 <- contrast_state_obj %>%
  as.data.frame() %>% 
  dplyr::mutate(
    estimate_pp = 100 * estimate,
    conf.low_pp = 100 * conf.low,
    conf.high_pp = 100 * conf.high
  )

contrast_state_H2

# checks
contrast_state_H2 %>% 
  dplyr::group_by(
    valid_comp_w14
  ) %>% 
  dplyr::summarise(
    sum_contrast = sum(estimate),
    .groups = "drop"
  )
# passt. ==0


# C. Probability-scale heterogeneity contrast
# valid_comp_w14 assignment effect

nd_H2 <- make_counterfactual_assignment_data(
  mod = h_state$H2_competence_history,
  assignment = "STARTMODE"
)

table(nd_H2$.assignment_cf)
# Expected counterfactual rows per assignment level: N = 4,292.


# formale probability-scale Heterogeneity contrasts


V_pred_H2 <- vcov(
  pred_state_H2_obj
)

dim(pred_state_H2)
dim(V_pred_H2)
all(is.finite(V_pred_H2))



heterogeneity_state_H2 <- 
  multinom_heterogeneity_contrasts(
    pred_obj = pred_state_H2_obj,
    moderator = "valid_comp_w14",
    level1 = "1",
    level0 = "0",
    assignment = "STARTMODE",
    assignment_pairs = pairs_STARTMODE,
    conf_level = 0.95
  )

# check
heterogeneity_state_H2 %>% 
  dplyr::group_by(contrast) %>% 
  dplyr::summarise(
    sum_H = sum(estimate),
    sum_H_pp = sum(estimate_pp),
    .groups = "drop"
  )




# 2.4 H3 education ----------------------------------------

# A. Average predicted state probabilities 
# by assignment times valid_comp_w14

pred_state_H3_obj <- marginaleffects::avg_predictions(
  model = h_state$H3_education,
  newdata = h_state$H3_education$model,
  variables = "STARTMODE",
  by = c("ISCED_suf", "STARTMODE"),
  type = "probs",
  vcov = V_robust_H3,
  conf_level = 0.95
) 

pred_state_H3 <- as.data.frame(
  pred_state_H3_obj
) %>% 
  dplyr::mutate(
    estimate_pct = 100 * estimate,
    conf.low_pct = 100 * conf.low,
    conf.high_pct = 100 * conf.high
  )

pred_state_H3

# Summencheck
pred_state_H3 %>%
  dplyr::group_by(
    ISCED_suf,
    STARTMODE
  ) %>% 
  dplyr::summarise(
    total_probability = sum(estimate),
    .groups = "drop"
  )
# passt != 1


# B. assignment contrasts for valid_comp_w14

contrast_state_H3 <- marginaleffects::avg_comparisons(
  model = h_state$H3_education,
  newdata = h_state$H3_education$model,
  variables = list(STARTMODE = "pairwise"),
  comparison = "difference",
  by = "ISCED_suf",
  type = "probs",
  vcov = V_robust_H3,
  conf_level = 0.95
) %>%
  as.data.frame() %>% 
  dplyr::mutate(
    estimate_pp = 100 * estimate,
    conf.low_pp = 100 * conf.low,
    conf.high_pp = 100 * conf.high
  )

contrast_state_H3

# checks
contrast_state_H3 %>% 
  dplyr::group_by(
    ISCED_suf
  ) %>% 
  dplyr::summarise(
    sum_contrast = sum(estimate),
    .groups = "drop"
  )
# passt. ==0


# C. Probability-scale heterogeneity contrast
# valid_comp_w14 assignment effect

nd_H3 <- make_counterfactual_assignment_data(
  mod = h_state$H3_education,
  assignment = "STARTMODE"
)

table(nd_H3$.assignment_cf)
# Expected counterfactual rows per assignment level: N = 4,292.


# formale probability-scale Heterogeneity contrasts


V_pred_H3 <- vcov(
  pred_state_H3_obj
)

dim(pred_state_H3)
dim(V_pred_H3)
all(is.finite(V_pred_H3))



heterogeneity_state_H3_34vs02 <- 
  multinom_heterogeneity_contrasts(
    pred_obj = pred_state_H3_obj,
    moderator = "ISCED_suf",
    level1 = "3-4",
    level0 = "0-2",
    assignment = "STARTMODE",
    assignment_pairs = pairs_STARTMODE,
    conf_level = 0.95
  ) %>% 
  dplyr::mutate(
    moderator_contrast = "ISCED3-4 - ISCED0-2"
  )

heterogeneity_state_H3_56vs02 <- 
  multinom_heterogeneity_contrasts(
    pred_obj = pred_state_H3_obj,
    moderator = "ISCED_suf",
    level1 = "5-6",
    level0 = "0-2",
    assignment = "STARTMODE",
    assignment_pairs = pairs_STARTMODE,
    conf_level = 0.95
  ) %>% 
  dplyr::mutate(
    moderator_contrast = "ISCED5-6 - ISCED0-2"
  )

# zusammenführen
heterogeneity_state_H3 <- dplyr::bind_rows(
  heterogeneity_state_H3_34vs02,
  heterogeneity_state_H3_56vs02
)

# check
heterogeneity_state_H3 %>% 
  dplyr::group_by(moderator_contrast) %>% 
  dplyr::summarise(
    sum_H = sum(estimate),
    sum_H_pp = sum(estimate_pp),
    .groups = "drop"
  )


# 2.5 H4 gender ----------------------------------------

# A. Average predicted state probabilities 
# by assignment times valid_comp_w14

pred_state_H4_obj <- marginaleffects::avg_predictions(
  model = h_state$H4_gender,
  newdata = h_state$H4_gender$model,
  variables = "STARTMODE",
  by = c("Gender_suf", "STARTMODE"),
  type = "probs",
  vcov = V_robust_H4,
  conf_level = 0.95
) 

pred_state_H4 <- as.data.frame(
  pred_state_H4_obj
) %>% 
  dplyr::mutate(
    estimate_pct = 100 * estimate,
    conf.low_pct = 100 * conf.low,
    conf.high_pct = 100 * conf.high
  )

pred_state_H4

# Summencheck
pred_state_H4 %>%
  dplyr::group_by(
    Gender_suf,
    STARTMODE
  ) %>% 
  dplyr::summarise(
    total_probability = sum(estimate),
    .groups = "drop"
  )
# passt != 1


# B. assignment contrasts for gender

contrast_state_H4 <- marginaleffects::avg_comparisons(
  model = h_state$H4_gender,
  newdata = h_state$H4_gender$model,
  variables = list(STARTMODE = "pairwise"),
  comparison = "difference",
  by = "Gender_suf",
  type = "probs",
  vcov = V_robust_H4,
  conf_level = 0.95
) %>%
  as.data.frame() %>% 
  dplyr::mutate(
    estimate_pp = 100 * estimate,
    conf.low_pp = 100 * conf.low,
    conf.high_pp = 100 * conf.high
  )

contrast_state_H4

# checks
contrast_state_H4 %>% 
  dplyr::group_by(
    Gender_suf
  ) %>% 
  dplyr::summarise(
    sum_contrast = sum(estimate),
    .groups = "drop"
  )
# passt. ==0


# C. Probability-scale heterogeneity contrast
# valid_comp_w14 assignment effect

nd_H4 <- make_counterfactual_assignment_data(
  mod = h_state$H4_gender,
  assignment = "STARTMODE"
)

table(nd_H4$.assignment_cf)
# Expected counterfactual rows per assignment level: N = 4,292.


# formale probability-scale Heterogeneity contrasts


V_pred_H4 <- vcov(
  pred_state_H4_obj
)

dim(pred_state_H4)
dim(V_pred_H4)
all(is.finite(V_pred_H4))



heterogeneity_state_H4 <- 
  multinom_heterogeneity_contrasts(
    pred_obj = pred_state_H4_obj,
    moderator = "Gender_suf",
    level1 = "männlich",
    level0 = "weiblich",
    assignment = "STARTMODE",
    assignment_pairs = pairs_STARTMODE,
    conf_level = 0.95
  )

# check
heterogeneity_state_H4 %>%
  dplyr::group_by(contrast) %>% 
  dplyr::summarise(
    sum_H = sum(estimate),
    sum_H_pp = sum(estimate_pp),
    .groups = "drop"
  )


# 3. Publication tables -----------------------------------------------------


# 3.1 M6 status.16 ------------------------------------------------------------------

### Output
# table
# M6 probabilities
# tab:mutlinomial_tdo_probabilities
pred_state_status.16_M6 %>% 
  dplyr::select(
    status.16, group, STARTMODE,
    estimate_pct, conf.low_pct, conf.high_pct
  )


### Output
# table
# assign
# M6 contrasts
# tab: multinomial_tdo_contrasts
# Panel A and B
contrast_state_status.16_M6 %>% 
  dplyr::select(
    group, contrast, status.16,
    estimate_pp, conf.low_pp, conf.high_pp
  )

# Panel C
contrast_state_status.16_obj %>% 
  mutate(row_id = row_number()) %>% 
  select(row_id, group, contrast, status.16, estimate) %>% 
  pivot_wider(
    names_from = status.16,
    values_from = c(row_id, estimate)
  ) %>% 
  rowwise() %>% 
  mutate(
    # TDO - repeater
    difference = estimate_TDO - estimate_Part,
    
    se = sqrt(
      vcov(contrast_state_status.16_obj)[row_id_TDO, row_id_TDO] +
        vcov(contrast_state_status.16_obj)[row_id_Part, row_id_Part] -
        2*vcov(contrast_state_status.16_obj)[row_id_TDO, row_id_Part]
    ),
    
    estimate_pp = 100 * difference,
    conf_low_pp = 100 * (difference - qnorm(.975) * se),
    conf_high_pp = 100 * (difference + qnorm(.975) * se),
    
    p_value = 2* pnorm(
      abs(difference / se),
      lower.tail = FALSE
    )
  ) %>% 
  ungroup() %>% 
  select(group, contrast, estimate_pp, conf_low_pp, conf_high_pp, p_value) %>% 
  mutate(
    across(
      c(estimate_pp, conf_low_pp, conf_high_pp), ~ round(.x, 1)
    )
  )

# Heterogeneity
heterogeneity_state_status.16_M6 %>% 
  dplyr::select(
    group, contrast, moderator_contrast,
    estimate_pp, conf.low_pp, conf.high_pp
  )




### Output
# table: Stability M1-M6
# tab:multinomial_assignment_stability
contrast_state_all %>% 
  dplyr::select(
    specification, group, contrast,
    estimate_pct, conf.low_pct, conf.high_pct
  ) %>% 
  mutate(
    estimate_pct = round(estimate_pct, 1),
    conf.low_pct = round(conf.low_pct, 1),
    conf.high_pct = round(conf.high_pct, 1)
  ) %>% 
  arrange(
    group, specification
  )




# 3.2 H2 valid comp w14 ------------------------------------------------------------------

### Output
# table
# M6 probabilities
# tab:mutlinomial_w14_probabilities
pred_state_H2 %>% 
  dplyr::select(
    group, STARTMODE, valid_comp_w14,
    estimate_pct, conf.low_pct, conf.high_pct
  )

### Output
# table
# assign
# M6 contrasts
# tab: multinomial_w14_contrasts
# Panel A and B
contrast_state_H2 %>% 
  dplyr::select(
    group, contrast, valid_comp_w14,
    estimate_pp, conf.low_pp, conf.high_pp
  )

# Panel C
# Heterogeneity
heterogeneity_state_H2 %>% 
  dplyr::select(
    group, contrast, moderator_contrast,
    estimate_pp, conf.low_pp, conf.high_pp
  )



# 3.3 H3 ISCED ------------------------------------------------------------------

### Output
# table
# probabilities
# tab:multinomial_isced_probabilities
pred_state_H3 %>% 
  dplyr::select(
    group, STARTMODE, ISCED_suf,
    estimate_pct, conf.low_pct, conf.high_pct
  )


### Output
# table
# M6 probabilities
# tab:mutlinomial_isced_contrast
# Panel A and B
contrast_state_H3 %>% 
  dplyr::select(
    group, contrast, ISCED_suf,
    estimate_pp, conf.low_pp, conf.high_pp
  )

# Panel C
# Heterogeneity
heterogeneity_state_H3 %>% 
  dplyr::select(
    group, contrast, moderator_contrast,
    estimate_pp, conf.low_pp, conf.high_pp
  )


# 3.4 H4 Gender ------------------------------------------------------------------

### Output
# table
# probabilities
# tab:multinomial_gender_probabilities
# probabilities
pred_state_H4 %>%
  dplyr::select(
    group, STARTMODE, Gender_suf,
    estimate_pct, conf.low_pct, conf.high_pct
  )



### Output
# table
# M6 probabilities
# tab:mutlinomial_gender_contrast
# Panel A and B
contrast_state_H4 %>% 
  dplyr::select(
    group, contrast, Gender_suf,
    estimate_pp, conf.low_pp, conf.high_pp
  )

# Panel C
# Heterogeneity
heterogeneity_state_H4 %>% 
  dplyr::select(
    group, contrast, moderator_contrast,
    estimate_pp, conf.low_pp, conf.high_pp
  )


# 3.5 Omnibus Wald test for assignment X moderator ------------------------
# Global heterogeneity test
# H1 bis H4 definieren
mod_H1 <- m_state$M6_full_pre_fieldwork
mod_H2 <- h_state$H2_competence_history
mod_H3 <- h_state$H3_education
mod_H4 <- h_state$H4_gender


# Wald Test für H1 bis H4
global_heterogeneity <- dplyr::bind_rows(
  
  multinom_interaction_wald(
    mod = mod_H1,
    moderator_pattern = "status\\.16",
    moderator_var = "status.16",
    moderator_label = "Prior panel status"
  ),
  
  multinom_interaction_wald(
    mod = mod_H2,
    moderator_pattern = "valid_comp_w14",
    moderator_var = "valid_comp_w14",
    moderator_label = "Valid competence data, Wave 14"
  ),
  
  multinom_interaction_wald(
    mod = mod_H3,
    moderator_pattern = "ISCED_suf",
    moderator_var = "ISCED_suf",
    moderator_label = "Educational attainment (ISCED)"
  ),
  
  multinom_interaction_wald(
    mod = mod_H4,
    moderator_pattern = "Gender_suf",
    moderator_var = "Gender_suf",
    moderator_label = "Gender"
  )
)


# Summary table
global_heterogeneity <- global_heterogeneity %>% 
  dplyr::mutate(
    chi_df = sprintf("%.2f (%d)", chisq, df),
    p_print = case_when(
      p_value < .001 ~ "<.001",
      TRUE ~ sprintf("%.3f", p_value)
    )
  ) %>% 
  dplyr::select(
    moderator,
    subgroup_sizes,
    chi_df,
    p_print
  )

global_heterogeneity



# 3.6 realised pathways in interview start only ----------------------------
# M = 1; Y_1 = 0; Y_2 = 1

# realised interview pathways among M = 1 cases
table(dat_state_analysis$Befragung_fidelity, useNA = "ifany")
table(
  dat_state_analysis$Befragung_fidelity, 
  dat_state_analysis$Befragung_modus_realised,
  useNA = "ifany"
)
table(
  dat_state_analysis$STARTMODE, 
  dat_state_analysis$Befragung_modus_realised,
  useNA = "ifany"
)

table(
  dat_state_analysis$Y_TBT,
  dat_state_analysis$valid
)


# mit oder ohne 4 ISCED unknown Fälle
c(
  gross = dat_state %>% 
    filter(Y_TBT == 0, Y_Befragung == 1) %>% 
    nrow(),
  
  analysis = dat_state_analysis %>% 
    filter(Y_TBT == 0, Y_Befragung == 1) %>% 
    nrow()
)
# jeweisl 610; passt

dat_realised_pathways <- dat_state_analysis %>% 
  
  # M = 1: interview start, but no valid competence-test data
  # filter(
  #   .data[["Y_TBT"]] == 0,
  #   .data[["Y_Befragung"]] == 1
  # )
  filter(Y_TBT == 0, Y_Befragung == 1)


dat_realised_pathways %>% 
  count(
    STARTMODE,
    Befragung_fidelity,
    Befragung_modus_realised
  ) %>% 
  arrange(
    STARTMODE,
    Befragung_fidelity,
    Befragung_modus_realised
  )

dat_realised_pathways <- dat_realised_pathways %>% 
  dplyr::mutate(
    interview_pathway = case_when(
      
      Befragung_fidelity == "Finished as assigned" ~ "Completed in assigned interview mode",
      Befragung_fidelity == "Finished in switched mode" & 
        Befragung_modus_realised == "CAPI" ~ 
        "Completed after switch to CAPI",
      
      Befragung_fidelity == "Finished in switched mode" & 
        Befragung_modus_realised == "CATI" ~ 
        "Completed after switch to CATI",
      
      Befragung_fidelity == "Finished in switched mode" & 
        Befragung_modus_realised == "CALVI" ~ 
        "Completed after switch to CAVI",
      
      Befragung_fidelity == "No interview" &
        grepl(
          "system break-off", 
          Befragung_modus_realised,
          ignore.case = TRUE
        ) ~
        "Started in assigned interview mode, system break-off",
      
      TRUE ~ "Other / unresolved"
    )
  )


table(dat_realised_pathways$interview_pathway, useNA = "ifany")
dat_realised_pathways %>% filter(interview_pathway == "Other / unresolved")


C12_long <- dat_realised_pathways %>% 
  count(
    STARTMODE,
    interview_pathway,
    name = "n",
    .drop = FALSE
  ) %>% 
  group_by(STARTMODE) %>% 
  mutate(
    N_interview_only = sum(n),
    pct = 100 * n / N_interview_only
  ) %>% 
  ungroup()

C12_long %>% 
  group_by(STARTMODE) %>% 
  summarise(
    N_interview_only = sum(n),
    pct_sum = sum(pct),
    .groups = "drop"
  )

C12_long <- C12_long %>% 
  mutate(
    cell = sprintf(
      "%d (%.1f%%)",
      n,
      pct
    )
  )


### Output
# table
# realised interview pathways among respondents with interview
# tab:interview_only_pathways

C12_table <- C12_long %>% 
  select(
    interview_pathway,
    STARTMODE,
    cell
  ) %>% 
  pivot_wider(
    names_from = STARTMODE,
    values_from = cell,
    values_fill = "0"
  )
C12_table


# 4. Figures ------------------------------------------------------------

# 4.1 point-error-bar plot ------------------------------------------------
# Prob Multinomial M6

plot_prob_M6 <- pred_state_status.16_M6 %>%
  mutate(
    STARTMODE = factor(
      STARTMODE,
      levels = c("CAPI", "CALVI", "CAWI-TBT+CATI"),
      labels = c("CBT+\nCAPI", "VBT+\nCAVI", "WBT+\nCATI")
    ),

    status_plot = factor(
      status.16,
      levels = c("Part", "TDO"),
      labels = c("Repeat respondents", "Temporary dropouts")
    ),

    state_plot = factor(
      group,
      levels = c(
        "No valid test / no interview",
        "Interview only",
        "Valid test + interview"),
      labels = c(
        "No valid test / no\ninterview start",
        "Interview start only",
        "Valid test +\ninterview start"
      )
    )
  )


mode_labels <- c(
  "CAPI" = "CBT+\nCAPI",
  "CALVI" = "VBT+\nCAVI",
  "CAWI-TBT+CATI" = "WBT+\nCATI"
)

status_levels <- c(
  "Repeat respondents",
  "Temporary dropouts"
)

position_status <- position_dodge(width = 0.10)

fig_state_prob_M6 <-
  ggplot(
    plot_prob_M6,
    aes(
      x = STARTMODE,
      y = estimate_pct,
      shape = status_plot,
      colour = status_plot,
      group = status_plot,
      linetype = status_plot
    )
  ) +

  geom_line(
    linewidth = 0.45,
    position = position_status
  ) +

  geom_errorbar(
    aes(
      ymin = conf.low_pct,
      ymax = conf.high_pct
    ),
    position = position_status,
    width = 0.07,
    linewidth = 0.40
  ) +

  geom_point(
    size = 2.15,
    stroke = 0.45,
    position = position_status
  ) +

  facet_wrap(
    ~ state_plot,
    nrow = 1#,
    # labeller = as_labeller(outcome_labels)
  ) +

  scale_x_discrete(
    labels = mode_labels
  ) +

  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100), #seq(0, 80, 10),
    labels= function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.0, 0.02))
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
      "Repeat respondents" = col$magenta,
      "Temporary dropouts" = col$lightblue
    )
  ) +

  labs(
    x = "Randomised initial assignment",
    y = "Adjusted predicted probability" #,
    # shape = NULL,
    # colour = NULL,
    # linetype = NULL
  ) +
  guides(
    colour = guide_legend(nrow = 1),
    shape = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  ) +
  theme_article()

fig_state_prob_M6

# abspeichern
ggplot2::ggsave(
  plot  = fig_state_prob_M6,
  filename = file.path(path.save, "Multinom M6 state prob.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 105,
  units = "mm",
  bg = "white",
  dpi = 300
)


# # 4.2 M6 Heterogeneity ----------------------------------------------------
# 
plot_he_M6 <- heterogeneity_state_status.16_M6 %>%
  mutate(
    contrast_plot = factor(
      contrast,
      levels = c(
        "CALVI - CAPI",
        "CAWI-TBT+CATI - CALVI",
        "CAWI-TBT+CATI - CAPI"),
      labels = c(
        "VBT+CAVI \u2212\nCBT+CAPI",
        "WBT+CATI \u2212\nVBT+CAVI",
        "WBT+CATI \u2212\nCBT+CAPI")
    ),

    state_plot = factor(
      group,
      levels = c(
        "No valid test / no interview",
        "Interview only",
        "Valid test + interview"),
      labels = c(
        "No valid test / no\ninterview start",
        "Interview start only",
        "Valid test +\ninterview start"
      )
    )
  )


fig_multinom_het_M6 <-
  ggplot(
    plot_he_M6,
    aes(
      x = contrast_plot,
      y = estimate_pp
    )
  ) +

  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.55
  ) +

  geom_errorbar(
    aes(
      ymin = conf.low_pp,
      ymax = conf.high_pp
    ),
    width = 0.07,
    linewidth = 0.6,
    colour = col$orange
  ) +

  geom_point(
    size = 3,
    shape = 18,
    colour = col$orange
  ) +

  facet_wrap(
    ~ state_plot,
    nrow = 1
  ) +

  scale_y_continuous(
    breaks = seq(-25, 25, 5),
    limits = c(-25, 25),
    labels = function(x) paste0(x)
  ) +

  labs(
    x = "Pairwise randomised assignment contrast",
    y = "Difference in assignment effects\n(TDO - repeat respondets), p.p."
  ) +

  theme_article()

fig_multinom_het_M6

# abspeichern
ggplot2::ggsave(
  plot  = fig_multinom_het_M6,
  filename = file.path(path.save, "Multinom M6 heterogeneity TDO.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 105,
  units = "mm",
  bg = "white",
  dpi = 300
)


# 3. Binary multinomial consistency ---------------------------------------

### Output
# tab:binary_multinomial_consistency

# binary Y_1
pred_pref %>% filter(outcome == "Valid competence test") %>% mutate(estimate_pp = round(estimate * 100, 1))

# multinomial
pred_state_all %>% 
  filter(specification == "M6_full_pre_fieldwork") %>% 
  select(group, STARTMODE, ends_with("pct")) %>% 
  mutate(
    estimate_pct = round(estimate_pct, 1),
    conf.low_pct = round(conf.low_pct, 1),
    conf.high_pct = round(conf.high_pct, 1)
  ) %>% 
  filter(group == "Valid test + interview")

# binary Y_2
pred_pref %>% filter(outcome == "Interview started") %>% mutate(estimate_pp = round(estimate * 100, 1))

# multinomial
pred_state_all %>% 
  filter(specification == "M6_full_pre_fieldwork") %>% 
  select(group, STARTMODE, ends_with("pct")) %>% 
  mutate(
    estimate_pct = round(estimate_pct, 1),
    conf.low_pct = round(conf.low_pct, 1),
    conf.high_pct = round(conf.high_pct, 1)
  ) %>% 
  filter(group %in% c("Valid test + interview", "Interview only")) %>% 
  group_by(STARTMODE) %>% 
  summarise(
    M1_M2 = sum(estimate_pct),
    .groups = "drop"
  )
