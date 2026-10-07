# ===
# Descriptive analyses
# Participation and Selectivity in Web-Based Cognitive Testing and Surveying:
# Evidence from a Randomized Mode Experiment
#
# Requires `00_run_analysis.R`.
# Restricted individual-level data are not included.
# ===

# DESCRIPTIVE RESULTS ------------------------------------------------------

# Functions used below are defined in R/00_functions.R.

# Functions ----------------------------------------------------------------


# Tabelle: fieldwork_combined ---------------------------------------------

# Output: Table
# Title: Fieldwork participation, data production, implementation fidelity, and timing
# LaTeX label: tab:fieldwork_combined

# A. gross sample and participation
table(AuswertungB160.deskriptiv$STARTMODE) %>% addmargins()

table(AuswertungB160.deskriptiv$Y_TBT & AuswertungB160.deskriptiv$valid == TRUE, 
      AuswertungB160.deskriptiv$STARTMODE) %>% addmargins()
table(AuswertungB160.deskriptiv$Y_TBT & AuswertungB160.deskriptiv$valid == TRUE, 
      AuswertungB160.deskriptiv$STARTMODE) %>% prop.table(margin = c(2))*100

table(AuswertungB160.deskriptiv$Y_Befragung, 
      AuswertungB160.deskriptiv$STARTMODE) %>% addmargins()
table(AuswertungB160.deskriptiv$Y_Befragung, 
      AuswertungB160.deskriptiv$STARTMODE) %>% prop.table(margin = c(2))*100

table(AuswertungB160.deskriptiv$Y_CAWI_nachgelagert, 
      AuswertungB160.deskriptiv$STARTMODE) %>% addmargins()
table(AuswertungB160.deskriptiv$Y_Befragung %in% 1 & AuswertungB160.deskriptiv$Y_CAWI_nachgelagert,
      AuswertungB160.deskriptiv$STARTMODE) %>% addmargins()
table(AuswertungB160.deskriptiv$Y_Befragung %in% 1 & AuswertungB160.deskriptiv$Y_CAWI_nachgelagert,
      AuswertungB160.deskriptiv$STARTMODE) %>% prop.table(margin = c(2))*100

# Teilnahme an Testung, Befragung und nachgelagertem CAWI
table(AuswertungB160.deskriptiv$Y_TBT == 1 & AuswertungB160.deskriptiv$valid == TRUE &
        AuswertungB160.deskriptiv$Y_Befragung == 1 & 
        AuswertungB160.deskriptiv$Y_CAWI_nachgelagert == 1,
      AuswertungB160.deskriptiv$STARTMODE) %>% addmargins()

table(AuswertungB160.deskriptiv$Y_TBT == 1 & AuswertungB160.deskriptiv$valid == TRUE &
        AuswertungB160.deskriptiv$Y_Befragung == 1 & 
        AuswertungB160.deskriptiv$Y_CAWI_nachgelagert == 1,
      AuswertungB160.deskriptiv$STARTMODE) %>%  prop.table(margin = c(2))




# B. Realisied testing-interview  states
# No interview
table(AuswertungB160.deskriptiv$Y_TBT == 0 & AuswertungB160.deskriptiv$Y_Befragung == 0,
      AuswertungB160.deskriptiv$STARTMODE) %>% addmargins()
table(AuswertungB160.deskriptiv$Y_TBT == 0 & AuswertungB160.deskriptiv$Y_Befragung == 0,
      AuswertungB160.deskriptiv$STARTMODE) %>% prop.table(margin=c(2))*100

# Interview only
table(AuswertungB160.deskriptiv$Y_TBT == 0 & AuswertungB160.deskriptiv$Y_Befragung == 1,
      AuswertungB160.deskriptiv$STARTMODE) %>% addmargins()
table(AuswertungB160.deskriptiv$Y_TBT == 0 & AuswertungB160.deskriptiv$Y_Befragung == 1,
      AuswertungB160.deskriptiv$STARTMODE) %>% prop.table(margin=c(2)) *100

# complete analytical data
table(AuswertungB160.deskriptiv$Y_TBT == 1 & AuswertungB160.deskriptiv$Y_Befragung == 1,
      AuswertungB160.deskriptiv$STARTMODE) %>% addmargins()
table(AuswertungB160.deskriptiv$Y_TBT == 1 & AuswertungB160.deskriptiv$Y_Befragung == 1,
      AuswertungB160.deskriptiv$STARTMODE) %>% prop.table(margin=c(2)) *100

# Test component nonresponse among interview starters
# händisch


# C implementation fidelity: competence testing
# valid test in assigned test mode
table(AuswertungB160.deskriptiv$TBT_modus_as_assigned,
      AuswertungB160.deskriptiv$STARTMODE) %>% addmargins()
table(AuswertungB160.deskriptiv$TBT_modus_as_assigned,
      AuswertungB160.deskriptiv$STARTMODE) %>% prop.table(margin=c(2)) *100


# valid test after mode switch

xtabs(~ TBT_modus_ende + STARTMODE, 
      data = AuswertungB160.deskriptiv,
      subset = AuswertungB160.deskriptiv$valid == TRUE) %>% addmargins()


# D. implentation fidelity: interview
# completed in assigned interview mode
table(AuswertungB160.deskriptiv$Y_Befragung,
      AuswertungB160.deskriptiv$STARTMODE) %>% addmargins()
table(AuswertungB160.deskriptiv$Y_Befragung,
      AuswertungB160.deskriptiv$STARTMODE) %>% prop.table(margin=c(2)) *100

table(AuswertungB160.deskriptiv$Befragung_fidelity,
      AuswertungB160.deskriptiv$STARTMODE) %>% addmargins()
table(AuswertungB160.deskriptiv$Befragung_fidelity,
      AuswertungB160.deskriptiv$STARTMODE) %>% prop.table(margin=c(2))* 100

AuswertungB160.deskriptiv %>% 
  filter(Y_TBT == 0, Y_Befragung == 1) %>% 
  count(STARTMODE, Befragung_fidelity, .drop = FALSE)

AuswertungB160.deskriptiv %>% 
  filter(Y_TBT == 0, Y_Befragung == 1) %>% 
  filter(is.na(Befragung_fidelity)) %>% 
  select(ID_t, STARTMODE, Befragung_fidelity, everything())

# E. timing among participations

# duration competence-test
AuswertungB160.deskriptiv %>% 
  filter(Y_TBT == 1) %>% 
  # group_by(STARTMODE) %>% 
  summarise(
    n = n(),
    mean = mean(tbtdauer, na.rm = TRUE),
    sd = sd(tbtdauer, na.rm = TRUE),
    median = median(tbtdauer, na.rm = TRUE),
    IQR = IQR(tbtdauer, na.rm = TRUE),
    .groups = "drop"
  )

AuswertungB160.deskriptiv %>%
  filter(Y_TBT == 1) %>% 
  group_by(STARTMODE) %>% 
  summarise(
    n = n(),
    mean = mean(tbtdauer, na.rm = TRUE),
    sd = sd(tbtdauer, na.rm = TRUE),
    median = median(tbtdauer, na.rm = TRUE),
    IQR = IQR(tbtdauer, na.rm = TRUE),
    .groups = "drop"
  )



# duration interview
# Kernfragebogendauer
AuswertungB160.deskriptiv %>% 
  filter(Y_Befragung == 1) %>% 
  group_by(STARTMODE) %>% 
  summarise(
    n = n(),
    mean = mean(INTDAUK, na.rm = TRUE),
    sd = sd(INTDAUK, na.rm = TRUE),
    median = median(INTDAUK, na.rm = TRUE),
    IQR = IQR(INTDAUK, na.rm = TRUE),
    .groups = "drop"
  )

AuswertungB160.deskriptiv %>%
  filter(Y_Befragung == 1) %>% 
  summarise(
    n = n(),
    mean = mean(INTDAUK, na.rm = TRUE),
    sd = sd(INTDAUK, na.rm = TRUE),
    median = median(INTDAUK, na.rm = TRUE),
    IQR = IQR(INTDAUK, na.rm = TRUE),
    .groups = "drop"
  )



# duration CAWI_nachgelagert
AuswertungB160.deskriptiv %>% 
  filter(Y_CAWI_nachgelagert == 1) %>% 
  group_by(STARTMODE) %>%
  summarise(
    n = n(),
    mean = mean(CAWI_nachgelagert_duration_minutes, na.rm = TRUE),
    sd = sd(CAWI_nachgelagert_duration_minutes, na.rm = TRUE),
    median = median(CAWI_nachgelagert_duration_minutes, na.rm = TRUE),
    IQR = IQR(CAWI_nachgelagert_duration_minutes, na.rm = TRUE),
    .groups = "drop"
  )


# lag between testung und Befragung
# Differenzen berechnen zwischen 
# Ende TBT und Anfang Befragung
AuswertungB160.deskriptiv$interview_lag <- as.numeric(difftime(as.POSIXct(AuswertungB160$FOKDAT,
                                                               format = "%d.%m.%Y %H:%M",
                                                               tz = "Europe/Berlin"), 
                                                    AuswertungB160$tbt_finishdate_comp,  units = "days"))
# AuswertungB160$interview_lag[is.na(AuswertungB160$interview_lag)] <- 0
# table(AuswertungB160$interview_lag, exclude = NULL)


AuswertungB160.deskriptiv %>% 
  filter(Y_TBT == 1) %>% 
  group_by(STARTMODE) %>%
  summarise(
    n = n(),
    mean = mean(interview_lag, na.rm = TRUE),
    sd = sd(interview_lag, na.rm = TRUE),
    median = median(interview_lag, na.rm = TRUE),
    IQR = IQR(interview_lag, na.rm = TRUE),
    .groups = "drop"
  )



# Tabelle: randomisation balance ------------------------------------------

balance_long <- AuswertungB160.deskriptiv %>% 
  select(STARTMODE, all_of(vars$var)) %>% 
  pivot_longer(
    cols = all_of(vars$var),
    names_to = "var",
    values_to = "value",
    names_transform = list(var = ~factor(.x, levels = vars$var))
  ) %>% 
  group_by(var, STARTMODE) %>%
  summarise(
    n = sum(value == 1, na.rm = TRUE),
    percent = 100 * mean(value == 1, na.rm = TRUE),
    .groups = "drop"
  )


balance_totals <- AuswertungB160.deskriptiv %>% 
  select(STARTMODE, all_of(vars$var)) %>%
  pivot_longer(
    cols = all_of(vars$var),
    names_to = "var",
    values_to = "value",
    names_transform = list(var = ~factor(.x, levels = vars$var))
  ) %>% 
  group_by(var) %>% 
  summarise(
    Total_n = sum(value == 1, na.rm = TRUE),
    Total_pct = 100 * mean(value == 1, na.rm = TRUE),
    .groups = "drop"
  )


balance_wide <- balance_long %>% 
  select(var, STARTMODE, n, percent) %>% 
  pivot_wider(
    names_from = STARTMODE,
    values_from = c(n, percent),
    names_glue = "{STARTMODE}_{.value}"
  )

balance_wide <- balance_wide %>% 
  left_join(balance_totals, by = "var")

pct_cols <- grep("_percent$", names(balance_wide), value = TRUE)

balance_wide <- balance_wide %>%
  rowwise() %>% 
  mutate(
    Std.diff = {
      p <- c_across(all_of(pct_cols)) / 100
      
      max(
        combn(
          p, 2, FUN = \(x) abs(std_diff_binary(x[1], x[2]))
        ),
        na.rm = TRUE
      )
    }
  ) %>% 
  ungroup()


# Output: Table
# Title: Baseline characteristics by randomised initial assignment
# LaTeX label: tab:deskriptives_startmodi
balance_wide %>% 
  transmute(
    var,
    `CAPI Abs` = CAPI_n,
    `CAPI %` = round(CAPI_percent, 1),
    `CALVI Abs` = CALVI_n,
    `CALVI %` = round(CALVI_percent, 1),
    `CAWI Abs` = `CAWI-TBT+CATI_n`,
    `CAWI %` = round(`CAWI-TBT+CATI_percent`, 1),
    `Total Abs` = Total_n,
    `Total %` = round(Total_pct, 1),
    `Std.diff` = round(Std.diff, 3)
    ) %>% 
  kbl(
    # format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    caption = "Baseline composition of the gross target sample by randomised initial assignment",
    label = "tab:balance-assignment",
    align = c("lrrrr")
  ) %>% 
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general = paste(
      "Entries are percentages unless otherwise stated.",
      "Rows are defined by randomised initial assignment.",
      "Standardized differences compare each pair of assignment groups;",
      "the table reports the maximum absolute pairwise standardized difference."
    ),
    general_title = "Note:",
    threeparttable = TRUE,
    escape = FALSE
  )


# table: baseline characteristics by randomised initial assignment

# Tabelle: stage-specific selectivity -------------------------------------

selectivity_one <- function(data, varname) {
  
  gross <- pct(data[[varname]])
  
  valid_test <- pct(data %>% filter(Y_TBT == 1) %>% pull(all_of(varname)))
  interview <- pct(data %>% filter(Y_Befragung == 1) %>% pull(all_of(varname)))
  
  interview_base <- pct(data %>% filter(Y_Befragung == 1) %>% pull(all_of(varname)))
  followup <- pct(data %>% filter(Y_Befragung == 1, Y_CAWI_nachgelagert == 1) %>% pull(all_of(varname)))
  
  tibble(
    var = varname,
    gross = gross,
    valid_test_delta = valid_test - gross,
    interview_delta = interview - gross,
    followup_delta = followup - interview_base
  )
}

# selectivity_one(AuswertungB160.deskriptiv, vars$var)

table3_selectivity <- map_dfr(vars$var, ~selectivity_one(AuswertungB160.deskriptiv, .x)) %>% 
  left_join(vars, by = "var") %>% 
  transmute(
    "Characteristics" = label,
    `Gross Sample (%)` = fmt1(gross),
    `Valid test (delta pp.)` = sprintf("%+.1f", valid_test_delta),
    `Interview started (delta pp.)` = sprintf("%+.1f", interview_delta),
    `Follow-up CAWI (delta pp.)` = sprintf("%+.1f", followup_delta)
  )


# Output: Table
# Title: Changes in sample composition across fieldwork stages
# LaTeX label: tab:composition_descriptive_selectivity
# LaTeX label: tab:composition_descriptive_selectivity_full
table3_selectivity %>% 
  kbl(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    caption = "Stage-specific selectivity of realised samples",
    label = "tab:stage-selectivity",
    align = c("lrrrr")
  ) %>% 
  kable_styling(latex_options = c("hold_position")) %>% 
  save_kable(file = file.path(path.save, "stage-selectivity.tex"))

table3_selectivity %>% print(n=Inf)



# Tabelle: Composition effect across stages -------------------------------

AuswertungB160.comp <- AuswertungB160.deskriptiv %>% 
  dplyr::mutate(
    stage_valid_test = Y_TBT == 1
  )

get_composition(AuswertungB160, status.16, value = "TDO", label = "Temporary dropout")


# Output: Tabelle
# Kombinieren
comp_table <- dplyr::bind_rows(
  get_composition(AuswertungB160.deskriptiv, status.16, value = "TDO", label = "Temporary dropout"),
  get_composition(AuswertungB160.deskriptiv, CASMIN_lower, value = 1, label = "Lower education (CASMIN 1abc2ab")
  
)




# Tabelle: Stufe und STARTMODE --------------------------------------------

vars_short <- tibble::tribble(
  ~var, ~label,
  "female", "Female",
  "age_lower", "Older age (1994 and earlier)",   
  "age", "Older age (1995)",                 
  "age_higher", "Younger age (1996 and later)",
  "migration_background_no",      "Migration background (no)",        
  "migration_background_yes",     "Migration background (yes)",       
  "migration_background_unknown", "Migration background (unknown)",  
  "CASMIN_lower",                 "Lower education (CASMIN 1abc2ab)", 
  "CASMIN_middle",                "Middle educaton (CASMIN 2c)",      
  "CASMIN_higher",                "Higher education (CASMIN 3ab)",    
  "CASMIN_unknown",               "No education (CASMIN unknown)",   
  "temporary_dropout", "Temporary dropout",
  "prior_participation_low", "Less than 12 prior participations",
  "prior_participation_middle", "13--15 prior participations",
  "prior_participation_high", "16--17 prior participations"
)

vars %>% print(n = Inf)


stage_long <- AuswertungB160.deskriptiv %>% 
  select(
    CASEID,
    STARTMODE,
    Y_TBT,
    Y_Befragung,
    Y_CAWI_nachgelagert,
    all_of(vars_short$var)
  ) %>% 
  pivot_longer(
    cols = c(Y_TBT, Y_Befragung, Y_CAWI_nachgelagert),
    names_to = "stage",
    values_to = "participated"
  ) %>% 
  mutate(
    stage = recode(
      stage,
      Y_TBT = "Valid test",
      Y_Befragung = "Interview started",
      Y_CAWI_nachgelagert = "Follow-up CAWI"
    ),
    stage = factor(
      stage,
      levels = c("Valid test", "Interview started", "Follow-up CAWI")
    )
  ) %>% 
  filter(participated == 1)


table4_composition <- stage_long %>% 
  select(stage, STARTMODE, all_of(vars_short$var)) %>% 
  pivot_longer(
    cols = all_of(vars_short$var),
    names_to = "var",
    values_to = "value"
  ) %>% 
  group_by(stage, STARTMODE, var) %>% 
  summarise(
    percent = pct(value),
    n_stage = n(),
    .groups = "drop"
  ) %>% 
  left_join(vars_short, by = "var") %>% 
  select(STARTMODE, stage, label, percent) %>% 
  pivot_wider(
    names_from = label,
    values_from = percent
  ) %>% 
  arrange(STARTMODE, stage) %>% 
  rename(
    `Assigned initial mode` = STARTMODE,
    `Stage` = stage
  )


# Output: Tabelle
table4_composition %>% 
  kbl(
    # format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    caption = "Selected respondent characteristics by randomised initial assignment and fieldwork stage",
    label = "tab:composition-by-stage-mode",
    align = c("llrrrrr")
  ) %>% 
  kable_styling(latex_options = c("hold_position")) %>% 
  collapse_rows(
    columns = 1,
    valign = "top"
  ) %>% 





# Tabelle: Merkmalsausprägung Startmodus ----------------------------------

# DIESE ERGEBNISSE sind in tab:deskriptives_startmodi


table(AuswertungB160$SEX_A, AuswertungB160$STARTMODE) %>% addmargins(c(1,2))
table(AuswertungB160$SEX_A, AuswertungB160$STARTMODE) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

table(AuswertungB160$Alter2, exclude = NULL)
table(AuswertungB160$Alter2, AuswertungB160$STARTMODE) %>% addmargins(c(1,2))
table(AuswertungB160$Alter2, AuswertungB160$STARTMODE) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

table(AuswertungB160$Region, AuswertungB160$STARTMODE) %>% addmargins(c(1,2))
table(AuswertungB160$Region, AuswertungB160$STARTMODE) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

table(AuswertungB160$Herkunftsland, AuswertungB160$STARTMODE) %>% addmargins(c(1,2))
table(AuswertungB160$Herkunftsland, AuswertungB160$STARTMODE) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

table(AuswertungB160$migBG_suf, AuswertungB160$STARTMODE) %>% addmargins(c(1,2))
table(AuswertungB160$migBG_suf, AuswertungB160$STARTMODE) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

table(AuswertungB160$CASMIN, AuswertungB160$STARTMODE) %>% addmargins(c(1,2))
table(AuswertungB160$CASMIN, AuswertungB160$STARTMODE) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

table(AuswertungB160$ISCED_suf, AuswertungB160$STARTMODE) %>% addmargins(c(1,2))
table(AuswertungB160$ISCED_suf, AuswertungB160$STARTMODE) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

table(AuswertungB160$Erwerbstätigkeit_aktuell, AuswertungB160$STARTMODE) %>% addmargins(c(1,2))
table(AuswertungB160$Erwerbstätigkeit_aktuell, AuswertungB160$STARTMODE) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

table(AuswertungB160$Berufliche_Stellung, AuswertungB160$STARTMODE) %>% addmargins(c(1,2))
table(AuswertungB160$Berufliche_Stellung, AuswertungB160$STARTMODE) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100


table(AuswertungB160$TNt16, AuswertungB160$STARTMODE) %>% addmargins(c(1,2))
table(AuswertungB160$TNt16, AuswertungB160$STARTMODE) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

table(AuswertungB160$part_hist, AuswertungB160$STARTMODE) %>% addmargins(c(1,2))
table(AuswertungB160$part_hist, AuswertungB160$STARTMODE) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100



# Tabelle: Merkmalsausprägung Testung ----------------------------------

AuswertungB160$mode_start <- factor(AuswertungB160$mode_start, levels = c("CAPI", "CALVI", "CAWI"))
GESS_response_mode


xtabs(~ SEX_A + GESS_response_mode,
      data = AuswertungB160) %>% addmargins(c(1,2))

table(AuswertungB160$mode_end, AuswertungB160$GESS_response_mode, exclude = NULL)
table(AuswertungB160$mode_end, AuswertungB160$Y_TBT, exclude = NULL)
table(AuswertungB160$GESS_response_mode, AuswertungB160$Y_TBT, exclude = NULL)
table(AuswertungB160$valid, AuswertungB160$Y_TBT, exclude = NULL)
table(AuswertungB160$mode_end, AuswertungB160$valid, exclude = NULL)
table(AuswertungB160$STARTMODE, AuswertungB160$valid, exclude = NULL) %>% addmargins(c(1,2))

AuswertungB160$mode_start <- factor(AuswertungB160$mode_start,
                                    levels = c("CAPI",
                                               "CALVI",
                                               "CAWI"))

xtabs(~ SEX_A + mode_start, subset = Y_TBT %in% 1, #& mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ SEX_A + mode_start, subset = Y_TBT %in% 1,# & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

xtabs(~ Alter2 + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Alter2 + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100


xtabs(~ Region + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Region + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100


xtabs(~ Herkunftsland + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Herkunftsland + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100

xtabs(~ migBG_suf + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ migBG_suf + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100

xtabs(~ CASMIN + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ CASMIN + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100

xtabs(~ ISCED_suf + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ ISCED_suf + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100

xtabs(~ Erwerbstätigkeit_aktuell + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Erwerbstätigkeit_aktuell + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100


xtabs(~ Berufliche_Stellung + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Berufliche_Stellung + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100




xtabs(~ TNt16 + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ TNt16 + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100

xtabs(~ part_hist + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ part_hist + mode_start, subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100


# Tabelle: Merkmalsausprägung Befragung ----------------------------------

AuswertungB160$Befragung_valid_2 <- factor(AuswertungB160$Befragung_valid, levels = c("CAPI", "CALVI", "CATI"))

table(AuswertungB160$Befragung_valid_2, AuswertungB160$Y_Befragung, exclude = NULL)
table(AuswertungB160$Befragung_started, AuswertungB160$Y_Befragung, exclude = NULL)



xtabs(~ SEX_A + Befragung_started, # subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ SEX_A + Befragung_started, #subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

xtabs(~ Alter2 + Befragung_started, # subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Alter2 + Befragung_started, #subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100


xtabs(~ Region + Befragung_started, #subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Region + Befragung_started, #subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

xtabs(~ Herkunftsland + Befragung_started, #subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Herkunftsland + Befragung_started, #subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100

xtabs(~ migBG_suf + Befragung_started, #subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ migBG_suf + Befragung_started, #subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100

xtabs(~ CASMIN + Befragung_started, # subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ CASMIN + Befragung_started, # subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100

xtabs(~ ISCED_suf + Befragung_started, # subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ ISCED_suf + Befragung_started, # subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100

xtabs(~ Erwerbstätigkeit_aktuell + Befragung_started, #subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Erwerbstätigkeit_aktuell + Befragung_started, #subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100


xtabs(~ Berufliche_Stellung + Befragung_started, #subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Berufliche_Stellung + Befragung_started, #subset = Y_TBT %in% 1, # & mode_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100




xtabs(~ TNt16 + Befragung_started, # subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ TNt16 + Befragung_started, # subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100

xtabs(~ part_hist + Befragung_started, #subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ part_hist + Befragung_started, #subset = Befragung_switch %in% FALSE,
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100


# Tabelle: Merkmalsausprägung Nachgelagertes CAWI ----------------------------------


xtabs(~ SEX_A + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ SEX_A + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

xtabs(~ Alter2 + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Alter2 + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

xtabs(~ Region + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Region + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2)) %>% round(3)*100

xtabs(~ Herkunftsland + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Herkunftsland + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100

xtabs(~ migBG_suf + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ migBG_suf + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100

xtabs(~ CASMIN + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ CASMIN + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100

xtabs(~ ISCED_suf + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ ISCED_suf + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100


xtabs(~ Erwerbstätigkeit_aktuell + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Erwerbstätigkeit_aktuell + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100


xtabs(~ Berufliche_Stellung + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ Berufliche_Stellung + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100


xtabs(~ TNt16 + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ TNt16 + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100

xtabs(~ part_hist + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(1,2))
xtabs(~ part_hist + STARTMODE, subset = Befragung_switch %in% FALSE & CAWI_nachgelagert %in% "CAWI nachgelagert teilgenommen",
      data = AuswertungB160) %>% addmargins(c(2)) %>% prop.table(margin = c(2))  %>% round(3)*100
