# ===
# Helper functions
# Participation and Selectivity in Web-Based Cognitive Testing and Surveying:
# Evidence from a Randomized Mode Experiment
#
# Public replication release.
# Restricted microdata are not included.
# ===


##
`%nin%` <- function(x, table) !(x %in% table)


##
process_variable_valid <- function(
    data, 
    id_var, 
    time_var, 
    target_var, 
    new_var, 
    missing_codes,
    max_time = 16) {
  
  
  # IDs erhalten, auch wenn für eine Person überhaupt kein gültiger Wert vorliegt
  ids <- data %>% 
    dplyr::distinct(.data[[id_var]])
  
  latest_valid <- data %>% 
    dplyr::mutate(
      "{target_var}" := replace(
        .data[[target_var]],
        .data[[target_var]] %in% missing_codes,
        NA
      )
    ) %>% 
    # ausschließlich Informationen vor Wave 17 verwenden
    # dplyr::filter(.data[[time_var]] <= max_time) %>% 
    # mur gültige Werte kommen für "latest valid" infrage
    dplyr::filter(!is.na(.data[[target_var]])) %>% 
    dplyr::group_by(.data[[id_var]]) %>% 
    dplyr::arrange(dplyr::desc(.data[[time_var]]), .by_group = TRUE) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::transmute(
      "{id_var}" := .data[[id_var]],
      "{new_var}" := .data[[target_var]]
    )
  
  ids %>% 
    dplyr::left_join(latest_valid, by = id_var)
}

##
pct <- function(x) {
  mean(x %in% 1, na.rm = TRUE) * 100
}


##
fmt1 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.1f", x))
}


##
std_diff_binary <- function(p1, p2) {
  p_pool <- (p1 + p2) / 2
  (p1 - p2) / sqrt(p_pool * (1 - p_pool))
}


##
tidy_robust <- function(model, vcov_type = "HC2", conf_level = 0.95) {
  
  V <- sandwich::vcovHC(model, type = vcov_type)
  ct <- lmtest::coeftest(model, vcov. = V)
  
  out <- broom::tidy(ct) %>% 
    rename(
      estimate = estimate,
      std.error = std.error,
      statistic = statistic,
      p.value = p.value
    )
  
  z <- qnorm(1 - (1 - conf_level) / 2)
  
  out %>% 
    mutate(
      conf.low = estimate - z * std.error,
      conf.high = estimate + z * std.error,
      odds_ratio = exp(estimate),
      or_low = exp(conf.low),
      or_high = exp(conf.high)
    )
}


##
coef_table_models <- function(models, outcome_label) {
  imap_dfr(
    models,
    ~ tidy_robust(.x) %>% 
      mutate(
        model = .y,
        outcome = outcome_label
      ),
    .id = NULL
  ) %>% 
    select(outcome, model, term, estimate, std.error, statistic, 
           p.value, conf.low, conf.high, odds_ratio, or_low, or_high)
}


##
info_criteria <- function(models, outcome_label) {
  purrr::imap_dfr(
    models,
    ~ {
      ll <- stats::logLik(.x)
      tibble::tibble(
        outcome = outcome_label,
        model = .y,
        nobs = stats::nobs(.x),
        df = attr(ll, "df"),
        logLik = as.numeric(ll),
        AIC = stats::AIC(.x),
        BIC = stats::BIC(.x)
      )
    }
  ) %>% 
    dplyr::group_by(outcome) %>% 
    dplyr::mutate(
      delta_AIC = AIC - min(AIC, na.rm = TRUE),
      delta_BIC = BIC - min(BIC, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup()
}


##
lr_tests <- function(models, outcome_label, lr_sequence) {
  
  purrr::map2_dfr(
    lr_sequence$restricted,
    lr_sequence$full,
    ~ {
      m_restricted <- models[[.x]]
      m_full <- models[[.y]]
      
      if (is.null(m_restricted) || is.null(m_full)) {
        return(
          tibble::tibble(
            outcome = outcome_label,
            restricted = .x,
            full = .y,
            nobs = NA_integer_,
            LR_statistic = NA_real_,
            df = NA_real_,
            p_value = NA_real_,
            note = "Model not found"
          )
        )
      }
      
      if (stats::nobs(m_restricted) != stats::nobs(m_full)) {
        return(
          tibble::tibble(
            outcome = outcome_label,
            restricted = .x,
            full = .y,
            nobs = NA_integer_,
            LR_statistic = NA_real_,
            df = NA_real_,
            p_value = NA_real_,
            note = "Different estimation samples"
          )
        )
      }
      
      test <- lmtest::lrtest(m_restricted, m_full)
      
      tibble::tibble(
        outcome = outcome_label,
        restricted = .x,
        full = .y,
        nobs = stats::nobs(m_restricted),
        LR_statistic = test$Chisq[2],
        df = test$Df[2],
        p_value = test$`Pr(>Chisq)`[2],
        note = NA_character_
      )
    }
  )
}


##
get_mode_predictions <- function(model,
                                 model_name,
                                 outcome_label,
                                 assignment_var = "STARTMODE",
                                 vcov_type = "HC2") {
  marginaleffects::avg_predictions(
    model,
    variables = assignment_var,
    type = "response",
    vcov = sandwich::vcovHC(model, type = vcov_type)
  ) %>% 
    as.data.frame() %>% 
    dplyr::mutate(
      outcome = outcome_label,
      model = model_name
    )
}


##
get_cf_predictions <- function(model,
                               outcome_label,
                               assignment_var = "STARTMODE",
                               group_var = NULL,
                               vcov_type = "HC2") {
  
  mf <- stats::model.frame(model)
  
  if (!(assignment_var %in% names(mf))) {
    stop("Variable ", assignment_var, " not found in mode.frame(model).")
  }
  
  assignment_levels <- levels(mf[[assignment_var]])
  
  cf_data <- purrr::map_dfr(
    assignment_levels,
    function(a) {
      tmp <- mf
      tmp[[assignment_var]] <- factor(a, levels = assignment_levels)
      tmp[[paste0(assignment_var, "_cf")]] <- a
      tmp
    }
  )
  
  by_vars <- paste0(assignment_var, "_cf")
  
  if (!is.null(group_var)) {
    if (!(group_var %in% names(cf_data))) {
      stop("Grouping variable ", group_var, " not found in counterfactual data.")
    }
    by_vars <- c(by_vars, group_var)
  }
  
  marginaleffects::avg_predictions(
    model,
    newdata = cf_data,
    by = by_vars,
    type = "response",
    vcov = sandwich::vcovHC(model, type = vcov_type)
  ) %>% 
    as.data.frame() %>% 
    dplyr::mutate(outcome = outcome_label)
}


##
get_all_pairwise_contrasts <- function(
    model,
    outcome_label,
    assignment_var = "STARTMODE",
    vcov_type = "HC2",
    conf_level = 0.95,
    equivalence_margin_pp = 5
) {
  
  model_data <- stats::model.frame(model)
  
  if (!assignment_var %in% names(model_data)) {
    stop(
      sprintf(
        "Variable '%s' ist nicht im Modell enthalten.",
        assignment_var
      )
    )
  }
  
  equivalence_margin <- equivalence_margin_pp / 100
  
  mode_levels <- levels(
    droplevels(factor(model_data[[assignment_var]]))
  )
  
  if (length(mode_levels) < 2L) {
    stop("Die Zuweisungsvariable besitzt weniger als zwei beobachtete Stufen.")
  }
  
  # dynamische spezifikation,
  variables_spec <- stats::setNames(
    list("pairwise"),
    assignment_var
  )
  
  out <- marginaleffects::avg_comparisons(
    model,
    variables = variables_spec,
    type = "response",
    vcov = sandwich::vcovHC(
      model,
      type = vcov_type
    ),
    conf_level = conf_level,
    equivalence = c(
      -equivalence_margin,
      equivalence_margin
    )
  ) %>% 
    as.data.frame() %>% 
    dplyr::mutate(
      outcome = outcome_label,
      estimate_pp = 100 * estimate,
      
      # 95 CI for the main results table
      conf_low_pp = 100 * conf.low,
      conf_high_pp = 100 * conf.high,
      
      # 90% corresponding to TOST at alpha = 0.05
      conf_low90_pp  = 100 * (estimate - stats::qnorm(0.95) * std.error),
      conf_high90_pp = 100 * (estimate + stats::qnorm(0.95) * std.error),
      
      equivalence_margin_pp = equivalence_margin_pp,
      equivalence_result = dplyr::if_else(
        p.value.equiv < 0.05,
        "Established",
        "Not established"
      )
    )
  
  
  # für die unterschiedlichen 3, 5, 7.5
  out <- out %>% 
    dplyr::mutate(
      equiv_3 = dplyr::if_else(
        conf_low90_pp > -3 & conf_high90_pp < 3,
        "Established",
        "Not established"
      ),
      equiv_5 = dplyr::if_else(
        conf_low90_pp > -5 & conf_high90_pp < 5,
        "Established",
        "Not established"
      ),
      equiv_7_5 = dplyr::if_else(
        conf_low90_pp > -7.5 & conf_high90_pp < 7.5,
        "Established",
        "Not established"
      )
    )
  
  
  
  expected_n <- choose(length(mode_levels), 2)
  
  if (nrow(out) != expected_n) {
    warning(
      sprintf(
        "Erwartet wurden %d paarweise Kontraste, ausgegeben wurden %d. Beobachtete Faktorstufen: %s.",
        expected_n,
        nrow(out),
        paste(mode_levels, collapse = ", ")
      )
    )
  }
  
  out
}


##
format_ci <- function(
    estimate,
    conf_low,
    conf_high
) {
  sprintf(
    "%.1f [%.1f, %.1f]",
    estimate, 
    conf_low,
    conf_high
  )
}


##
format_p_value <- function(p) {
  dplyr::case_when(
    is.na(p) ~"",
    p < 0.001 ~ "<.001",
    TRUE ~ sub("^0", "", sprintf("%.3f", p))
  )
}


##
get_one_panel_status_contrast <- function(
    model,
    outcome_label,
    contrast_label,
    pair_values,
    group_var,
    reference_group,
    comparison_group,
    assignment_var = "STARTMODE",
    vcov_type = "HC2"
) {
  
  vcov_matrix <- sandwich::vcovHC(
    model,
    type = vcov_type
  )    
  
  variables_spec <- stats::setNames(
    list(pair_values),
    assignment_var
  )
  
  # Gruppenspezifische AMEs
  group_effects_obj <- marginaleffects::avg_comparisons(
    model,
    variables = variables_spec,
    by = group_var,
    type = "response",
    vcov = vcov_matrix
  ) 
  
  group_effects <- as.data.frame(group_effects_obj)
  
  if (nrow(group_effects) != 2L) {
    stop(
      "Expected exactly two groups for '", group_var,
      "', but obtained ", nrow(group_effects), "."
    )
  }
  
  reference_row <- match(
    reference_group,
    group_effects[[group_var]]
  )
  
  comparison_row <- match(
    comparison_group,
    group_effects[[group_var]]
  )
  
  if (is.na(reference_row) || is.na (comparison_row)) {
    stop(
      "Expected group labels not found. Observed values: ",
      paste(group_effects[[group_var]], collapse = ", ")
    )
  }
  
  # Differenz der gruppenspezifischen AMEs:
  # comparison group minus reference group
  hypothesis_string <- sprintf(
    "b%d - b%d = 0",
    comparison_row,
    reference_row
  )
  
  heterogeneity <- marginaleffects::avg_comparisons(
    model,
    variables = variables_spec,
    by = group_var,
    type = "response",
    vcov = vcov_matrix,
    hypothesis = hypothesis_string
  ) %>% 
    as.data.frame()
  
  tibble::tibble(
    outcome = outcome_label,
    moderator = group_var,
    contrast = contrast_label,
    reference_group = reference_group,
    reference_estimate_pp = 100 * group_effects$estimate[reference_row],
    reference_conf_low_pp = 100 * group_effects$conf.low[reference_row],
    reference_conf_high_pp = 100 * group_effects$conf.high[reference_row],
    
    comparison_group = comparison_group,
    comparison_estimate_pp = 100 * group_effects$estimate[comparison_row],
    comparison_conf_low_pp = 100 * group_effects$conf.low[comparison_row],
    comparison_conf_high_pp = 100 * group_effects$conf.high[comparison_row],
    
    difference_estimate_pp = 100 * heterogeneity$estimate[1],
    difference_conf_low_pp = 100 * heterogeneity$conf.low[1],
    difference_conf_high_pp = 100 * heterogeneity$conf.high[1],
    difference_p_value = heterogeneity$p.value[1]
  )
  
}


##
get_binary_group_table <- function(
    model,
    outcome_label,
    mode_pairs,
    group_var,
    reference_group,
    comparison_group,
    assignment_var = "STARTMODE",
    vcov_type = "HC2"
) {
  
  results <- vector(
    mode = "list",
    length = length(mode_pairs)
  )
  
  for (j in seq_along(mode_pairs)) {
    results[[j]] <- get_one_panel_status_contrast(
      model = model,
      outcome_label = outcome_label,
      contrast_label = names(mode_pairs)[j],
      pair_values = mode_pairs[[j]],
      group_var = group_var,
      reference_group = reference_group,
      comparison_group = comparison_group,
      assignment_var = assignment_var,
      vcov_type = vcov_type
    )
  }
  
  dplyr::bind_rows(results)
}


##
theme_article <- function(
    base_size = 9,
    base_family = "sans") {
  
  theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    theme(
      # Panel und Hintergrund
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(
        linewidth = 0.25,
        colour = "grey85"
      ),
      
      # facetten
      strip.text = element_text(
        face = "bold",
        size = rel(1.00),
        margin = margin(b = 5)
      ),
      
      # Achsen
      axis.title.x = element_text(
        margin = margin(t = 7)
      ),
      axis.title.y = element_text(
        margin = margin(r = 7)
      ),
      axis.text.x = element_text(
        size = rel(0.92),
        lineheight = 0.9
      ),
      axis.text.y = element_text(
        size = rel(0.92)
      ),
      
      # Legende
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = rel(0.92)),
      legend.key.width = grid::unit(1.4, "lines"),
      legend.margin = margin(t = 4),
      legend.box.margin = margin(0),
      
      # Abstand
      panel.spacing.x = grid::unit(1.0, "lines"),
      plot.margin = margin(6, 7, 5, 6)
    )
}


##
make_formula <- function(outcome, rhs_vars) {
  if (is.null(rhs_vars)){
    as.formula(
      paste(outcome, "~", "1")
    )
  } else {
    as.formula(
      paste(outcome, "~", "1 + " ,paste(rhs_vars, collapse = "+"))
    )
  }
}


##
get_multiplegroup_contrast <- function(
    model,
    outcome_label,
    contrast_label,
    pair_values,
    group_var,
    group_levels,
    assignment_var = "STARTMODE",
    vcov_type = "HC2"
) {
  
  if (length(group_levels) != 3L) {
    stop(
      "This function currently expects exactly three moderator groups."
    )
  }
  
  vcov_matrix <- sandwich::vcovHC(
    model,
    type = vcov_type
  )    
  
  variables_spec <- stats::setNames(
    list(pair_values),
    assignment_var
  )
  
  # Gruppenspezifische AMEs
  group_effects <- marginaleffects::avg_comparisons(
    model,
    variables = variables_spec,
    by = group_var,
    type = "response",
    vcov = vcov_matrix
  ) %>% as.data.frame()
  
  observed_groups <- as.character(
    group_effects[[group_var]]
  )
  
  # Gewünschte Gruppenreihenfolge herstellen
  group_effects <- group_effects[
    match(group_levels, observed_groups),
    ,
    drop = FALSE
  ]
  
  
  # Sicherheitsprüfung b1, b2, b3
  stopifnot(
    identical(
      as.character(group_effects[[group_var]]),
      group_levels
    )
  )
  
  # Unterschiede zwischen gruppenspezifischen AMEs:
  # b1 = low, b2 = medium, b3 = high
  hypothesis_string <- c(
    "b2 - b1 = 0",
    "b3 - b1 = 0",
    "b3 - b2 = 0"
  )
  
  heterogeneity <- marginaleffects::avg_comparisons(
    model,
    variables = variables_spec,
    by = group_var,
    type = "response",
    vcov = vcov_matrix,
    hypothesis = hypothesis_string
  )  %>% 
    as.data.frame()
  
  tibble::tibble(
    outcome = outcome_label,
    contrast = contrast_label,
    
    low_estimate_pp = 100 * group_effects$estimate[1],
    low_conf_low_pp = 100 * group_effects$conf.low[1],
    low_conf_high_pp = 100 * group_effects$conf.high[1],
    
    medium_estimate_pp = 100 * group_effects$estimate[2],
    medium_conf_low_pp = 100 * group_effects$conf.low[2],
    medium_conf_high_pp = 100 * group_effects$conf.high[2],
    
    high_estimate_pp = 100 * group_effects$estimate[3],
    high_conf_low_pp = 100 * group_effects$conf.low[3],
    high_conf_high_pp = 100 * group_effects$conf.high[3],
    
    medium_minus_low_pp = 100 * heterogeneity$estimate[1],
    medium_minus_low_conf_low_pp = 100 * heterogeneity$conf.low[1],
    medium_minus_low_conf_high_pp = 100 * heterogeneity$conf.high[1],
    medium_minus_low_p_value = heterogeneity$p.value[1],
    
    high_minus_low_pp = 100 * heterogeneity$estimate[2],
    high_minus_low_conf_low_pp = 100 * heterogeneity$conf.low[2],
    high_minus_low_conf_high_pp = 100 * heterogeneity$conf.high[2],
    high_minus_low_p_value = heterogeneity$p.value[2],
    
    high_minus_medium_pp = 100 * heterogeneity$estimate[3],
    high_minus_medium_conf_low_pp = 100 * heterogeneity$conf.low[3],
    high_minus_medium_conf_high_pp = 100 * heterogeneity$conf.high[3],
    high_minus_medium_p_value = heterogeneity$p.value[3]
    
  )
  
}


##
get_multigroup_table <- function(
    model,
    outcome_label,
    mode_pairs,
    group_var,
    group_levels,
    assignment_var = "STARTMODE",
    vcov_type = "HC2"
) {
  
  results <- vector(
    mode = "list",
    length = length(mode_pairs)
  )
  
  for (j in seq_along(mode_pairs)) {
    
    results[[j]] <- get_multiplegroup_contrast(
      model = model,
      outcome_label = outcome_label,
      contrast_label = names(mode_pairs)[j],
      pair_values = mode_pairs[[j]],
      group_var = group_var,
      group_levels = group_levels,
      assignment_var = assignment_var,
      vcov_type = vcov_type
    )
  }
  
  dplyr::bind_rows(results)
}


make_heterogeneity_coef_table <- function(
    models,
    coef_map,
    caption,
    label,
    moderator_reference,
    output_file,
    vcov_type = "HC2"
) {
  
  # alle tatsächlich vorkommenden Koeffizienten
  model_terms <- unique(
    unlist(
      lapply(
        models,
        function(model) names(stats::coef(model))
      )
    )
  )
  
  missing_labels <- setdiff(
    model_terms,
    names(coef_map)
  )
  if (length(missing_labels) > 0L) {
    warning("Missing coefficient labels: ", paste(missing_labels, collapse = ", "))
  }
  
  # HC2 Kovarianzmatrix
  vcov_list <- lapply(
    models,
    function(model) {
      sandwich::vcovHC(
        model,
        type = vcov_type
      )
    }
  )
  
  gof_map <- data.frame(
    raw = c(
      "nobs",
      "AIC",
      "BIC"
    ),
    clean = c(
      "Observations",
      "AIC",
      "BIC"
    ), 
    fmt = c(
      0,
      1,
      1
    )
  )
  
  table_note <- paste0(
    "Entries are logit coefficients with ",
    vcov_type,
    " robust standard errors in parentheses.",
    "CBT+CAPI is the reference assignment strategy; ",
    moderator_reference,
    " is the reference moderator category. ",
    "Substantive interpretation is based on probability-scale contrasts."
  )
  
  modelsummary::modelsummary(
    models = models,
    vcov = vcov_list,
    coef_map = coef_map,
    
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    
    fmt = modelsummary::fmt_decimal(
      digits = 3,
      pdigits = 3
    ),
    
    stars = c(
      "*" = 0.05,
      "**" = 0.01,
      "***" = 0.001
    ),
    
    gof_map = gof_map,
    
    title = paste0(
      caption,
      "\\label{",
      label,
      "}"
    ),
    
    notes = table_note,
    escape = FALSE,
    output = output_file
  )
  
  invisible(output_file)

}


##
get_assignment_contrasts_across <- function(
    model,
    outcome_label,
    specification_label,
    mode_pairs,
    assignment_var = "STARTMODE",
    vcov_type = "HC2"
) {
  vcov_matrix <- sandwich::vcovHC(
    model,
    type = vcov_type
  )
  
  results <- lapply(
    seq_along(mode_pairs),
    function(j) {
      variables_spec <- stats::setNames(
        list(mode_pairs[[j]]),
        assignment_var
      )
      
      tmp <- marginaleffects::avg_comparisons(
        model,
        variables = variables_spec,
        type = "response",
        vcov = vcov_matrix
      ) %>% 
        as.data.frame()
      
      tibble::tibble(
        outcome = outcome_label,
        specification = specification_label,
        contrast = names(mode_pairs)[j],
        
        estimate_pp = 100 * tmp$estimate,
        conf_low_pp = 100 * tmp$conf.low,
        conf_high_pp = 100* tmp$conf.high,
        
        p_value = tmp$p.value,
        n = stats::nobs(model)
      )
    }
  )
  
  dplyr::bind_rows(results)
  
}


##
make_counterfactual_assignment_data <- function(
    mod,
    assignment = "STARTMODE"
) {
  
  dat <- mod$model
  
  # assignment levels in their original order
  assignment_levels <- levels(dat[[assignment]])
  
  out <- lapply(
    assignment_levels,
    function(a) {
      tmp <- dat
      
      tmp[[assignment]] <- factor(
        a,
        levels = assignment_levels
      )
      
      # keep explicit counterfactual scenario label
      tmp$.assignment_cf <- a
      
      # order row id
      tmp$.rowid_original <- seq_len(nrow(tmp))
    
      tmp
    }
  )
  
  dplyr::bind_rows(out)
}

##
get_cf_states_by_group <- function(
    model,                       
    assignment_var = "STARTMODE",         
    group_var = "status.16"
) {
  
  mf <- model.frame(model)
  
  assign_levels <- levels(mf[[assignment_var]])
  
  cf_data <- purrr::map_dfr(assign_levels, function(a) {
    
    tmp <- mf
    tmp[[assignment_var]] <- factor(a, levels = assign_levels)
    tmp$STARTMODE_cf <- a
    tmp
  })
  
  preds <- marginaleffects::predictions(
    model,
    newdata = cf_data,
    type = "probs"
  ) %>% 
    as.data.frame()
  
  preds %>% 
    dplyr::group_by(
      STARTMODE_cf,
      !!rlang::sym(group_var),
      group
    ) %>% 
    dplyr::summarise(
      estimate = mean(estimate),
      .groups = "drop"
    )
}


##
get_multinomial_results <- function(
    mod, 
    model_name
) {
  
  # 1. robust sandwich covariates
  
  V_robust <- sandwich::sandwich(mod)
  
  stopifnot(
    all(is.finite(V_robust)),
    nrow(V_robust) == ncol(V_robust)
  )
  
  # 2. Average predicted state probabilities by assignment
  
  pred <- marginaleffects::avg_predictions(
    mod,
    newdata = mod$model,
    variables = "STARTMODE",
    type = "probs",
    vcov = V_robust,
    conf_level = 0.95
  ) %>% 
    as.data.frame() %>% 
    dplyr::mutate(
      specification = model_name,
      .before = 1
    )
  
  # 3. pairwise assignment contrasts in state probabilities
  
  contrasts <- marginaleffects::avg_comparisons(
    mod,
    newdata = mod$model,
    variables = list(STARTMODE = "pairwise"),
    comparison = "difference",
    type = "probs",
    vcov = V_robust,
    conf_level = 0.95
  ) %>% 
    as.data.frame() %>% 
    dplyr::mutate(
      specification = model_name,
      .before = 1
    )
  
  # 4. return
  
  list(
    predictions = pred,
    contrasts = contrasts,
    vcov = V_robust
  )
  
}


##
multinom_heterogeneity_contrasts <- function(
    pred_obj,
    moderator,
    level1,
    level0,
    assignment = "STARTMODE",
    assignment_pairs,
    conf_level = 0.95
) {
  
  # prediction estimates and their full covariance matrix
  dat <- as.data.frame(pred_obj)
  V <- vcov(pred_obj)
  
  stopifnot(
    nrow(V) == nrow(dat),
    ncol(V) == nrow(dat),
    all(is.finite(V))
  )
  
  # helper function:
  # identify exactly one required prediction
  
  get_index <- function(state, mod_level, ass_level) {
    idx <- which(
      dat$group == state &
        dat[[moderator]] == mod_level &
        dat[[assignment]] == ass_level
    )
    
    idx
  }
  
  # critical value
  alpha <- 1 - conf_level
  crit <- qnorm(1 - alpha/2)
  
  # build one linear contrast for every
  # state x assignment x comparison
  
  states <- unique(dat$group)
  
  out <- list()
  counter <- 1L
  
  for (state in states) {
    
    for (pair in assignment_pairs) {
      
      hi <- pair[1]
      lo <- pair[2]
      
      w <- rep(0, nrow(dat))
      
      w[get_index(state, level1, hi)] <- 1
      w[get_index(state, level1, lo)] <- -1
      w[get_index(state, level0, hi)] <- -1
      w[get_index(state, level0, lo)] <- 1
      
      est <- sum(w * dat$estimate)
      
      var_est <- as.numeric(
        t(w) %*% V %*% w
      )
      
      #  protect against tiny negative numerical rounding
      if (var_est < 0 && abs(var_est) < 1e-12) {
        var_est <- 0
      }
      
      if (var_est < 0) {
        stop("Negative variance encountered.")
      }
      
      se <- sqrt(var_est)
      
      z <- est / se
      p <- 2 * pnorm(-abs(z))
      
      out[[counter]] <- data.frame(
        group = state,
        contrast = paste0(hi, " - ", lo),
        moderator_contrast = paste0(level1, " - ", level0),
        
        estimate = est,
        std.error = se,
        statistic = z,
        p.value = p,
        conf.low = est - crit * se,
        conf.high = est + crit * se,
        
        stringsAsFactors = FALSE
      )
      
      counter <- counter + 1L
    }
  }
  
  out <- dplyr::bind_rows(out)
  
  # percentage
  out <- out %>% 
    dplyr::mutate(
      estimate_pp = 100 * estimate,
      conf.low_pp = 100 * conf.low,
      conf.high_pp = 100 * conf.high
    )
  
  out
  
}


##
multinom_interaction_wald <- function(
    mod,
    moderator_pattern,
    moderator_var,
    moderator_label
){
  # robust variance matrix
  V <- sandwich::sandwich(mod)
  
  # all interaction coefficients in the multinomial model
  interaction_terms <- car::matchCoefs(mod, ":")
  
  # retain only interactions involving the moderator of interest
  test_terms <- interaction_terms[
    grepl(moderator_pattern, interaction_terms)
  ]
  
  if (length(test_terms) == 0L) {
    stop(
      "No interaction terms found for pattern: ",
      moderator_pattern
    )
  }
  
  # joint Wald chi-square test
  wald <- car::linearHypothesis(
    mod, 
    hypothesis.matrix = test_terms,
    vcov. = V,
    test = "Chisq"
  )
  
  wald_df <- as.data.frame(wald)
  
  # final row contains the test
  test_row <- wald_df[nrow(wald_df), , drop = FALSE]
  
  chisq_col <- grep(
    "Chisq",
    names(test_row),
    value = TRUE
  )[1]
  
  p_col <- grep(
    "^Pr\\(",
    names(test_row),
    value = TRUE
  )[1]
  
  df_col <- grep(
    "^Df$",
    names(test_row),
    value = TRUE
  )[1]
  
  # subgroup N
  n_tab <- table(
    mod$model[[moderator_var]],
    useNA = "no"
  )
  
  subgroup_sizes <- paste(
    paste0(
      names(n_tab),
      ": ",
      as.integer(n_tab)
    ),
    collapse = "; "
  )
  
  tibble(
    moderator = moderator_label,
    subgroup_sizes = subgroup_sizes,
    chisq = as.numeric(test_row[[chisq_col]]),
    df = as.integer(test_row[[df_col]]),
    p_value = as.numeric(test_row[[p_col]]),
    n_terms_tested = length(test_terms)
  )
  
}


# function: get composition
get_composition <- function(data, group_var, value, label = NULL) {
  
  group_name <- rlang::as_name(rlang::ensym(group_var))
  
  if (is.null(group_name)) {
    label <- group_name
  }
  
  data %>%
    dplyr::summarise(
      gross = mean({{group_var}} == value, na.rm = TRUE),
      valid_test = mean({{group_var}} == value & Y_TBT == 1, na.rm = TRUE) / 
        mean(Y_TBT == 1, na.rm = TRUE),
      interview = mean({{group_var}} == value & Y_Befragung == 1, na.rm = TRUE) / 
        mean(Y_Befragung == 1, na.rm = TRUE),
      followup = mean({{group_var}} == value & Y_CAWI_nachgelagert == 1, na.rm = TRUE) / 
        mean(Y_CAWI_nachgelagert == 1, na.rm = TRUE)
    ) %>% 
    dplyr::mutate(
      group = label, .before = 1
    )
}