# Prepare and Format LaTex Tables

tidy_create_kable_felm <- function(input_model_list,
                                   input_term_names,
                                   dv_name_vec,
                                   ...) {

  # Create a data.frame
  df_tidy <- tidy_felm_prep_kable(
    input_model_list = input_model_list,
    input_term_names = input_term_names,
    dv_name_vec = dv_name_vec,
    ... = ...
  )

  # Create a kable
  f_kable <- create_kable_felm(
    data = df_tidy,
    dv_name_vec = dv_name_vec,
    input_term_names = input_term_names,
    ... = ...
  )

  # Return output
  return(f_kable)
}

# COPIED FROM GEOGRAPHY REPO ON 2021-07-08

tidy_felm_prep_kable <- function(input_model_list,
                                 input_term_names,
                                 dv_name_vec) {
  dv_name_vec <- names(dv_name_vec)

  # ---- TIDY ---- #

  f_newdata_estimate_ci_df <- input_model_list %>%
    .[dv_name_vec] %>%
    map(.f = function(x) {
    broom::tidy(x, conf.int = TRUE)
  }) %>%
    map_df( ~ bind_rows(.),
            .id = "dv_name")

  # ---- CLEAN UP ---- #

  f_df_model_tidy_kable_ready <- f_newdata_estimate_ci_df %>%
  # Create combined statistic column
  mutate(
    p.value_str = case_when(
      p.value <= 0.001 ~ "***",
      p.value <= 0.01 ~ "**",
      p.value <= 0.05 ~ "*",
      p.value <= 0.10 ~ "+",
      TRUE ~ ""
    )
  ) %>%
    # mutate(p.value_str = str_pad(p.value_str, width = 3, side = "right")) %>%
    # mutate_if(is.numeric, ~ round(., 3)*100) %>%
    mutate_if(is.numeric, ~ sprintf("%.3f", # Keep trailing zeroes
                                    round(., 3))) %>%
    mutate(estimate_mod = paste0(estimate, " ", p.value_str)) %>%
    mutate(estimate = as.character(estimate)) %>% # NEW
    mutate_at(.vars = quos(estimate, estimate_mod, conf.low, conf.high),
              .funs = ~ ifelse(str_detect(., "^\\-"), # NEW
                               .,
                               paste0(" ", .))) %>%
    mutate(ci.low.high = paste0("[", conf.low, ", ", conf.high, "]")) %>%
    # unite(estimate_p_ci, estimate_mod, ci.low.high, sep = " ") %>% # NEW
    mutate(std.error = paste0("(", std.error, ")")) %>%
    unite(estimate_se_p, estimate, std.error, p.value_str, sep = " ") %>% # NEW
    select(dv_name, term, estimate_se_p) %>% # NEW
    # select(dv_name, term, estimate_mod, ci.low.high) %>% # OLD
    # gather(statistic, value, estimate_mod, ci.low.high) %>% # OLD
    # mutate(statistic = fct_recode( # OLD
    #   statistic,
    #   "1estimate_mod" = "estimate_mod",
    #   "2ci.low.high" = "ci.low.high"
    # )) %>%
    # unite(dv_name_statistic, c("dv_name", "statistic")) %>% # OLD
    # spread(dv_name_statistic, value) %>% # OLD
    spread(dv_name, estimate_se_p) %>% # OLD
    # Combined estimate + CI + p-value
    # Recode terms
    left_join(input_term_names, by = "term") %>%
    mutate(display_name = ifelse(is.na(display_name), term, display_name))

  # Overlapping display names (causes issues in fct_relevel if new ones appear)
  input_term_names_short <- input_term_names %>%
    # Just joining by "display_name" is causing issues when the same display
    # name shows up in multiple places (e.g. white, college, high income
    # binary indicators in supplementary interaction models). Does joining
    # by "term" as well help?
    semi_join(f_df_model_tidy_kable_ready, by = c("term", "display_name"))

  # Finish pipe
  f_df_model_tidy_kable_ready <- f_df_model_tidy_kable_ready %>%
    mutate(display_name = fct_relevel(display_name, # !!!c("Republican", "% White ('18 ZIP)"))) %>%
                                      !!! unique(input_term_names_short$display_name))) %>%
    arrange(display_name)


  # Move arranging of columns + labels to a different function

  # dv_name_vec_expand <- dv_name_vec %>% # OLD
  #   map( ~ paste0(., c("_1estimate_mod", "_2ci.low.high"))) %>%
  #   unlist()

  # f_output <- f_df_model_tidy_kable_ready %>% # OLD
  #   select(display_name, !!!dv_name_vec_expand)

  f_output <- f_df_model_tidy_kable_ready %>%
    select(display_name, !!!dv_name_vec)

  # Add N, R-Squared, Adjusted R-Squared

  f_summary_stats_list <- input_model_list %>%
    map( ~ summary(.)[c("N", "r.squared", "adj.r.squared")]) %>%
    map(.f = function(x) {
      x$N <- comma_format(3)(x$N)
      x$r.squared <- sprintf("%.3f", # Keep trailing zeroes
                             round(x$r.squared, 3))
      x$adj.r.squared <- sprintf("%.3f", # Keep trailing zeroes
                                 round(x$adj.r.squared, 3))
      return(x)
    }) %>%
    map(.f = function(x) {
      output <- x %>%
        as_tibble() %>%
        gather(term, value) %>%
        mutate(term = fct_relevel(term, "N", "r.squared", "adj.r.squared")) %>%
        arrange(term) %>%
        mutate(term = fct_recode(term,
                                 "N" = "N",
                                 "R-Squared" = "r.squared",
                                 "Adj. R-Squared" = "adj.r.squared")) %>%
        rename(display_name = term)

      return(output)
    }) %>%
    map2(.x = .,
         .y = names(.),
         .f = ~ rename(mutate(.x, value = paste0(" ", value)), # To line up with estimates etc.
                       !!.y := value))

  # Combine using `full_join` rather than `bind_col`
  f_summary_stats_df <- f_summary_stats_list[[1]]

  if (length(f_summary_stats_list) > 1) {
    for (i in 2:length(f_summary_stats_list)) { # f_output_names[2:length(f_output)]
      f_summary_stats_df <- full_join(f_summary_stats_df,
                                      f_summary_stats_list[[i]],
                                      by = "display_name")
    }
  }

  f_summary_stats_df <- f_summary_stats_df %>%
    select(display_name, !!!dv_name_vec) %>%
    mutate_all(as.character)

  # Combine coefficients and diagnostics

  f_output_final <- bind_rows(f_output,
                              f_summary_stats_df)

  # Remove missing

  f_output_final <- f_output_final %>%
    mutate_all( ~ ifelse(is.na(.) , "", .))

  # Return Output

  return(f_output_final)
}

# Takes output from other function. Funny html error when saving RDS.
# Dots for allowing pipes
create_kable_felm <- function(data,
                              dv_name_vec,
                              input_term_names,
                              kable_format = "latex",
                              packed_rows = TRUE,
                              escape_first_space = TRUE,
                              caption = NULL,
                              fixed_effect_info = NULL, # data.frame
                              ...) {

  if (escape_first_space) {

    # Escape first space so that coefficients without a negative line up with
    # those who do
    data <- data %>%
      mutate(display_name = paste0(" ", display_name)) %>% # Not sure why just using `mutate_at` didn't work...
      mutate_all( ~ str_replace(., " ", fixed("\\\\ ")))
      # mutate_at(.vars = names(dv_name_vec),
      #           .funs = ~ str_replace(., " ", fixed("\\\\ ")))

    # Escape space to match above (not sure why needed above...)
    input_term_names <- input_term_names %>%
      mutate(display_name = paste0(" ", display_name)) %>% # Not sure why just using `mutate_at` didn't work...
      mutate_at(.vars = vars(display_name),
                .funs = ~ str_replace(., " ", fixed("\\\\ ")))

  }

  # Header

  vec_header <- c(
    " " = 1,
    map(set_names(x = dv_name_vec, nm = NULL),
        # ~ unlist(set_names(x = 2, .))) # OLD
        ~ unlist(set_names(x = 1, .))) # NEW
  ) %>%
    unlist()

  ## TODO: Instead of doing a header just rename columns (since now one
  ## column per model)

  # data <- data %>%
  #   select(display_name,
  #          c(" " = display_name,
  #          !!!set_names(x = names(dv_name_vec_four_primary),  # Flip names/elements
  #                       nm = dv_name_vec_four_primary)))


  # ---- Kable ---- #

  # Add fixed effect info


  if (!is.null(fixed_effect_info)){
    df_crossed_var_fixed_effects <- crossing(
      tibble(vars = fixed_effect_info[["vars"]]),
      tibble(display_name   = fixed_effect_info[["fe"]])
    ) %>%
      mutate(temp_col = fixed_effect_info[["cell_value"]]) %>%
      spread(vars, temp_col) %>%
      mutate(display_name = fct_relevel(display_name, !!!fixed_effect_info[["fe"]])) %>%
      arrange(display_name) %>%
      mutate(display_name = as.character(display_name))

    df_diagnostic_vars <- data %>%
      slice((nrow(data) - 2):nrow(data))

    data <- data %>%
      slice(1:(nrow(data) - 3)) %>%
      bind_rows(df_crossed_var_fixed_effects) %>%
      bind_rows(df_diagnostic_vars)

    num_fixed_effect_rows <- nrow(df_crossed_var_fixed_effects)
  } else {
    num_fixed_effect_rows <- NULL
  }

  # Create kable

  f_kable <- data %>%
    kable(
      format = kable_format,
      booktabs = TRUE,
      # col.names = rep(" ", ncol(data)),
      col.names = names(vec_header), # TEMP (and drop header)
      escape = !escape_first_space,
      caption = caption
    )
    # add_header_above(header = vec_header, # linebreak(vec_header),
    #                  line = FALSE) %>%

  if (!is.null(num_fixed_effect_rows)) {
    f_kable <- f_kable %>%
      row_spec(nrow(data) - 3 - num_fixed_effect_rows, hline_after = TRUE) # %>%
      # row_spec((nrow(data) - 3 - num_fixed_effect_rows):nrow(data) - 3,
      #          align = "c")
  }

  f_kable <- f_kable %>%
    row_spec(nrow(data) - 3, hline_after = TRUE) %>%
    column_spec(column = seq(2, ncol(data)),
                width_max = "1cm") %>%
    kable_styling(latex_options = "scale_down") %>%
    footnote(general = "0.1 + 0.05 * 0.01 ** 0.001 ***",
             general_title =  "P-Value thresholds:",
             footnote_as_chunk = T,
             title_format = "bold")

  # ---- ADD PACKED ROWS ---- #

  # Note: Packed rows not currently working with escaping spaces (`escape_first_space`)

  f_packed_rows_info <- input_term_names %>%
    semi_join(data,
              by = "display_name") %>%
    mutate(row_num = row_number()) %>%
    filter(packed_rows != "Intercept") %>%
    group_by(packed_rows) %>%
    summarise(min_row = min(row_num),
              max_row = max(row_num)) %>%
    ungroup()

  f_kable_packed_rows <- f_kable

  if (packed_rows) {
    for (i in 1:nrow(f_packed_rows_info)) {
      f_kable_packed_rows <- f_kable_packed_rows %>%
        pack_rows(f_packed_rows_info$packed_rows[i],
                  f_packed_rows_info$min_row[i],
                  f_packed_rows_info$max_row[i],
                  hline_after = FALSE)
    }
  }

  # ---- Output ---- #
  return(f_kable_packed_rows)
}


# extract ggplot legend
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
