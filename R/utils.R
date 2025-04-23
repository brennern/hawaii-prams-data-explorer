
### PLOT FUNCTIONS ###
#### TAB 1 ####
default_plot_univariate_trend_line <- function(data) {
  
  data$diabetes_indicator <- ifelse(data$`Pregestational Diabetes` == "Yes", 1, 0)
  
  options(survey.lonely.psu = "adjust")
  
  dat <- svydesign(ids = ~1, weights = ~WTANAL, strata = ~STRATUMC, 
                   nest = TRUE, data = data)
  
  weighted_results <- svyby(
    formula = ~diabetes_indicator,
    by = ~Detailed_Race + Today_Year,
    design = dat,
    FUN = svymean,
    na.rm = TRUE
  )
  
  weighted_results <- weighted_results |> 
    mutate(
      Percentage = diabetes_indicator * 100
    )
  
  sample_sizes <- data |> 
    filter(!is.na(`Pregestational Diabetes`),
           !is.na(Today_Year),
           !is.na(Detailed_Race)) |> 
    group_by(Detailed_Race, Today_Year) |>  
    summarise(Total_Sample_Size = n(), .groups = "drop")
  
  merged_data <- left_join(weighted_results, sample_sizes, by = c("Detailed_Race", "Today_Year"))
  
  p <- ggplot(merged_data, aes(x = as.numeric(Today_Year), y = Percentage, color = Detailed_Race)) +
    geom_line(size = 1, alpha = 0.6) +
    geom_point(aes(
      text = paste(
        "Year:", Today_Year,
        "<br>Race:", Detailed_Race,
        "<br>Percentage:", scales::percent(Percentage / 100),
        "<br>Total Sample Size:", Total_Sample_Size
      )
    ), size = 2, alpha = 0.8) +
    scale_x_continuous(
      breaks = unique(weighted_results$Today_Year),
      labels = unique(weighted_results$Today_Year)
    ) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_color_brewer(palette = "Set2") +
    theme_minimal(base_size = 14) + 
    theme(
      plot.title = element_text(size = rel(1.5)),
      axis.text.x = element_text(angle = 20, vjust = 1, size = rel(1.25)),
      axis.text.y = element_text(size = rel(2)),
      axis.title.x = element_text(size = rel(1.5)),
      axis.title.y = element_text(size = rel(1.5)),
      axis.ticks.x = element_line(size = rel(1.5)),
      legend.position = "top",
      legend.title = element_text(size = rel(1.25)),
      legend.text = element_text(size = rel(1.25))
    ) +
    labs(
      title = "Pregestational Diabetes by Year",
      x = "Year",
      y = "Percentage",
      color = "Demographic"
    )
  
  p
  
}


univariate_trend_line <- function(data, categorical_var, demographic_var) {
  
  var_name <- rlang::as_name(rlang::ensym(categorical_var))
  
  data <- data |> 
    mutate(
      binary_indicator = if_else(
        !!sym(categorical_var) == "Yes",
        1,
        0
      )
    )
  
  options(survey.lonely.psu = "adjust")
  
  dat <- svydesign(ids = ~1, weights = ~WTANAL, strata = ~STRATUMC, 
                   nest = TRUE, data = data)
  
  weighted_results <- svyby(
    formula = ~binary_indicator,
    by = as.formula(paste("~", demographic_var, "+ Today_Year")),
    design = dat,
    FUN = svymean,
    na.rm = TRUE
  )
  
  weighted_results <- weighted_results |> 
    mutate(
      Percentage = binary_indicator * 100
    )
  
  sample_sizes <- data |> 
    filter(!is.na(!!sym(categorical_var)),
           !is.na(Today_Year),
           !is.na(!!sym(demographic_var))) |> 
    group_by(!!sym(demographic_var), Today_Year) |>  
    summarise(Total_Sample_Size = n(), .groups = "drop")
  
  merged_data <- left_join(weighted_results, sample_sizes, by = c(demographic_var, "Today_Year"))
  
  p <- ggplot(merged_data, aes(x = as.numeric(Today_Year), y = Percentage, color = !!sym(demographic_var))) +
    geom_line(size = 1, alpha = 0.6) +
    geom_point(aes(
      text = paste(
        "Year:", Today_Year,
        "<br>Demographics:", !!sym(demographic_var),
        "<br>Percentage:", scales::percent(Percentage / 100),
        "<br>Total Sample Size:", Total_Sample_Size
      )
    ), size = 2, alpha = 0.8) +
    scale_x_continuous(
      breaks = unique(weighted_results$Today_Year),
      labels = unique(weighted_results$Today_Year)
    ) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_color_brewer(palette = "Set2") +
    theme_minimal(base_size = 14) + 
    theme(
      plot.title = element_text(size = rel(1.5)),
      axis.text.x = element_text(angle = 20, vjust = 1, size = rel(1.25)),
      axis.text.y = element_text(size = rel(2)),
      axis.title.x = element_text(size = rel(1.5)),
      axis.title.y = element_text(size = rel(1.5)),
      axis.ticks.x = element_line(size = rel(1.5)),
      legend.position = "top",
      legend.title = element_text(size = rel(1.25)),
      legend.text = element_text(size = rel(1.25))
    ) +
    labs(
      title = paste(rlang::ensym(categorical_var), "by Year"),
      x = "Year",
      y = "Percentage",
      color = "Demographic"
    )
  
  p
  
}

univariate_trend_line_multiple_levels <- function(data, categorical_var) {
  
  # Ensure the provided variable is treated as a factor
  data[[categorical_var]] <- as.factor(data[[categorical_var]])
  
  options(survey.lonely.psu = "adjust")
  
  # Create the survey design object using the provided data
  dat <- svydesign(ids = ~1, weights = ~WTANAL, strata = ~STRATUMC, 
                   nest = TRUE, data = data)
  
  # Build a formula dynamically to compute weighted means for the categorical variable
  formula_obj <- as.formula(paste0("~", categorical_var))
  
  # Compute the weighted means for each level by race and year using svyby.
  # This returns one column per level (with names like "VariableLevel")
  weighted_results <- svyby(
    formula = formula_obj,
    by = ~Race + Today_Year,
    design = dat,
    FUN = svymean,
    na.rm = TRUE
  )
  
  # Identify estimate columns, excluding any that start with "se." (which are standard errors)
  est_cols <- names(weighted_results)[
    !grepl("^(se\\.)", names(weighted_results), ignore.case = TRUE) &
      !(names(weighted_results) %in% c("Race", "Today_Year"))
  ]
  
  # Pivot the data into long format so each row represents one factor level's estimate
  weighted_results_long <- weighted_results %>%
    pivot_longer(
      cols = all_of(est_cols),
      names_to = "level",
      values_to = "proportion"
    ) %>%
    mutate(Percentage = proportion * 100)
  
  # Remove the original variable name from the level labels.
  # For instance, if categorical_var is "First_PNC_Visit_Wks_Mnths", then levels
  # might be "First_PNC_Visit_Wks_MnthsMonths", which will be cleaned to "Months".
  weighted_results_long <- weighted_results_long %>%
    mutate(level = gsub(paste0("^", categorical_var), "", level))
  
  # Compute the (unweighted) sample sizes for each race, year, and level.
  sample_sizes <- data %>%
    filter(!is.na(!!sym(categorical_var)),
           !is.na(Today_Year),
           !is.na(Race)) %>%
    group_by(Race, Today_Year, !!sym(categorical_var)) %>%
    summarise(Total_Sample_Size = n(), .groups = "drop") %>%
    rename(level = !!sym(categorical_var))
  
  # Merge the weighted estimates with the sample sizes
  merged_data <- left_join(weighted_results_long, sample_sizes,
                           by = c("Race", "Today_Year", "level"))
  
  # Create the plot with facet_wrap for each level
  p <- ggplot(merged_data, aes(x = as.numeric(Today_Year), y = Percentage, color = Race)) +
    geom_line(size = 1, alpha = 0.6) +
    geom_point(aes(
      text = paste(
        "Year:", Today_Year,
        "<br>Race:", Race,
        "<br>Level:", level,
        "<br>Percentage:", scales::percent(Percentage / 100),
        "<br>Total Sample Size:", Total_Sample_Size
      )
    ), size = 2, alpha = 0.8) +
    scale_x_continuous(
      breaks = unique(data$Today_Year),
      labels = unique(data$Today_Year)
    ) +
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       expand = expansion(mult = c(0, 0.1))) +
    scale_color_brewer(palette = "Set2") +
    theme_minimal(base_size = 14) +
    facet_wrap(vars(level), scales = "free_y") +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 1, size = rel(1.15)),
      axis.text.y = element_text(size = rel(2)),
      axis.ticks.x = element_line(size = rel(1.5)),
      axis.title = element_text(size = rel(2)),
      legend.title = element_text(size = rel(1.5)),
      legend.text = element_text(size = rel(1.5)),
      panel.spacing = unit(1.5, "lines")
    ) +
    labs(
      x = "Year",
      y = "Percentage",
      color = "Race"
    )
  
  plotly::ggplotly(p, height = 600, width = 1450, tooltip = "text") |> 
    layout(
      margin = list(t = 120, r = 20, b = 120, l = 20),
      title = list(x = 0.5,
                   y = 0.925),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 1.1
      )
    )
}

default_plot_make_line_graph_race_health_condition_by_year <- function(data) {
  
  p <- data |> 
    group_by(`Chronic Hypertension`, `Pregestational Diabetes`, Today_Year, Race) |> 
    filter(!is.na(`Pregestational Diabetes`)) |> 
    filter(!is.na(Today_Year)) |> 
    filter(!is.na(`Chronic Hypertension`)) |> 
    filter(!is.na(Race)) |> 
    summarise(
      Count = n(),
      .groups = "drop"
    ) |> 
    group_by(Today_Year, Race) |> 
    mutate(
      Proportion = Count / sum(Count),
      Percentage = Proportion * 100,
      Total_Sample_Size = sum(Count)
    ) |> 
    filter(`Pregestational Diabetes` == "Yes") |> 
    filter(`Chronic Hypertension` == "Yes") |> 
    ggplot(aes(x = as.numeric(Today_Year), y = Percentage, color = Race)) +
    geom_line(size = 1, alpha = 0.6) +
    geom_point(aes(
      text = paste(
        "Year:", Today_Year,
        "<br>Race:", Race,
        "<br>Percentage:", scales::percent(Percentage / 100),
        "<br>Total Sample Size:", Total_Sample_Size
      )
    ), size = 2, alpha = 0.8) +
    scale_x_continuous(
      breaks = unique(data$Today_Year),  # Use unique years as tick marks
      labels = unique(data$Today_Year)  # Ensure labels are correctly aligned
    ) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 20, vjust = 1, size = rel(1.25)),
      axis.text.y = element_text(size = rel(2)),
      axis.ticks.x = element_line(size = rel(1.5)),
      axis.title = element_text(size = rel(2)),
      legend.title = element_text(size = rel(1.5)),
      legend.text = element_text(size = rel(1.5))
    ) +
    labs(
      title = paste("Pregestational Diabetes and Chronic Hypertension by Year"),
      x = "Year",
      y = "Percentage",
      color = "Race"
    )
  
  plotly::ggplotly(p)
  
}

multivariate_trend_line <- function(data, categorical_var, grouping_var) {
  
  var_name <- rlang::as_name(rlang::ensym(categorical_var))
  var_name2 <- rlang::as_name(rlang::ensym(grouping_var))
  
  p <- data |> 
    group_by(!!sym(grouping_var), !!sym(categorical_var), Today_Year, Race) |> 
    filter(!is.na(!!sym(categorical_var))) |> 
    filter(!is.na(Today_Year)) |> 
    filter(!is.na(!!sym(grouping_var))) |> 
    filter(!is.na(Race)) |> 
    summarise(
      Count = n(),
      .groups = "drop"
    ) |> 
    group_by(Today_Year, Race) |> 
    mutate(
      Proportion = Count / sum(Count),
      Percentage = Proportion * 100,
      Total_Sample_Size = sum(Count)
    ) |> 
    filter(!!sym(categorical_var) == "Yes") |> 
    filter(!!sym(grouping_var) == "Yes") |> 
    ggplot(aes(x = as.numeric(Today_Year), y = Percentage, color = Race)) +
    geom_line(size = 1, alpha = 0.6) +
    geom_point(aes(
      text = paste(
        "Year:", Today_Year,
        "<br>Race:", Race,
        "<br>Percentage:", scales::percent(Percentage / 100),
        "<br>Total Sample Size:", Total_Sample_Size
      )
    ), size = 2, alpha = 0.8) +
    scale_x_continuous(
      breaks = unique(data$Today_Year),  # Use unique years as tick marks
      labels = unique(data$Today_Year)  # Ensure labels are correctly aligned
    ) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    scale_color_brewer(palette = "Set2") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = rel(1.5)),
      axis.text.x = element_text(angle = 20, vjust = 1, size = rel(1.25)),
      axis.text.y = element_text(size = rel(2)),
      axis.title.x = element_text(size = rel(1.5)),
      axis.title.y = element_text(size = rel(1.5)),
      axis.ticks.x = element_line(size = rel(1.5)),
      legend.position = "top",
      legend.title = element_text(size = rel(1.25)),
      legend.text = element_text(size = rel(1.25))
    ) +
    labs(
      title = paste({{categorical_var}}, "and", {{grouping_var}}, "by Year"),
      x = "Year",
      y = "Percentage",
      color = "Race"
    )
  
  plotly::ggplotly(p, tooltip = "text") |> 
    layout(
      margin = list(t = 120, r = 20, b = 120, l = 20),
      title = list(x = 0.5,
                   y = 0.925),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 1.1
      )
    )
  
}

multivariate_trend_line_3_vars <- function(data, categorical_var, grouping_var, third_var) {
  
  var_name <- rlang::as_name(rlang::ensym(categorical_var))
  var_name2 <- rlang::as_name(rlang::ensym(grouping_var))
  
  p <- data |> 
    group_by(!!sym(grouping_var), !!sym(categorical_var), !!sym(third_var), Today_Year, Race) |> 
    filter(!is.na(!!sym(categorical_var))) |> 
    filter(!is.na(Today_Year)) |> 
    filter(!is.na(!!sym(grouping_var))) |> 
    filter(!is.na(!!sym(third_var))) |> 
    filter(!is.na(Race)) |> 
    summarise(
      Count = n(),
      .groups = "drop"
    ) |> 
    group_by(Today_Year, Race) |> 
    mutate(
      Proportion = Count / sum(Count),
      Percentage = Proportion * 100,
      Total_Sample_Size = sum(Count)
    ) |> 
    filter(!!sym(categorical_var) == "Yes") |> 
    filter(!!sym(grouping_var) == "Yes") |> 
    filter(!!sym(third_var) == "Yes") |> 
    ggplot(aes(x = as.numeric(Today_Year), y = Percentage, color = Race)) +
    geom_line(size = 1, alpha = 0.6) +
    geom_point(aes(
      text = paste(
        "Year:", Today_Year,
        "<br>Race:", Race,
        "<br>Percentage:", scales::percent(Percentage / 100),
        "<br>Total Sample Size:", Total_Sample_Size
      )
    ), size = 2, alpha = 0.8) +
    scale_x_continuous(
      breaks = unique(data$Today_Year),  # Use unique years as tick marks
      labels = unique(data$Today_Year)  # Ensure labels are correctly aligned
    ) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    scale_color_brewer(palette = "Set2") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = rel(1.5)),
      axis.text.x = element_text(angle = 20, vjust = 1, size = rel(1.25)),
      axis.text.y = element_text(size = rel(2)),
      axis.title.x = element_text(size = rel(1.5)),
      axis.title.y = element_text(size = rel(1.5)),
      axis.ticks.x = element_line(size = rel(1.5)),
      legend.position = "top",
      legend.title = element_text(size = rel(1.25)),
      legend.text = element_text(size = rel(1.25))
    ) +
    labs(
      title = paste({{categorical_var}}, {{third_var}}, "and", {{grouping_var}}, "by Year"),
      x = "Year",
      y = "Percentage",
      color = "Race"
    )
  
  plotly::ggplotly(p, tooltip = "text") |> 
    layout(
      margin = list(t = 120, r = 20, b = 120, l = 20),
      title = list(x = 0.5,
                   y = 0.925),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 1.1
      )
    )
  
}

multivariate_trend_line_multiple_level_vars <- function(data, categorical_var, grouping_var) {
  
  var_name <- rlang::as_name(rlang::ensym(categorical_var))
  var_name2 <- rlang::as_name(rlang::ensym(grouping_var))
  
  p <- data |> 
    group_by(!!sym(grouping_var), !!sym(categorical_var), Today_Year, Race) |> 
    filter(!is.na(!!sym(categorical_var))) |> 
    filter(!is.na(Today_Year)) |> 
    filter(!is.na(!!sym(grouping_var))) |> 
    filter(!is.na(Race)) |> 
    summarise(
      Count = n(),
      .groups = "drop"
    ) |> 
    group_by(Today_Year, !!sym(grouping_var), Race) |> 
    mutate(
      Proportion = Count / sum(Count),
      Percentage = Proportion * 100
    ) |> 
    filter(!!sym(categorical_var) == "Yes") |> 
    filter(!!sym(grouping_var) == "Yes") |> 
    ggplot(aes(x = as.numeric(Today_Year), y = Percentage, color = Race)) +
    geom_line(size = 1, alpha = 0.6) +
    geom_point(size = 2, alpha = 0.8) +
    scale_x_continuous(
      breaks = unique(data$Today_Year),  # Use unique years as tick marks
      labels = unique(data$Today_Year)  # Ensure labels are correctly aligned
    ) +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    scale_color_brewer(palette = "Set2") +
    theme_minimal(base_size = 14) +
    facet_wrap(vars({{grouping_var}}), scales = "free_y") +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 1, size = rel(1.15)),
      axis.text.y = element_text(size = rel(2)),
      axis.ticks.x = element_line(size = rel(1.5)),
      axis.title = element_text(size = rel(2)),
      legend.title = element_text(size = rel(1.5)),
      legend.text = element_text(size = rel(1.5))
    ) +
    labs(
      title = paste({{categorical_var}}, "and", {{grouping_var}}, "by Year"),
      x = "Year",
      y = "Percentage",
      color = "Race"
    )
  
  plotly::ggplotly(p) |> 
    layout(
      margin = list(t = 120, r = 20, b = 120, l = 20),
      title = list(x = 0.5,
                   y = 0.925),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 1.1
      )
    )
  
}

#### TAB 2: Forest Plot ####
default_forest_plot <- function(data) {
  
  data$diabetes_indicator <- ifelse(data$`Pregestational Diabetes` == "Yes", 1, 0)
  
  data <- data |> 
    mutate(
      Race = as.factor(Race),
      Race = factor(Race, levels = c("White", "Asian/Pac Islander", "Black", "Other/Multiple Race")),
      Race = relevel(Race, ref = "White"),
      SES = as.factor(SES),
      SES = relevel(SES, ref = "High SES"),
      Education = as.factor(Education),
      Education = relevel(factor(Education, levels = c("<=8th Grade", "9-12th Grade, No Diploma", "High School Graduate/GED", "Some College, No Degree/Associate Degree", "Bachelors Degree or More")), ref = "High School Graduate/GED"),
      Today_Year = as.character(Today_Year)
    )
  
  options(survey.lonely.psu = "adjust")
  
  dat <- svydesign(ids = ~1, weights = ~WTANAL, strata = ~STRATUMC, 
                   nest = TRUE, data = data)
  
  model <- svyglm(
    diabetes_indicator ~ Race + Education + SES + Today_Year,
    design = dat,
    family = quasibinomial(link = "logit")
  )
  
  results <- tidy(model, exponentiate = TRUE, conf.int = TRUE) |> 
    filter(term != "(Intercept)")
  
  results$errorbar_color <- ifelse(results$estimate > 1, "cyan3", "coral1")
  results$point_color <- ifelse(results$estimate > 1, "cyan3", "coral1")
  
  results$term <- gsub("^Race", "Race: ", results$term)
  results$term <- gsub("^Education", "Education: ", results$term)
  results$term <- gsub("^SES", "SES: ", results$term)
  results$term <- gsub("^Today_Year", "Year: ", results$term)
  
  # Convert terms to factors for ordering
  results$term <- factor(results$term, levels = rev(results$term))
  
  # Create OR (95% CI) label
  results$label <- paste0(
    "Variable: ", results$term, "\n",
    "OR: ", sprintf("%.2f", results$estimate), "\n",
    "95% CI: (", sprintf("%.2f", results$conf.low), " - ", sprintf("%.2f", results$conf.high), ")\n",
    "p-value: ", formatC(results$p.value, format = "e", digits = 2)  # Scientific notation for clarity
  )
  
  # Max x-axis value for spacing
  max_x <- max(results$conf.high, na.rm = TRUE) + 1  
  
  x_ticks <- seq(0, max(results$conf.high, na.rm = TRUE) + 1, by = 1)
  
  
  # Create ggplot
  p <- ggplot(results, aes(x = estimate, y = term, text = label)) +
    geom_point(shape = 15, size = 3, aes(color = point_color)) +  # Square markers for OR
    geom_errorbarh(aes(color = errorbar_color, xmin = conf.low, xmax = conf.high), 
                   height = 0.2,
                   linewidth = 1.2) +  # Thicker CIs
    geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.75) +  # OR = 1 reference line
    scale_color_identity() +
    scale_x_continuous(
      breaks = seq(0, ceiling(max(results$conf.high, na.rm = TRUE) + 3), by = 1),
      labels = seq(0, ceiling(max(results$conf.high, na.rm = TRUE) + 3), by = 1)
    ) +
    labs(
      title = "Forest Plot of Survey-Weighted Logistic Regression",
      x = "Odds Ratio (95% CI)",
      y = "Predictor Variable"
    ) +
    theme_minimal() +
    theme(
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
      panel.grid.minor = element_blank(),  
      panel.grid.major.x = element_line(linetype = "dotted", color = "gray90"),
      panel.grid.major.y = element_line(color = "gray90", linetype = "solid"),
      axis.text.y = element_text(margin = margin(r = 5), size = 14, face = "bold"),  
      axis.text.x = element_text(size = 12, face = "bold"),  # Bold x-axis labels
      axis.title.x = element_text(size = 16, face = "bold"),  
      axis.title.y = element_text(size = 16, face = "bold"),  
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "none"
    ) +
    coord_cartesian(xlim = c(0, max(results$conf.high, na.rm = TRUE) + 1))
  
  # Convert to interactive plot with tooltips
  interactive_plot <- ggplotly(p, tooltip = "text")
  
  # Display the plot
  interactive_plot
  
}

forest_plot <- function(data, categorical_var) {
  
  data <- data |> 
    mutate(
      Race = as.factor(Race),
      Race = factor(Race, levels = c("White", "Asian/Pac Islander", "Black", "Other/Multiple Race")),
      Race = relevel(Race, ref = "White"),
      SES = as.factor(SES),
      SES = relevel(SES, ref = "High SES"),
      Education = as.factor(Education),
      Education = relevel(factor(Education, levels = c("<=8th Grade", "9-12th Grade, No Diploma", "High School Graduate/GED", "Some College, No Degree/Associate Degree", "Bachelors Degree or More")), ref = "High School Graduate/GED"),
      Today_Year = as.character(Today_Year)
    )
  
  data <- data |> 
    mutate(
      binary_indicator = if_else(
        !!sym(categorical_var) == "Yes",
        1,
        0
      )
    )
  
  options(survey.lonely.psu = "adjust")
  
  dat <- svydesign(ids = ~1, weights = ~WTANAL, strata = ~STRATUMC, 
                   nest = TRUE, data = data)
  
  model <- svyglm(
    binary_indicator ~ Race + Education + SES + Today_Year,
    design = dat,
    family = quasibinomial(link = "logit")
  )
  
  results <- tidy(model, exponentiate = TRUE, conf.int = TRUE) |> 
    filter(term != "(Intercept)")
  
  results$errorbar_color <- ifelse(results$estimate > 1, "cyan3", "coral1")
  results$point_color <- ifelse(results$estimate > 1, "cyan3", "coral1")
  
  results$term <- gsub("^Race", "Race: ", results$term)
  results$term <- gsub("^Education", "Education: ", results$term)
  results$term <- gsub("^SES", "SES: ", results$term)
  results$term <- gsub("^Today_Year", "Year: ", results$term)
  
  # Convert terms to factors for ordering
  results$term <- factor(results$term, levels = rev(results$term))
  
  # Create OR (95% CI) label
  results$label <- paste0(
    "Variable: ", results$term, "\n",
    "OR: ", sprintf("%.2f", results$estimate), "\n",
    "95% CI: (", sprintf("%.2f", results$conf.low), " - ", sprintf("%.2f", results$conf.high), ")\n",
    "p-value: ", formatC(results$p.value, format = "e", digits = 2)  # Scientific notation for clarity
  )
  
  x_ticks <- seq(0, 5, by = 1)
  
  # Create ggplot
  p <- ggplot(results, aes(x = estimate, y = term, text = label)) +
    geom_point(shape = 15, size = 3, aes(color = point_color)) +  # Square markers for OR
    geom_errorbarh(aes(color = errorbar_color, xmin = conf.low, xmax = conf.high), 
                   height = 0.2,
                   linewidth = 1.2) +  # Thicker CIs
    geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.75) +  # OR = 1 reference line
    scale_color_identity() +
    scale_x_continuous(
      breaks = seq(0, ceiling(max(results$conf.high, na.rm = TRUE) + 3), by = 1),
      labels = seq(0, ceiling(max(results$conf.high, na.rm = TRUE) + 3), by = 1)
    ) +
    labs(
      title = paste(rlang::ensym(categorical_var), "Forest Plot"),
      x = "Odds Ratio (95% CI)",
      y = "Predictor Variable"
    ) +
    theme_minimal() +
    theme(
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
      panel.grid.minor = element_blank(),  
      panel.grid.major.x = element_line(linetype = "dotted", color = "gray90"), 
      panel.grid.major.y = element_line(color = "gray90", linetype = "solid"),
      axis.text.y = element_text(margin = margin(r = 5), size = 14, face = "bold"),  
      axis.text.x = element_text(size = 12, face = "bold"),  # Bold x-axis labels
      axis.title.x = element_text(size = 16, face = "bold"),  
      axis.title.y = element_text(size = 16, face = "bold"),  
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "none"
    ) +
    coord_cartesian(xlim = c(0, max(results$conf.high, na.rm = TRUE) + 1))
  
  p
  
}

forest_plot_detailed <- function(data, categorical_var) {
  
  data <- data |> 
    mutate(
      Detailed_Race = as.factor(Detailed_Race),
      Detailed_Race = relevel(factor(Detailed_Race, levels = c("White", "Asian", "Hawaiian/Oth Pac Islndr", "Black", "Other/Multiple Race")), ref = "White"),
      SES = as.factor(SES),
      SES = relevel(SES, ref = "High SES"),
      Education = as.factor(Education),
      Education = relevel(factor(Education, levels = c("<=8th Grade", "9-12th Grade, No Diploma", "High School Graduate/GED", "Some College, No Degree/Associate Degree", "Bachelors Degree or More")), ref = "High School Graduate/GED"),
      Today_Year = as.character(Today_Year)
    )
  
  data <- data |> 
    mutate(
      binary_indicator = if_else(
        !!sym(categorical_var) == "Yes",
        1,
        0
      )
    )
  
  options(survey.lonely.psu = "adjust")
  
  dat <- svydesign(ids = ~1, weights = ~WTANAL, strata = ~STRATUMC, 
                   nest = TRUE, data = data)
  
  model <- svyglm(
    binary_indicator ~ Detailed_Race + Education + SES + Today_Year,
    design = dat,
    family = quasibinomial(link = "logit")
  )
  
  results <- tidy(model, exponentiate = TRUE, conf.int = TRUE) |> 
    filter(term != "(Intercept)")
  
  results$errorbar_color <- ifelse(results$estimate > 1, "cyan3", "coral1")
  results$point_color <- ifelse(results$estimate > 1, "cyan3", "coral1")
  
  results$term <- gsub("^Detailed_Race", "Detailed Race: ", results$term)
  results$term <- gsub("^Education", "Education: ", results$term)
  results$term <- gsub("^SES", "SES: ", results$term)
  results$term <- gsub("^Today_Year", "Year: ", results$term)
  
  # Convert terms to factors for ordering
  results$term <- factor(results$term, levels = rev(results$term))
  
  # Create OR (95% CI) label
  results$label <- paste0(
    "Variable: ", results$term, "\n",
    "OR: ", sprintf("%.2f", results$estimate), "\n",
    "95% CI: (", sprintf("%.2f", results$conf.low), " - ", sprintf("%.2f", results$conf.high), ")\n",
    "p-value: ", formatC(results$p.value, format = "e", digits = 2)  # Scientific notation for clarity
  )
  
  x_ticks <- seq(0, 5, by = 1)
  
  # Create ggplot
  p <- ggplot(results, aes(x = estimate, y = term, text = label)) +
    geom_point(shape = 15, size = 3, aes(color = point_color)) +  # Square markers for OR
    geom_errorbarh(aes(color = errorbar_color, xmin = conf.low, xmax = conf.high), 
                   height = 0.2,
                   linewidth = 1.2) +  # Thicker CIs
    geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.75) +  # OR = 1 reference line
    scale_color_identity() +
    scale_x_continuous(
      breaks = seq(0, ceiling(max(results$conf.high, na.rm = TRUE) + 3), by = 1),
      labels = seq(0, ceiling(max(results$conf.high, na.rm = TRUE) + 3), by = 1)
    ) +
    labs(
      title = paste(rlang::ensym(categorical_var), "Forest Plot"),
      x = "Odds Ratio (95% CI)",
      y = "Predictor Variable"
    ) +
    theme_minimal() +
    theme(
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
      panel.grid.minor = element_blank(),  
      panel.grid.major.x = element_line(linetype = "dotted", color = "gray90"), 
      panel.grid.major.y = element_line(color = "gray90", linetype = "solid"),
      axis.text.y = element_text(margin = margin(r = 5), size = 14, face = "bold"),  
      axis.text.x = element_text(size = 12, face = "bold"),  # Bold x-axis labels
      axis.title.x = element_text(size = 16, face = "bold"),  
      axis.title.y = element_text(size = 16, face = "bold"),  
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "none"
    ) +
    coord_cartesian(xlim = c(0, max(results$conf.high, na.rm = TRUE) + 1))
  
  p
  
}

default_forest_table <- function(data, categorical_var) {
  
  data <- data |> 
    mutate(
      Detailed_Race = as.factor(Detailed_Race),
      Detailed_Race = relevel(factor(Detailed_Race, levels = c("White", "Asian", "Hawaiian/Oth Pac Islndr", "Black", "Other/Multiple Race")), ref = "White"),
      
      Education = as.factor(Education),
      Education = relevel(factor(Education, levels = c("<=8th Grade", "9-12th Grade, No Diploma", "High School Graduate/GED", "Some College, No Degree/Associate Degree", "Bachelors Degree or More")), ref = "High School Graduate/GED"),
      
      SES = as.factor(SES),
      SES = relevel(factor(SES, levels = c("Low SES", "Middle SES", "High SES")), ref = "Low SES"),
      
      Today_Year = as.character(Today_Year)
      
    )
  
  data <- data |> 
    mutate(
      binary_indicator = if_else(
        `Pregestational Diabetes` == "Yes",
        1,
        0
      )
    )
  
  options(survey.lonely.psu = "adjust")
  
  dat <- svydesign(ids = ~1, weights = ~WTANAL, strata = ~STRATUMC, 
                   nest = TRUE, data = data)
  
  model <- svyglm(
    binary_indicator ~ Detailed_Race + Education + SES + Today_Year,
    design = dat,
    family = quasibinomial(link = "logit")
  )
  
  tbl <- tbl_regression(model, exponentiate = TRUE, label = list(Detailed_Race = "Detailed Race")) |> 
    modify_header(label ~ "**Variable**") |> 
    add_forest() |> 
    gt::tab_header(title = "Pregestational Diabetes Forest Plot Table") |> 
    tab_options(
      table.width = "100%",
      data_row.padding = px(5)
    ) |> 
    tab_style(
      style = list(
        cell_text(style = "italic")
      ),
      locations = gt::cells_body(
        columns = "label",
        rows = label %in% c("Detailed Race", "Education", "SES", "Today_Year")
      )
    ) |> 
    tab_style(
      style = list(
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_body(
        columns = "p.value",
        rows = as.numeric(p.value) < 0.05
      )
    ) |> 
    gt::tab_options(
      table.font.size = px(20)  # Adjust the px() value as needed
    )
  
  tbl
  
}

forest_table <- function(data, categorical_var) {
  
  data <- data |> 
    mutate(
      Race = as.factor(Race),
      Race = relevel(factor(Race, levels = c("White", "Asian/Pac Islander", "Black", "Other/Multiple Race")), ref = "White"),
      
      Education = as.factor(Education),
      Education = relevel(factor(Education, levels = c("<=8th Grade", "9-12th Grade, No Diploma", "High School Graduate/GED", "Some College, No Degree/Associate Degree", "Bachelors Degree or More")), ref = "High School Graduate/GED"),
      
      SES = as.factor(SES),
      SES = relevel(factor(SES, levels = c("Low SES", "Middle SES", "High SES")), ref = "Low SES"),
      
      Today_Year = as.character(Today_Year)
      
    )
  
  data <- data |> 
    mutate(
      binary_indicator = if_else(
        !!sym(categorical_var) == "Yes",
        1,
        0
      )
    )
  
  options(survey.lonely.psu = "adjust")
  
  dat <- svydesign(ids = ~1, weights = ~WTANAL, strata = ~STRATUMC, 
                   nest = TRUE, data = data)
  
  model <- svyglm(
    binary_indicator ~ Race + Education + SES + Today_Year,
    design = dat,
    family = quasibinomial(link = "logit")
  )
  
  tbl <- tbl_regression(model, exponentiate = TRUE) |> 
    modify_header(label ~ "**Variable**") |> 
    add_forest() |> 
    gt::tab_header(title = paste(rlang::ensym(categorical_var), "Forest Plot Table")) |> 
    tab_options(
      table.width = "100%",
      data_row.padding = px(5)
    ) |> 
    tab_style(
      style = list(
        cell_text(style = "italic")
      ),
      locations = gt::cells_body(
        columns = "label",
        rows = label %in% c("Race", "Education", "SES", "Today_Year")
      )
    ) |> 
    tab_style(
      style = list(
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_body(
        columns = "p.value",
        rows = as.numeric(p.value) < 0.05
      )
    ) |> 
    gt::tab_options(
      table.font.size = px(20)  # Adjust the px() value as needed
    )
  
  tbl
  
}

forest_table_detailed <- function(data, categorical_var) {
  
  data <- data |> 
    mutate(
      Detailed_Race = as.factor(Detailed_Race),
      Detailed_Race = relevel(factor(Detailed_Race, levels = c("White", "Asian", "Hawaiian/Oth Pac Islndr", "Black", "Other/Multiple Race")), ref = "White"),
      
      Education = as.factor(Education),
      Education = relevel(factor(Education, levels = c("<=8th Grade", "9-12th Grade, No Diploma", "High School Graduate/GED", "Some College, No Degree/Associate Degree", "Bachelors Degree or More")), ref = "High School Graduate/GED"),
      
      SES = as.factor(SES),
      SES = relevel(factor(SES, levels = c("Low SES", "Middle SES", "High SES")), ref = "Low SES"),
      
      Today_Year = as.character(Today_Year)
      
    )
  
  data <- data |> 
    mutate(
      binary_indicator = if_else(
        !!sym(categorical_var) == "Yes",
        1,
        0
      )
    )
  
  options(survey.lonely.psu = "adjust")
  
  dat <- svydesign(ids = ~1, weights = ~WTANAL, strata = ~STRATUMC, 
                   nest = TRUE, data = data)
  
  model <- svyglm(
    binary_indicator ~ Detailed_Race + Education + SES + Today_Year,
    design = dat,
    family = quasibinomial(link = "logit")
  )
  
  tbl <- tbl_regression(model, exponentiate = TRUE, label = list(Detailed_Race = "Detailed Race")) |> 
    modify_header(label ~ "**Variable**") |> 
    add_forest() |> 
    gt::tab_header(title = paste(rlang::ensym(categorical_var), "Forest Plot Table")) |> 
    tab_options(
      table.width = "100%",
      data_row.padding = px(5)
    ) |> 
    tab_style(
      style = list(
        cell_text(style = "italic")
      ),
      locations = gt::cells_body(
        columns = "label",
        rows = label %in% c("Detailed Race", "Education", "SES", "Today_Year")
      )
    ) |> 
    tab_style(
      style = list(
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_body(
        columns = "p.value",
        rows = as.numeric(p.value) < 0.05
      )
    ) |> 
    gt::tab_options(
      table.font.size = px(20)  # Adjust the px() value as needed
    )
  
  tbl
  
}

phase8_variable_choices <- list(
  "Chronic Health Conditions" = c(
    "Pregestational Diabetes",
    "Chronic Hypertension",
    "Pre-Pregnancy Depression",
    "Pre-Pregnancy Asthma",
    "Gestational Diabetes", 
    "Pregnancy-Induced Hypertension",
    "Gestational Depression", 
    "Gestational Asthma", 
    "Smoked Cigarettes in Last 2 Years",
    "Drank Alcohol in Last 2 Years",
    "Domestic Violence"
  ),
  "Insurance Types" = c(
    "Pre-Pregnancy Insurance: Job",
    "Pre-Pregnancy Insurance: Medicaid",
    "Pre-Pregnancy Insurance: Military",
    "Pre-Pregnancy Insurance: Other",
    "Pre-Pregnancy Insurance: None"
  )#,
  #"Maternal and Infant Factors" = c(
  #  "Mother Transferred",
  #  "Infant Transferred",
  #  "Is Infant Alive Now",
  #  "First_PNC_Visit_Wks_Mnths",
  #  "Postpartum_MH_Depressed_Since_Birth",
  #  "Postpartum_Checkup_For_Self"
  #)
)

phase6_7_8_variable_choices <- list(
  "Chronic Health Conditions" = c(
    "Pregestational Diabetes",
    "Chronic Hypertension",
    "Gestational Diabetes",
    "Smoked Cigarettes in Last 2 Years",
    "Drank Alcohol in Last 2 Years",
    "Domestic Violence"
  ),
  "Insurance Types" = c(
    "Pre-Pregnancy Insurance: Job",
    "Pre-Pregnancy Insurance: Medicaid",
    "Pre-Pregnancy Insurance: Military",
    "Pre-Pregnancy Insurance: Other",
    "Pre-Pregnancy Insurance: None"
  )#,
  #"Maternal and Infant Factors" = c(
   # "Mother Transferred",
    #"Infant Transferred",
    #"Is_Infant_Alive_Now",
    #"First_PNC_Visit_Wks_Mnths",
    #"Postpartum_MH_Depressed_Since_Birth"
  #)
)

all_phases_variable_choices <- list(
  "Chronic Health Conditions" = c(
    "Pregestational Diabetes",
    "Gestational Diabetes",
    "Chronic Hypertension",
    "Domestic Violence"
  )#,
  #"Maternal and Infant Factors" = c(
   # "First_PNC_Visit_Wks_Mnths",
    #"Test"
  )

