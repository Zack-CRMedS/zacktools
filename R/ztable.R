ztable <- function(data, x, y = NULL, digits = list(stats = 1, pval = 3)) {
  if (is.numeric(x)) x <- names(data)[x]

  # Handle no stratification case
  stratify <- !is.null(y) && y %in% names(data)
  if (stratify) {
    data[[y]] <- as.factor(data[[y]])
    group_levels <- levels(data[[y]])
    n_groups <- length(group_levels)
    if (n_groups < 2) stop("Stratifying variable must have at least 2 levels.")
    group_sizes <- table(data[[y]])
  }

  output <- list()
  test_map <- list()
  test_order <- character()

  get_superscript <- function(test_name) {
    if (!test_name %in% names(test_map)) {
      test_order <<- c(test_order, test_name)
      test_map[[test_name]] <<- letters[length(test_order)]
    }
    return(test_map[[test_name]])
  }

  large_data <- nrow(data) > 5000

  for (var in x) {
    var_data <- data[[var]]
    n_non_missing <- sum(!is.na(var_data))
    var_label <- if (n_non_missing < nrow(data)) {
      sprintf("%s (n = %d)", var, n_non_missing)
    } else var

    if (is.numeric(var_data)) {
      # Determine normality
      normal <- TRUE
      if (!large_data) {
        if (stratify) {
          for (grp in group_levels) {
            grp_vals <- na.omit(data[data[[y]] == grp, var])
            if (length(grp_vals) < 3 || shapiro.test(grp_vals)$p.value <= 0.05) {
              normal <- FALSE
              break
            }
          }
        } else {
          vals <- na.omit(var_data)
          if (length(vals) < 3 || shapiro.test(vals)$p.value <= 0.05) {
            normal <- FALSE
          }
        }
      }

      if (normal) {
        if (stratify) {
          stats <- data %>%
            dplyr::group_by(!!rlang::sym(y)) %>%
            dplyr::summarise(mean = mean(!!rlang::sym(var), na.rm = TRUE),
                             sd = sd(!!rlang::sym(var), na.rm = TRUE), .groups = "drop")
        }
        total_mean <- mean(var_data, na.rm = TRUE)
        total_sd <- sd(var_data, na.rm = TRUE)

        row <- data.frame(
          Variable = var_label,
          Total = sprintf(paste0("%.", digits$stats, "f ± %.", digits$stats, "f"), total_mean, total_sd),
          stringsAsFactors = FALSE
        )

        if (stratify) {
          if (n_groups == 2) {
            test_name <- "t-test"
            pval <- t.test(as.formula(paste(var, "~", y)), data = data)$p.value
          } else {
            test_name <- "ANOVA"
            pval <- summary(aov(as.formula(paste(var, "~", y)), data = data))[[1]][["Pr(>F)"]][1]
          }
          sup <- get_superscript(test_name)

          for (i in seq_len(n_groups)) {
            row[[group_levels[i]]] <- sprintf(paste0("%.", digits$stats, "f ± %.", digits$stats, "f"),
                                              stats$mean[i], stats$sd[i])
          }
          row[["P-value"]] <- paste0(formatC(pval, format = "f", digits = digits$pval), sup)
        }

        output[[length(output) + 1]] <- row

      } else {
        if (stratify) {
          stats <- data %>%
            dplyr::group_by(!!rlang::sym(y)) %>%
            dplyr::summarise(median = median(!!rlang::sym(var), na.rm = TRUE),
                             q1 = quantile(!!rlang::sym(var), 0.25, na.rm = TRUE),
                             q3 = quantile(!!rlang::sym(var), 0.75, na.rm = TRUE), .groups = "drop")
        }
        total_median <- median(var_data, na.rm = TRUE)
        total_q1 <- quantile(var_data, 0.25, na.rm = TRUE)
        total_q3 <- quantile(var_data, 0.75, na.rm = TRUE)

        row <- data.frame(
          Variable = var_label,
          Total = sprintf(paste0("%.", digits$stats, "f (%.", digits$stats, "f, %.", digits$stats, "f)"),
                          total_median, total_q1, total_q3),
          stringsAsFactors = FALSE
        )

        if (stratify) {
          if (n_groups == 2) {
            test_name <- "Mann-Whitney U test"
            pval <- wilcox.test(as.formula(paste(var, "~", y)), data = data)$p.value
          } else {
            test_name <- "Kruskal-Wallis test"
            pval <- kruskal.test(as.formula(paste(var, "~", y)), data = data)$p.value
          }
          sup <- get_superscript(test_name)

          for (i in seq_len(n_groups)) {
            row[[group_levels[i]]] <- sprintf(paste0("%.", digits$stats, "f (%.", digits$stats, "f, %.", digits$stats, "f)"),
                                              stats$median[i], stats$q1[i], stats$q3[i])
          }
          row[["P-value"]] <- paste0(formatC(pval, format = "f", digits = digits$pval), sup)
        }

        output[[length(output) + 1]] <- row
      }

    } else {
      total_counts <- table(data[[var]])
      total_props <- prop.table(total_counts) * 100

      if (stratify) {
        tbl <- table(data[[var]], data[[y]])
        props <- prop.table(tbl, margin = 2) * 100
        if (any(tbl < 5)) {
          test_name <- "Fisher's exact test"
          test <- fisher.test(tbl, simulate.p.value = TRUE)
        } else {
          test_name <- "Chi-squared test"
          test <- chisq.test(tbl)
        }
        sup <- get_superscript(test_name)

        first_row <- data.frame(
          Variable = var_label,
          Total = "",
          stringsAsFactors = FALSE
        )
        for (i in seq_len(n_groups)) first_row[[group_levels[i]]] <- ""
        first_row[["P-value"]] <- paste0(formatC(test$p.value, format = "f", digits = digits$pval), sup)
        output[[length(output) + 1]] <- first_row

        for (level in rownames(tbl)) {
          row <- data.frame(
            Variable = paste0("  ", level),
            Total = sprintf(paste0("%d (%.", digits$stats, "f)"), total_counts[level], total_props[level]),
            stringsAsFactors = FALSE
          )
          for (i in seq_len(n_groups)) {
            row[[group_levels[i]]] <- sprintf(paste0("%d (%.", digits$stats, "f)"),
                                              tbl[level, group_levels[i]], props[level, group_levels[i]])
          }
          row[["P-value"]] <- ""
          output[[length(output) + 1]] <- row
        }

      } else {
        row <- data.frame(
          Variable = var_label,
          Total = "",
          stringsAsFactors = FALSE
        )
        output[[length(output) + 1]] <- row

        for (level in names(total_counts)) {
          output[[length(output) + 1]] <- data.frame(
            Variable = paste0("  ", level),
            Total = sprintf(paste0("%d (%.", digits$stats, "f)"), total_counts[level], total_props[level]),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  result <- do.call(rbind, output)

  # Add total n and group ns to column names
  names(result)[names(result) == "Total"] <- sprintf("Total (n = %d)", nrow(data))
  if (stratify) {
    for (i in seq_along(group_levels)) {
      names(result)[names(result) == group_levels[i]] <- sprintf("%s (n = %d)", group_levels[i], group_sizes[i])
    }
  }

  footnotes_text <- if (stratify) {
    paste0(mapply(function(letter, test) paste0(letter, " = ", test),
                  letters[seq_along(test_order)], test_order),
           collapse = "; ")
  } else ""

  list(
    table = knitr::kable(result),
    footnotes = footnotes_text
  )
}
