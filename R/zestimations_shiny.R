zestimations.shiny <- function(model) {
  # Required packages
  if (!requireNamespace("broom", quietly = TRUE)) stop("Please install 'broom'")
  if (!requireNamespace("survival", quietly = TRUE)) stop("Please install 'survival'")

  model_class <- class(model)[1]

  # Detect model type
  if (model_class == "lm") {
    model_type <- "lm"
    effect_label <- "Beta"
    trans_fun <- identity
  } else if (model_class == "glm" && family(model)$family == "binomial") {
    model_type <- "logistic"
    effect_label <- "OR"
    trans_fun <- exp
  } else if (model_class == "coxph") {
    model_type <- "cox"
    effect_label <- "HR"
    trans_fun <- exp
  } else {
    stop("Model must be lm, glm(binomial), or coxph")
  }

  # Get adjusted results
  adj <- broom::tidy(model, conf.int = TRUE, exponentiate = (model_type != "lm"))
  adj <- adj[adj$term != "(Intercept)", c("term", "estimate", "conf.low", "conf.high", "p.value")]

  # Prepare crude results
  data <- model.frame(model)
  response <- all.vars(formula(model))[1]
  predictors <- setdiff(all.vars(formula(model)), response)

  crude_list <- lapply(predictors, function(var) {
    form <- as.formula(paste(response, "~", var))
    if (model_type == "lm") {
      fit <- lm(form, data = data)
    } else if (model_type == "logistic") {
      fit <- glm(form, data = data, family = binomial)
    } else if (model_type == "cox") {
      fit <- survival::coxph(form, data = data)
    }
    res <- broom::tidy(fit, conf.int = TRUE, exponentiate = (model_type != "lm"))
    res <- res[res$term != "(Intercept)", c("term", "estimate", "conf.low", "conf.high")]
    return(res)
  })

  crude <- do.call(rbind, crude_list)

  # Merge crude and adjusted results
  merged <- merge(crude, adj, by = "term", suffixes = c("_crude", "_adj"))

  # Format outputs
  merged$crude_fmt <- sprintf("%.3f (%.3f, %.3f)",
                              merged$estimate_crude, merged$conf.low_crude, merged$conf.high_crude)
  merged$adj_fmt <- sprintf("%.3f (%.3f, %.3f)",
                            merged$estimate_adj, merged$conf.low_adj, merged$conf.high_adj)
  merged$p_fmt <- ifelse(merged$p.value < 0.001, "<0.001", sprintf("%.3f", merged$p.value))

  out <- merged[, c("term", "crude_fmt", "adj_fmt", "p_fmt")]
  names(out) <- c("Variable",
                  paste(effect_label, "(Crude, 95% CI)"),
                  paste(effect_label, "(Adjusted, 95% CI)"),
                  "P-value")

  print(out)
}

