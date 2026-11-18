#' Interactive selector for choosing an appropriate statistical test.
#' 
#' This is an interactive tool which runs in the console of RStudio, and allows
#' the user to select an appropriate statistical test for their data. This tool
#' is not meant for users that do not possess knowledge of how to successfully
#' run such tests. It is not a substitute for learning!
#' 
#' @author Thomas Warburton
#' 
#' @return A text based response with the statistical method the tool has picked
#' based on the answers of the input of the user.
#' 
#' @export

choose_stat_test <- function() {
  
  if (!interactive()) {
    stop("choose_stat_test() must be run in an interactive R session.", call. = FALSE)
  }
  
  # Helper function to safely get validated user input
  ask_question <- function(prompt_text, options) {
    cat("\n------------------------------------------------------------\n")
    cat(prompt_text, "\n")
    for (i in seq_along(options)) {
      cat(sprintf("  [%d] %s\n", i, options[i]))
    }
    
    choice <- NA
    while (is.na(choice) || !(choice %in% seq_along(options))) {
      input <- readline(prompt = "Enter the number of your choice (or 'q' to quit): ")
      if (tolower(trimws(input)) == "q") {
        stop("Test selection cancelled by user.", call. = FALSE)
      }
      choice <- suppressWarnings(as.integer(trimws(input)))
      if (is.na(choice) || !(choice %in% seq_along(options))) {
        cat("Invalid input. Please enter a number between 1 and", length(options), "\n")
        choice <- NA
      }
    }
    return(choice)
  }
  
  # Helper function to format the final output cleanly
  # 'pkgs' = character vector of non-base packages needed; missing ones trigger an install hint
  print_recommendation <- function(test_name, r_code, notes = NULL, pkgs = NULL) {
    cat("\n============================================================\n")
    cat("RECOMMENDED STATISTICAL TEST:\n")
    cat("  >> ", test_name, "\n\n", sep = "")
    cat("R FUNCTION / SYNTAX:\n")
    cat("  ", r_code, "\n", sep = "")
    if (!is.null(notes)) {
      cat("\nNOTES & ASSUMPTIONS:\n")
      cat("  ", notes, "\n", sep = "")
    }
    if (!is.null(pkgs)) {
      missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
      if (length(missing_pkgs) > 0) {
        cat("\nINSTALL REQUIRED PACKAGE(S):\n")
        cat("  install.packages(c(", paste0("'", missing_pkgs, "'", collapse = ", "), "))\n", sep = "")
      }
    }
    cat("============================================================\n")
    invisible(test_name)
  }
  
  cat("\n=== INTERACTIVE STATISTICAL TEST SELECTOR ===\n")
  
  # STEP 1: Primary Goal
  goal <- ask_question(
    "What is your primary analytical goal?",
    c(
      "Compare groups (differences in means, medians, or distributions)",
      "Examine relationships or associations between variables",
      "Predict an outcome variable from one or more predictors (Regression)",
      "Analyze time-to-event data (Survival Analysis)",
      "Assess agreement, reliability, or dimensionality"
    )
  )
  
  # =========================================================================
  # BRANCH 1: COMPARE GROUPS
  # =========================================================================
  if (goal == 1) {
    outcome_type <- ask_question(
      "What type of outcome (dependent) variable are you comparing?",
      c(
        "Continuous / Interval / Ratio (e.g., height, weight, test scores)",
        "Ordinal (e.g., Likert scale, rankings)",
        "Categorical / Binary (e.g., yes/no, pass/fail, blood type)"
      )
    )
    
    # 1A: Continuous Outcome
    if (outcome_type == 1) {
      num_groups <- ask_question(
        "How many groups or timepoints are you comparing?",
        c(
          "1 group compared against a known population value",
          "2 groups or 2 timepoints",
          "3 or more groups or timepoints",
          "2 or more grouping factors simultaneously (e.g., Treatment AND Gender)"
        )
      )
      
      # 1 Group
      if (num_groups == 1) {
        normality <- ask_question(
          "Is your data approximately normally distributed (or sample size n >= 30)?",
          c("Yes (Parametric)", "No / Heavily skewed (Non-parametric)")
        )
        if (normality == 1) {
          print_recommendation(
            "One-Sample t-test",
            "t.test(x, mu = 0)",
            "Tests whether the sample mean differs significantly from a known value (mu)."
          )
        } else {
          print_recommendation(
            "One-Sample Wilcoxon Signed-Rank Test",
            "wilcox.test(x, mu = 0)",
            "Tests whether the sample median differs from a known value (mu)."
          )
        }
        
        # 2 Groups
      } else if (num_groups == 2) {
        design <- ask_question(
          "Are the observations independent or paired/matched?",
          c(
            "Independent (different subjects in each group)",
            "Paired / Dependent (same subjects measured twice, or matched pairs)"
          )
        )
        normality <- ask_question(
          "Are the data (or paired differences) normally distributed?",
          c("Yes (Parametric)", "No / Outliers present (Non-parametric)")
        )
        
        if (design == 1 && normality == 1) {
          var_equal <- ask_question(
            "Do the two groups have equal variances (homoscedasticity)?",
            c("Yes (check with var.test() or car::leveneTest())", "No / Unsure (Safer default)")
          )
          if (var_equal == 1) {
            print_recommendation(
              "Student's Independent Two-Sample t-test",
              "t.test(y ~ group, data = df, var.equal = TRUE)"
            )
          } else {
            print_recommendation(
              "Welch's Independent Two-Sample t-test",
              "t.test(y ~ group, data = df, var.equal = FALSE)",
              "Does not assume equal variances between groups; generally preferred over Student's t-test."
            )
          }
          
        } else if (design == 1 && normality == 2) {
          spread <- ask_question(
            "Which best describes your two groups?",
            c(
              "Similar spread/shape and similar sample sizes",
              "Unequal variances, different shapes, or unbalanced sample sizes (or unsure)",
              "Very small samples and you want exact/permutation inference",
              "Outliers/skew, but you want to compare typical values using a robust mean"
            )
          )
          if (spread == 1) {
            print_recommendation(
              "Mann-Whitney U Test (Wilcoxon Rank-Sum Test)",
              "wilcox.test(y ~ group, data = df)",
              "Interpret as a location shift only if the distributions have the same shape; otherwise it tests stochastic dominance."
            )
          } else if (spread == 2) {
            print_recommendation(
              "Brunner-Munzel Test",
              "brunnermunzel::brunnermunzel.test(y ~ group, data = df)\n  # Alternative: lawstat::brunner.munzel.test(x, y)",
              "Tests whether P(X < Y) + 0.5*P(X = Y) = 0.5. Does not assume equal variances or shapes, works for ordinal data, and is generally safer than Mann-Whitney under heteroscedasticity. For small n, use brunnermunzel::brunnermunzel.permutation.test().",
              pkgs = "brunnermunzel"
            )
          } else if (spread == 3) {
            print_recommendation(
              "Exact / Permutation Wilcoxon Test",
              "coin::wilcox_test(y ~ group, data = df, distribution = 'exact')\n  # Or: brunnermunzel::brunnermunzel.permutation.test(x, y)",
              "'group' must be a factor. Exact tests avoid reliance on large-sample approximations.",
              pkgs = c("coin", "brunnermunzel")
            )
          } else {
            print_recommendation(
              "Yuen's Trimmed-Mean t-test",
              "WRS2::yuen(y ~ group, data = df, tr = 0.2)",
              "Compares 20% trimmed means; robust to outliers and skew. Paired version: WRS2::yuend(x, y).",
              pkgs = "WRS2"
            )
          }
          
        } else if (design == 2 && normality == 1) {
          print_recommendation(
            "Paired Samples t-test",
            "t.test(before, after, paired = TRUE)",
            "Assumes the differences between pairs are normally distributed."
          )
        } else {
          print_recommendation(
            "Wilcoxon Signed-Rank Test (Paired)",
            "wilcox.test(before, after, paired = TRUE)",
            "Non-parametric alternative to the paired t-test."
          )
        }
        
        # 3+ Groups
      } else if (num_groups == 3) {
        design <- ask_question(
          "Are the observations independent or repeated measures?",
          c(
            "Independent groups (between-subjects)",
            "Repeated measures / Matched subjects (within-subjects)"
          )
        )
        covariate <- ask_question(
          "Do you need to control for a continuous covariate (confounding variable)?",
          c("No", "Yes")
        )
        
        if (covariate == 2 && design == 1) {
          print_recommendation(
            "One-Way ANCOVA (Analysis of Covariance)",
            "aov(y ~ group + covariate, data = df)",
            "Assumes normality of residuals, homogeneity of variance, and homogeneity of regression slopes."
          )
        } else if (covariate == 2 && design == 2) {
          print_recommendation(
            "Linear Mixed-Effects Model with covariate",
            "lme4::lmer(y ~ time + covariate + (1 | subject_id), data = df)",
            "Repeated measures with a covariate are best handled with a mixed model (consider lmerTest for p-values).",
            pkgs = "lme4"
          )
        } else if (design == 1) {
          normality <- ask_question(
            "Are the residuals normally distributed?",
            c("Yes (Parametric)", "No (Non-parametric)")
          )
          if (normality == 1) {
            var_equal <- ask_question(
              "Are variances equal across all groups?",
              c("Yes", "No / Unsure")
            )
            if (var_equal == 1) {
              print_recommendation(
                "One-Way ANOVA (followed by Tukey's HSD post-hoc)",
                "model <- aov(y ~ group, data = df); summary(model); TukeyHSD(model)"
              )
            } else {
              print_recommendation(
                "Welch's One-Way ANOVA (followed by Games-Howell post-hoc)",
                "oneway.test(y ~ group, data = df, var.equal = FALSE)",
                "Use rstatix::games_howell_test() for post-hoc pairwise comparisons.",
                pkgs = "rstatix"
              )
            }
          } else {
            print_recommendation(
              "Kruskal-Wallis Test (followed by Dunn's post-hoc test)",
              "kruskal.test(y ~ group, data = df)",
              "Use dunn.test::dunn.test() or rstatix::dunn_test() for post-hoc comparisons.",
              pkgs = "rstatix"
            )
          }
        } else {
          normality <- ask_question(
            "Are the data normally distributed across timepoints?",
            c("Yes (Parametric)", "No (Non-parametric)")
          )
          if (normality == 1) {
            print_recommendation(
              "Repeated Measures ANOVA (or Linear Mixed-Effects Model)",
              "aov(y ~ time + Error(subject_id/time), data = df)\n  # Or using lme4: lme4::lmer(y ~ time + (1|subject_id), data = df)",
              "Requires sphericity (check with Mauchly's test). Linear mixed models handle missing data better.",
              pkgs = "lme4"
            )
          } else {
            print_recommendation(
              "Friedman Test",
              "friedman.test(y ~ time | subject_id, data = df)",
              "Non-parametric alternative to repeated measures ANOVA."
            )
          }
        }
        
        # Factorial / Multiple Outcomes
      } else if (num_groups == 4) {
        multi_dv <- ask_question(
          "How many dependent (outcome) variables are you testing simultaneously?",
          c("1 dependent variable", "2 or more correlated dependent variables")
        )
        if (multi_dv == 1) {
          print_recommendation(
            "Factorial ANOVA (Two-Way / N-Way ANOVA)",
            "summary(aov(y ~ factorA * factorB, data = df))",
            "Tests main effects of each factor and their interaction effect (*)."
          )
        } else {
          print_recommendation(
            "MANOVA (Multivariate Analysis of Variance)",
            "summary(manova(cbind(y1, y2) ~ group, data = df))",
            "Assumes multivariate normality and homogeneity of covariance matrices."
          )
        }
      }
      
      # 1B: Ordinal Outcome
    } else if (outcome_type == 2) {
      design <- ask_question(
        "What is your group structure?",
        c(
          "2 independent groups",
          "2 paired/matched groups",
          "3 or more independent groups",
          "3 or more repeated measures"
        )
      )
      if (design == 1) {
        print_recommendation(
          "Mann-Whitney U or Brunner-Munzel Test",
          "wilcox.test(y ~ group, data = df)\n  # If spread/shape differs between groups:\n  brunnermunzel::brunnermunzel.test(y ~ group, data = df)\n  # Or model it: MASS::polr(y ~ group, data = df, Hess = TRUE)",
          "Brunner-Munzel is preferred when groups differ in variability. Ordinal logistic regression also gives an effect size (odds ratio). For polr(), y must be an ordered factor.",
          pkgs = "brunnermunzel"
        )
      } else if (design == 2) {
        print_recommendation("Wilcoxon Signed-Rank Test", "wilcox.test(before, after, paired = TRUE)")
      } else if (design == 3) {
        print_recommendation("Kruskal-Wallis Test", "kruskal.test(y ~ group, data = df)")
      } else {
        print_recommendation("Friedman Test", "friedman.test(y ~ time | subject_id, data = df)")
      }
      
      # 1C: Categorical / Binary Outcome
    } else if (outcome_type == 3) {
      design <- ask_question(
        "What is your study design?",
        c(
          "Comparing observed proportions against expected theoretical proportions (1 variable)",
          "Comparing proportions across 2 or more independent groups",
          "Comparing paired/matched binary outcomes (e.g., before vs. after on same subjects)",
          "Comparing 3 or more repeated binary outcomes"
        )
      )
      if (design == 1) {
        print_recommendation(
          "Chi-Square Goodness-of-Fit Test (or Exact Binomial Test)",
          "chisq.test(x = observed_counts, p = expected_probs)\n  # For 2 categories with small N: binom.test(successes, total, p = 0.5)"
        )
      } else if (design == 2) {
        sample_size <- ask_question(
          "What are the expected cell counts in your contingency table?",
          c(
            "All expected cell counts are >= 5 (Large sample)",
            "Any expected cell count is < 5 (Small sample)"
          )
        )
        if (sample_size == 1) {
          print_recommendation(
            "Pearson's Chi-Square Test of Independence",
            "chisq.test(table(df$var1, df$var2))"
          )
        } else {
          print_recommendation(
            "Fisher's Exact Test",
            "fisher.test(table(df$var1, df$var2))",
            "Exact test recommended when sample sizes are small or tables are sparse."
          )
        }
      } else if (design == 3) {
        print_recommendation(
          "McNemar's Test",
          "mcnemar.test(table(df$before, df$after))",
          "Tests for marginal homogeneity in paired 2x2 contingency tables."
        )
      } else {
        print_recommendation(
          "Cochran's Q Test",
          "rstatix::cochran_qtest(df, y ~ time | subject_id)",
          "Extension of McNemar's test for 3 or more matched timepoints/conditions. Data must be in long format. DescTools::CochranQTest is an alternative.",
          pkgs = "rstatix"
        )
      }
    }
    
    # =========================================================================
    # BRANCH 2: RELATIONSHIPS / CORRELATIONS
    # =========================================================================
  } else if (goal == 2) {
    var_types <- ask_question(
      "What types of variables are you correlating?",
      c(
        "Two continuous variables (linear relationship, normally distributed)",
        "Two continuous or ordinal variables (monotonic relationship, non-normal, or outliers)",
        "Two continuous variables while controlling for a third variable",
        "Two categorical variables (association)"
      )
    )
    if (var_types == 1) {
      print_recommendation(
        "Pearson's Product-Moment Correlation (r)",
        "cor.test(df$x, df$y, method = 'pearson')",
        "Measures linear correlation. Sensitive to outliers."
      )
    } else if (var_types == 2) {
      ties <- ask_question(
        "Do you have a very small sample size or many tied ranks?",
        c("No (Standard rank correlation)", "Yes (Small N or many ties)")
      )
      if (ties == 1) {
        print_recommendation(
          "Spearman's Rank Correlation (rho)",
          "cor.test(df$x, df$y, method = 'spearman')"
        )
      } else {
        print_recommendation(
          "Kendall's Tau Rank Correlation",
          "cor.test(df$x, df$y, method = 'kendall')",
          "More robust than Spearman's rho when sample sizes are small or ties are frequent."
        )
      }
    } else if (var_types == 3) {
      print_recommendation(
        "Partial Correlation",
        "ppcor::pcor.test(df$x, df$y, df$z)",
        "Requires the 'ppcor' package. Measures relationship between x and y controlling for z.",
        pkgs = "ppcor"
      )
    } else {
      print_recommendation(
        "Chi-Square Test / Fisher's Exact Test & Cramer's V",
        "chisq.test(table(df$x, df$y))\n  # Effect size: rcompanion::cramerV(table(df$x, df$y))",
        pkgs = "rcompanion"
      )
    }
    
    # =========================================================================
    # BRANCH 3: REGRESSION / PREDICTION
    # =========================================================================
  } else if (goal == 3) {
    dv_type <- ask_question(
      "What is the nature of your outcome (dependent) variable?",
      c(
        "Continuous (unbounded, normally distributed residuals)",
        "Binary (2 categories: 0/1, Yes/No)",
        "Nominal (3 or more unordered categories)",
        "Ordinal (3 or more ordered categories)",
        "Count data (non-negative integers: 0, 1, 2, 3...)",
        "Clustered / Longitudinal / Hierarchical continuous data"
      )
    )
    if (dv_type == 1) {
      print_recommendation(
        "Ordinary Least Squares (OLS) Linear Regression",
        "model <- lm(y ~ x1 + x2, data = df); summary(model)",
        "Check assumptions (linearity, normality of residuals, homoscedasticity) using plot(model)."
      )
    } else if (dv_type == 2) {
      print_recommendation(
        "Binary Logistic Regression",
        "model <- glm(y ~ x1 + x2, data = df, family = binomial(link = 'logit'))\n  summary(model); exp(coef(model)) # Odds ratios"
      )
    } else if (dv_type == 3) {
      print_recommendation(
        "Multinomial Logistic Regression",
        "nnet::multinom(y ~ x1 + x2, data = df)",
        "Requires the 'nnet' package.",
        pkgs = "nnet"
      )
    } else if (dv_type == 4) {
      print_recommendation(
        "Ordinal Logistic Regression (Proportional Odds Model)",
        "MASS::polr(as.ordered(y) ~ x1 + x2, data = df, Hess = TRUE)",
        "Requires the 'MASS' package. Assumes proportional odds across thresholds.",
        pkgs = "MASS"
      )
    } else if (dv_type == 5) {
      dispersion <- ask_question(
        "Is your count data overdispersed (variance much larger than the mean) or full of excess zeros?",
        c(
          "No (Variance approximately equals Mean)",
          "Yes, overdispersed (Variance > Mean)",
          "Yes, excessive number of zeros"
        )
      )
      if (dispersion == 1) {
        print_recommendation(
          "Poisson Regression",
          "glm(y ~ x1 + x2, data = df, family = poisson(link = 'log'))"
        )
      } else if (dispersion == 2) {
        print_recommendation(
          "Negative Binomial Regression",
          "MASS::glm.nb(y ~ x1 + x2, data = df)",
          "Handles overdispersion better than standard Poisson regression.",
          pkgs = "MASS"
        )
      } else {
        print_recommendation(
          "Zero-Inflated Poisson / Negative Binomial Regression",
          "pscl::zeroinfl(y ~ x1 + x2 | z1, data = df, dist = 'negbin')",
          "Requires the 'pscl' package.",
          pkgs = "pscl"
        )
      }
    } else {
      print_recommendation(
        "Linear Mixed-Effects Model (Multilevel / Hierarchical Model)",
        "lme4::lmer(y ~ x1 + x2 + (1 | cluster_id), data = df)",
        "Use glmer(..., family = binomial) for binary clustered outcomes.",
        pkgs = "lme4"
      )
    }
    
    # =========================================================================
    # BRANCH 4: SURVIVAL ANALYSIS
    # =========================================================================
  } else if (goal == 4) {
    surv_goal <- ask_question(
      "What do you want to do with your time-to-event data?",
      c(
        "Estimate survival curves and compare survival between groups",
        "Model the effect of multiple covariates (continuous or categorical) on hazard rate"
      )
    )
    if (surv_goal == 1) {
      print_recommendation(
        "Kaplan-Meier Estimator & Log-Rank Test",
        "library(survival)\n  survdiff(Surv(time, status) ~ group, data = df)\n  plot(survfit(Surv(time, status) ~ group, data = df))",
        "Assumes non-informative censoring. 'status' should be 1 = event, 0 = censored.",
        pkgs = "survival"
      )
    } else {
      print_recommendation(
        "Cox Proportional Hazards Regression",
        "library(survival)\n  model <- coxph(Surv(time, status) ~ x1 + x2, data = df)\n  summary(model); cox.zph(model) # Check proportional hazards assumption",
        "Semi-parametric model assuming hazard ratios remain constant over time.",
        pkgs = "survival"
      )
    }
    
    # =========================================================================
    # BRANCH 5: AGREEMENT, RELIABILITY & DIMENSIONALITY
    # =========================================================================
  } else if (goal == 5) {
    special_goal <- ask_question(
      "Which specific task applies to your data?",
      c(
        "Inter-rater reliability between 2 raters on categorical ratings",
        "Inter-rater reliability among 3+ raters on categorical ratings",
        "Inter-rater reliability on continuous measurements",
        "Internal consistency of a multi-item survey/scale",
        "Reduce many correlated continuous variables into fewer components/factors"
      )
    )
    if (special_goal == 1) {
      print_recommendation(
        "Cohen's Kappa (or Weighted Kappa for ordinal ratings)",
        "irr::kappa2(df[, c('rater1', 'rater2')], weight = 'unweighted')",
        "Use weight = 'squared' or 'equal' for ordinal categories.",
        pkgs = "irr"
      )
    } else if (special_goal == 2) {
      print_recommendation(
        "Fleiss' Kappa",
        "irr::kappam.fleiss(df[, c('rater1', 'rater2', 'rater3')])",
        "Requires the 'irr' package.",
        pkgs = "irr"
      )
    } else if (special_goal == 3) {
      print_recommendation(
        "Intraclass Correlation Coefficient (ICC)",
        "irr::icc(df[, c('rater1', 'rater2')], model = 'twoway', type = 'agreement', unit = 'single')",
        "Requires the 'irr' or 'psych' package.",
        pkgs = "irr"
      )
    } else if (special_goal == 4) {
      print_recommendation(
        "Cronbach's Alpha (or McDonald's Omega)",
        "psych::alpha(df[, c('item1', 'item2', 'item3')])",
        "Requires the 'psych' package.",
        pkgs = "psych"
      )
    } else {
      print_recommendation(
        "Principal Component Analysis (PCA) or Exploratory Factor Analysis (EFA)",
        "# PCA:\n  prcomp(df, scale. = TRUE)\n  # EFA:\n  factanal(df, factors = 3, rotation = 'varimax')",
        "Use PCA for variance reduction; use EFA to uncover latent constructs."
      )
    }
  }
}