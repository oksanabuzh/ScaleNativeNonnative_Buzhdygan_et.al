#' Run a single model
#'
#' This function builds and fits a generalized linear mixed-effects model using
#' the provided data and specified response variable and fixed effects. If the
#' model is overdispersed, it will be refit using a quasibinomial family.
#'
#' @param data The data frame containing the necessary variables for the model.
#' @param response_variable The name of the response variable in the data frame.
#' @param fixed_effects A character vector specifying the names of the fixed
#'   effects variables in the data frame. Default is c("pca1_clima", "pH").
#' @param random_effect The name of the random effect in the model. Default is
#'  "series".
#' @param use_optim A logical value indicating whether to use optimizers for
#'  the model. Default is TRUE.
#'
#' @return A tibble containing the model output, including fixed effects,
#'   response variable, model formula, dispersion factor, R2m, R2c, and model
#'   coefficients.
#'
#' @examples
#' data <- read.csv("data.csv")
#' output <- run_single_model(data,
#'   response_variable = "non_native_percent",
#'   fixed_effects = c("pca1_clima", "pH")
#' )
#' print(output)
#'
#' @importFrom lme4 glmer
#' @importFrom MASS glmmPQL
#' @importFrom car Anova
#' @importFrom MuMIn r.squaredGLMM
#' @importFrom r2glmm r2beta
#' @importFrom dplyr bind_rows filter
#' @importFrom tibble tibble
run_single_model <- function(data_to_model,
                             response_variable,
                             fixed_effects,
                             random_effect = "series",
                             use_optim = TRUE) {
  # Check if the data contains all necessary variables
  necessary_variables <- c(response_variable, fixed_effects)
  if (!all(necessary_variables %in% colnames(data_to_model))) {
    stop(paste0(
      "The data does not contain all necessary variables. Missing: ",
      paste(setdiff(necessary_variables, colnames(data_to_model)), collapse = ", ")
    ))
  }
  # Build the model formula
  model_formula <- paste(
    response_variable, "~", paste(fixed_effects, collapse = " + ")
  )

  # # Warning log to record all the warnings during the run of the function
  warning_log <- character()
  # Use tryCatch to handle errors
  tryCatch(
    {
      withCallingHandlers(
        {
          # Build the binomial model (with or without optimizers)
          if (use_optim) {
            model_1 <- lme4::glmer(
              formula(paste(model_formula, "+ (1 | ", random_effect, ")")),
              data = data_to_model, family = binomial,
              weights = total_species,
              control = lme4::glmerControl(
                optimizer = "bobyqa",
                optCtrl = list(maxfun = 50000)
              )
            )
          } else {
            model_1 <- lme4::glmer(
              formula(paste(model_formula, "+ (1 | ", random_effect, ")")),
              data = data_to_model, family = binomial,
              weights = total_species
            )
          }
          # check model convergence
          model_converged <- performance::check_convergence(model_1, tolerance = 1.3)[1]

          used_model <- "binomial"
          # Check over/underdispersion
          dispersion_ratio <- performance::check_overdispersion(model_1)$dispersion_ratio
          if (dispersion_ratio < 0.4 | dispersion_ratio > 1.6) {
            # If overdispersion, use quasibinomial
            model_1 <- MASS::glmmPQL(
              formula(model_formula),
              random = formula(paste0("~ 1 | ", random_effect)),
              data = data_to_model,
              family = quasibinomial,
              weights = total_species
            )
            used_model <- "quasibinomial"
          }
          # Extract relevant model output ---------------------------------------------

          # Model coefficients (tables look different depending on the model used)
          if (used_model == "binomial") {
            # Estimates and p-values for all variables
            slope <- summary(model_1)$coefficients[, c("Estimate", "Pr(>|z|)")]
            # turn slopes into a table and remove the first row with the intercept
            slope <- slope |>
              as_tibble(rownames = "predictor") |>
              rename_all(~ c("predictor", "slope", "p_value_slope")) |>
              # remove the intercept
              filter(predictor != "(Intercept)")
            # Take Std.Estimate if available
            std_slope <- piecewiseSEM::coefs(model_1) |>
              select(Predictor, Std.Estimate) |>
              rename(predictor = Predictor, std_slope = Std.Estimate)
            # add standardized slope to slope table
            slope <- slope |> left_join(std_slope, by = "predictor")
          } else {
            slope <- summary(model_1)$tTable[, c("Value", "p-value")]
            slope <- slope |>
              as_tibble(rownames = "predictor") |>
              rename_all(~ c("predictor", "slope", "p_value_slope")) |>
              # remove the intercept
              filter(predictor != "(Intercept)")
            # add standardized slope
            slope$std_slope <- NA
          }

          # Chisq and p-value for all
          chisq <- car::Anova(model_1) |>
            as_tibble(rownames = "predictor") |>
            select(predictor, Chisq, `Pr(>Chisq)`) |>
            rename_all(~ c("predictor", "chisq", "p_value_chisq"))

          # r2 take theoretical R2m & R2c
          r2 <- MuMIn::r.squaredGLMM(model_1, envir = environment())["theoretical", ]
          # print(environment() |> ls())
          # print(MuMIn::r.squaredGLMM(model_1, envir = environment()))
            
          # Partial R2 for fixed effects, only for variable of interest
          # keep only Rsq but all variables
          # This can only be run if the model converges. But the conversion criteria are
          # very strict. So we use tryCatch here to catch errors

          r2_part <- tryCatch(
            {
              r2glmm::r2beta(model_1, partial = TRUE, method = "sgv") |>
                as_tibble() |>
                select(Effect, Rsq) |>
                rename(predictor = Effect, r2_partial = Rsq)
            },
            error = function(e) {
              tibble(predictor = slope$predictor, r2_partial = NA)
            }
          )

          # remove the overall model r2
          r2_part <- r2_part |> filter(predictor != "Model")

          # Combine everything in one table
          model_results <- slope |>
            left_join(chisq, by = "predictor") |>
            left_join(r2_part, by = "predictor") |>
            bind_cols(
              r2m = r2["R2m"],
              r2c = r2["R2c"]
            )

          output <- tibble(
            model_res = list(model_results),
            status = "ok",
            model_converged = model_converged,
            model_used = used_model,
            model_raw = list(model_1),
            warning_log = list(warning_log)
          )
          return(output)
        },
        warning = function(w) {
          warning_log <<- c(warning_log, w$message)
          invokeRestart("muffleWarning")
        }
      )
    },
    error = function(e) {
      output <- tibble(
        model_res = NA,
        model_converged = ifelse(exists("model_converged"), model_converged, NA),
        status = e$message,
        model_used = NA,
        model_raw = NA,
        warning_log = list(warning_log)
      )
      return(output)
    }
  )
}

#' Run a single quasibinomial model
#'
#'
#' @param data The data frame containing the necessary variables for the model.
#' @param response_variable The name of the response variable in the data frame.
#' @param fixed_effects A character vector specifying the names of the fixed
#'   effects variables in the data frame. Default is c("pca1_clima", "pH").
#' @param random_effect The name of the random effect in the model. Default is
#'  "series".
#' @param use_optim A logical value indicating whether to use optimizers for
#'  the model. Default is TRUE.
#'
#' @return A tibble containing the model output, including fixed effects,
#'   response variable, model formula, dispersion factor, R2m, R2c, and model
#'   coefficients.
#'
#' @examples
#' data <- read.csv("data.csv")
#' output <- run_single_model(data,
#'   response_variable = "non_native_percent",
#'   fixed_effects = c("pca1_clima", "pH")
#' )
#' print(output)
#'
#' @importFrom lme4 glmer
#' @importFrom MASS glmmPQL
#' @importFrom car Anova
#' @importFrom MuMIn r.squaredGLMM
#' @importFrom r2glmm r2beta
#' @importFrom dplyr bind_rows filter
#' @importFrom tibble tibble
run_single_model_quasi <- function(data_to_model,
                             response_variable,
                             fixed_effects,
                             random_effect = "series",
                             use_optim = TRUE) {
  # Check if the data contains all necessary variables
  necessary_variables <- c(response_variable, fixed_effects)
  if (!all(necessary_variables %in% colnames(data_to_model))) {
    stop(paste0(
      "The data does not contain all necessary variables. Missing: ",
      paste(setdiff(necessary_variables, colnames(data_to_model)), collapse = ", ")
    ))
  }
  # Build the model formula
  model_formula <- paste(
    response_variable, "~", paste(fixed_effects, collapse = " + ")
  )
  
  # # Warning log to record all the warnings during the run of the function
  warning_log <- character()
  # Use tryCatch to handle errors
  tryCatch(
    {
      withCallingHandlers(
        {
          model_converged <- TRUE
          used_model <- "quasibinomial"
          # Check over/underdispersion
            # If overdispersion, use quasibinomial
            model_1 <- MASS::glmmPQL(
              formula(model_formula),
              random = formula(paste0("~ 1 | ", random_effect)),
              data = data_to_model,
              family = quasibinomial,
              weights = total_species
            )
          # Extract relevant model output ---------------------------------------------
          

            slope <- summary(model_1)$tTable[, c("Value", "p-value")]
            slope <- slope |>
              as_tibble(rownames = "predictor") |>
              rename_all(~ c("predictor", "slope", "p_value_slope")) |>
              # remove the intercept
              filter(predictor != "(Intercept)")
            # add standardized slope
            slope$std_slope <- NA
        
          
          # Chisq and p-value for all
          chisq <- car::Anova(model_1) |>
            as_tibble(rownames = "predictor") |>
            select(predictor, Chisq, `Pr(>Chisq)`) |>
            rename_all(~ c("predictor", "chisq", "p_value_chisq"))
          
          # r2 take theoretical R2m & R2c
          r2 <- MuMIn::r.squaredGLMM(model_1, envir = environment())["theoretical", ]
          # print(environment() |> ls())
          # print(MuMIn::r.squaredGLMM(model_1, envir = environment()))
          
          # Partial R2 for fixed effects, only for variable of interest
          # keep only Rsq but all variables
          # This can only be run if the model converges. But the conversion criteria are
          # very strict. So we use tryCatch here to catch errors
          
          r2_part <- tryCatch(
            {
              r2glmm::r2beta(model_1, partial = TRUE, method = "sgv") |>
                as_tibble() |>
                select(Effect, Rsq) |>
                rename(predictor = Effect, r2_partial = Rsq)
            },
            error = function(e) {
              tibble(predictor = slope$predictor, r2_partial = NA)
            }
          )
          
          # remove the overall model r2
          r2_part <- r2_part |> filter(predictor != "Model")
          
          # Combine everything in one table
          model_results <- slope |>
            left_join(chisq, by = "predictor") |>
            left_join(r2_part, by = "predictor") |>
            bind_cols(
              r2m = r2["R2m"],
              r2c = r2["R2c"]
            )
          
          output <- tibble(
            model_res = list(model_results),
            status = "ok",
            model_converged = model_converged,
            model_used = used_model,
            model_raw = list(model_1),
            warning_log = list(warning_log)
          )
          return(output)
        },
        warning = function(w) {
          warning_log <<- c(warning_log, w$message)
          invokeRestart("muffleWarning")
        }
      )
    },
    error = function(e) {
      output <- tibble(
        model_res = NA,
        model_converged = ifelse(exists("model_converged"), model_converged, NA),
        status = e$message,
        model_used = NA,
        model_raw = NA,
        warning_log = list(warning_log)
      )
      return(output)
    }
  )
}

