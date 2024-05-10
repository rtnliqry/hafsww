#' fitModel
#'
#' @param data data.frame input
#' @param col_faction character string representing column of faction labels
#' @param col_wins character string representing column of wins
#' @param col_total_games character string representing column of total games
#'
#' @return rstanarm model object
#' @export
#'
#' @examples
fitModel <- function(data
                     ,col_faction
                     ,col_wins
                     ,col_total_games) {

  # Load rstanarm
  require(rstanarm)

  # Mutate the outcome variable to integer
  if (length(as.character(unique(data[[col_faction]]))) < 2) {
    stop("Your data contain < 2 factions! Edit your data and try again.")
  }

  # Coerce col_faction column to factor and calculate win_rate
  data <- data %>%
    dplyr::mutate(
      !!rlang::sym(col_faction) := as.factor(!!rlang::sym(col_faction))
      ,win_rate = !!rlang::sym(col_wins) / !!rlang::sym(col_total_games)
    )

  # Make formula
  form <- as.formula(paste0("win_rate ~ 0 + ", col_faction))
  environment(form) <- environment()
  model_weights <- data[[col_total_games]]

  fit <- rstanarm::stan_glm(
    formula = form
    ,data = data
    ,family = binomial(link = "logit")
    ,seed = 1234
    ,cores = 1
    ,iter = 4000
    ,adapt_delta = 0.95
    ,prior = rstanarm::normal(0, 0.5)
    ,weights = model_weights
  )
  return(fit)

  ## Legacy code using brms (uses too much memory for shinyapps.io)
  # require(brms)

  # form <- paste0(col_wins, " | trials(", col_total_games, ") ~ 0 + ", col_faction)

  # fit <- brms::brm(
  #   formula = form
  #   ,data = data
  #   ,family = binomial
  #   ,prior = prior(normal(0, 0.5), class = "b")
  #   ,seed = 1234 # for reproducibility
  #   ,iter = 4000
  #   ,cores = 4
  #   ,refresh = 0
  #   ,control = list(
  #     adapt_delta = 0.99
  #     ,max_treedepth = 15
  #   )
  # )

}
