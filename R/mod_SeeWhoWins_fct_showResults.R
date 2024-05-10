#' showResults
#'
#' @param data data.frame input
#' @param model rstanarm model object
#' @param col_faction character string representing column of values
#' @param col_wins character string representing column of wins
#' @param col_total_games character string representing column of total games
#' @param table_dim clientData from session
#'
#' @return list containing: (i) DT data table, (ii) shiny box
#' @export
#'
#' @examples
showResults <- function(data
                         ,model
                         ,col_faction
                         ,col_wins
                         ,col_total_games
                         ,table_dim) {

  # Basic model checks
  if (any(as.data.frame(model$stan_summary)$Rhat > 1.1) || any(as.data.frame(model$stan_summary)$n_eff < 100)) {
    value_check <- "Fail"
    sub_check <- "Something has gone wrong with the Bayesian model. Interpret results with caution."
    colour_check <- "red"
    icon_check <- "triangle-exclamation"
  } else {
    value_check <- "Pass"
    sub_check <- "Bayesian model checks passed"
    colour_check <- "green"
    icon_check <- "circle-check"
  }

  # Summarise point estimates from data
  data_summary <- data %>%
    dplyr::group_by(!!rlang::sym(col_faction)) %>%
    dplyr::summarise(
      total_wins = sum(!!rlang::sym(col_wins))
      ,total_games = sum(!!rlang::sym(col_total_games))
    ) %>%
    dplyr::mutate(
      win_rate = total_wins / total_games
    )

  # Generate DT data table to show results
  posterior_summary <- as.data.frame(model) %>%
    dplyr::mutate(draw = 1:8000) %>%
    tidyr::pivot_longer(-draw, names_to = "faction", values_to = "win_rate") %>%
    dplyr::mutate(
      win_rate_p = plogis(win_rate) * 100
      ,!!rlang::sym(col_faction) := stringr::str_remove(faction, col_faction)
    ) %>%
    dplyr::select(-faction) %>%
    dplyr::group_by(!!rlang::sym(col_faction)) %>%
    dplyr::summarise(
      `Posterior win rate (%)` = round(median(win_rate_p), 1)
      ,`89% credible interval` = paste0(
        "(", round(quantile(win_rate_p, 0.055), 1), ", "
        ,round(quantile(win_rate_p, 0.945), 1), ")"
      )
    )

  data_table <- DT::datatable(
    posterior_summary
    ,extensions = list("Scroller")
    ,options = list(
      scrollY = as.character(table_dim$output_pid_height * 0.5)
      ,scrollCollapse = TRUE
      ,paging = FALSE
      ,pageLength = 10
    )
    ,rownames = FALSE
    # ,fillContainer = TRUE
  )

  # Generate value box for model check
  box <- valueBox(
    value = value_check
    ,color = colour_check
    ,subtitle = sub_check
    ,icon = icon(icon_check)
    ,width = 12
  )

  return(list(data_table, box))

}
