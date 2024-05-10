#' plotModel
#'
#' @param data data.frame input
#' @param model rstanarm model object
#' @param col_faction character string representing column of faction labels
#' @param col_wins character string representing column of wins
#' @param col_total_games character string representing column of total games
#' @param plot_dim clientData from session
#'
#' @return plotly object
#' @export
#'
#' @examples
plotModel <- function(data
                      ,model
                      ,col_faction
                      ,col_wins
                      ,col_total_games
                      ,plot_dim) {

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

  # Gather posterior information
  posterior_draws <- as.data.frame(model) %>%
    dplyr::mutate(draw = 1:8000) %>%
    tidyr::pivot_longer(-draw, names_to = "faction", values_to = "win_rate") %>%
    dplyr::mutate(
      win_rate_p = plogis(win_rate) * 100
      ,!!rlang::sym(col_faction) := stringr::str_remove(faction, col_faction)
    )

  posterior_summary <- posterior_draws %>%
    dplyr::group_by(!!rlang::sym(col_faction)) %>%
    dplyr::summarise(
      median = round(median(win_rate_p), 1)
      ,lwr = round(quantile(win_rate_p, 0.055), 1)
      ,upr = round(quantile(win_rate_p, 0.945), 1)
    ) |>
    dplyr::arrange(-median)

  # Get faction ordered by highest win rate
  faction_order <- posterior_summary[[col_faction]]
  posterior_draws[[col_faction]] <- factor(posterior_draws[[col_faction]], levels = faction_order)

  # Make plot
  p <- ggplot2::ggplot(posterior_draws, ggplot2::aes(x = !!rlang::sym(col_faction)))+
    ggplot2::geom_hline(yintercept = 50, linetype = "dashed", alpha = 0.5)+
    ggplot2::geom_violin(
      ggplot2::aes(y = win_rate_p, fill = !!rlang::sym(col_faction), colour = !!rlang::sym(col_faction))
      ,alpha = 0.5
      ,draw_quantiles = c(0.055, 0.5, 0.945)
      ,trim = TRUE
      ,adjust = 1.5
    )+
    ggplot2::geom_point(data = data_summary, ggplot2::aes(y = win_rate * 100), alpha = 0.5)+
    plotTheme(font_size = 10)+
    ggplot2::ylab("Win rate (%)")+
    ggplot2::ylim(c(0, 100))+
    ggplot2::theme(
      axis.text.x =  ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
      ,axis.title.x =  ggplot2::element_blank()
      ,legend.position = "none"
    )

  gg <- plotly::ggplotly(p, width = plot_dim$output_pid_width, height = plot_dim$output_pid_height * 3)

  # geom_violin - loop across all elements of col_faction within the plotly object list
  num_factions <- length(data_summary[[col_faction]])
  hover_str <- paste0(
    "<b>", col_faction, " =</b> ", posterior_summary[[col_faction]], "\n"
    ,"<b>Posterior median win rate</b> = ", posterior_summary$median, "%\n"
    ,"<b>89% credible interval</b> = ", posterior_summary$lwr, "%, ", posterior_summary$upr, "%"
  )

  # geom_intercept
  gg$x$data[[1]]$text <- NA

  # geom_violin
  for (i in 2:(num_factions + 1)) {
    gg$x$data[[i]]$text <- NA
    gg$x$data[[i]]$text <- hover_str[i - 1]
  }

  # geom_point
  gg$x$data[[length(gg$x$data)]]$text <- paste0(
    "<b>", col_faction, " =</b> ", data_summary[[col_faction]], "\n"
    ,"<b>Raw win rate</b> = ", round(data_summary$win_rate * 100, 1), "%\n"
    ,"<b>Total games</b> = ", data_summary$total_games
  )

  return(gg)


  ## Legacy code for brms models
  # data_summary <- data %>%
  #   dplyr::group_by(!!rlang::sym(col_faction)) %>%
  #   dplyr::summarise(
  #     total_wins = sum(!!rlang::sym(col_wins))
  #     ,total_games = sum(!!rlang::sym(col_total_games))
  #   ) %>%
  #   dplyr::mutate(
  #     win_rate = total_wins / total_games
  #     ,faction = stringr::str_remove_all(!!rlang::sym(col_faction), " ") %>%
  #       stringr::str_remove_all("[[:punct:]]")
  #   )

  # posterior_draws <- brms::as_draws_df(model) %>%
  #   dplyr::select(-.draw) %>%
  #   tidyr::pivot_longer(-c(.chain, .iteration), names_to = "faction", values_to = "win_rate") %>%
  #   dplyr::filter(!faction %in% c("lp__", "lprior")) %>%
  #   dplyr::mutate(
  #     win_rate_p = plogis(win_rate) * 100
  #     ,faction = stringr::str_remove(faction, paste0("b_", col_faction))
  #   ) %>%
  #   dplyr::left_join(dplyr::select(data_summary, faction, !!rlang::sym(col_faction)), by = "faction")

}

