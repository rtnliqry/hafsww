#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr "%>%"
#' @noRd
app_server <- function(input, output, session) {

  # General functions ----
  window_height <- reactive({ifelse(is.null(input$height), 0, as.numeric(input$height))})
  window_width <- reactive({ifelse(is.null(input$width), 0, as.numeric(input$width))})

  # Instructions module ----
  information_tab <- mod_Information_server("information_tab")

  # Imprecision module ----
  see_who_wins_tab <- mod_SeeWhoWins_server("see_who_wins_tab")

}
