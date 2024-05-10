#' Information UI Function
#'
#' @description A shiny Module for the "Information" tab
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Information_ui <- function(id) {

  ns <- NS(id)

  # UI elements
  tagList(

    fluidRow(
      box(
        title = "Have a fight, see who wins!"
        ,solidHeader = TRUE
        ,status = "primary"
        ,collapsible = TRUE
        ,width = 6
        ,htmlOutput(ns("instructions"))

      )
      ,box(
        title = "Win rate prior distribution"
        ,solidHeader = TRUE
        ,status = "primary"
        ,collapsible = TRUE
        ,width = 4
        ,plotOutput(ns("prior_distribution"))
      )
    )
  ) # ends tagList

}

#' Information Server Functions
#'
#' @noRd
mod_Information_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$instructions <- renderUI({
      instructions() # separate function containing raw HTML
    })

    output$prior_distribution <- renderPlot({

      df_prior <- data.frame(x = seq(-3, 3, length.out = 500)) %>%
        dplyr::mutate(
          y_density = dnorm(x, mean = 0, sd = 0.5)
          ,x_plogis = plogis(x)
        )

      p <- ggplot2::ggplot(df_prior, ggplot2::aes(x = x_plogis * 100, y = y_density))+
        ggplot2::geom_area(fill = "#FF5733", alpha = 0.75)+
        ggplot2::geom_line(colour = "#581845", alpha = 0.75, linewidth = 2)+
        ggplot2::geom_vline(xintercept = 50, colour = "black", linewidth = 1, alpha = 0.75)+
        plotTheme(12)+
        ggplot2::xlab("Win rate (%)")+
        ggplot2::ylab("Density (A.U.)")+
        ggplot2::xlim(c(0,100))

      return(p)

    })

  })

}
