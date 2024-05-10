#' Imprecision UI Function
#'
#' @description A shiny Module for the "Imprecision" tab
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SeeWhoWins_ui <- function(id) {

  ns <- NS(id)

  # UI elements
  tagList(

    tabsetPanel(
      id = ns("see_who_wins_tab")

      # Data input
      ,tabPanel(
        title = "Data input"
        ,p()
        ,fluidRow(
          box(
            title = "Data input"
            ,solidHeader = TRUE
            ,collapsible = TRUE
            ,status = "primary"
            ,width = 4
            ,fileInput(
              inputId = ns("input_file")
              ,label = "Select your input file containing your data:"
              ,accept = c(".csv", ".xls", ".xlsx")
              ,multiple = FALSE
            )
            ,checkboxInput(
              inputId = ns("header")
              ,label = "Are the column headers in row 1?"
              ,value = TRUE
            )
            # ,textInput(
            #   inputId = ns("war_game_name")
            #   ,label = "Enter your game's name:"
            #   ,placeholder = "e.g., 'Warhammer 40k'"
            # )
          )
        )
        ,fluidRow(
          conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true")
            ,box(
              title = "Your data"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,DT::dataTableOutput(ns("data_table"))
              ,width = 6
            )
          )
          ,conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true")
            ,box(
              title = "Column selection"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,selectInput(
                inputId = ns("col_faction")
                ,label = "Select the column that represents the teams/players/factions:"
                ,choices = ""
              )
              ,selectInput(
                inputId = ns("col_wins")
                ,label = "Select the column that represents the number of wins:"
                ,choices = ""
              )
              ,selectInput(
                inputId = ns("col_total_games")
                ,label = "Select the column that represents the number of games played:"
                ,choices = ""
              )
              ,actionButton(
                inputId = ns("run_model")
                ,label = "Fit model(s)"
                ,icon = icon("play")
                ,width = "100%"
              )
              ,width = 3
            )
          )
        )
      )

      # Plots
      ,tabPanel(
        title = "Results"
        ,value = "see_who_wins_tab_plots"
        ,p()
        ,conditionalPanel(
          condition = paste0("input[\'", ns("run_model"), "\'] != 0")
          ,fluidRow(

            # Column 1
            box(
              title = "Model fit"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,shinycssloaders::withSpinner(
                uiOutput(ns("plots"))
                ,type = 6
              )
              ,shinycssloaders::withSpinner(
                uiOutput(ns("model_checks"))
                ,type = 6
              )
            )

            # Column 2
            ,box(
              title = "Results summary"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,shinycssloaders::withSpinner(
                DT::dataTableOutput(ns("results"))
                ,type = 6
              )
            )
          )
        )
      )

    ) # closes tabSetPanel
  ) # closes tagList

}

#' Imprecision Server Functions
#'
#' @noRd
mod_SeeWhoWins_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    cdata <- session$clientData

    output$file <- reactive({
      return(!is.null(input$input_file))
    })
    outputOptions(output, "file", suspendWhenHidden = FALSE)

    # Update selectInputs
    observe({
      updateSelectInput(
        session
        ,"col_faction"
        ,choices = colnames(df())
        ,selected = ""
      )
      updateSelectInput(
        session
        ,"col_wins"
        ,choices = colnames(df())
        ,selected = ""
      )
      updateSelectInput(
        session
        ,"col_total_games"
        ,choices = colnames(df())
        ,selected = ""
      )
    })

    # Data checks
    observeEvent(input$input_file, {
      checkInputFile(input, "input_file")
    })

    # Data
    df <- reactive({
      readFile(input$input_file, headings = input$header)
    })

    # Column checks
    observeEvent(input$col_faction,{
      checkInputLevels(input, "col_faction", df())
    })

    observeEvent(input$col_wins,{
      checkInputInteger(input, "col_wins", df())
    })

    observeEvent(input$col_total_games,{
      checkInputInteger(input, "col_total_games", df())
    })

    # Render data
    output$data_table <- DT::renderDataTable(
      {
        DT::datatable(
          df()
          ,extensions = list("Scroller")
          ,options = list(
            scrollY = as.character(cdata$output_pid_height * 0.5)
            ,scrollCollapse = TRUE
            ,paging = FALSE
            ,pageLength = 10
          )
          ,rownames = FALSE
          ,fillContainer = TRUE
        )
      }
    )

    # Reactive values to store results
    model_output <- reactiveValues(
      model = NULL # brms model object
      ,results = NULL # list of DT and valueBox
    )
    plot_output <- reactiveValues(
      plots = NULL # ggplotly object
      ,checks = NULL # ggplot2 objects
    )

    # Listen to "run_model" input
    toListen <- reactive({
      list(input$run_model) # this is a list in case extra things need to be added later
    })

    # Run if "run_model" is pressed
    observeEvent(toListen(), {

      withProgress(message = "Fitting model(s), please wait...", {

        # Requirements before processing
        req(input$col_faction, input$col_wins, input$col_total_games)

        # Update focus to plots tab
        updateTabsetPanel(session, "see_who_wins_tab", selected = "see_who_wins_tab_plots")

        # Fit models
        model_output$model <- fitModel(
          data = df()
          ,col_faction = input$col_faction
          ,col_wins = input$col_wins
          ,col_total_games = input$col_total_games
        )

        setProgress(0.5, "Extracting model information...")

        # Plot data
        plot_output$plots <- plotModel(
          data = df()
          ,model = model_output$model
          ,col_faction = input$col_faction
          ,col_wins = input$col_wins
          ,col_total_games = input$col_total_games
          ,plot_dim = cdata
        )

        # Extract model info into DT
        model_output$results <- showResults(
          data = df()
          ,model = model_output$model
          ,col_faction = input$col_faction
          ,col_wins = input$col_wins
          ,col_total_games = input$col_total_games
          ,table_dim = cdata
        )

      }) # ends withProgress

    })

    output$plots <- renderUI({
      plot_output$plots
    })

    output$results <- DT::renderDataTable(
      {model_output$results[[1]]}
    )

    output$model_checks <- renderUI({
      model_output$results[[2]]
    })

  })
}
