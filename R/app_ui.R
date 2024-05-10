#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(

    # Leave this function for adding external resources
    golem_add_external_resources()

    ## Start dashboardPage() ----
    ,dashboardPage(

      ## Title ----
      title = "Have a fight, see who wins"

      ## Header ----
      ,dashboardHeader(
        title = tags$a(
          href='http://www.github.com/rtnliqry/HAFSWW'
          ,tags$img(src = 'www/text-HAFSWW.png', width = '100%')
        )
      )

      ## Sidebar ----
      ,dashboardSidebar(
        sidebarMenu(
          menuItem("Information", tabName = "information", icon = icon("circle-info"))
          ,menuItem("See who wins", tabName = "see_who_wins", icon = icon("chart-simple"))
        )
      )

      ## Body ----
      ,dashboardBody(

        # Tags ----
        tags$style(
          HTML(
            ".skin-blue .main-header .logo {
          background-color:#8f0000
        }
        .skin-blue .main-header .navbar {
          background-color:#8f0000
        }
        .box.box-solid.box-primary>.box-header {
          color:#fff;
          background:#8f0000
        }
       .box.box-solid.box-primary {
          border-bottom-color:#8f0000;
          border-left-color:#8f0000;
          border-right-color:#8f0000;
          border-top-color:#8f0000;
       }
       .fa, .fas {
          background.color:#FF5733;
       }
       .h2 {
          margin-top:0;
       }"
          )
        )
        ,shinyFeedback::useShinyFeedback()
        ,tabItems(

          # First tab "Information" ----
          tabItem(
            tabName = "information"
            ,mod_Information_ui("information_tab")
          )

          # Second tab "See who wins" ----
          ,tabItem(
            tabName = "see_who_wins"
            ,mod_SeeWhoWins_ui("see_who_wins_tab")
          )

        ) # ends tabItems

        # Window sizing code ----
        ,tags$head(
          tags$script('
                var height = 0;
                var width = 0;
                $(document).on("shiny:connected", function(e) {
                  height = window.innerHeight;
                  width = window.innerWidth;
                  Shiny.onInputChange("height", height);
                  Shiny.onInputChange("width", width);
                });
                $(window).resize(function(e) {
                  height = window.innerHeight;
                  width = window.innerWidth;
                  Shiny.onInputChange("height", height);
                  Shiny.onInputChange("width", width);
                });
                '
          )
        )

        # Skin colour ----
        ,skin = "blue"

      ) # ends dashboardBody
    ) # ends dashboardPage
  ) # ends tagList
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Have a fight, see who wins"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
