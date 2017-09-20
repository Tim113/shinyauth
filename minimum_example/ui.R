## ui.R ##
shiny::shinyUI(
  shinydashboard::dashboardPage(
    title = "shinyauth",

    shinydashboard::dashboardHeader(
      title = "shinyauth"),

    shinyauth::saSidebar(
      shiny::uiOutput("sidebar")
    ),

    shinyauth::saBody(
      shiny::uiOutput("body")
    )
  )
)
