## ui.R ##
shiny::shinyUI(
  shinydashboard::dashboardPage(
    title = "shinyauth",

    shinydashboard::dashboardHeader(
      title = "shinyauth"),

    shinydashboard::dashboardSidebar(

      ### Your sidebar deffined in server_post_auth ###
      shiny::uiOutput("sidebar"),

      # Requiered for shinyauth
      shiny::uiOutput("auth_sidebar")
    ),

    shinydashboard::dashboardBody(

      # Requiered for shinyauth
      shiny::tags$head(
        # Import the Java script for the pop-up error message box
        shiny::tags$script(
          "Shiny.addCustomMessageHandler('shiny_alert', function(msg){ alert(msg); })")),

      shiny::uiOutput("auth_body"),


      ### Your body Start ###
      shiny::uiOutput("body")
      ### Your body End ###

    ))
)
