#' @export
text_with_linebreaks = function(text) {
  # Replace \n line breaks with the <br/> tags
  shiny::HTML(gsub(
    pattern = "\n",
    replacement = "<br/>",
    x = (text)))
}

#' @export
page_tile = function(title, text = NULL, main = TRUE){
  shiny::fluidRow(
    shinydashboard::box(
      width  = 12,
      title = NULL,
      if (main) {
        shiny::h2(title)
      } else {
        shiny::h3(title)
      },
      shiny::p(text)
    ))
}

body = function(...) {
  shinydashboard::dashboardBody(

    # Requiered for shinyauth
    shiny::tags$head(
      # Import the Java script for the pop-up error message box
      shiny::tags$script(
        "Shiny.addCustomMessageHandler('shiny_alert', function(msg){ alert(msg); })")),

    shiny::uiOutput("auth_body"),


    ### Your body Start ###
    ...
    ### Your body End ###

  )
}