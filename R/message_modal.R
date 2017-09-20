#'Show a message to the user, without custom javascript
#'
#'@export
message_modal = function(input, output, session, title, message) {
  #  The user to change the password is the active user
  shiny::modalDialog(
    title     = title,
    size      = "s",
    easyClose = FALSE,
    fade      = FALSE,
    footer    = shiny::tagList(
      shiny::modalButton("Close")
    ),

    # Body of Modal Window
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::p(message)
      ))
  )
}