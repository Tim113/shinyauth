#' Function that creates a sidebar with the id authMenuItems and shinyauth tabs automaticly
#' This should be used like shinydashboard::sidebarMenu exept the id cannot be changed
#'
#' @import data.table
#' @export
authSidebarMenu = function(auth, ...) {
  shinydashboard::sidebarMenu(
    id = "authMenuItems",

    ...,

    shinydashboard::menuItem(text     = "Settings",
                             tabName  = "settings"),

    if (auth$dt_user[, admin]) {
      shinydashboard::menuItem(text     = "Admin",
                               tabName  = "admin")
    }
  )
}

#' Create the sidbar for the dashboard, diffrent dependent upon the status of the app
#'
#' @param status What is the status of the app and as such which of the sidebars should be shown
#'
#' @import data.table
#' @import magrittr
auth_sidebar = function(input, output, session, status){
  #################### Explanation
  # There are three possible states for the sidebar
  # 1. No-login attempt      ("start")
  # 2. User logged in        ("logged-in")
  # 3. Login Failed          ("failed")
  # 4. Password just changed ("password_changed")
  # Each of these will have their own side bar, and thus will need diffrent deffintions
  # When this fucntion is called the state argument must be one of the three listed above
  # so that the funciton calls the rifht one

  # Insure that staus is on of the valid statu's for the list
  if (!(status %in% c("start", "logged-in", "failed", "password_changed"))) {
    stop("Agrument staus must be a member of {start, logged-in, failed, password_changed}")
  }

  #################### Create Sidebar   ################
  # If status is start show the logon sidebar
  if (status %in% c("start", "failed", "password_changed")) {
    ## Render the Sidbar Meneu
    output$auth_sidebar = shiny::renderUI({
      # The inital sidebar menue
      shinydashboard::sidebarMenu(
        ### Login to the app
        shiny::textInput(
          inputId = "user",
          label   = shiny::h4("Employee ID:"),
          value   = ""),

        shiny::passwordInput(
          inputId = "password",
          label   = shiny::h4("Password:"),
          value   = ""),

        shiny::HTML("<p> <br/> </p>"),

        shiny::actionButton(
          inputId = "login",
          label   = "Login",
          width   = "100%",
          icon    = shiny::icon("sign-in"),
          style   = "margin: 6px 5px 6px 0;"),

        if (status == "failed") {
          # Dispaly error message
          shinydashboard::valueBox(
            value    = "",
            subtitle = "Username or password incorrect.",
            icon     = NULL,
            width    = "100%",
            color    = "yellow")
        } else if (status == "password_changed") {
          shinydashboard::valueBox(
            value    = "",
            subtitle = "Please login with new password.",
            icon     = NULL,
            width    = "100%",
            color    = "light-blue")
        }

        )
    })

  } else if (status == "logged-in") {

    output$auth_sidebar = shiny::renderUI({
    })

  }
}
