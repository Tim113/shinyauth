#' Create the body for the auth part of the dashboard
#' This will differ diffrent dependent upon the status of the app
#'
#' @param status What is the status of the app and as such which of the sidebars should be shown
#'
#' @import data.table
#' @import magrittr
auth_body = function(input, output, session, status){
  #################### Explanation
  # There are three possible states for the sidebar
  # 1. No-login attempt ("start")
  # 3. User logged in ("logged-in")
  # 2. Login Failed     ("failed")
  # Each of these will have their own side bar, and thus will need diffrent deffintions
  # When this fucntion is called the state argument must be one of the three listed above
  # so that the funciton calls the rifht one

  # Insure that staus is on of the valid statu's for the list
  if (!(status %in% c("start", "logged-in", "failed"))) {
    stop("Agrument staus must be a member of {start, logged-in, failed}")
  }

  #################### Create Sidebar   ################
  # If status is start show the logon sidebar
  if (status == "start") {

    output$auth_body = shiny::renderUI({

    })

  } else if (status == "failed") {

    output$auth_body = shiny::renderUI({

    })

  } else if (status == "logged-in") {

    output$auth_body = shiny::renderUI({
      shiny::tagList(
        shiny::uiOutput("settings"),
        shiny::uiOutput("admin")
      )
    })

  }
}
