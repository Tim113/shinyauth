#' Create the settings tab
#' This function needs to be able to trigger the fech of a new dt_user
#'
#' @param dt_user Data table identifying the current user
#'
#' @export
settings_tab = function(input, output, session, auth, permissions = "user"){

  ### Render the summary page
  # This must be reset then the "reset" button is pressed
  shiny::observeEvent(
    eventExpr   = input$reset,
    ignoreNULL  = FALSE,
    handlerExpr = {
      output$settings = render_settings_page(input, output, session, auth,
                                             permissions)
    })

  ### Save changes to the users details
  shiny::observeEvent(
    eventExpr   = input$save,
    handlerExpr = {
      save_user_details(input, output, session, auth, permissions)
    })

  ### Change password
  shiny::observeEvent(
    eventExpr   = input$password_change,
    handlerExpr = {

      password_change_manager(
        input, output, session, auth,
        admin        = FALSE,
        user_id      = auth$dt_user[, users_id],
        old_password = auth$dt_user[, password])
    })

}

#' Save the current user datailes to the db
#'
#' @param dt_user data.table of user for whom the settings page is opened, defult so acitve user
#'
#' @import data.table
save_user_details = function(input, output, session, auth,
                             permissions, dt_user = NULL){
  # Defult dt_user to auth$dt_user
  if (is.null(dt_user)) {dt_user = auth$dt_user}

  ### Get list of valid chagable verables
  # Find all of the rows that are spesificly user changable
  if (permissions == "admin") {
    cond = sapply(auth$table_cofig, function(x) shiny::isTruthy(x$admin_changeable))
  } else if (permissions == "moderator") {
    cond = sapply(auth$table_cofig, function(x) shiny::isTruthy(x$moderator_changeable))
  } else {
    cond = sapply(auth$table_cofig, function(x) shiny::isTruthy(x$user_changeable))
  }

  # Extract just the chanable columns, excluding those changed via other means
  changeable_columns = setdiff(
    x = names(auth$table_cofig[cond]),
    y = c("password", "admin"))

  # Remove moderator if it is not used
  if (!auth$table_cofig$moderator$use_moderatior) {
    changeable_columns = changeable_columns[changeable_columns != "moderator"]
  }

  # // TODO change this to something less awfull
  for (col_name in changeable_columns) {
    # Make nice injection proof queury
    sql_save_user_details =
      paste0(
        "UPDATE Users SET ",
        col_name, " = ?value ",
        " WHERE ",
        " user_id = ?user_id;"
      )

    query_save_user_details =
      DBI::sqlInterpolate(auth$pool_auth, sql_save_user_details,
                          value   = input[[col_name]],
                          user_id = dt_user[, user_id])

    # Send the qeruy
    DBI::dbGetQuery(auth$pool_auth, query_save_user_details)
  }

  # Tell the user the save has been sucessfull
  session$sendCustomMessage(
    type    = "shiny_alert",
    message = "Save changes to the datatbase")

}

#' Function creating the ui for a settings page for a given user table
#'
#' @param permissions The level of permision the accessing user has
#' @param dt_user data.table of user for whom the settings page is opened, defult so acitve user
#'
#' @import data.table
render_settings_page = function(input, output, session, auth,
                                permissions, dt_user = NULL,
                                time_stamp = NULL){

  # Defult dt_user to auth$dt_user
  if (is.null(dt_user)) {dt_user = auth$dt_user}

  ### Get list of valid chagable verables
  # Find all of the rows that are spesificly user changable
  if (permissions == "admin") {
    changeable = sapply(auth$table_cofig, function(x) shiny::isTruthy(x$admin_changeable))
  } else if (permissions == "moderator") {
    changeable = sapply(auth$table_cofig, function(x) shiny::isTruthy(x$moderator_changeable))
  } else {
    changeable = sapply(auth$table_cofig, function(x) shiny::isTruthy(x$user_changeable))
  }

  # Extract just the chanable columns, excluding those changed via other means
  changeable_columns = setdiff(
    x = names(auth$table_cofig[changeable]),
    y = c("password", "admin"))

  # Remove moderator from changeable_columns if user moderaror is false
  if (!shiny::isTruthy(auth$table_cofig$moderator$use_moderatior)) {
    changeable_columns = setdiff(
      x = changeable_columns,
      y = c("moderator"))
  }

  ### Get list of valid viewable verables
  # All columns defult to viewable (expect password)
  viewable = sapply(auth$table_cofig, function(x) !shiny::isTruthy(x$hidden))

  # Extract just the viewable columns,
  viewable_columns = setdiff(
    x = names(auth$table_cofig[viewable]),
    y = c("password", "admin", "date_created", "last_password_change", "change_password",
          changeable_columns))

  # Remove moderator from changeable_columns if user moderaror is false
  if (!shiny::isTruthy(auth$table_cofig$moderator$use_moderatior)) {
    viewable_columns = setdiff(
      x = viewable_columns,
      y = c("moderator"))
  }

  ns = session$ns

  ### Render the ui
  # This will be diffrent dependent on if it is called by settings or admin
  if (permissions == "admin") {
    ui = shiny::renderUI({
      shiny::fluidPage(
          page_tile("Settings"),

          # Verable boxes
          shiny::fluidRow(
            lapply(
              X = changeable_columns,
              FUN     = render_settings_box,
              input   = input,
              output  = output,
              session = session,
              auth    = auth,
              dt_user = dt_user),

            lapply(
              X = viewable_columns,
              FUN       = render_settings_box,
              input     = input,
              output    = output,
              session   = session,
              auth      = auth,
              dt_user   = dt_user,
              read_only = TRUE)),

          shiny::fluidRow(
            shinydashboard::box(
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              title = "Grant and Revoke Admin Rights",

              shiny::checkboxInput(
                inputId = ns("is_admin"),
                label   = "User is Admin",
                value   = dt_user[, admin]),
              shiny::HTML("<p> <br/> </p>"),

              paste0("To make sure that you want to do this please type in to the box the users ",
                     auth$table_cofig$user_id$human_name,
                     "."),
              shiny::HTML("<p> <br/> </p>"),

              shiny::textInput(
                inputId = ns("confirm_text"),
                label   = NULL,
                width   = "200px"),
              shiny::HTML("<p> <br/> </p>"),

              shiny::checkboxInput(
                inputId = ns("confirm_box"),
                label   = "Confirm Status Change"),
              shiny::HTML("<p> <br/> </p>"),

              shiny::actionButton(
                inputId = ns(paste0("change_admin_status",
                                    time_stamp)),
                label   = "Change Admin Status")
            )
      ))
    })
  } else {
    ui = shiny::renderUI({
      shiny::conditionalPanel(
        condition = "input.authMenuItems == 'settings'",

        shiny::fluidPage(

          page_tile("Settings"),

          # Verable boxes
          shiny::fluidRow(
            lapply(
              X = changeable_columns,
              FUN     = render_settings_box,
              input   = input,
              output  = output,
              session = session,
              auth    = auth,
              dt_user = dt_user),

            lapply(
              X = viewable_columns,
              FUN       = render_settings_box,
              input     = input,
              output    = output,
              session   = session,
              auth      = auth,
              dt_user   = dt_user,
              read_only = TRUE)),

          shiny::fluidRow(
            shinydashboard::box(width = 4,
                                shiny::actionButton(
                                  inputId = ns("password_change"),
                                  label   = "Change Password",
                                  width   = "100%"
                                )),
            shinydashboard::box(width = 4,
                                shiny::actionButton(
                                  inputId = ns("save"),
                                  label   = "Save Changes",
                                  width   = "100%"
                                )),
            shinydashboard::box(width = 4,
                                shiny::actionButton(
                                  inputId = ns("reset"),
                                  label   = "Reset",
                                  width   = "100%"
                                ))
          )

        ))
    })
  }

  return(ui)
}



#' @import data.table
render_settings_box = function(input, output, session, auth,
                               column_name, dt_user, read_only = FALSE) {

  ns = session$ns

  # Find the type of verable we need to genorate a box for
  type = auth$table_cofig[[column_name]]$type

  # Check for special case
  if (read_only) {
    ui = shinydashboard::box(
      width = 4,
      title = auth$table_cofig[[column_name]]$human_name,
      dt_user[, ..column_name])

  } else if (column_name == "moderator" &
             shiny::isTruthy(auth$table_cofig$moderator$use_moderatior)) {
    ui = shinydashboard::box(
      width = 4,
      title = auth$table_cofig[[column_name]]$human_name,
      shiny::checkboxInput(
        inputId = ns(column_name),
        label   = NULL,
        value   = as.logical(dt_user[, ..column_name])))

  } else if (column_name %in% c("admin", "change_password")) {
    ui = shinydashboard::box(
      width = 4,
      title = auth$table_cofig[[column_name]]$human_name,
      shiny::checkboxInput(
        inputId = ns(column_name),
        label   = NULL,
        value   = as.logical(dt_user[, ..column_name])))

  } else if (column_name == "users_moderator") {
    # Get list of moderators
    moderators_list = DBI::dbGetQuery(
      conn      = auth$pool_auth,
      statement = "SELECT user_id FROM Users WHERE moderator = '1';")

    ui = shinydashboard::box(
      width = 4,
      title = auth$table_cofig[[column_name]]$human_name,
      shiny::selectInput(
        inputId  = ns(column_name),
        label    = NULL,
        choices  = moderators_list[, "user_id"],
        selected = dt_user[, ..column_name]))

  } else if (type %in% c("character", "intiger", "decimal")) {
    ui = shinydashboard::box(
      width = 4,
      title = auth$table_cofig[[column_name]]$human_name,
      shiny::textInput(
        inputId = ns(column_name),
        label   = NULL,
        value   = dt_user[, ..column_name]))

  } else if (type == "logical") {
    ui = shinydashboard::box(
      width = 4,
      title = auth$table_cofig[[column_name]]$human_name,
      shiny::checkboxInput(
        inputId = ns(column_name),
        label   = NULL,
        value   = as.logical(dt_user[, ..column_name])))

  } else if (type == "categorical") {
    # Get the catagorys from auth
    catagorys = auth$table_cofig[[column_name]]$categories

    ui = shinydashboard::box(
      width = 4,
      title = auth$table_cofig[[column_name]]$human_name,
      shiny::selectInput(
        inputId  = ns(column_name),
        label    = NULL,
        choices  = catagorys,
        selected = dt_user[, ..column_name]))

  } else {
    stop("The coloumn ", column_name, " is said to to be of type ", type,
         ".  This is not a valid column type.")
  }

  return(ui)
}

page_tile = function(title){
  shiny::fluidRow(
    shinydashboard::box(width  = 12,
                        height = "40px",
                        title  = shiny::HTML(paste0("<p style='font-size:20px'>",
                                                    title,
                                                    "</p>"))
    ))
}
