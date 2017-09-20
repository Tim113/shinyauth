#' Create the Admin Tab tab
#'
#' @export
admin_tab = function(input, output, session, auth){

  # Check that the user has admin rights
  if (auth$dt_user[, admin]) {
    output$admin = render_admin_page(input, output, session)

    #  Get table of users to create_admin_tab
    dt_users = get_users_table(input, output, session, auth)

    # DT of the users
    users_table_creation(input, output, session, auth, dt_users)

    ## Change Password
    change_user_password(input, output, session, auth, dt_users)

    shiny::observeEvent(
      eventExpr  = input$details_button,
      handlerExpr = {
        # Find the number of the row that has been selected
        selected_row = as.numeric(strsplit(input$details_button, "_")[[1]][2])

        shiny::callModule(change_user_details, "admin_settings",
                          auth             = auth,
                          dt_settings_user = dt_users[selected_row, ])
      })

    ### Create User
    shiny::observeEvent(
      eventExpr   = input$create_user,
      handlerExpr = {
        user_creation_manager(input, output, session, auth)
      })
  }
}

#' Change users Deitals must be called from inside a name space
change_user_details = function(input, output, session, auth, dt_settings_user) {

  ns = session$ns

  # Create time stamp for this instance
  time_stamp = Sys.time() %>%
    gsub('[[:punct:], [:space:]]', '', .)

  output$settings =
    render_settings_page(input, output, session, auth,
                         permissions = "admin",
                         dt_user     = dt_settings_user,
                         time_stamp  = time_stamp)

  # Show the Modal box with the user's details in
  shiny::showModal(
    shiny::modalDialog(
      title = paste0("Details for: ",
                     dt_settings_user[, user_id]),
      size      = "l",
      easyClose = TRUE,
      fade      = FALSE,
      footer    = shiny::tagList(
        shiny::actionButton(inputId =
                              ns(paste0("save",
                                        time_stamp)),
                            label   = "Save Changes"),
        shiny::modalButton("Close")
      ),
      shiny::uiOutput(ns("settings"))
    ))

  ### Save changes to the users details
  shiny::observeEvent(
    eventExpr   = input[[paste0("save", time_stamp)]],
    handlerExpr = {
      save_user_details(input, output, session, auth,
                        dt_user     = dt_settings_user,
                        permissions = "admin")
    })

  ### Change users admin status
  shiny::observeEvent(
    eventExpr   = input[[paste0("change_admin_status", time_stamp)]],
    handlerExpr = {
      change_user_admin_status(input, output, session, auth,
                               dt_user = dt_settings_user)
    }
  )
}

#' Function that allos for the awaring and revoking of admin rights
#'
#' @import data.table
change_user_admin_status = function(input, output, session, auth,
                                    dt_user) {

    # Check that the confirm text is correct
  if (input$confirm_text != dt_user[, user_id]) {
    session$sendCustomMessage(
      type    = "shiny_alert",
      message = "Username is not given correctly")
  } else if (!input$confirm_box) {
    session$sendCustomMessage(
      type    = "shiny_alert",
      message = "Please tick the confermation box.")
  } else if (dt_user[, user_id] == auth$user_id) {
    session$sendCustomMessage(
      type    = "shiny_alert",
      message = "No you cannot revoke your own admin privileges.")
  } else {

    # Make nice injection proof queury
    sql_save_user_details =
      paste0(
        "UPDATE Users SET ",
        " admin = ?admin ",
        " WHERE ",
        " user_id = ?user_id;"
      )

    query_save_user_details =
      DBI::sqlInterpolate(auth$pool_auth, sql_save_user_details,
                          admin   = as.numeric(input$is_admin),
                          user_id = dt_user[, user_id])

    # Send the qeruy
    DBI::dbGetQuery(auth$pool_auth, query_save_user_details)

    session$sendCustomMessage(
      type    = "shiny_alert",
      message = "Updated Admin Privileges.")
  }

}

#' Function creating the ui for a admin page
#'
#' @import data.table
render_admin_page = function(input, output, session){

  shiny::renderUI({
    shiny::conditionalPanel(
      condition = "input.authMenuItems == 'admin'",
      shiny::fluidPage(

        page_tile("Admin"),

        shiny::fluidRow(
          shinydashboard::box(
            width = 6,
            shiny::actionButton(
              inputId = "create_user",
              label   = "Create New User",
              width   = "100%"
            ))
        ),

        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            DT::dataTableOutput("users_table")
          )
        )

      )
    )
  })
}

#' Chagne users password
change_user_password = function(input, output, session, auth, dt_users){
  #  When the button is pressed, extract the row number
  shiny::observeEvent(
    eventExpr   = input$password_button,
    handlerExpr = {
      # # Find the number of the row that has been selected
      selected_row = as.numeric(strsplit(input$password_button, "_")[[1]][2])

      password_change_manager(
        input, output, session, auth,
        admin   = TRUE,
        user_id = dt_users[selected_row, user_id])

      #  Get table of userscreate_admin_tab
      dt_users = get_users_table(input, output, session, auth)

      # Remake the users table
      users_table_creation(input, output, session, auth, dt_users)

    })
}

#' Create table of users with buttons to change password and user details
users_table_creation = function(input, output, session, auth, dt_users) {
  ############################# Requiered for function #############################
  # This function will create the buttons for the datatable
  DT_Buttons = function(FUN, len, id, ...) {inputs = character(len)
  for (i in seq_len(len)) {
    inputs[i] = as.character(FUN(paste0(id, i), ...))}
  inputs
  }

  # Extract the name space form the session object
  ns = session$ns

  # Make copy of dt_users to make changes to
  dt_users_table = data.table::copy(dt_users)

  user_table_colnames =
    c(auth$table_cofig$user_id$human_name, "Admin", "Password", "Detils")

  ############################# Create Data Table to display #############################
  # Create reactive data.table that allows for the pressing of a button
  dt_users_reactive = shiny::reactive({
    data.table::as.data.table(cbind(
      # The data that we want to dispaly less the comments
      dt_users_table[, .(user_id, admin)],

      # Row containing the buttons for comments
      Password = DT_Buttons(actionButton, nrow(dt_users_table), 'row_', label = "Change Password",
                            onclick =   paste0("Shiny.onInputChange(\'", ns('password_button'), "\',  this.id)")),

      # Row containg the buttons
      Details = DT_Buttons(actionButton, nrow(dt_users_table), 'row_', label = "Change Details",
                           onclick =   paste0("Shiny.onInputChange(\'", ns('details_button'), "\',  this.id)"))
    ))
  })

  ############################# Render Table of Users  #############################
  output$users_table = DT::renderDataTable(
    expr = DT::datatable(
      ### Genral and verbose DT settigns
      # Any setting that is not obviose will be commented
      data       = dt_users_reactive(),
      rownames   = FALSE,
      colnames   = user_table_colnames,
      # Add in the buttons that allow for the table to be dowloaded:
      selection  = "none",
      escape     = FALSE,

      ### DT settings that must be set in options
      options  = list(
        # Set the page length to be the number of rows
        pageLength    = -1,
        scrollX       = TRUE,
        bLengthChange = FALSE,
        paging        = FALSE)
    ))
}


#' Query the db for the table of all of the user information
get_users_table = function(input, output, session, auth) {

  # Build the Qury to send to the db
  query_users_table =
    paste0(
      "SELECT * FROM Users;"
    )

  suppressWarnings({
    dt_res =  DBI::dbGetQuery(conn      = auth$pool_auth,
                              statement = query_users_table)
  })


  # Verbose message on fail if unexpected query is passed
  if (!("data.frame" %in% class(dt_res))) stop("get_dt did not return a data.frame")

  data.table::setDT(dt_res)

  dt_res[, admin := as.logical(admin)]

  # Return the query results as a data.table
  return(dt_res[]) # call [.data.table to force evaluation
}
