#' This funciton allows an admin to create a new user of the app
#'
#' @import data.table
#' @import magrittr
#' @export
user_creation_manager = function(input, output, session, auth){

  # Create time stamp for this instance
  time_stamp = Sys.time() %>%
    gsub('[[:punct:], [:space:]]', '', .)

  # Make the user modal
  create_user_modal = create_user_modal(input, output, session, time_stamp)

  # Get a list of all current user ids so that there is no duplication
  current_user_ids = get_curret_user_ids(auth)

  # Show the modal containg user creation
  shiny::showModal(create_user_modal)

  ### Listen for the confirm button
  shiny::observeEvent(
    eventExpr   = input[[paste0("create_user", time_stamp)]],
    ignoreNULL  = TRUE,
    handlerExpr = {

      # The users dosent exist in the db
      # // TODO check the user is new
      if (input$user_id %in% current_user_ids) {
        user_id_new = FALSE
      } else {
        user_id_new = TRUE
      }

      # New password vaild
      # // TODO Check the password is storng
      password_strong_enough = TRUE

      if (user_id_new & password_strong_enough & input$user_creation_confirm) {

        # Save new user to the db
        save_new_user(input, output, session, auth)

        # Make the user modal
        create_user_modal = create_user_modal(input, output, session, time_stamp)

        gc()
      } else {
        ### Find the error then dispaly message to user explaing the problem
        if (!user_id_new) {
          session$sendCustomMessage(
            type    = "shiny_alert",
            message = "This user is alredy exists.")
        } else if (!password_strong_enough) {
          session$sendCustomMessage(
            type    = "shiny_alert",
            message = "The supplyed password is not strong enough.")
        } else if (!input$user_creation_confirm) {
          session$sendCustomMessage(
            type    = "shiny_alert",
            message = "Please confirm user creation.")
        } else {
          stop("Password not saved for with no reson given")
        }
      }
    })
}

#' Save a new user to the db
save_new_user = function(input, output, session, auth) {

  ### Get the user cration defults
  # Return a named list of user defults
  ls_defaults = list_defults(auth)

  # All users defult to not being moderators if moderators exist
  if (auth$table_cofig$moderator$use_moderatior) {
    sql_create_user = paste0(
      "INSERT INTO Users ",
      " ( user_id, password, admin, moderator, date_created, last_password_change, change_password, " ,
      paste0(names(ls_defaults), collapse = ", "),
      ") ",
      " VALUES ( ?user_id, ?password, ?admin, '0', NOW(), NOW(), '1',  ",
      paste0("?", names(ls_defaults), collapse = ", "),
      " );")

  } else {
    sql_create_user = paste0(
      "INSERT INTO Users ",
      " ( user_id, password, admin, date_created, last_password_change, change_password, " ,
      paste0(names(ls_defaults), collapse = ", "),
      ") ",
      " VALUES ( ?user_id, ?password, ?admin, NOW(), NOW(), '1',  ",
      paste0("?", names(ls_defaults), collapse = ", "),
      " );")
  }

  query_create_user = do.call(
    DBI::sqlInterpolate,
    c(
      auth$pool_auth, sql_create_user,
      user_id      = input$user_id,
      password     = sodium::password_store(input$password),
      admin        = as.numeric(input$make_admin),
      ls_defaults
    )
  )

  DBI::dbGetQuery(auth$pool_auth, query_create_user)

  # Close the modal dialog box
  shiny::removeModal()

  # Let the user know the new user has been saved to the db
  session$sendCustomMessage(
    type    = "shiny_alert",
    message = "New User Created")
}

#' User creation modal
#'
#' @param time_stamp Creates a uniqe box each time the box is opends
#'
#' @import data.table
create_user_modal = function(input, output, session, time_stamp){

  # Gen defult password for user
  defult_password = bcrypt::gensalt() %>%
    substr(start = 8, stop = 18)

  #  The user to change the password is the active user
  shiny::modalDialog(
    title     = "Create User",
    size      = "s",
    easyClose = TRUE,
    fade      = FALSE,
    footer    = shiny::tagList(
      shiny::actionButton(inputId =
                            paste0("create_user",
                                   time_stamp),
                          label   = "Create User"),
      shiny::modalButton("Cancel")
    ),

    # Body of Modal Window
    shiny::fluidRow(
      shiny::textInput(
        inputId = "user_id",
        label   = "User ID",
        value   = "",
        width   = "100%"),
      shiny::textInput(
        inputId = "password",
        label   = "Password",
        value   = defult_password,
        width   = "100%"),
      shiny::checkboxInput(
        inputId = "make_admin",
        label   = "Make User Admin",
        value   = FALSE,
        width   = "100%"),
      shiny::checkboxInput(
        inputId = "user_creation_confirm",
        label   = "Confirm User Creation",
        value   = FALSE,
        width   = "100%")
      )
    )

}

#' Query the db for the table of all of the user information
get_curret_user_ids = function(auth) {

  # Build the Qury to send to the db
  query_users_table =
    paste0(
      "SELECT user_id FROM Users;"
    )

  suppressWarnings({
    dt_res =  DBI::dbGetQuery(conn      = auth$pool_auth,
                              statement = query_users_table)
  })


  # Verbose message on fail if unexpected query is passed
  if (!("data.frame" %in% class(dt_res))) stop("get_dt did not return a data.frame")

  data.table::setDT(dt_res)

  # Return the query results as a data.table
  return(unlist(dt_res[])) # call [.data.table to force evaluation
}

#' Given an auth object create a list of the defult values
list_defults = function(auth) {

  # Extract a list of the defult valeues
  defults = lapply(
    X   = names(auth$table_cofig),
    FUN = function(var_name) {

      defult_val = auth$table_cofig[[var_name]]$defult

      if (!is.null(defult_val)) {
        defult_val
      }

    })

  # Remove the NULL values
  defults[sapply(defults, is.null)] = NULL

  # Extract the names of the varaibles that have defults
  defults_names = lapply(
    X   = names(auth$table_cofig),
    FUN = function(var_name) {

      defult_val = auth$table_cofig[[var_name]]$defult

      if (!is.null(defult_val)) {
        var_name
      }
    }) %>% unlist()


  # Set the names of the defults
  names(defults) = defults_names

  defults
}
