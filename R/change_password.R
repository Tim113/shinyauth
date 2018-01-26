#' This function creates and manages the life cycle of a uniqe password_change_modal that
#' allos the user to change there password.  Or an andmin to change the user passwords for
#' any user.
#'
#' @param admin Is true if this is an admin changin the datils for another user
#' @param message Optional message to show in the password modal
#'
#' @import data.table
#' @import magrittr
#' @export
password_change_manager = function(input, output, session, auth,
                                   admin        = FALSE,
                                   user_id      = NULL,
                                   old_password = NULL,
                                   message      = NULL){
  # If user_id has not been given then take it form auth object
  # This must only happen if auth == TRUE, else this will produce an error
  if (!admin) {
    user_id = auth$user_id
  } else {
    if (is.null(user_id)) {
      stop("password_change_manager has been sarted from admin without a user_is")
    }
  }

  # If not acting as admin then there must be an old_password
  if (!admin & is.null(old_password)) {
    stop("If not acting as admin then users old password must be given.")
  }

  # Create time stamp for this instance
  time_stamp = Sys.time() %>%
    gsub('[[:punct:], [:space:]]', '', .)

  # Make the password modal
  password_modal = password_change_modal(
    input, output, session,
    admin,
    user_id,
    time_stamp,
    message)

  # Show the modal containg password change options
  shiny::showModal(password_modal)

  ################ listen for password buttons
  # There are two diffrent password modals that can be shown depening on the context
  # that this funcition is called from, they are disigised via the button id they use
  # for saving
  ### Listen for the password change button
  shiny::observeEvent(
    eventExpr = input[[paste0("check_and_change_password", time_stamp)]],
    handlerExpr = {

      # Check the old password given matches the one in the db
      old_password_correct =
        sodium::password_verify(password =  input$old_password,
                                hash     = old_password)

      # The two new passwords given match
      new_passwords_match =
        input$new_password_main == input$new_password_confirm

      # New password vaild
      # // TODO Check htr password is storng
      password_strong_enough = TRUE

      if (old_password_correct &
          new_passwords_match &
          input$password_change_confirm &
          password_strong_enough) {

        # Save the new password to the db
        save_new_password(
          session, auth,
          user_id,
          user_changed = TRUE,
          new_password =  sodium::password_store(input$new_password_main))

        # Close the modal dialog box
        shiny::removeModal()

        # # Remake the modal to remove the passwords from memory
        # password_modal = password_change_modal(input, output, session,
        #                                        admin,
        #                                        user_id,
        #                                        time_stamp)
        gc()
      } else {
        ### Find the error then dispaly message to user explaing the problem
        if (!old_password_correct) {
          session$sendCustomMessage(
            type    = "shiny_alert",
            message = "Old password not given correctly.")
        } else if (!new_passwords_match) {
          session$sendCustomMessage(
            type    = "shiny_alert",
            message = "New passwords do not match.")
        } else {
          session$sendCustomMessage(
            type    = "shiny_alert",
            message = "Please check Confirm Password Change.")
        }
      }
    })


  ### Listen for the password reset button
  shiny::observeEvent(
    eventExpr = input[[paste0("reset_password", time_stamp)]],
    handlerExpr = {

      # Check that the conferm password change box has been ticked
      if (input$password_change_confirm) {
        # Save the new password to the db
        save_new_password(
          session, auth,
          user_id,
          user_changed = FALSE,
          new_password = sodium::password_store(input$new_password_main))

        # Remake the modal to remove the passwords from memory
        password_modal = password_change_modal(
          input, output, session,
          admin,
          user_id,
          time_stamp)

        gc()

        # Close the modal dialog box
        shiny::removeModal()
      } else {
        ### Error message
        session$sendCustomMessage(
          type    = "shiny_alert",
          message = "Please check Confirm Password Reset.")
      }
    })
}

#' Save a new password to the db
#'
#' @param user_changed Has the password been changed by the user, or reset by admin
save_new_password = function(session, auth,
                             user_id,
                             user_changed,
                             new_password) {

  if (user_changed) {
    # The user has changed there password, so set change_password to 0
    change_password = 0
  } else {
    # The password has been reset by admin so will have to be changed next time the user
    # access' the sight
    change_password = 1
  }

  sql_update_password =
    paste0(
      "UPDATE Users",
      " SET password = ?password, ",
      " last_password_change = NOW(), ",
      " change_password = ?change_password ",
      " WHERE ",
      "user_id = ?user_id;"
    )

  query_update_password =
    DBI::sqlInterpolate(auth$pool_auth, sql_update_password,
                        password        = new_password,
                        user_id         = user_id,
                        change_password = change_password)

  DBI::dbGetQuery(auth$pool_auth, query_update_password)#

  # Let the user know the password has been saved to the db
  session$sendCustomMessage(
    type    = "shiny_alert",
    message = "New password Saved")

}

#' Password Reset/Change modal ui box
#' The UI shown depends on if this is for the logged inuser or an admin
#' changeing the passowrd of anothe user
#'
#' @param time_stamp Creates a uniqe box each time the box is opends
#'
#' @import data.table
password_change_modal = function(input, output, session,
                                 admin,
                                 user_id,
                                 time_stamp,
                                 message = NULL){

  # Check to see if this is a password change for the current user
  if (!admin) {

    #  The user to change the password is the active user
    shiny::modalDialog(
      title     = "Change Password",
      size      = "s",
      easyClose = TRUE,
      fade      = FALSE,
      footer    = shiny::tagList(
        shiny::actionButton(inputId =
                              paste0("check_and_change_password",
                                     time_stamp),
                            label   = "Change Password"),
        shiny::modalButton("Close")
      ),

      # Body of Modal Window
      shiny::fluidPage(
        shiny::fluidRow(

          if (!is.null(message)) {
              shiny::p(message)
          },

          shiny::passwordInput(
            inputId = "old_password",
            label   = "Old Password",
            width   = "100%"),
          shiny::passwordInput(
            inputId = "new_password_main",
            label   = "New Password",
            width   = "100%"),
          shiny::passwordInput(
            inputId = "new_password_confirm",
            label   = "Confirm New Password",
            width   = "100%"),
          shiny::checkboxInput(
            inputId = "password_change_confirm",
            label   = "Confirm Password Change",
            value   = FALSE,
            width   = "100%")
        )
      )
    )
  } else {

    reset_password = generate_random_password()


    # The user to change password for is not the active user
    shiny::modalDialog(
      title     = "Reset Password",
      size      = "s",
      easyClose = TRUE,
      fade      = FALSE,
      footer    = shiny::tagList(
        shiny::actionButton(inputId =
                              paste0("reset_password",
                                     time_stamp),
                            label   = "Reset Password"),
        shiny::modalButton("Close")
      ),

      # Body of Modal Window
      shiny::fluidPage(
      shiny::fluidRow(
        shiny::textInput(
          inputId = "new_password_main",
          label   = "Make Note of New Tempory Password",
          value   = reset_password),
        shiny::checkboxInput(
          inputId = "password_change_confirm",
          label   = "Confirm Password Reset",
          value   = FALSE,
          width   = "100%")
      )
    ))
  }

}


#' Check if a given user's password needs changing.  A password need's chaning if either the
#' change_password var is set to TRUE in dt_user, or if the age of the password exeads the max
#' allowed age
#'
#' @import data.table
password_change_required = function(auth) {

  # If the change_password flag is true return TRUE
  if (auth$dt_user[, change_password]) {

    TRUE

    # Check if there is a max password age
  } else if (is.null(auth$table_cofig$password$max_age_in_days)) {

    FALSE

    # Specal case where there is no limit to password age
  } else if (auth$table_cofig$password$max_age_in_days == 0 ) {

    FALSE

  } else if (auth$table_cofig$password$max_age_in_days < # max_age < password_age
             auth$dt_user[, Sys.Date() -  as.Date(last_password_change)]) {

    TRUE

  } else {
    FALSE
  }
}

#' Generate a random integer using sodium 
#'
get_random_int = function(){
      packBits(rawToBits(sodium::random(4)), type = "integer")
}

#' Generate a random character using sodium 
#'
get_random_char = function(){
      c(0:9, letters, LETTERS)[ (get_random_int() %% 62) + 1 ]
}

#' Generate a random password using sodium 
#'
generate_random_password = function(n=1, lenght=11){
      paste( sapply(1:lenght, function(x)get_random_char()), collapse =  "")
}