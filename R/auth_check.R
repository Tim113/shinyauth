#' Check the username and password given in input$user and input$password respectivly in the current namespace
#' If the credentuals are correct return the user information
#'
#' @import data.table
#' @import magrittr
#' @export
auth_check = function(input, output, session, auth) {

  # Only run if there are valid emplyee_ids and password
  shiny::req(
    input$user,
    input$password
  )

  ### Get given usrname and passowrd
  given_user_id = shiny::isolate(input$user)
  given_password   = shiny::isolate(input$password)

  # Rest the password field
  shiny::updateTextInput(session = session,
                         inputId = "password")

  ### Test if the user is in the data base
  # If the user exists then get there hased password

  # Create interpolated quirey to prevent sql injection
  sql_hashed_password   = paste0("SELECT password ",
                                 " FROM Users " ,
                                 " WHERE ",
                                 " user_id = ?id1;")

  query_hashed_password = DBI::sqlInterpolate(auth$pool_auth, sql_hashed_password,
                                              id1 = given_user_id)

  # Retreve the query from the db
  hashed_password =
    DBI::dbGetQuery(auth$pool_auth, query_hashed_password) %>%
    as.character()

  ### Check the given_user_id
  if (hashed_password == "character(0)") {
    # There was no password retreaved from db => User name incorrect
    rm(given_user_id, given_password); gc(verbose = FALSE);
    return()
  }

  ### Check the password
  if (sodium::password_verify(hashed_password, given_password)) {

    # # Remove the hashed_password and inputs
    rm(hashed_password, given_password); gc(verbose = FALSE);

    # Return the user id
    return(given_user_id)

  } else {
    # The password is wrong
    rm(given_user_id, given_password); gc(verbose = FALSE);
    return()
  }
}
