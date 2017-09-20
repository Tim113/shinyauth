#' Make an auth object like the one that would be present after a sucessful login,
#' To this end the funciton can  create an auth object of either no-login or the login of a given
#' user.  However, the defult behavoir of the function is create auth for the 'first_user' given
#' in the config file
#'
#' @param config_path Path to config.yaml used by shinyauth
#' @param user_id The user id to create a loged in auth object for, if null defults to first_user
#' @param no_user Will override user_id and create an auth object for no user loged in
#'
#' @import data.table
#' @import magrittr
#' @export
test_auth = function(config_path, user_id = NULL, no_user = FALSE) {

  # If this function is being run from shiny-server stop and return an error as this funciton
  # should only be used for testing localy
  if (shiny::serverInfo()$shinyServer) {
    stop(paste0(
      "You are using the funciton test_auth on shiny-server, this function is only for testing ",
      " code in a console or IDE."
    ))
  }

  # Create auth object
  auth = make_auth_object(config_path)

  # If there is not to be a user logged in mocked then return auth
  if (no_user) {
    return(auth)
  }

  # If user_id is null defult to the 'first_user' given in the config file
  if (is.null(user_id)) {

    auth_config = yaml::yaml.load_file(config_path)

    user_id = auth_config$first_user$user_id

  }

  # Check that user_id exits in the db
  if (!check_user_id(auth, user_id)) {
    stop(paste0(
      "user_id ", user_id, " does not exist in the db."
    ))
  }

  # Add uer_id to auth object
  auth$user_id = user_id

  # Get the user from the db
  auth$dt_user = get_dt_user(auth)[]

  # Retun auth
  auth
}




#' Given a user id return TRUE if they exist in the db and FALSE if not
check_user_id = function(auth, user_id) {

  # Create qerey to check for users existance
  sql = "SELECT EXISTS(SELECT * FROM Users WHERE user_id = ?user_id);"

  # Interpolate user id
  sql = DBI::sqlInterpolate(
    conn    = auth$pool_auth,
    sql     = sql,
    user_id = user_id)

  # Get from db, exists will be 1 if exits and 0 else
  exists = DBI::dbGetQuery(conn = auth$pool_auth, sql)

  # Convert 1 or 0 in to TRUE or FALSE return
  if (exists == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}


