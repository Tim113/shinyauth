#' Make the auth object from the auth_config file and the connection
#'
#' @param config_path The path to the config file used by Shiny Basic Auth
#'
#' @import data.table
#' @import magrittr
make_auth_object = function(config_path) {
  # Extract the table names, or set defults in the auth object from auth_config

  ### Connect to db
  # Load the config file
  auth_config = yaml::yaml.load_file(config_path)

  # Connect to the auth db
  pool_auth = pool::dbPool(
    drv      = RMySQL::MySQL(),
    dbname   = auth_config$users_table$dbname_auth,
    username = auth_config$users_table$username,
    password = auth_config$users_table$password,
    host     = auth_config$users_table$host,
    port     = auth_config$users_table$port
  )

  # Connect to the data_db
  if (is.null(auth_config$users_table$dbname_data)) {
    pool_data = NULL
  } else if (auth_config$users_table$dbname_data ==
             auth_config$users_table$dbname_auth) {
    pool_data = pool_auth
  } else {
    pool_data = pool::dbPool(
      drv      = RMySQL::MySQL(),
      dbname   = auth_config$users_table$dbname_data,
      username = auth_config$users_table$username,
      password = auth_config$users_table$password,
      host     = auth_config$users_table$host,
      port     = auth_config$users_table$port
    )
  }


  auth = list(
    pool_auth   = pool_auth,
    pool_data   = pool_data,
    user_id     = NULL,
    dt_user     = NULL,
    table_cofig = auth_config$table_cofig
  )

  return(auth)
}