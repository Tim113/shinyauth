#' Function to create tables for shinyauth, this funciton will also chec that the
#' auth.config.yaml file contanis the requite information
#'
#' @import data.table
#' @import magrittr
#' @export
create_auth_tables = function(auth_config_path) {

  # Load the config file
  auth_config = yaml::yaml.load_file(auth_config_path)

  #################### Basic Checks ##########
  # Check that auth_config has all of the requiered columns
  required_columns = c(
    "user_id",
    "password",
    "date_created",
    "last_password_change",
    "change_password",
    "admin",
    "moderator")

  # Stop the funciton if not all of the columns are present
  if (!all(required_columns %in% names(auth_config$table_cofig))) {
    # Find all of the columsn that are not included to give a nice error message
    missing_columns =
      required_columns[!(required_columns %in% names(auth_config$table_cofig))]
    stop(paste0("Requiered column: ", as.character(missing_columns),
                " is not present.  "))
  }

  # Check that the use_moderatior entary exist iff use_moderatior: TRUE
  if (auth_config$table_cofig$moderator$use_moderatior &
      !("users_moderator" %in% names(auth_config$table_cofig))) {
    stop("use_moderatior: TRUE but there is no users_moderator in the config file")
  } else if (!auth_config$table_cofig$moderator$use_moderatior &
             "users_moderator" %in% names(auth_config$table_cofig)) {
    stop("use_moderatior: FALSE but there is a users_moderator in the config file.  ",
         "You have probably made a mistake.")
  }

  # Check that a valid first user has been given
  if (is.null(auth_config$first_user)) {
    stop("No first user has been given for table creation.")
  } else if (is.null(auth_config$first_user$user_id)) {
    stop("No user id has been given for the first user.")
  } else if (is.null(auth_config$first_user$password)) {
    stop("No password has been given for the first user.")
  }

  # Check that no deults have been given for anything other than custom columns
  # if there has been return an error
  for (col_name in required_columns) {
    if (!is.null(auth_config$table_cofig[[col_name]]$defult)) {
      stop(paste0(
        "A defult value has been given for ", col_name, ".  Please remove this from the config file."
      ))
    }
  }

  #################### Requiered Columns ##########
  ########## User_id
  # Set the type of column for user id
  if (auth_config$table_cofig$user_id$type == "integer") {

    model_Users = data.table::data.table(user_id = integer())

  } else if (auth_config$table_cofig$user_id$type == "numeric") {

    model_Users = data.table::data.table(user_id = numeric())

  } else if (auth_config$table_cofig$user_id$type == "character") {

    model_Users = data.table::data.table(user_id = character())

  } else {
    # The user_id column is not of a valid form
    stop(paste0(
      "The type fo the user_id column has been given as ",
      auth_config$table_cofig$user_id$type,
      ".  Howerver user_id must be one of integer, numeric, character"))
  }

  ########## Password
  # make the password column
  model_Users[, password := character()]

  ########## Date Created
  # make the date_created column
  model_Users[, date_created := as.Date(character())]

  ########## Last Password Change
  # make the last_password_change column
  model_Users[, last_password_change := as.Date(character())]

  ########## Cahnge Password
  # make the change_passowrd column
  model_Users[, change_password := logical()]

  ########## Admin
  # make the admin column
  model_Users[, admin := logical()]

  ########## moderator
  # Only run this section if use_moderatior: TRUE
  if (auth_config$table_cofig$moderator$use_moderatior) {
    ### Moderator
    # make the moderator column
    model_Users[, moderator := logical()]

    ### Users Moderatior
    # make the users_moderator column
    # The type of column for users_modeator must be the same as user_id
    model_Users[, users_moderator := user_id]
  }

  #################### Additonal Columns ##########
  # Find all of the addional columns that the user has created
  additonal_columns =
    names(auth_config$table_cofig)[!(names(auth_config$table_cofig) %in%
                                       c(required_columns, "users_moderator"))]


  # Now we have a list of all of the additonal columns we must add each of them in tern
  # to the model table
  for (col_name in additonal_columns) {
    # They type of column for users_modeator must be the same as user_id
    if (auth_config$table_cofig[[col_name]]$type == "integer") {

      model_Users[, (col_name) := integer()]

    } else if (auth_config$table_cofig[[col_name]]$type == "numeric") {

      model_Users[, (col_name) := numeric()]

    } else if (auth_config$table_cofig[[col_name]]$type == "character") {

      model_Users[, (col_name) := character()]

    } else if (auth_config$table_cofig[[col_name]]$type == "datetime") {

      model_Users[, (col_name) := as.Date(character())]

    } else if (auth_config$table_cofig[[col_name]]$type == "logical") {

      model_Users[, (col_name) := logical()]

    } else if (auth_config$table_cofig[[col_name]]$type == "categorical") {

      # Check that the leves of the catagorcial verable have been set
      if (is.null(auth_config$table_cofig[[col_name]]$categories)) {
        stop(paste0(
          "There are no categories set in the config file for column: ",
          col_name,
          "."
        ))
      } else {
        model_Users[, (col_name) := character()]
      }

    } else {
      # If we are here then the given column type is not valid so return an erro
      stop("The given type of column ", col_name, " is ",
           auth_config$table_cofig[[col_name]]$type,
           ".  This is not a valid column type please reffer back to the documentation.")
    }
  }

  #################### Create Table ##########
  # Set key for the table
  data.table::setkeyv(model_Users, "user_id")

  # Create the Query
  create_query_Users = dbUpdateTable::create(
    verbose = FALSE,
    model   = model_Users)

  # Replace the NA for date with just date
  create_query_Users = gsub("NA", "datetime",  create_query_Users)

  # Connect to the db
  con = RMySQL::dbConnect(
    RMySQL::MySQL(),
    dbname   = auth_config$users_table$dbname_auth,
    username = auth_config$users_table$username,
    password = auth_config$users_table$password,
    host     = auth_config$users_table$host,
    port     = auth_config$users_table$port)

  # Make the tables
  DBI::dbGetQuery(con, create_query_Users)

  #################### Make first user ##########
  ### Get the user cration defults
  # Return a named list of user defults
  ls_defaults = list_defults(auth_config)

  # All users defult to not being moderators if moderators exist
  sql_create_user = paste0(
    "INSERT INTO Users ",
    " ( user_id, password, admin, date_created, last_password_change, change_password" ,

    if (auth_config$table_cofig$moderator$use_moderatior) {
      ", moderator"
    },

    # Only add in defult values if the schema includes them
    if (length(ls_defaults) != 0) {
      paste0(
        ", ",
        paste0( names(ls_defaults), collapse = ", ")
      )
    },
    " ) ",
    " VALUES ( ?user_id, ?password, '1', NOW(), NOW(), '1'",

    if (auth_config$table_cofig$moderator$use_moderatior) {
      ", '0'"
    },

    # Only add in defult values if the schema includes them
    if (length(ls_defaults) != 0) {
      paste0(
        ", ",
        paste0(" ?", names(ls_defaults), collapse = ", ")
      )
    },
    " );")


  query_create_user = do.call(
    DBI::sqlInterpolate,
    c(
      con, sql_create_user,
      user_id      = auth_config$first_user$user_id,
      password     = sodium::password_store(auth_config$first_user$password),
      ls_defaults
    )
  )

  DBI::dbGetQuery(con, query_create_user)

  # Kill the connection
  RMySQL::dbDisconnect(con)

}
