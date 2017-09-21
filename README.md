# shinyauth 
  
To use this pacakge you must have a MySQL databse accessable by a shiny app where your dashboard is to be run, and a config file based on the tempate provided. 
  
### shinyauth setup 
First you must have a shema in your MySQL database and a user with full write privileges for the shema.  The name of this shema and the users login detils need to be in the config file for shiny basic auth (see example for the whole file), 
``` 
users_table: 
  dbname_auth: shinyauth 
  username: auth_user 
  password: password123 
  host: 127.0.0.1 
  port: 3306 
``` 
 
Then you can make the User tables using the following command, it will create a table in your db called Users, and along with a first user as specified in your config file (again see the minimal example). 
``` 
shinyauth::create_auth_tables(auth_config_path = "./auth_conf.yaml") 
> TRUE 
``` 
Once this table exists you can run the dashboard and will be able to log in user the user just created. 
## Creating the dashboard 
There are two main differences between a regular shinydashboard, and a dashboard made using shinyauth. 
1) The top level server function is called passed `auth_server` instead of being called in server.R (or main.R 
2) The `saSidebar` and `asBody` are used inseted of `dashboardSidebar` and ``dashboardBody` respectivly 
 
### 1) Changes to server function 
For a regular shinydashbaord your server file will look something like this 
``` 
## server.R ## 
library(shiny) 
server = function(input, output, session) {  
   # Some code and hopefully comments 
} 
``` 
To change this to work with shinyauth it must look like this 
``` 
## server.R ## 
library(shiny) 
server_post_auth =  function(input, output, session, auth) {  
   # Some code and hopefully comments 
} 
### Call the auth server 
shinyauth::auth_server( 
  server      = server_post_auth, 
  config_path = "./auth_conf.yaml") 
``` 
There are two differences.  Firstly your server function is passed to shinyauth::auth_server which will not run it until a users successfully logs in.  Secondly the server function accepts a forth argument, `auth`, this list is needed by all shinyauth functions.  (Read more about the auth object below). 
 
### 2) Changes to ui 
In shinydashbaord your ui file will have this layout: 
``` 
## ui.R ##  
library(shiny)  
dashboardPage(  
   dashboardHeader(),  
   dashboardSidebar(),  
   dashboardBody() 
) 
``` 
For shinyauth this needs to be: 
## ui.R ## 
library(shiny) 
dashboardPage( 
   dashboardHeader(), 
   saSidebar(), 
   saBody() 
) 
``` 
This is because shinyauth needs additional ui elements in the side bar and the ui to function. 
 
 
 
 
