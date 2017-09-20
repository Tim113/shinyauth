library(shiny)
library(shinyauth)

# # The following line must have been run:
# shinyauth::create_auth_tables(auth_config_path = "~/.auth_example.yaml")

### Server funciton to run when the user is logged in
server_post_auth = function(input, output, session, auth) {

  # Tranditional Server functions
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

  # Render the ui the ui
  output$body = renderUI({
    shiny::conditionalPanel(  # You must use condtional panels for the tabs
      condition = "input.authMenuItems == 'histogram'",
      # Boxes need to be put in a row (or column)
      fluidRow(
        box(plotOutput("plot1", height = 250)),

        box(
          title = "Controls",
          sliderInput("slider", "Number of observations:", 1, 100, 50)
        )
      ),

      # Let the user know if running in shiny server
      fluidRow(
        box(
          title = "Running in Shiny Server",
          serverInfo()$shinyServer
        )
      )

      )
  })

  output$sidebar = renderUI({
    shinyauth::authSidebarMenu(
      auth = auth,
      shinydashboard::menuItem(text     = "A Histogram",
                               tabName  = "histogram")

    )
  })

  # Select the histogram tab
  updateTabItems(session, inputId = "authMenuItems", selected = "histogram")

}

### Call the sheppey auth server
shinyauth::auth_server(
  server      = server_post_auth,
  config_path = "./auth_conf.yaml")