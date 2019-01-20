library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)

ui <- dashboardPage(
  
  skin = "black",
  ###############################
  dashboardHeader(title = "Me, my, mine",
                  dropdownMenu(type = "notifications", badgeStatus = "warning",
                               notificationItem(icon = icon("exclamation-triangle"), status = "info",
                                                "This app is underdeveloped"
                               )
                  )),
  #################################
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Wojtek1", tabName = "dashboard1", icon = icon("dashboard")),
      menuItem("Wojtek2", tabName = "dashboard2", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about", badgeLabel = "new",
               badgeColor = "green")
    )
  ),
  ###########################
  dashboardBody(
    tabItems(
      tabItem("dashboard1",
              fluidRow(
                height =20,
                column(width = 6,
                       box(title = "Plot",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="day_steps_plt", height = 600)
                       )
                ),
                
                column(width = 6,
                       box(title = "Plot",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="day_step_length", height = 600)
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(title = "Plot",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="series_steps")
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(title = "Plot",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="series_step_length")
                       )
                )
              )
      ),
      ##################################
      tabItem("dashboard2",
              fluidRow(
                column(width = 12,
                       box(title = "Plot",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="sleep_steps_density", height = 1000)
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       box(title = "Plot",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="day_steps_violin")
                       )
                ),
                column(width = 6,
                       box(title = "Plot",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="day_sleep_violin")
                       )
                )
              )
      ),
      #################################
      tabItem("about",
              "About the app"
      )
    )
  )
  #########################
)

server <- function(input, output) {
  
  output[["day_steps_plt"]] <- renderPlot(
    day_steps_plt
    )
  
  output[["day_step_length"]] <- renderPlot(
    day_step_length
  )
  
  output[["series_step_length"]] <- renderPlot(
    series_step_length
  )
  
  output[["series_steps"]] <- renderPlot(
    series_steps
  )
  
  output[["sleep_steps_density"]] <- renderPlot(
    sleep_steps_density
  )
  
  output[["day_steps_violin"]] <- renderPlot(
    day_steps_violin
  )
  
  output[["day_sleep_violin"]] <- renderPlot(
    day_sleep_violin
  )
  
}

shinyApp(ui, server)