library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(ggiraph)

ui <- dashboardPage(
  
  skin = "black",
  ###############################
  dashboardHeader(title = "Me, my, mine"),
  #################################
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Kroki", tabName = "dashboard1", icon = icon("dashboard")),
      menuItem("Kroki a sen", tabName = "dashboard2", icon = icon("dashboard")),
      menuItem("Sen", tabName = "dashboard3", icon=icon("dashboard")),
      menuItem("Napoje", tabName = "dashboard4", icon = icon("dashboard")),
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
                       box(title = "Ile kroków dziennie?",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="day_steps_plt", height = 600)
                       )
                ),
                
                column(width = 6,
                       box(title = "Jak duże są moje kroki?",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="day_step_length", height = 600)
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(title = "Piesze wycieczki",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="series_steps")
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(title = "Piesze wycieczki - długość kroku",
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
      ########################
      tabItem("dashboard3",
              fluidRow(
                height =20,
                column(width = 12,
                       box(title = "Plot",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="czas_snu", height = 600)
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       box(title = "Plot",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="sen_Ola_plt")
                       )
                ),
                column(width = 6,
                       box(title = "Plot",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           plotOutput(outputId="sen_Gosia_plt")
                       )
                )
              )
      ),
      #################################
      tabItem("dashboard4",
              fluidRow(
                height =20,
                column(width = 12,
                       box(title = "Pić znaczy żyć",
                           solidHeader = TRUE,
                           width = 12,
                           HTML(
                             paste(
                               p("Przedstawione poniżej wykresy dotyczą napojów wypitych przez Gosię Wachulec przez okres 30 dni. Dane pochodzą z przełomu grudnia 2018 roku i stycznia 2019 roku.")
                             )
                           )
                       )
                )
              ),
              fluidRow(
                height =20,
                column(width = 12,
                       box(title = "Co piję i w jakich ilościach?",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           HTML(
                             paste(
                               p("Ponad 70 litrów na napojów w 30 dni, lecz czego wypiłam najwięcej? Sprawdźmy. \nJedna kratka na wykresie to jeden procent wypitych przez miesiąc napojów.")
                             )
                           ),
                           ggiraphOutput("napoje")
                       )
                )
              ),
              fluidRow(
                height =20,
                column(width = 12,
                       box(title = "Kiedy spożywam dane napoje i w jakich ilościach?",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           width = 12,
                           HTML(
                             paste(
                               p("Do którego posiłku najchętniej piję kawę, a kiedy sięgam po wino? Na poniższym wykresie widać, kiedy piję dane napoje. Nazwy posiłków i pór dnia są ułożone od najwcześniejszych do najpoźniejszych, co oznacza, że góra wykresu pokazuje jakie napoje spożywam rano, a dół wykresu - co piję popołudniu i wieczorem. Dodatkowo wielkość punktów jest proporcjonalna do liczby spożytych przeze mnie napojów.")
                             )
                           ),
                           ggiraphOutput("posilki")
                       )
                )
              )
      ),
      #################################
      tabItem("about",
              fluidRow(
                height =20,
                column(width = 12,
                       box(title = "Me, my, mine",
                           solidHeader = TRUE,
                           width = 12,
                           HTML(
                             paste(
                               p("Ta aplikacja została utworzona w ramach drugiego projektu grupowego z przedmiotu Techniki Wizualizacji Danych na Politechnice Warszawskiej przez Wojciecha Kretowicza, Małgorzatę Wachulec i Aleksandrę Wichrowską. Wybrany przez nas temat to 'Projekt Ja' i w tej aplikacji przedstawiamy nasze dane dotyczące liczby kroków oraz przebytego przez nas dystans, czasu oraz godzin naszego snu, a także spożywanych przez nas napojów."),'<br/>',
                               p("Zapraszamy!")
                             )
                           )
                       )
                )
              )
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
  
  output[["czas_snu"]] <- renderPlot(
    czas_snu
  )
  
  output[["sen_Ola_plt"]] <- renderPlot(
    sen_Ola_plt
  )
  
  output[["sen_Gosia_plt"]] <- renderPlot(
    sen_Gosia_plt
  )
  
  output[["napoje"]] <- renderggiraph({
    df <- read.csv2("DataFrameNapoje.csv")
    df$category <- as.character(df$category)
    df$category <- factor(df$category, levels=c("Herbata","Woda","Kawa", "Wino","Mocny Alkohol"))
    g <- ggplot(df, aes(x = x, y = y, fill = category)) + 
      geom_tile_interactive(aes(width = 1, height = 1,tooltip = df$text),color = "black", size = 0.2)+
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
      labs(title="Procentowy udział napojów", subtitle="Najedź na daną sekcję, żeby zobaczyć dokładne wartości") +
      scale_fill_manual(values=c("lightcyan","darkolivegreen1", "lightyellow","lightsalmon","lightpink")) +
      theme(plot.title = element_text(size = rel(1.6)),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.title = element_blank(),
            legend.position = "right")
    ggiraph(code = print(g))
  }) 
  
  output[["posilki"]] <- renderggiraph({
    picie3 <- read.csv2("Picie2.csv")
    picie3 <- picie3[,1:3]
    names(picie3) <- c("kiedy","co","ile")
    picie3$kiedy <- as.character(picie3$kiedy)
    picie3$kiedy <- factor(picie3$kiedy, levels=c("Wieczorem","Obiad","Lunch", "Bez posilku","Cos slodkiego","Sniadanie"))
    picie3$co <- as.character(picie3$co)
    picie3$co <- factor(picie3$co, levels=c("Herbata","Woda","Kawa", "Wino","Alkohol mocny"))
    
    gdot <- ggplot(picie3,aes(x=co,y=kiedy,fill=co))+
      
      geom_point_interactive(stat='identity',tooltip = picie3$ile,shape = 21, colour = "black", size = picie3$ile) +
      guides(fill=FALSE) +
      theme_minimal()+
      scale_fill_manual(values=c("lightcyan","darkolivegreen1", "lightyellow","lightsalmon","lightpink")) +
      scale_y_discrete(expand = c(0.08,0,0.15,0)) +
      labs(title="Napoje a posiłki",subtitle="Najedź na punkt, żeby zobaczyć ile jednostek wypiłam miesięczne")+
      theme(plot.title = element_text(size = rel(1.6)),
            plot.subtitle = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
            axis.title = element_blank(),
            legend.position = "right")
    girafe(code = print(gdot))
  })
  
}

shinyApp(ui, server)