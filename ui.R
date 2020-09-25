ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Mathieu Boully"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Vue générale", tabName = "1", icon = icon("dashboard")),
                        menuItem("Analyse biomécanique", tabName = "2", icon = icon("angle-double-right")),
                        menuItem("Analyse énergétique", tabName = "3", icon = icon("atom")),
                        menuItem("Analyse factorielle", tabName = "4", icon = icon("chart-bar"))
                      ),
                      dateRangeInput("dateRange",
                                     label = "Sélectionner une période :",
                                     start = min(df$Date), end = max(df$Date)),
                      sliderInput("slider_distance", 
                                  label = "Distance [km]", 
                                  value = c(min(df$Distance), max(df$Distance)),
                                  min = min(df$Distance),
                                  max = max(df$Distance),
                                  step = 2),
                      sliderInput("slider_cadence", 
                                  label = "Cadence [ppm]", 
                                  value = c(min(df$Cadence.de.course.moyenne), max(df$Cadence.de.course.moyenne)),
                                  min = min(df$Cadence.de.course.moyenne),
                                  max = max(df$Cadence.de.course.moyenne),
                                  step = 2),
                      sliderInput("slider_fc", 
                                  label = "Fréquence cardiaque [bpm]", 
                                  value = c(min(df$Fréquence.cardiaque.moyenne), max(df$Fréquence.cardiaque.moyenne)),
                                  min = min(df$Fréquence.cardiaque.moyenne),
                                  max = max(df$Fréquence.cardiaque.moyenne),
                                  step = 5),
                      sliderInput("slider_cal", 
                                  label = "Calories [kcal]", 
                                  value = c(min(df$Calories), max(df$Calories)),
                                  min = min(df$Calories),
                                  max = max(df$Calories),
                                  step = 200),
                      sliderInput("slider_den", 
                                  label = "Gain d'altitude [m]", 
                                  value = c(min(df$Gain.alt), max(df$Gain.alt)),
                                  min = min(df$Gain.alt),
                                  max = max(df$Gain.alt),
                                  step = 200)
                      
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "1",
                                fluidRow(
                                  valueBoxOutput("moyenne_distance"),
                                  valueBoxOutput("dist_cum"),
                                  valueBoxOutput("den_cum"),
                                  box(plotlyOutput(outputId = "violin")),
                                  box(plotlyOutput(outputId = "zone_fc")),
                                  box(plotlyOutput(outputId = "hist_distance")),
                                  tabBox(
                                    id = "tabset1",
                                    tabPanel("Par mois",
                                             selectInput("select_annee",
                                                         selected = 2020,
                                                         label = "Sélectionner une année d'analyse :",
                                                         choices = df$Annee),
                                             plotlyOutput(outputId = "evolution_par_mois")),
                                    tabPanel("Par semaine",
                                             plotlyOutput(outputId = "evolution_par_semaine")),
                                    tabPanel("Par jour",
                                             plotlyOutput(outputId = "evolution_par_jour"))
                                  )
                                  
                                )),
                        tabItem(tabName = "2",
                                fluidRow(
                                  valueBoxOutput("moyenne_cadence"),
                                  valueBoxOutput("moyenne_foulee"),
                                  valueBoxOutput("ecart_foulee"),
                                  box(plotlyOutput(outputId = "long_foulee")),
                                  box(plotlyOutput(outputId = "fc"))
                                )),
                        tabItem(tabName = "3",
                                fluidRow(
                                  valueBoxOutput("moyenne_fc"),
                                  valueBoxOutput("cal_cum"),
                                  valueBoxOutput("moyenne_aerobie"),
                                  box(plotlyOutput(outputId = "zone_aerobie")),
                                  box(selectInput("select_annee_fc",
                                                  selected = "2020",
                                                  label = "Sélectionner une année d'analyse :",
                                                  choices = df$Annee),
                                      plotlyOutput(outputId = "fc_par_mois"))
                                )),
                        tabItem(tabName = "4",
                                fluidRow(
                                  box("Graphique des valeurs propres", plotOutput(outputId = "scree_plot")),
                                  box("Valeurs propres et proportion de variances", dataTableOutput(outputId = "axe")),
                                  box("Graphique de corrélation des variables", plotOutput(outputId = "acm_variable")),
                                  box("Qualité de représentation", dataTableOutput(outputId = "cos2"))
                                ))
                      )
                    )
)
