################## SERVEUR ###################
server <- shinyServer(function(input, output) {
  
  df_update <- reactive({
    
    df <- df[df$Date >= input$dateRange[1],]
    df <- df[df$Date <= input$dateRange[2],]
    
    df <- df[df$Distance >= input$slider_distance[1],]
    df <- df[df$Distance <= input$slider_distance[2],]
    
    df <- df[df$Cadence.de.course.moyenne >= input$slider_cadence[1],]
    df <- df[df$Cadence.de.course.moyenne <= input$slider_cadence[2],]
    
    df <- df[df$Fréquence.cardiaque.moyenne >= input$slider_fc[1],]
    df <- df[df$Fréquence.cardiaque.moyenne <= input$slider_fc[2],]
    
    df <- df[df$Calories >= input$slider_cal[1],]
    df <- df[df$Calories <= input$slider_cal[2],]
    
    df <- df[df$Gain.alt >= input$slider_den[1],]
    df <- df[df$Gain.alt <= input$slider_den[2],]
    
    return(df)
  })
  
  df_update_annee <- reactive({
    
    df <- df[df$Annee == input$select_annee,]
    
    return(df)
  })
  
  df_update_annee_fc <- reactive({
    
    df <- df[df$Annee == input$select_annee_fc,]
    
    return(df)
  })
  
  output$fc <- renderPlotly({
    df <- df_update()
    
    graph <- plot_ly(x = df$Fréquence.cardiaque.moyenne, 
                     y = df$Cadence.de.course.moyenne,
                     type = "scatter",
                     color = df$Fréquence.cardiaque.moyenne)
    ##modlinXY <- lm(df$Longueur.foulÃ.e ~ df$Vitesse)
    ##abline(modlinXY, col="red")
    graph <- layout(p = graph,
                    data = df,
                    title = "Cadence en fonction de la FC",
                    xaxis = list(title = "FC [bpm]"),
                    yaxis = list(title = "Cadence [ppm]"))
  })
  
  output$long_foulee <- renderPlotly({
    df <- df_update()
    graph <- plot_ly(x = df$Longueur.foulee, y = df$Cadence.de.course.moyenne,
                     type = "scatter",
                     size = df$Fréquence.cardiaque.moyenne)
    graph <- layout(p = graph,
                    data = df,
                    title = "Cadence en fonction de la longueur de la foulée",
                    xaxis = list(title = "Longueur de la foulée [m]"),
                    yaxis = list(title = "Cadence [ppm]"))
  })
  
  output$aerobie <- renderPlotly({
    df <- df_update()
    graph <- plot_ly(x = df$Longueur.foulee, y = df$TE.aérobie,
                     type = "scatter")
    graph <- layout(p = graph,
                    data = df,
                    title = "Aérobie en fonction de la longueur de la foulée",
                    xaxis = list(title = "Longueur de la foulée [m]"),
                    yaxis = list(title = "Aérobie"))
  })
  
  output$hist_distance <- renderPlotly({
    df <- df_update()
    graph <- plot_ly(data = df, x = df$Distance,
                     type = "histogram",
                     histnorm = "probability")
    graph <- layout(p = graph,
                    data = df,
                    title = "Histogramme de la distance",
                    xaxis = list(title = "Distance [km]"),
                    yaxis = list(title = "Fréquence"))
  })
  
  output$hist_fc <- renderPlotly({
    df <- df_update()
    graph <- plot_ly(data = df, x = df$Fréquence.cardiaque.moyenne,
                     type = "histogram",
                     histnorm = "probability")
    graph <- layout(p = graph,
                    data = df,
                    title = "Histogramme de la Fréquence cardiaque",
                    xaxis = list(title = "FC [bpm]"),
                    yaxis = list(title = "Fréquence"))
  })
  
  output$fc_par_mois <- renderPlotly({
    df <- df_update_annee_fc()
    
    df$Mois <- month(df$Date)
    df$Mois <- as.factor(df$Mois)
    
    df$Noms_Mois <- fct_collapse(df$Mois,
                                 Janvier = c("1"),
                                 Fevier = c("2"),
                                 Mars = c("3"),
                                 Avril = c("4"),
                                 Mai = c("5"),
                                 Juin = c("6"),
                                 Juillet = c("7"),
                                 Août = c("8"),
                                 Semptembre = c("9"),
                                 Octobre = c("10"),
                                 Novembre = c("11"),
                                 Décembre = c("12")
    )
    
    fc_moy <- tapply(df$Fréquence.cardiaque.moyenne,df$Noms_Mois, mean)
    fc_min <- tapply(df$Fréquence.cardiaque.moyenne,df$Noms_Mois, min)
    fc_max <- tapply(df$Fréquence.cardiaque.moyenne,df$Noms_Mois, max)
    
    graph <- plot_ly(y = fc_max,
                     type = "scatter",
                     mode = "lines+markers", name = "Elevé")
    graph <- graph %>% add_trace(y = fc_moy, name = "Moyenne", mode = 'lines+markers')
    graph <- graph %>% add_trace(y = fc_min, name = "Basse", mode = 'lines+markers')
    graph <- layout(p = graph,
                    data = df,
                    title = "Evolution de la FC par mois",
                    xaxis = list(title = "Mois"),
                    yaxis = list(title = "Fréquence cardiaque [bpm]"))
  })
  
  output$evolution_par_jour <- renderPlotly({
    df <- df_update_annee()
    graph <- plot_ly(data = df, x = df$Date, y = df$Distance,
                     type = "bar")
    graph <- layout(p = graph,
                    data = df,
                    title = "Evolution de la distance par jour",
                    xaxis = list(title = "Jours"),
                    yaxis = list(title = "Distance [km]"))
  })
  
  output$evolution_par_mois <- renderPlotly({
    df <- df_update_annee()
    df$Mois <- month(df$Date)
    df$Mois <- as.factor(df$Mois)
    
    df$Noms_Mois <- fct_collapse(df$Mois,
                                 Janvier = c("1"),
                                 Fevier = c("2"),
                                 Mars = c("3"),
                                 Avril = c("4"),
                                 Mai = c("5"),
                                 Juin = c("6"),
                                 Juillet = c("7"),
                                 Août = c("8"),
                                 Semptembre = c("9"),
                                 Octobre = c("10"),
                                 Novembre = c("11"),
                                 Décembre = c("12")
    )
    graph <- plot_ly(data = df, x = df$Noms_Mois, y = df$Distance,
                     type = "bar")
    graph <- layout(p = graph,
                    data = df,
                    title = "Evolution de la distance par mois",
                    xaxis = list(title = "Mois"),
                    yaxis = list(title = "Distance [km]"))
  })
  
  output$evolution_par_semaine <- renderPlotly({
    df <- df_update_annee()
    df$Semaine <- week(df$Date)
    df$Semaine <- as.factor(df$Semaine)
    
    graph <- plot_ly(data = df, x = df$Semaine, y = df$Distance,
                     type = "bar")
    graph <- layout(p = graph,
                    data = df,
                    title = "Evolution de la distance par semaine",
                    xaxis = list(title = "Semaine"),
                    yaxis = list(title = "Distance [km]"))
  })
  
  output$violin <- renderPlotly({
    df <- df_update()
    
    graph <- plot_ly(data = df, y = df$Distance,
                     type = "violin",
                     box = list(
                       visible = T
                     ),
                     meanline = list(
                       visible = T
                     ))
    graph <- layout(p = graph,
                    data = df,
                    title = "Distribution de la distance",
                    yaxis = list(title = "Distance [km]"))
  })
  
  output$dist_cum <- renderValueBox({
    df <- df_update()
    
    somme <- sum(df$Distance)
    somme <- round(somme, digits = 0)
    valueBox(
      paste0(somme, " km"), "Distance totale [km]", icon = icon("road"),
      color = "blue"
    )
  })
  
  output$moyenne_distance <- renderValueBox({
    df <- df_update()
    
    moyenne <- mean(df$Distance)
    moyenne <- round(moyenne, digits = 0)
    valueBox(
      paste0(moyenne, " km"), "Distance moyenne [km]", icon = icon("route"),
      color = "blue")
  })
  
  output$den_cum <- renderValueBox({
    df <- df_update()
    
    somme <- sum(df$Gain.alt)
    valueBox(
      paste0(somme, " m"), "Dénivelé positif total [m]", icon = icon("mountains"),
      color = "blue"
    )
  })
  
  #indicateurs biomécaniques
  output$moyenne_cadence <- renderValueBox({
    df <- df_update()
    
    moyenne <- mean(df$Cadence.de.course.moyenne)
    moyenne <- round(moyenne, digits = 1)
    valueBox(
      paste0(moyenne, " ppm"), "Cadence de course moyenne [pas par minute]", icon = icon("shoe-prints"),
      color = "blue"
    )
  })
  
  output$moyenne_foulee <- renderValueBox({
    df <- df_update()
    
    moyenne <- mean(df$Longueur.moyenne.des.foulées)
    moyenne <- round(moyenne, digits = 1)
    valueBox(
      paste0(moyenne, " m"), "Longueur moyenne des foulées [m]",
      color = "blue"
    )
  })
  
  output$ecart_foulee <- renderValueBox({
    df <- df_update()
    
    ecart <- sd(df$Longueur.moyenne.des.foulées)
    ecart <- round(ecart, digits = 2)
    valueBox(
      paste0(ecart, " m"), "Ecart-type de la longueur des foulées [m]",
      color = "blue"
    )
  })
  
  #indicateurs énergétiques
  output$moyenne_fc <- renderValueBox({
    df <- df_update()
    
    moyenne <- mean(df$Fréquence.cardiaque.moyenne)
    moyenne <- round(moyenne, digits = 0)
    valueBox(
      paste0(moyenne, " bpm"), "FC moyenne [battements par minute]", icon = icon("heartbeat"),
      color = "blue"
    )
  })
  
  output$cal_cum <- renderValueBox({
    df <- df_update()
    
    somme <- sum(df$Calories)
    valueBox(
      paste0(somme, " kcal"), "Calories totale [kcal]", icon = icon("fire-alt"),
      color = "blue"
    )
  })
  
  output$moyenne_aerobie <- renderValueBox({
    df <- df_update()
    
    moyenne <- mean(df$TE.aérobie)
    moyenne <- round(moyenne, digits = 1)
    valueBox(
      moyenne, "Aérobie", icon = icon("battery-half"),
      color = "blue"
    )
  })
  
  output$zone_fc <- renderPlotly({
    df <- df_update()
    
    df$fc5cl <- cut(df$Fréquence.cardiaque.moyenne, c(95, 113, 132, 151, 170, 210), right = T, include.lowest = T)
    zone <- list("Zone 1 [95-113 bpm]", "Zone 2 [114-132 bpm]", "Zone 3 [133-151 bpm]", "Zone 4 [152-170 bpm]", "Zone 5 [> 171]")
    data <- table(df$fc5cl)
    total <- sum(data)
    pourc <- round((data / total)*100, digits = 0)
    graph <- plot_ly(x = data, y = zone, 
                     type = "bar", orientation = "h",
                     text = paste0(pourc, " %"), textposition = 'auto',
                     marker = list(color = c('rgba(163,167,235,92)', 'rgba(28,62,255,100)',
                                             'rgba(41,255,16,100)', 'rgba(255,129,13,100)',
                                             'rgba(255,53,43,100)')))
    graph <- layout(p = graph,
                    data = df,
                    title = "Zones de FC",
                    xaxis = list(title = "Nombre de courses"))
  })
  
  output$zone_aerobie <- renderPlotly({
    df <- df_update()
    
    df$aerobie5cl <- cut(df$TE.aérobie, 5, include.lowest = T)
    zone <- list("Aucune intensité [0.0-0.9]", "Faible intensité [1.0-1.9]", "Légère intensité [2.0-2.9]", "Intensité soutenue [3.0-3.9]","Forte intensité [4.0-5.0]")
    data <- table(df$aerobie5cl)
    total <- sum(data)
    pourc <- round((data / total)*100, digits = 0)
    graph <- plot_ly(x = data, y = zone, 
                     type = "bar", orientation = "h",
                     text = paste0(pourc, " %"), textposition = 'auto',
                     marker = list(color = c('rgba(163,167,235,92)', 'rgba(28,62,255,100)',
                                             'rgba(41,255,16,100)', 'rgba(255,129,13,100)',
                                             'rgba(255,53,43,100)')))
    graph <- layout(p = graph,
                    data = df,
                    title = "Impact des activités sur l'aérobie (de 0 à 5)",
                    xaxis = list(title = "Nombre de courses"))
  })
  
  #####ACP
  output$scree_plot <- renderPlot ({
    df <- df_update()
    
    df.active <- df[1:170, c(5,6,8,9,10,11,15,17)]
    res.pca <- PCA(df.active, graph = F)
    fviz_screeplot(res.pca, addlabels = T, ylim = c(0,20))
  })
  
  output$axe <- renderDataTable ({
    df <- df_update()
    
    df.active <- df[1:170, c(5,6,8,9,10,11,15,17)]
    res.pca <- PCA(df.active, graph = F)
    eig.val <- get_eigenvalue(res.pca)
    eig.val
  })
  
  output$acm_variable <- renderPlot({
    df <- df_update()
    
    df.active <- df[1:170, c(5,6,8,9,10,11,15,17)]
    res.pca <- PCA(df.active, graph = F)
    fviz_pca_var(res.pca, col.var = "black")
  })
  
  output$cos2 <- renderDataTable({
    df <- df_update()
    
    df.active <- df[1:170, c(5,6,8,9,10,11,15,17)]
    res.pca <- PCA(df.active, graph = F)
    var <- get_pca_var(res.pca)
    contr <- var$contrib[, 1:2]
    contr[order(contr[,1], decreasing = T),]
  })
  #####
  
})
