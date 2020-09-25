#chargement des packages
library(shiny)
library(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
library(shinydashboard)
library(FactoMineR)
library(factoextra)
library(lubridate)
library(tidyverse)

df <- read.csv2("activities_garmin.csv", sep = ",", header = T, dec=".", encoding="UTF-8")

df$Date <- as.Date(df$Date)


df$Calories <- str_replace(df$Calories, ",", "")
df$Calories <- as.integer(df$Calories)

df$Gain.alt <- str_replace(df$Gain.alt, ",", "")
df$Gain.alt <- as.integer(df$Gain.alt)

df$Annee <- year(df$Date)

source("ui.R", encoding = "UTF-8")
source("server.R", encoding = "UTF-8")
shinyApp(ui, server)
