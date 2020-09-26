@mathieu

## Performance en course à pied

Cette page github contient le code et les données utilisées pour construire l'application suivante :

L'objectif de cette [application](https://mydata-mb.shinyapps.io/mathieu-boully-running/) est de pouvoir visualiser plusieurs indicateurs qui montrent le niveau de performance sur le plan physiologique et mécaniques (deux facteurs de la performance en course à pied).
Je peux donc contrôler mes performances en analysant ma technique de course et ma dépense énergétique suivant plusieurs variables. On retrouve des indicateurs de performance, des visualisations graphiques, des calculs agrégés, des filtres dynamiques et une analyse factorielle afin de voir l’influence des variables entre elles.

![Aperçu de l'application RShiny](www/application.png)

## Interface Shiny

Pour mettre en application et synthétiser les données, j'ai utilisé le package [Shiny](https://shiny.rstudio.com/) disponible dans [RStudio.](https://rstudio.com/)

Le package Shiny permet de créer des tableaux de bord dynamiques pour le web.

## Librairie Plotly

Un package important dans cette application est la librairie [Plotly.](https://plotly.com/)

Cette outil est utile dans l'analyse et la visualisation de données dynamique.

## Les données

📦 Les données sont issues de l'application [Garmin Connect.](https://connect.garmin.com/)

Le fichier de données source est disponible [ici !](data/activities_garmin.csv)

Le jeu de données importé dans le code R est un fichier CSV de mes activités de course à pied associées à plusieurs variables comme la vitesse, la distance, la fréquence cardiaque, ... Les informations que regroupent ce fichier sont des données sur mes performances pour chacune de mes sorties. 1 ligne = 1 activité.

Le fichier regroupe 30 variables qualitatives et quantitatives avec plus de 100 sorties de course à pied.

![Aperçu des données](www/donnees_csv.png)

## Packages R

- shiny
- shinydashboard
- plotly
- ggplot2
- plyr
- dplyr
- FactoMineR
- factoextra
- lubridate
- tidyverse

## Auteur

Mathieu Boully, Étudiant en science des données à l'Université Toulouse Paul Sabatier :school:

## Contact et réseaux

mathieu.boully@hotmail.com

Consultez [mes autres publications.](https://mathieu-boully.000webhostapp.com/sports-analysis/)

[Linkedin.](https://www.linkedin.com/in/mathieu-boully-61b910175/)

[Strava.](https://www.strava.com/)
