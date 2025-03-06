#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(bslib)
library(tidyverse)
library(colourpicker)
library(leaflet)
library(sf)
library(plotly)
library(reshape2)
library(stringr)
library(stats)
library(DT)
library(bsicons)
library(car)
library(corrplot)

data = read_delim(file="data_complet.csv", delim=",", col_names = TRUE)
dpt = read_sf("dpt")
dpt = dpt %>% 
  select(CODE_DEPT, geometry)
data = data %>% 
  right_join(dpt, by=c("Code"="CODE_DEPT"))

# Define UI for application that draws a histogram

page_navbar(
  title = "Etude de l'enseignement secondaire privée en France Métropolitaine",
  bg = "#2D89C8",
  inverse = TRUE,
  nav_panel(title = "Description", 
            navset_card_underline(
              title = "Présentation de l'application",
              
              # Panel with summary ----
              nav_panel("Présentation", h2("Objectif"),p("Cette application sert à visualiser notre jeu de données sur l'enseignement secondaire privé en France métropolitaine, l'objectif est de représenter les variables pour comprendre le lien entre ces dernières et la part d'élèves du privé parmi les élèves du secondaire en France métropolitaine."), h2("Tableau explicatif des variables"), tableOutput("Tabexpvar")),
              
              # Panel with table ----
              nav_panel("Tableau de données", dataTableOutput("tabdonnées"))
            )), ############################# Ajouter le code du 1er onglet ici !!!!!
  
  nav_panel(title = "Analyse Univariée", # Code 2ème onglet 
            layout_sidebar(
              sidebar=sidebar(varSelectInput(inputId="Varselect_univ1", label="Variable:", data = subset(data, select = -c(Code, Libellé, geometry, `Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`)))),
              layout_columns(
                card(layout_sidebar(sidebar=sidebar(colourInput("col_boxplot", "Couleur:", "white"), open=FALSE),card_header("Boite à Moustache"), card_body(plotlyOutput("boxplot")),fill = TRUE)),
                card(layout_sidebar(sidebar=sidebar(colourInput("col_carte1", "Couleur pour les valeurs faibles:", "white"), colourInput("col_carte2", "Couleur pour les valeurs hautes:", "red"), open = FALSE),card_header("Carte"), card_body(leafletOutput("mapplot")))),
                card(card_header("Densité"), card_body(plotlyOutput("densityplot"))),
                card(layout_sidebar(sidebar=sidebar(sliderInput(inputId="bins", label = "Bins", min = 1, max = 50, value = 30), colourInput("col_hist", "Couleur:", "yellow"), open=FALSE),card_header("Histogramme"), card_body(plotlyOutput("histplot")))),
                col_widths = c(6, 6, 6, 6)
              )),
  ), ############################# Ajouter le code du 2ème onglet ici !!!!!
  
  nav_panel(title = "Variable indicatrice", 
            layout_sidebar(
              sidebar=sidebar(varSelectInput(inputId="Varselect_indic2", label="Variable Y :", data = subset(data, select = -c(Code, Libellé, geometry, `Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`)) %>% select("% d'élèves du privé dans les élèves du secondaire", everything()) ),
                              colourInput("col_indic0", "Couleur Indicatrice = 0 :", "yellow"),
                              colourInput("col_indic1", "Couleur Indicatrice = 1 : ", "red")
                              ),
              layout_columns(
                card(card_header("Boîte à moustaches"), card_body(plotlyOutput("boxplot_double")), fill=TRUE),
                card(card_header("Carte de répartition de l'indicatrice"), card_body(leafletOutput("map_ind"))),
                card(layout_sidebar(sidebar=sidebar(varSelectInput("Varselect_indic1", "Variable X :", data = subset(data, select = -c(Code, Libellé, geometry, `Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`)) %>% select("% Votes Macron", everything())), open=FALSE), card_header("Nuage de points"), card_body(plotlyOutput("pointplot")))),
                col_widths = c(6,6,12)
              )
            )
  ), ############################# Ajouter le code du 3ème onglet ici !!!!!
  
  nav_panel(
    title = "Valeurs extrêmes", 
    layout_columns(
      card(card_header("Tableau récapitulatifs valeurs extrêmes"), card_body(dataTableOutput("tableau_valeurs_extremes"))),
      card(layout_sidebar(sidebar = sidebar(
        varSelectInput(inputId="Var_VE", label="Variable :", data = subset(data, select = -c(Code, Libellé, geometry, `Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`))),
        colourInput("col_carte_VE", "Couleur des variables extrêmes : ", "red"),
        colourInput("col_carte_nonVE", "Couleur du reste de la carte : ", "yellow"),
        open = FALSE), 
        card_header("Informations sur une variable"), 
        card_body(
          layout_columns(
                  value_box(title = "La plus grande valeur", value = textOutput("val_max"), showcase = bs_icon("bookmark-plus"), theme = value_box_theme(bg="lightskyblue"), p(textOutput("nom_var_VB1")), p("(Pas forcément une valeur extrême)")),
                  value_box(title = "La plus petite valeur", value = textOutput("val_min"), showcase = bs_icon("bookmark-dash"), theme = value_box_theme(bg="lightsalmon"), p(textOutput("nom_var_VB2")), p("(Pas forcément une valeur extrême)")),
                  value_box(title = "La valeur moyenne", value = textOutput("val_mean"), showcase = bs_icon("bookmark-star"), theme = value_box_theme(bg="#FFD39B"), p(textOutput("nom_var_VB3")))),
          leafletOutput("map_VE"))),
  ))),
  
  ############################# Ajouter le code du 4ème onglet ici !!!!!
  
  nav_panel(
    title = "Analyse des Corrélations", 
    layout_sidebar(
      sidebar = sidebar(
        # Premier menu déroulant : Choix de la première variable
        varSelectInput(
          inputId = "Varselect_cor1", 
          label = "Première Variable:", 
          data = subset(data, select = -c(Code, Libellé, geometry, `Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`)) %>% select("% d'élèves du privé dans les élèves du secondaire", everything())
        ),
        
        # Deuxième menu déroulant : Choix de la deuxième variable
        varSelectInput(
          inputId = "Varselect_cor2", 
          label = "Deuxième Variable:", 
          data = subset(data, select = -c(Code, Libellé, geometry, `Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`)) %>% select("% Votes Macron", everything())
        )
      ),
      layout_columns(
        # Colonne 1 : Matrice de Corrélation (en haut à gauche, 6/12 de la largeur)
        card(
          card_header("Matrice de Corrélation"),
          card_body(plotOutput("corrPlot"))
        ),
        # Colonne 2 : Tableau de Cohérence des Signes (en haut à droite, 6/12 de la largeur)
        card(
          card_header("Tableau de Cohérence des Signes"),
          card_body(DTOutput("cohSignTable"))
        ),
        # VIF 
        card(layout_sidebar(sidebar=sidebar(colourInput("col_VIF", "Couleur point:", "lightblue"), open=FALSE),
                            card_header("VIF des Variables"),
                            card_body(plotlyOutput("vifPlot"))
        )),
        # Colonne : Nuage de points 
        card(layout_sidebar(sidebar=sidebar(colourInput("col_scatt", "Couleur point:", "black"), colourInput("col_droite", "Couleur droite:", "red"), open=FALSE),
                            card_header("Nuage de points"),
                            card_body(plotlyOutput("scatterPlot")))
        ), col_widths = c(6,6,6,6)) # Prendre toute la largeur et ajuster la hauteur
      
    )
  ), ############################# Ajouter le code du 6ème onglet ici !!!!!
  
  nav_panel(title = "Analyse Trivariée", 
            layout_sidebar(
              sidebar=sidebar(varSelectInput(inputId="Var_select_tri1", label="Variable cible Y | axe Z:", data = subset(data, select = -c(Code, Libellé, geometry, `Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`)) %>% select("% d'élèves du privé dans les élèves du secondaire", everything()) ),
                              varSelectInput(inputId="Var_select_tri2", label="Variable explicative X1 | axe X:", data = subset(data, select = -c(Code, Libellé, geometry, `Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`)) %>% select("% Votes Macron", everything()) ),
                              varSelectInput(inputId="Var_select_tri3", label="Variable explicative X2 | axe Y:", data = subset(data, select = -c(Code, Libellé, geometry, `Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`)) %>% select("% Votes Jadot", everything()) )),
              layout_columns(
                card(card_header("Graphe de régression 3D"), card_body(plotlyOutput("threeDRegressionplot")),fill = TRUE),
                col_widths = c(12)
              )),
  ), ############################# Ajouter le code du 7ème onglet ici !!!!!
  
  nav_panel(title = "ACP", 
            layout_sidebar(
              sidebar=sidebar(varSelectInput(inputId="Var_ACP", label="Variable retiré en couleur:", data = subset(data, select = -c(Code, Libellé, geometry, `Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`)))),
              layout_columns(
                card(card_header("Graphes des trois premières composantes principales"), card_body(plotlyOutput("ACPPLot1")),fill = TRUE),
                card(card_header("Cercle des corrélations"), card_body(plotlyOutput("ACPPLot2")),fill = TRUE),
                card(card_header("Graphes des deux premières composantes principales"), card_body(plotlyOutput("ACPPLot3")),fill = TRUE),
                card(card_header("Graphe de la variance retenue par composante principale"), card_body(plotlyOutput("ACPPLot4")),fill = TRUE),
                col_widths = c(6, 6, 6, 6)
              ))
  )
  ############################# Ajouter le code du 8ème onglet ici !!!!!
)