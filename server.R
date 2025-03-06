#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Imports et codes préliminaires
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

fr = list(
  sProcessing = "Traitement en cours...", sSearch = "Rechercher&nbsp;:", 
  sLengthMenu = "Afficher _MENU_ &eacute;l&eacute;ments", 
  sInfo = "Affichage de l'&eacute;l&eacute;ment _START_ &agrave; _END_ sur _TOTAL_ &eacute;l&eacute;ments", 
  sInfoEmpty = "Affichage de l'&eacute;l&eacute;ment 0 &agrave; 0 sur 0 &eacute;l&eacute;ment", 
  sInfoFiltered = "(filtr&eacute; de _MAX_ &eacute;l&eacute;ments au total)", 
  sInfoPostFix = "", sLoadingRecords = "Chargement en cours...", 
  sZeroRecords = "Aucun &eacute;l&eacute;ment &agrave; afficher", 
  sEmptyTable = "Aucune donn&eacute;e disponible dans le tableau", 
  oPaginate = list(
    sFirst = "Premier", sPrevious = "Pr&eacute;c&eacute;dent", 
    sNext = "Suivant", sLast = "Dernier"
  ), 
  oAria = list(
    sSortAscending = ": activer pour trier la colonne par ordre croissant", 
    sSortDescending = ": activer pour trier la colonne par ordre d&eacute;croissant"
  )
)

dataexp = data.frame(Variables = colnames(subset(data, select=-c(geometry))), Description = c("Code administratif de la région.","Nom du département.", "Rapport entre le nombre de familles monoparentales et le nombre total de familles en 2021.", "Part des élèves du secondaire (lycées, collèges) dans un établissement d'enseignement privé en 2023.", "Revenu disponible du ménage divisé par le nombre d'unités de consommation en 2021.", "Le taux de pauvreté monétaire correspond à la proportion d'individus (ou de ménages) étant en situation de pauvreté monétaire. Un individu (ou un ménage) est considéré comme pauvre lorsqu'il vit dans un ménage dont le niveau de vie est inférieur au seuil de pauvreté. En France et en Europe, le seuil est le plus souvent fixé à 60 % du niveau de vie médian. Les chiffres sont de 2021.", "Rapport du nombre de locataires HLM sur le nombre total de résidences principales en 2021.", "Taux de chômage moyen en 2023.", "Rapport entre le nombre de familles avec 3 enfants ou plus de moins de 25 ans et le nombre total de familles en 2021.", "L’indice de position sociale (IPS) permet d'appréhender le statut social des élèves à partir des professions et catégories sociales (PCS) de leurs parents. Pour chaque PCS, la valeur numérique de l’IPS correspond à un résumé quantitatif d’un ensemble d’attributs socio-économiques et culturels liés à la réussite scolaire. Les chiffres sont de 2021.", "Pourcentage de votes parmi les inscrits en faveur de Nathalie Arthaud aux élections présidentielles de 2022.", "Pourcentage de votes parmi les inscrits en faveur de Fabien Roussel aux élections présidentielles de 2022.", "Pourcentage de votes parmi les inscrits en faveur d'Emmanuel Macron aux élections présidentielles de 2022.", "Pourcentage de votes parmi les inscrits en faveur de Jean Lassalle aux élections présidentielles de 2022.", "Pourcentage de votes parmi les inscrits en faveur de Marine Le Pen aux élections présidentielles de 2022.", "Pourcentage de votes parmi les inscrits en faveur d'Eric Zemmour aux élections présidentielles de 2022.", "Pourcentage de votes parmi les inscrits en faveur de Jean-Luc Mélenchon aux élections présidentielles de 2022.", "Pourcentage de votes parmi les inscrits en faveur d'Anne Hidalgo aux élections présidentielles de 2022.", "Pourcentage de votes parmi les inscrits en faveur de Yannick Jadot aux élections présidentielles de 2022.", "Pourcentage de votes parmi les inscrits en faveur de Valérie Pécresse aux élections présidentielles de 2022.", "Pourcentage de votes parmi les inscrits en faveur de Philippe Poutou aux élections présidentielles de 2022.", "Pourcentage de votes parmi les inscrits en faveur de Nicolas Dupont-Aignan aux élections présidentielles de 2022.", "Indicatrice valant 1 si la part des élèves du privé est supérieur au 3ème quartile de cette variable et 0 sinon."))

function(input, output, session) {
  # Maison des variables réactives
  var_acp = reactive({
    input$Var_ACP
  })
  
  data_pca = reactive({
    data_pca = subset(data, select = -c(Code, Libellé, eval(as.symbol(var_acp())), geometry, `Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`)) 
    data_pca %>% mutate_all(~(scale(.) %>% as.vector)) # Centre-réduit les données
  })
  
  var = reactive({
    input$Varselect_univ1
  })
  
  
  ###########################################################################
  #                                                                         #
  #           Matrice des corrélations                                      #
  #                                                                         #
  ###########################################################################
  
  # Crée une expression réactive pour la matrice de corrélation et le modèle de régression
  correlation_data <- reactive({
    # Sélectionner les deux variables choisies par l'utilisateur
    var1 <- input$Varselect_cor1
    var2 <- input$Varselect_cor2
    
    # Vérifier si les deux variables sont sélectionnées
    if (!is.null(var1) && !is.null(var2)) {
      # Exclure les variables non pertinentes pour la corrélation
      data_corr <- subset(data, select = -c(Code, Libellé, geometry, `Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`))
      
      # Accéder aux colonnes sélectionnées dynamiquement par leur nom
      data_selected <- data_corr[, var1, drop = FALSE]  # Sélectionner la première variable
      data_selected <- cbind(data_selected, data_corr[, var2, drop = FALSE])  # Ajouter la deuxième variable
      
      # Calculer la matrice de corrélation pour les variables sélectionnées
      corr_matrix <- cor(data_selected, use = "complete.obs", method = "pearson")
      
      # Calculer les coefficients de la régression
      reg <- lm(`% d'élèves du privé dans les élèves du secondaire` ~ ., data = data_corr)
      R2 <- summary(reg)$r.squared
      
      # Appliquer la règle de Klein : corrélation au carré > R²
      final <- corr_matrix^2 > R2
      
      # Remplacer TRUE par 1 et FALSE par 0 dans la matrice de corrélation
      final_numeric <- as.data.frame(final)
      final_numeric[] <- lapply(final_numeric, function(x) ifelse(x, 1, 0))
      
      # Convertir le tableau en matrice pour corrplot
      final_matrix <- as.matrix(final_numeric)
      
      # Retourner la matrice finale pour une utilisation ultérieure
      return(list(corr_matrix = corr_matrix, final_matrix = final_matrix))
    }
    
    # Si les variables ne sont pas sélectionnées, retourner NULL
    return(NULL)
  })
  
  # Observer pour afficher la matrice de corrélation pour les deux variables sélectionnées
  observe({
    correlation_results <- correlation_data()  # Appeler la fonction réactive
    
    # Vérifier si la réactive a retourné des résultats
    if (!is.null(correlation_results)) {
      corr_value <- correlation_results$corr_matrix[1, 2]
      
      # Affichage du corrplot pour les deux variables sélectionnées
      output$corrPlot <- renderPlot({
        corrplot(matrix(c(1, corr_value, corr_value, 1), nrow = 2), 
                 method = "number", 
                 type = "upper", 
                 tl.cex = 0.8, 
                 addCoef.col = "black")
      })
      
    }
  })
  
  
  ###########################################################################
  #                                                                         #
  #           Tableau de la cohérence des signes                            #
  #                                                                         #
  ########################################################################### 
  
  # Exclure les variables non pertinentes pour la corrélation
  data_corr_coeff <- subset(data, select = -c(Code, Libellé, geometry, `Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`))
  
  # Calcul de la matrice de corrélation globale en excluant les colonnes non pertinentes
  correlations <- cor(data_corr_coeff, use = "complete.obs", method = "pearson")
  
  # Déterminons les coeffcients 
  reg <- lm(`% d'élèves du privé dans les élèves du secondaire` ~ ., data = data_corr_coeff)
  coef = round(reg$coef,3)
  
  # Créer un tableau de cohérence des signes basé sur les corrélations et les coefficients
  coh_signe <- data.frame(
    Variable = colnames(correlations),
    Corrélation = as.vector(correlations),
    Coefficient = as.vector(round(coef,3))  # On arrondi à 3 décimales près
  )
  
  # Appliquer une coloration aux cellules du tableau où les signes des coefficients sont opposés
  coh_signe_colore <- coh_signe %>%
    mutate(across(Corrélation:Coefficient, 
                  ~ ifelse(sign(.) == -sign(Corrélation),  # Si les signes sont opposés
                           paste0('<b><span style="color: #00008B;">', ., '</span></b>'),
                           as.character(.))
    ))
  # Tableau coh_signe_colore
  output$cohSignTable <- renderDT({
    # Vérification que vous voulez afficher uniquement les 20 premières lignes
    datatable(coh_signe_colore[1:20, ], escape = FALSE,  # Limite à 20 lignes
              options = list(
                pageLength = 20,   # Limite le nombre de lignes affichées à 20
                lengthMenu = c(20),  # Seulement l'option de 20 lignes par page
                paging = FALSE,    # Désactive la pagination pour n'afficher qu'une seule page
                info = FALSE,      # Masquer l'information "Showing X to Y of Z entries"
                language = list(
                  search = "Rechercher"  # Remplace "Search" par "Rechercher"
                )
              )) %>%
      formatStyle(
        columns = 1:ncol(coh_signe_colore),
        target = "cell",
        backgroundColor = styleEqual("blue", "#00008B")  # Applique un fond bleu foncé pour les cellules spécifiques
      )
  })
  
  ###########################################################################
  #                                                                         #
  #           VIF                                                           #
  #                                                                         #
  ########################################################################### 
  
  # Calcul du modèle de régression et du VIF dans le serveur (au démarrage de l'application)
  observe({
    # Créer le modèle de régression
    reg <- lm(`% d'élèves du privé dans les élèves du secondaire` ~ ., data = data_corr_coeff)
    
    # Calculer le VIF
    data_vif <- round(vif(reg), 2)
    
    # Créer un data frame pour ggplot
    vif_df <- data.frame(
      variable = names(data_vif),
      vif_value = data_vif
    )
    
    # Affichage du graphique du VIF 
    output$vifPlot <- renderPlotly({
      vif_df$variable = reorder(vif_df$variable, -vif_df$vif_value)
      graph_VIF = ggplot(vif_df, aes(x = variable, y = vif_value)) +
        geom_bar(stat = "identity", fill = input$col_VIF) +
        coord_flip() +  # Retourner les barres pour plus de lisibilité
        labs(title = "VIF des variables", x = "Variables", y = "VIF") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(graph_VIF)
    })
  })
  
  ###########################################################################
  #                                                                         #
  #          Nuage de points                                               #
  #                                                                         #
  ########################################################################### 
  
  # Observe() pour générer les graphiques en fonction des variables sélectionnées
  observe({
    # Sélectionner les variables choisies par l'utilisateur
    var1 <- input$Varselect_cor1
    var2 <- input$Varselect_cor2
    
    # Vérifier si les deux variables sont sélectionnées
    if (!is.null(var1) && !is.null(var2)) {
      
      # Nuage de points
      output$scatterPlot <- renderPlotly({
        scatter_plot =  ggplot(data, aes(x = !!var1, y = !!var2)) +
          geom_point(color = input$col_scatt) + 
          geom_smooth(method = "lm", color = input$col_droite, se = FALSE) +
          labs(title = paste("Nuage de points entre", var1, "et", var2)) +
          theme_minimal()
        ggplotly(scatter_plot)
      })
      
    }
  })
  
  #Tableau explicatifs des variables
  output$Tabexpvar = renderTable(
    dataexp
  )
  
  # Tableau de données
  output$tabdonnées = renderDataTable(
    {
      datatable(data = subset(data, select=-c(geometry)), options = list(language = fr) )
    })
  
  # Histogramme 
  output$histplot <- renderPlotly({
    histplot = data %>% 
      ggplot(aes(x=!!var())) +
      geom_histogram(bins = input$bins, fill=input$col_hist, color="black")+
      ylab("Effectifs")
    ggplotly(histplot)
  })
  
  # Carte
  output$mapplot <- renderLeaflet({
    mapvar = data %>% 
      pull(var())
    
    data$geometry <- st_transform(data$geometry, crs = '+proj=longlat +datum=WGS84')
    pal <- colorNumeric(palette = c(input$col_carte1, input$col_carte2), domain = mapvar)
    
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data=data$geometry, color = pal(mapvar), fillOpacity = 1, smoothFactor = 0.2, highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                                                                                    bringToFront = TRUE), label = paste(str_to_title(data$Libellé), "| Valeur: ", mapvar)) %>% 
      addLegend("bottomright", pal=pal, values=mapvar, title = as.character(var()))
  })
  
  # Boxplot/boite à moustache
  output$boxplot <- renderPlotly({
    
    box_plot = data %>% 
      ggplot()+
      geom_boxplot(aes(y=!!var()), fill=input$col_boxplot)
    ggplotly(box_plot)
  })
  
  # Densité
  output$densityplot <- renderPlotly({
    
    density_plot = data %>% 
      ggplot(aes(!!var()))+
      stat_density(geom = "line")+
      ylab("Densité")
    
    ggplotly(density_plot) 
  })
  
  # Grahe de régression 3D
  output$threeDRegressionplot <- renderPlotly({
    Y = data %>% pull(input$Var_select_tri1)
    X1 = data %>% pull(input$Var_select_tri2)
    X2 = data %>% pull(input$Var_select_tri3)
    
    lm1 <- lm(Y ~ X1 + X2)
    
    wide_x1 = seq(min(X1), max(X1), by = 0.05)
    wide_x2 = seq(min(X2), max(X2), by = 0.05)
    
    disp_grid <- expand_grid(X1 = wide_x1, X2 = wide_x2)
    
    grid2 <- 
      disp_grid %>% 
      mutate(pred_lm1 = predict(lm1, newdata = data.frame(X1, X2)))
    
    grid_wide <- 
      grid2 %>% 
      pivot_wider(names_from = X1, values_from = pred_lm1) %>% 
      select(-1) %>% 
      as.matrix()
    
    plot_ly(
      x = ~ X1,
      y = ~ X2,
      z = ~ Y,
      type = "scatter3d", mode = 'markers') %>% add_trace(
        z = grid_wide,
        x = wide_x1,
        y = wide_x2,
        type = "surface")
    
  })
  
  # Graphe des 3 premières CP
  output$ACPPLot1 <- renderPlotly({
    
    vec_var_cible = data %>% 
      select(var_acp())
    
    prin_comp = prcomp(data_pca(), rank. = 3)
    components <- prin_comp[["x"]]
    components <- data.frame(components)
    components <- cbind(components, vec_var_cible)
    
    plot_ly(components, x = ~PC1, y = ~PC2, z = ~PC3, color = ~eval(as.symbol(var_acp()))) %>%
      add_markers() %>% 
      colorbar(title=as.character(var_acp())) 
    
  })
  
  #Cercle des corrélations
  output$ACPPLot2 <- renderPlotly({
    
    prin_comp_2CP <- prcomp(data_pca(), rank. = 2) # ACP à deux composantes principales
    
    explained_variance <- summary(prin_comp_2CP)[["sdev"]]
    explained_variance <- explained_variance[1:2]
    comp <- prin_comp_2CP[["rotation"]]
    loadings <- comp
    
    for (i in seq(explained_variance)){
      
      loadings[,i] <- comp[,i] * explained_variance[i]
      
    }
    
    features = colnames(data_pca())
    
    fig <- plot_ly() %>%
      layout(
        xaxis = list(
          title = "PC1"),
        yaxis = list(title = "PC2"),
        shapes = list(
          list(type = 'circle',
               xref = 'x', x0 = -1, x1 = 1,
               yref = 'y', y0 = -1, y1 = 1))) 
    
    for (i in seq(length(features))){
      
      fig <- fig %>% 
        add_segments(x = 0, xend = loadings[i, 1], y = 0, yend = loadings[i, 2], line = list(color = 'black'),inherit = FALSE, showlegend = FALSE) %>%
        add_annotations(x=loadings[i, 1], y=loadings[i, 2], ax = 0, ay = 0,text = features[i], xanchor = 'center', yanchor= 'bottom')
    }
    fig
    
  })
  
  #Graphe des 2 premières CP
  output$ACPPLot3 <- renderPlotly({
    vec_var_cible = data %>% 
      select(var_acp())
    
    prin_comp_2CP <- prcomp(data_pca(), rank. = 2) # ACP à deux composantes principales
    
    components = prin_comp_2CP[["x"]]
    components = data.frame(components)
    components = cbind(components, vec_var_cible)
    
    plot_ly(components, x = ~PC1, y = ~PC2, color = ~eval(as.symbol(var_acp())), type = 'scatter', mode = 'markers') %>% 
      colorbar(title=as.character(var_acp()))
    
  })
  
  #Graphe de la variance retenu par rapport aux nombres de CP
  output$ACPPLot4 <- renderPlotly({
    
    prin_comp = prcomp(data_pca())
    explained_variance_ratio = summary(prin_comp)[["importance"]]['Proportion of Variance',]
    data_variance = data.frame(explained_variance_ratio,seq(1, length(explained_variance_ratio), 1))
    colnames(data_variance) = c('Variance_retenue','Composantes')
    
    
    plot_ly(data = data_variance, x = ~Composantes, y = ~Variance_retenue, type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
      layout(
        xaxis = list(
          title = "Composantes n°", tickvals = seq(1, length(cumsum), 1)),
        yaxis = list(
          title = "Variance retenue"))
  })
  
  #################### Partie Edwyna
  
  # Onglet sur la valeur indicatrice
  # Boîte à moustache doubles
  output$boxplot_double <- renderPlotly({
    box_plot_double = data %>%
      ggplot()+
      aes(x=`Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`, y=!!input$Varselect_indic2 , group=`Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`)+
      geom_boxplot(fill = c(input$col_indic0, input$col_indic1))+
      labs(x = "Indicatrice de la part d'élèves du privé dans les élèves du secondaire")
    ggplotly(box_plot_double)
  }) 
  
  # Carte 
  output$map_ind <- renderLeaflet({
    mapvar = data %>% 
      pull(`Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`)
    
    data$geometry <- st_transform(data$geometry, crs = '+proj=longlat +datum=WGS84')
    pal <- colorNumeric(palette = c(input$col_indic0, input$col_indic1), domain = mapvar)
    
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data=data$geometry, color = pal(mapvar), fillOpacity = 1, smoothFactor = 0.2, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), label = paste(str_to_title(data$Libellé), "| Valeur: ", mapvar))
  })
  
  # Nuage de points 
  output$pointplot <- renderPlotly({
      nuage_de_point = data %>%
        ggplot()+
        aes(x=!!input$Varselect_indic1, y=!!input$Varselect_indic2)+
        geom_point(aes(color = as.factor(`Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`))) + 
        scale_color_manual(values = c(input$col_indic0, input$col_indic1)) + 
        facet_wrap(~`Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`) + 
        labs(color = "Indicatrice")
  })
  
  # Onglet sur les valeurs extrêmes
  
  # Pour le tableau
  
  data_reactive <- reactive({
    data_sans_indicatrice <- data %>%
      select(-`Indicatrice >Q3 % d'élèves du privé dans les élèves du secondaire`)
    data_sans_indicatrice
  })
  
  detecter_valeurs_extremes <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    seuil_inf <- Q1 - 1.5 * IQR
    seuil_sup <- Q3 + 1.5 * IQR
    return(ifelse(x < seuil_inf | x > seuil_sup, 1, 0))
  }
  
  Compter_nb_valeurs_extremes <- function(df) {
    nb_extremes <- colSums(df, na.rm = TRUE)
    ratio_extremes <- nb_extremes / nrow(df) * 100
    data.frame(Variable = names(nb_extremes), "Nombre valeurs extrêmes" = nb_extremes, Ratio = ratio_extremes, row.names = NULL)
  }
  
  valeurs_extremes <- reactive({
    data_numeric <- data_reactive() %>%
      select_if(is.numeric)
    data_numeric %>% mutate(across(everything(), detecter_valeurs_extremes))
  })
  
  valeurs_extremes_tts_variables <- reactive({
    Compter_nb_valeurs_extremes(valeurs_extremes())
  })
  
  output$tableau_valeurs_extremes <- renderDataTable(
    {
    datatable(valeurs_extremes_tts_variables(), options = list(language = fr) )
    })
  
  # Seconde partie de l'onglet Valeurs extrêmes 
  
  output$val_max <- renderText({
    max_val = data %>% 
      pull(input$Var_VE)
    max(max_val)
  })
  
  output$val_min <- renderText({
    max_val = data %>% 
      pull(input$Var_VE)
    min(max_val)
  })
  
  output$val_mean <- renderText({
    max_val = data %>% 
      pull(input$Var_VE)
    round(mean(max_val), 2)
  })
  
  output$nom_var_VB1 <- renderText({
    input$Var_VE
  })
  
  output$nom_var_VB2 <- renderText({
    input$Var_VE
  })
  
  output$nom_var_VB3 <- renderText({
    input$Var_VE
  })
  
  output$map_VE <- renderLeaflet({
    
    data_VE_map <- data_reactive()  # Utilisation des données transformées
    mapvar <- data_VE_map %>% 
      pull(input$Var_VE)  # Récupération de la variable choisie
    
    data_VE_map$geometry <- st_transform(data_VE_map$geometry, crs = '+proj=longlat +datum=WGS84')
    
    pal <- colorFactor(palette = c(input$col_carte_nonVE, input$col_carte_VE), 
                       domain = c(FALSE, TRUE))
    
    valeurs_extremes <- detecter_valeurs_extremes(mapvar)
    
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data = data_VE_map$geometry, color = pal(valeurs_extremes == 1), fillOpacity = 1, smoothFactor = 0.2, highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), label = paste(str_to_title(data_VE_map$Libellé), "| Valeur: ", round(mapvar, 2)))
  })
  
  
}