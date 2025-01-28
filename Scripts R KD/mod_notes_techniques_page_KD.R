# ─────────────────────────────────────────────────────────────────────────────
# mod_notes_techniques_page_KD.R
# Page "Notes Techniques"
# ─────────────────────────────────────────────────────────────────────────────

mod_notes_techniques_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),  # Activate shinyjs for toggle functionality
    fluidRow(
      column(
        12,
        tags$h2("Au delà des indicateurs"),
        tags$hr(),
        
        # Section 1 - Taux moyen de malaria entre 2000 et 2022
        tags$div(style="background:#f0f0f0; padding:10px;",
                 actionButton(ns("btn1"), label = "Taux moyen de malaria entre 2000 et 2022", class = "btn-primary"),
                 hidden(div(id = ns("details1"),
                            tags$ul(
                              tags$li("Ce qu'il faut savoir"),
                              tags$p("Le taux moyen de malaria, calculé ici sur une période de 22 ans (2000-2022), permet de suivre l'évolution de la prévalence de cette maladie infectieuse dans une région donnée sur trois niveaux administratifs (pays, région, département, commune)."),
                              tags$p("Le paludisme, également connu sous le nom de 'fièvre des marais', est causé par un parasite du genre Plasmodium et transmis par les piqûres de moustiques femelles du genre Anopheles. Malgré les progrès réalisés, cette maladie demeure un défi majeur de santé publique, en particulier dans les régions tropicales et subtropicales.")
                            ),
                            tags$ul(
                              tags$li("Derrière les chiffres : comment calculer cet indicateur ?"),
                              tags$p("Pour une compréhension de la méthodologie de calcul, vous pouvez consulter le TP4 du projet 'Statistique-Exploratoire-Spatiale' disponible sur GitHub avec 4 logiciels GEE with Python, GEE with JavaScript, R, Python :"),
                              tags$a("Lien vers TP4", href="https://github.com/Abson-dev/Statistique-Exploratoire-Spatiale/tree/main/TP4", target="_blank")
                            ),
                            tags$ul(
                              tags$li("En savoir plus"),
                              tags$p("Le Malaria Atlas Project (MAP) est une initiative qui compile et fournit des données détaillées sur la distribution du paludisme à l'échelle mondiale. Le MAP propose un package R, nommé malariaAtlas :"),
                              tags$a("CRAN: Package malariaAtlas", href="https://cran.r-project.org/web/packages/malariaAtlas/index.html", target="_blank"),
                              tags$p("Ce package permet aux chercheurs et aux professionnels de la santé de télécharger et d'analyser des données ouvertes sur le paludisme, y compris des points d'enquête sur les taux de parasites, des occurrences de moustiques vecteurs et des surfaces raster modélisées. Ce package facilite l'intégration de ces données dans des analyses spatiales et temporelles pour une meilleure compréhension de la dynamique du paludisme."),
                              tags$p("Pour plus d'informations sur le Malaria Atlas Project et pour accéder à leurs ressources, visitez leur site officiel :"),
                              tags$a("Malaria Atlas Project | Home", href="https://malariaatlas.org/", target="_blank")
                            ),
                 )
                 )
        ),
        
        # Section 2 - Taux de malaria chez les enfants en 2022
        tags$div(style="background:#f0f0f0; padding:10px;",
                 actionButton(ns("btn2"), label = "Taux de malaria chez les enfants en 2022", class = "btn-primary"),
                 hidden(div(id = ns("details2"),
                            tags$ul(
                              tags$li("Ce qu'il faut savoir"),
                              tags$p("Cet indicateur mesure la prévalence de la malaria chez les enfants de moins de 5 ans en 2022. Le taux de malaria chez les enfants, particulièrement ceux de moins de 5 ans, est un indicateur clé de la vulnérabilité des populations face à cette maladie. En 2013, un enfant mourrait chaque minute du paludisme en Afrique, soulignant l’ampleur de cette crise sanitaire."),
                              tags$p("En 2022, la région africaine a enregistré 233 millions de cas de paludisme, causant 580 000 décès soit 94 % des cas et 95 % des décès liés au paludisme dans le monde. Bien que ce chiffre représente une légère baisse par rapport à 2021, les enfants de moins de 5 ans continuent d’être les plus touchés, représentant environ 78 % des décès liés au paludisme. (Site officiel de l’OMS)")
                            ),
                            tags$ul(
                              tags$li("Derrière les chiffres : comment calculer cet indicateur ?"),
                              tags$p("Pour une compréhension de la méthodologie de calcul, vous pouvez consulter le TP5 du projet 'Statistique-Exploratoire-Spatiale' disponible sur GitHub avec 4 logiciels GEE with Python, GEE with JavaScript, R, Python :"),
                              tags$a("Lien vers TP5", href="https://github.com/Abson-dev/Statistique-Exploratoire-Spatiale/tree/main/TP5", target="_blank")
                            ),
                            tags$ul(
                              tags$li("En savoir plus : Saviez-vous qu’il existe un journal scientifique dédié exclusivement au paludisme ? "),
                              tags$p("Le Malaria Journal est une source précieuse pour les chercheurs et décideurs. Un article particulièrement intéressant explore la distribution estimée des cas de malaria parmi les enfants en Afrique subsaharienne en fonction de catégories d'âge spécifiques, à partir des données du Global Burden of Diseases 2019. Lien vers l’article "),
                              tags$a("Malaria Journal - Estimated distribution of malaria cases among children.", href="https://malariajournal.biomedcentral.com/articles/10.1186/s12936-023-04811-z", target="_blank"),
                              
                            ),
                 )
                 )
        ),
        
        # Section 3 - Conflict Diffusion Indicator
        tags$div(style="background:#f0f0f0; padding:10px;",
                 actionButton(ns("btn3"), label = "Conflict Diffusion Indicator", class = "btn-primary"),
                 hidden(div(id = ns("details3"),
                            tags$ul(
                              tags$li("Ce qu'il faut savoir"),
                              tags$p("Le Conflict Diffusion Indicator (CDI) mesure l’extension géographique et temporelle des conflits à travers les régions. Cet indicateur permet de suivre comment un conflit initialement localisé peut se propager à d’autres zones, soit par effet de contagion, soit à travers des dynamiques socio-politiques ou géographiques.L’histoire récente des hommes est marquée par un XXème siècle caractérisé, notamment dans sa première moitié, par des conflits de dimensions internationales aux conséquences sociales, économiques et humaines désastreuses. L’humanité a entamé sa marche vers le 3ème millénaire par une série de conflits certes de plus faibles dimensions, puisqu’impliquant moins d’Etats et aux origines internes avec généralement une combinaison des soubassements à la fois politiques, économiques et sociétales. (La modélisation de la relation entre démographie, paix et sécurité, UNFPA ONU)"),
                              tags$p("Le CDI est donc essentiel pour comprendre la nature des conflits modernes, qui sont souvent influencés par des variables complexes telles que la proximité des frontières, les divisions ethniques ou religieuses, et les vulnérabilités socio-économiques.")
                            ),
                            tags$ul(
                              tags$li("Derrière les chiffres : comment calculer cet indicateur ?"),
                              tags$p("Pour une compréhension de la méthodologie de calcul, vous pouvez consulter le TP6 du projet 'Statistique-Exploratoire-Spatiale' disponible sur GitHub avec 4 logiciels GEE with Python, GEE with JavaScript, R, Python :"),
                              tags$a("Lien vers TP6", href="https://github.com/Abson-dev/Statistique-Exploratoire-Spatiale/tree/main/TP6", target="_blank")
                            ),
                            tags$ul(
                              tags$li("En savoir plus:« Bringing clarity to crisis », à la découverte de ACLED"),
                              tags$p("L’Armed Conflict Location & Event Data Project (ACLED) constitue une ressource essentielle pour l’analyse des dynamiques de conflits et de violence politique à l’échelle mondiale. Ses bases de données fournissent des informations détaillées sur les localisations des incidents, les types d’événements, les acteurs impliqués, ainsi que leurs évolutions temporelles. ACLED s’impose comme un outil indispensable pour les chercheurs, décideurs et analystes souhaitant mieux comprendre les zones de tension, anticiper les tendances, ou cartographier les conflits. Pour explorer ces données essentielles : "),
                              tags$a("ACLED (Armed Conflict Location and Event Data)", href="https://acleddata.com/", target="_blank")
                            ),
                 )
                 )
        ),        
        # Section 4 - NDVI
        tags$div(style="background:#f0f0f0; padding:10px;",
                 actionButton(ns("btn4"), label = "NDVI (Normalized Difference Vegetation Index)", class = "btn-primary"),
                 hidden(div(id = ns("details4"),
                            tags$ul(
                              tags$li("Ce qu'il faut savoir"),
                              tags$p("Le Normalized Difference Vegetation Index (NDVI), ou Indice de Végétation par Différence Normalisée, est un indicateur spectral clé en télédétection, utilisé pour évaluer la présence et la santé de la végétation verte. Cet indice, allant de -1 à +1, est calculé à partir des valeurs de réflectance dans les longueurs d'onde rouge et proche infra-rouge.Les valeurs négatives indiquent généralement la présence d’eau.Celles proches de zéro reflètent des surfaces urbaines ou dépourvues de végétation.Enfin les valeurs proches de +1 : Sont caractéristiques d’une végétation dense et saine."),
                              tags$p("Le NDVI est particulièrement prisé pour sa capacité à surveiller la dynamique de la végétation sur de vastes territoires et sur de longues périodes. Selon la NASA, le NDVI est un indicateur fiable pour identifier les périodes de sécheresse, car une végétation stressée par le manque d’eau affichera des valeurs NDVI plus faibles.")
                            ),
                            tags$ul(
                              tags$li("Derrière les chiffres : comment calculer cet indicateur ?"),
                              tags$p("Pour une compréhension de la méthodologie de calcul, vous pouvez consulter le TP10 du projet 'Statistique-Exploratoire-Spatiale' disponible sur GitHub avec 4 logiciels GEE with Python, GEE with JavaScript, R, Python :"),
                              tags$a("Lien vers TP10", href="https://github.com/Abson-dev/Statistique-Exploratoire-Spatiale/tree/main/TP10", target="_blank")
                            ),
                            tags$ul(
                              tags$li("En savoir plus : Exploration du NDVI (Du spectre lumineux à la chlorophylle)"),
                              tags$p("Le NDVI (Normalized Difference Vegetation Index) est calculé en utilisant la réflectance mesurée dans deux bandes spécifiques du spectre électromagnétique : le proche infra-rouge (NIR ou Near Infra Red) et la lumière rouge (Red). Ces deux longueurs d’onde jouent un rôle clé dans l’analyse de la végétation. Les plantes saines, riches en chlorophylle, absorbent fortement la lumière rouge pour la photosynthèse, tout en réfléchissant une grande quantité de lumière proche infra-rouge grâce à la structure cellulaire de leurs feuilles. Cela explique pourquoi nous percevons les plantes comme vertes : elles renvoient plus de lumière verte que de lumière rouge ou bleue. Si nous pouvions voir dans le spectre du proche infra-rouge, la végétation apparaîtrait encore plus brillante, car elle réfléchit intensément cette longueur d’onde. Ce contraste entre l’absorption de la lumière rouge et la réflexion du proche infra-rouge est la base du calcul du NDVI, qui permet d’évaluer la santé et la densité de la végétation de manière standardisée et précise. Pour approfondir vos connaissances, consultez "),
                              tags$a("NDVI (indice de végétation par différence normalisée) et PRI (indice de réflectance photochimique) - Le guide complet du chercheur - METER Group.", href="https://metergroup.com/fr/education-guides/ndvi-and-pri-the-researchers-complete-guide/", target="_blank"),
                            ),
                 )
                 )
        ),
        # Section 5 - NDBI
        tags$div(style="background:#f0f0f0; padding:10px;",
                 actionButton(ns("btn5"), label = "NDBI (Normalized Difference Built-up Index)", class = "btn-primary"),
                 hidden(div(id = ns("details5"),
                            tags$ul(
                              tags$li("Ce qu'il faut savoir"),
                              tags$p("L'Indice Différentiel Normalisé des Zones Bâties (NDBI - Normalized Difference Built-up Index) est un indice dérivé d’images satellites, qui utilise les bandes Proche Infrarouge (NIR) et Infrarouge à Ondes Courtes (SWIR)."),
                              tags$p("Il permet de mettre en évidence les zones urbaines et bâties. Sa formule basée sur un ratio réduit les effets des variations d’illumination du terrain ainsi que des conditions atmosphériques, ce qui en fait un outil fiable pour analyser les dynamiques d’urbanisation.")
                            ),
                            tags$ul(
                              tags$li("Derrière les chiffres : comment calculer cet indicateur ?"),
                              tags$p("Pour une compréhension de la méthodologie de calcul, vous pouvez consulter le TP10 du projet 'Statistique-Exploratoire-Spatiale' disponible sur GitHub avec 4 logiciels GEE with Python, GEE with JavaScript, R, Python :"),
                              tags$a("Lien vers TP10", href="https://github.com/Abson-dev/Statistique-Exploratoire-Spatiale/tree/main/TP10", target="_blank")
                            ),
                            tags$ul(
                              tags$li("En savoir plus : les avantages et incovénients du NDBI"),
                              tags$p("Le NDBI (Normalized Difference Built-up Index) est un outil efficace pour cartographier les zones bâties, grâce à sa simplicité et à sa capacité à réduire les biais liés à l’éclairage et aux conditions atmosphériques grace à sa formule basée sur le rapport SWIR/NIR. Il est largement utilisé avec des images satellitaires gratuites comme Landsat et Sentinel-2. Cependant, il peut confondre certaines surfaces bâties avec des sols nus et sa précision dépend de la résolution des données. Pour des analyses plus fiables, il est souvent complété par d'autres indices comme le NDVI."),
                              tags$a("NDBI—ArcGIS Pro | Documentation", href="https://pro.arcgis.com/en/pro-app/latest/arcpy/spatial-analyst/ndbi.htm", target="_blank"),
                            )
                 )
                 )
        ),
      )
    )
  )
}

mod_notes_techniques_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Toggles for each section
    observeEvent(input$btn1, {
      toggle("details1")
    })
    observeEvent(input$btn2, {
      toggle("details2")
    })
    observeEvent(input$btn3, {
      toggle("details3")
    })
    observeEvent(input$btn4, {
      toggle("details4")
    })
    observeEvent(input$btn5, {
      toggle("details5")
    })
    
  })
} 