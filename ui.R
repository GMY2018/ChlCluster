## --------------------------------
## User interface for ChlCluster
## --------------------------------

Chl535 <- readRDS('data/Chl535.rds')   # not the smoothed version
ID535 <- Chl535$ChlID
name535 <- Chl535$ChlName
label535 <- paste(ID535, name535)

fluidPage( # padding = c(0, 10), # pad left and right only
  
  titlePanel(p('Cluster the smoothed chlorophyll curves', style = 'color:steelblue')),
  # font-family:georgia
  strong('This app explores the clustering of the log(chlorophyll) data in three different ways, using
         (i) the original smoothed curves, (ii) the smoothed trend, (iii) the smoothed seasonality. Select
         one category on the left to start. The details are provided in three tabs on the right.', 
         style = 'color:steelblue'),
  br(),
  br(),

  fluidRow(
    column(8,
           radioButtons('Pick', strong('Select a category to start', style = 'color:gray'),
                        choices = list('Original', 'Trend', 'Seasonality'), 
                        inline=TRUE, selected = 'Original'),
           br()),
    column(4)
  ),
  
  # sidebarLayout() default to 1:2...
  
  # Side panel for giving inputs ----
  sidebarPanel(
    
    ## 1. Selecting the number of curves
    br(),
    strong('Tab 1: Overview ', style = 'color:gray'), 
    br(),
    em('This is the K-means clustering of the smoothed curves. You can select a cluster number
       to see the general results from the K-means algorithm on this tab.', style = 'color:gray'),
    br(),
    br(),
    
    conditionalPanel(
      condition = "input.Pick == 'Original'",
      # em('Clustering 529 lakes.', style = 'color:gray'),
      selectInput('KM.original', p('Select cluster numbers', style = 'color:gray'),
                  choices = list('11 clusters', '12 clusters', '13 clusters', '14 clusters'), 
                  selected = NULL, width='70%')
    ),
    
    conditionalPanel(
      condition = "input.Pick == 'Trend'",
      # em('Clustering 531 lakes.', style = 'color:gray'),
      selectInput('KM.trend', p('Select cluster numbers', style = 'color:gray'),
                  choices = list('4 clusters', '8 clusters', '11 clusters', '13 clusters'), 
                  selected = NULL, width='70%')
    ),
    
    conditionalPanel(
      condition = "input.Pick == 'Seasonality'",
      # em('Clustering 531 lakes.', style = 'color:gray'),
      selectInput('KM.season', p('Select luster numbers', style = 'color:gray'),
                  choices = list('8 clusters', '9 clusters', '10 clusters'), 
                  selected = NULL, width='70%')
    ),
    
    br(),
    br(),
    # The suggested cluster numbers from 11 to 15 are based on the gap statistics...
    
    ## 2. Selecting the clusters for a detailed look
    strong('Tab 2: Clusters in detail ', style = 'color:gray'),
    br(),
    em('You can view the details of each clusters by selecting a specific cluster. 
       You can find the maps and the list of lakes within each cluster on this tab.',
       style = 'color:gray'),
    br(),
    br(),
    selectInput('Group', p('Select a cluster', style = 'color:gray'), 
                choices = paste('Cluster', 1:14), selected = 'Cluster 1', width='70%'),
    br(),
    br(),
    # The suggested cluster numbers from 11 to 15 are based on the gap statistics...
    
    ## 3. The outlying lakes
    strong('Tab 3: Outlying curves ', style = 'color:gray'),
    br(),
    em('This panel displays the outlying curves according to the functional box plot 
       and hierarchical clustering.', 
       style = 'color:gray'),
    br(),
    br(),
    
    ## 4. The Significance
    br(),
    strong('Tab 4: Significance ', style = 'color:gray'),
    br(),
    em('This panel investigates whether the trend and seasonality are 
       statistcally significant for individual lake. You can search for a lake
       in the table for some attribution information and click the button to fit
       a additive model', 
       style = 'color:gray'),
    br(),
    br(),
    
    width = 3
    ),
  
  # width = '20%'
  
  
  # Main panel for displaying outputs ----
  
  mainPanel(
    ## three tabs, one for overview, one for a detailed investigation, one for outlying lakes
    
    tabsetPanel(
      # Tab 1 for the general clustering results
      tabPanel(strong('Overview'), 
               br(),
               p('-- Summary table of K-means clustering.', style = 'color:dimgray'),
               
               conditionalPanel(
                 condition = "input.Pick == 'Original'",
                 div(tableOutput(outputId = 'ClusterTab1'), style = 'font-size:95%')
               ),
               
               conditionalPanel(
                 condition = "input.Pick == 'Trend'",
                 div(tableOutput(outputId = 'ClusterTab2'), style = 'font-size:95%')
               ),
               
               conditionalPanel(
                 condition = "input.Pick == 'Seasonality'",
                 div(tableOutput(outputId = 'ClusterTab3'), style = 'font-size:95%')
               ),
               
               fluidRow(
                 column(10,
                        p('-- Data curves in each clusters with the red mean curves.', style = 'color:dimgray'),
                        
                        conditionalPanel(
                          condition = "input.Pick == 'Original'",
                          imageOutput(outputId = 'MeanCurve1', width = '80%', inline=T)
                        ),
                        
                        conditionalPanel(
                          condition = "input.Pick == 'Trend'",
                          imageOutput(outputId = 'MeanCurve2', width = '80%', inline=T)
                        ),
                        
                        conditionalPanel(
                          condition = "input.Pick == 'Seasonality'",
                          imageOutput(outputId = 'MeanCurve3', width = '80%', inline=T)
                        ),
                        
                        br()),
                 
                 column(2)
               ),
               
               br(),
               em('Note: 529 lakes are investigated in the original data set; 531 lakes are investigated 
                  in the trend and seasonal data set. The cluster numbers provided are what we considered 
                  as appropriate from a statistical point of view. They reflect different levels of details.
                  A final decision are to be made based on these results.',
                  style = 'color:dimgray'),
               br(),
               br(),
               br()
               ),
      
      
      ## Tab 2 for the cluster details (maps and tables)
      tabPanel(strong('Clusters in detail'), 
               
               conditionalPanel(condition = "input.Pick == 'Original'",
                    fluidRow(
                        column(8,
                        br(),
                        p('-- Clustering results on a map.', style = 'color:dimgray'),
                        plotOutput(outputId = 'Allmap1', height='350px'),
                        br(),
                        p('-- The lakes in each cluster.', style = 'color:dimgray'),
                        plotOutput(outputId = 'ClusterMap1', height='350px'),
                        br()),
                                  
                        column(4,
                        br(),
                        p('-- The basic infos of lakes in the cluster.', style = 'color:dimgray'),
                        br(),
                        div(DT::dataTableOutput(outputId = 'Lakelist1'), 
                        style = 'font-size:85%; width:90%; overflow:scroll'),
                        br())
                        )
                    ),
               
               
               conditionalPanel(condition = "input.Pick == 'Seasonality'",
                       fluidRow(
                          column(8,
                          br(),
                          p('-- Clustering results on a map.', style = 'color:dimgray'),
                          plotOutput(outputId = 'Allmap2', height='350px'),
                          br(),
                          p('-- The lakes in each cluster.', style = 'color:dimgray'),
                          plotOutput(outputId = 'ClusterMap2', height='350px'),
                          br()),
                                  
                          column(4,
                          br(),
                          p('-- The basic infos of lakes in the cluster.', style = 'color:dimgray'),
                          br(),
                          div(DT::dataTableOutput(outputId = 'Lakelist3'), 
                          style = 'font-size:85%; width:90%; overflow:scroll'),
                          br())
                      )
               ),
               
               
               conditionalPanel(condition = "input.Pick == 'Trend'",
                        fluidRow(
                            column(8,
                            br(),
                            p('-- Clustering results on a map.', style = 'color:dimgray'),
                            plotOutput(outputId = 'Allmap3', height='350px'),
                            br(),
                            p('-- The lakes in each cluster.', style = 'color:dimgray'),
                            plotOutput(outputId = 'ClusterMap3', height='350px'),
                            br()),
                                  
                            column(4,
                            br(),
                            p('-- The basic infos of lakes in the cluster.', style = 'color:dimgray'),
                            br(),
                            div(DT::dataTableOutput(outputId = 'Lakelist2'), 
                                style = 'font-size:85%; width:90%; overflow:scroll'),
                            br())
                      )
               )
               
        ),
      
      
      ## Tab 3 for the the outlying lakes
      tabPanel(strong('Outlying curves'), 
               br(),
               p('-- The outlying curves identified by the hierarchical clustering. 
                 They belong to the following lakes. The patterns of some curves may not be
                 unusual, only the Euclidean distance of these curves to the rest of the curves
                 are among the largest.', style = 'color:dimgray; width:75%'),
               
               conditionalPanel(
                 condition = "input.Pick == 'Original'",
                 img(src = 'Chl529-remove.png', width = '72%')
               ),
               
               conditionalPanel(
                 condition = "input.Pick == 'Trend'",
                 img(src = 'Trend531-remove.png', width = '60%')
               ),
               
               conditionalPanel(
                 condition = "input.Pick == 'Seasonality'",
                 img(src = 'Season531-remove.png', width = '60%')
               ),
               
               # img(src = 'Chl-529-remove.png', width = '72%'),
               # tags$iframe(style='height:70%; width:70%', src='www/Chl-529-remove.pdf'),
               br(),
               br(),
               br(),
               p('-- Here is a map of these lakes.', style = 'color:dimgray'),
               
               conditionalPanel(
                 condition = "input.Pick == 'Original'",
                 img(src = 'Chl529-remove-map.png', width = '54%')
               ),
               
               conditionalPanel(
                 condition = "input.Pick == 'Trend'",
                 img(src = 'Trend531-remove-map.png', width = '54%')
               ),
               
               conditionalPanel(
                 condition = "input.Pick == 'Seasonality'",
                 img(src = 'Season531-remove-map.png', width = '54%')
               ),
               
               br(),
               br()),
      
      
      ## Tab 4 for the significance of the trend and seasonality
      tabPanel(strong('The significance'),
               fluidRow(
                 column(7,
                        br(),
                        br(),
                        p('-- To investigate the trend and seasonality of individual lakes, we can fit and
                          additive model to the original data. Search for a lake in the table and fit the model',
                          style = 'color:dimgray'),
                        br(),
                        em("We can look at the effective degrees of freedom (edf) of the smooth functions of time and month
                           to see if any of these are significantly non-constant. For time series data, an edf greater than 1
                           suggests that there is certain linear or curved feature.
                           We can also plot the fitted smooth functions to have a better idea of this feature.",
                           style = 'color:dimgray'),
                        br()),

                 # column(1),
                 column(5,
                        br(),
                        br(),
                        selectInput('gamLake', p('Select the lake ID', style = 'color:gray'), 
                                    choices = label535, selected = label535[1], 
                                    width='70%'),
                        br(),
                        actionButton('gam', strong('Fit the model', style = 'color:steelblue')),
                        br())
                 ),

               fluidRow(
                 column(11,
                 br(),
                 # conditionalPanel("is.null(input['gam']) == FALSE", # input['xx'] to get the values
                 br(),
                 verbatimTextOutput("GamSummary"),
                 plotOutput("GamPlot", height = "350px"),
                 br(),
                 br() # )
                 ),

                 column(1)
               )
          )
      
      ),
    
    width = 9
    
      )  # end of main panel
  
  ) # end of the ui design


