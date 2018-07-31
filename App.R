## -------------------------------------------
## Basic R Shiny - examples from tutorials
## -------------------------------------------

library(shiny)
library(rworldmap)
library(ggplot2)
library(DT)


## (1) Read in the essential data
Chl529 <- readRDS('Chl529.rds')
ClusterChl <- readRDS('ClusterChl.rds')
Trend531 <- readRDS('Trend531.rds')
ClusterTrend <- readRDS('ClusterTrend.rds')
Season531 <- readRDS('Season531.rds')
ClusterSeason <- readRDS('ClusterSeason.rds')

Chl535 <- readRDS('Chl535.rds')   # not the smoothed version
Curve <- Chl535$ChlCurve
CurveID <- Chl535$ChlID

Time <- Chl529$Time

Colours <- c('gray60', 'royalblue4', 'dodgerblue2', 'lightseagreen', 'forestgreen', 
             'olivedrab3', 'goldenrod1','darkorange', 'salmon', 'rosybrown2', 
             'violetred3', 'red3', 'tan4', 'navajowhite4', 'gray20')

mapWorld <- borders('world', colour='gray60', fill='gray60') # create a layer of borders
mp <- ggplot() + mapWorld


##################################################################################################


## (2) Define UI for app that displays clustering results ----
# ui <- navbarPage(p('Cluster the smoothed curves', style = 'color:steelblue'),
                 
ui <- fluidPage( # padding = c(0, 10), # pad left and right only
                   
    titlePanel(p('Cluster the smoothed chlorophyll curves', style = 'color:steelblue')),
    # font-family:georgia
    strong('This app explores the clustering of the log(chlorophyll) data in three different ways, using
            (i) the original smoothed curves, (ii) the smoothed trend, (iii) the smoothed seasonality. Select
            one category on the left to start. The details are provided in three tabs on the right.', 
            style = 'color:steelblue'),
    br(),
    br(),
    
    # sidebarLayout() default to 1:2...
    
      # Side panel for giving inputs ----
      sidebarPanel(
             
        ## 1. Selecting the number of curves
        strong('Select a category to view the clustering results', style = 'color:gray'),
        br(),
        fluidRow(
          column(6,
                 radioButtons('Pick', p('', style = 'color:gray'),
                              choices = list('Original', 'Trend', 'Seasonality'), 
                              selected = 'Original')),
          column(6,
                 br(),
                 br())
                 # submitButton(strong('Update', style = 'color:white'), width='70%'))
                 # actionButton('Go', strong('Update', style = 'color:gray'), width='70%'))
        ),
        
        br(),
        br(),
        strong('Tab 1: Overview ', style = 'color:gray'), 
        br(),
        em('This is the K-means clustering of the smoothed curves. You can select a cluster number
           to see the general results from the K-means algorithm on this tab.', style = 'color:gray'),
        br(),
        br(),
        
        # checkboxInput("smooth", "Smooth"),
        # conditionalPanel(
        #   condition = "input.smooth == true",
        #   selectInput("smoothMethod", "Method",
        #               list("lm", "glm", "gam", "loess", "rlm"))
        # ),
        
        conditionalPanel(
          condition = "input.Pick == 'Original'",
          em('Clustering 529 lakes.', style = 'color:gray'),
          selectInput('KM.original', p('Select the cluster numbers', style = 'color:gray'),
                       choices = list('11 clusters', '12 clusters', '13 clusters', '14 clusters'), 
                       selected = NULL, width='60%')
        ),
        
        conditionalPanel(
          condition = "input.Pick == 'Trend'",
          em('Clustering 531 lakes.', style = 'color:gray'),
          selectInput('KM.trend', p('Select the cluster numbers', style = 'color:gray'),
                       choices = list('4 clusters', '8 clusters', '11 clusters', '13 clusters'), 
                       selected = NULL, width='60%')
        ),
        
        conditionalPanel(
          condition = "input.Pick == 'Seasonality'",
          em('Clustering 531 lakes.', style = 'color:gray'),
          selectInput('KM.season', p('Select the cluster numbers', style = 'color:gray'),
                       choices = list('8 clusters', '9 clusters', '10 clusters'), 
                       selected = NULL, width='60%')
        ),
        
        br(),
        br(),
        # The suggested cluster numbers from 11 to 15 are based on the gap statistics...
        
        ## 2. Selecting the clusters for a detailed look
        strong('Tab 2: Clusters in detail ', style = 'color:gray'),
        br(),
        em('You can view the details of each clusters by selecting a specific cluster. 
          The clusters are ordered by their size. You can find the maps and the list of lakes
          within each cluster on this tab.', style = 'color:gray'),
        br(),
        br(),
        selectInput('Group', p('Select a cluster', style = 'color:gray'), 
                    choices = paste('Cluster', 1:14), selected = 'Cluster 1', width='60%'),
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
        
        width = 3
      ),
      
    # width = '20%'
    
    
    # Main panel for displaying outputs ----
              
    mainPanel(
      ## three tabs, one for overview, one for a detailed investigation, one for outlying lakes
      
      tabsetPanel(
        tabPanel(strong('Overview'), 
                 br(),
                 em('The cluster numbers provided are what we considered as appropriate from a statistical
                    point of view. They reflect different levels of details. A final decision are to be 
                    made based on these results.', style = 'color:steelblue'),
                 br(),
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
                 
                 # div(tableOutput(outputId = 'ClusterTab'), style = 'font-size:95%'), 
                 br(),
                 p('-- Data curves in each clusters with the red mean curves.', style = 'color:dimgray'),
                 
                 conditionalPanel(
                   condition = "input.Pick == 'Original'",
                   imageOutput(outputId = 'MeanCurve1', width = '100%')
                 ),
                 
                 conditionalPanel(
                   condition = "input.Pick == 'Trend'",
                   imageOutput(outputId = 'MeanCurve2', width = '100%', inline=T)
                 ),
                 
                 conditionalPanel(
                   condition = "input.Pick == 'Seasonality'",
                   imageOutput(outputId = 'MeanCurve3', width = '100%')
                 ),
                 
                 # imageOutput(outputId = 'MeanCurve', width = '100%'),
                 br(),
                 br(),
                 br()),
        
        tabPanel(strong('Clusters in detail'), 
            fillCol(# flex = c(2, 1),
              
              tags$style(type = 'text/css',
                         '.left-fill { width: 62%; }',
                         '#left { float: left }'
              ),
              
              tags$style(type = 'text/css',
                         '.right-fill { width: 38%;}',
                         '#right { float: right }'
              ),
              
              div(id = 'left', class = 'left-fill',
                 br(),
                 br(),
                 p('-- The clustering results on a map.', style = 'color:dimgray'),
                 
                 conditionalPanel(
                   condition = "input.Pick == 'Original'",
                   imageOutput(outputId = 'Allmap1')
                 ),
                 
                 conditionalPanel(
                   condition = "input.Pick == 'Trend'",
                   imageOutput(outputId = 'Allmap2')
                 ),
                 
                 conditionalPanel(
                   condition = "input.Pick == 'Seasonality'",
                   imageOutput(outputId = 'Allmap3')
                 ),
                 
                 # imageOutput(outputId = 'Allmap'),
                 br(),
                 p('-- The lakes in each cluster.', style = 'color:dimgray'),
                 
                 conditionalPanel(
                   condition = "input.Pick == 'Original'",
                   imageOutput(outputId = 'ClusterMap1')
                 ),
                 
                 conditionalPanel(
                   condition = "input.Pick == 'Trend'",
                   imageOutput(outputId = 'ClusterMap2')
                 ),
                 
                 conditionalPanel(
                   condition = "input.Pick == 'Seasonality'",
                   imageOutput(outputId = 'ClusterMap3')
                 ),
                 
                 # imageOutput(outputId = 'ClusterMap'),
                 br(),
                 br()
                 # style = 'height:1500px'
                 ),
              
              div(id = 'right', class = 'right-fill',
                 br(),
                 br(),
                 p('-- The basic infos of lakes in the cluster.', style = 'color:dimgray'),
                 br(),
                 # div(tableOutput(outputId = 'Lakelist'), 
                 #    style = 'font-size:85%; height:750px; width:90%; overflow:scroll'),
                 conditionalPanel(
                   condition = "input.Pick == 'Original'",
                   div(DT::dataTableOutput(outputId = 'Lakelist1'), 
                       style = 'font-size:85%; width:90%; overflow:scroll')
                 ),
                 
                 conditionalPanel(
                   condition = "input.Pick == 'Trend'",
                   div(DT::dataTableOutput(outputId = 'Lakelist2'), 
                       style = 'font-size:85%; width:90%; overflow:scroll')
                 ),
                 
                 conditionalPanel(
                   condition = "input.Pick == 'Seasonality'",
                   div(DT::dataTableOutput(outputId = 'Lakelist3'), 
                       style = 'font-size:85%; width:90%; overflow:scroll')
                 ),
                 
                 # div(DT::dataTableOutput(outputId = 'Lakelist'), 
                 #     style = 'font-size:85%; width:90%; overflow:scroll'),
                 br(),
                 br()
                 )
            )
            
          ),
        
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
                 br())
      ),
      
    width = 9
    
    )  # end of main panel
    
    # The end ----
)


##########################################################################################################


## (3) Define server logic required ----
server <- function(input, output, session) {
  
  dataInput <- reactive({  # eventReactive(input$Go, {
    Data <- switch(input$Pick,
                  'Original' = ClusterChl, 'Trend' = ClusterTrend, 'Seasonality' = ClusterSeason)
    return(Data)
  })
  
  clusterInput1 <- reactive({
    KM <- switch(input$KM.original, 
                 '11 clusters' = dataInput()[[1]], '12 clusters' = dataInput()[[2]],  
                 '13 clusters' = dataInput()[[3]], '14 clusters' = dataInput()[[4]])
    return(KM)
  })
  
  clusterInput2 <- reactive({
    KM <- switch(input$KM.trend, 
                 '4 clusters' = dataInput()[[1]], '8 clusters' = dataInput()[[2]],  
                 '11 clusters' = dataInput()[[3]], '13 clusters' = dataInput()[[4]])
    return(KM)
  })
  
  clusterInput3 <- reactive({
    KM <- switch(input$KM.season, 
                 '8 clusters' = dataInput()[[1]], '9 clusters' = dataInput()[[2]],  
                 '10 clusters' = dataInput()[[3]])
    return(KM)
  })
  
  attrInput <- reactive({
    Attr <- switch(input$Pick,
            'Original' = Chl529, 'Trend' = Trend531, 'Seasonality' = Season531)
    return(Attr)  
  })
  
  
  
  ## -------------------------------
  ## Outputs for Tab 1
  ## -------------------------------
  ## Tables
  output$ClusterTab1 <- shiny::renderTable({
    KM <- clusterInput1()
    k <- length(KM$size)
    big <- order(KM$size, decreasing=T)
    size <- sort(table(KM$cluster), decreasing=T)
      
    isolate( tab <- rbind(1:k, size) )
    rownames(tab) <- c('Cluster ID', 'Cluster size')
    return(tab)
  },
  
  rownames = T,
  colnames = F,
  bordered = T,
  hover = T
  # digits = matrix(c(rep(0, 2*5), rep(2, 2*5)), nrow=4, ncol=5, byrow=T)
  # caption = paste('Summary of K-means clustering'),
  # caption.placement = 'top'
  )
  
  output$ClusterTab2 <- shiny::renderTable({
    KM <- clusterInput2()
    k <- length(KM$size)
    big <- order(KM$size, decreasing=T)
    size <- sort(table(KM$cluster), decreasing=T)
    
    tab <- rbind(1:k, size)
    rownames(tab) <- c('Cluster ID', 'Cluster size')
    return(tab)
  },
  
  rownames = T,
  colnames = F,
  bordered = T,
  hover = T
  
  )
  
  output$ClusterTab3 <- shiny::renderTable({
    KM <- clusterInput3()
    k <- length(KM$size)
    big <- order(KM$size, decreasing=T)
    size <- sort(table(KM$cluster), decreasing=T)
    
    tab <- rbind(1:k, size)
    rownames(tab) <- c('Cluster ID', 'Cluster size')
    return(tab)
  },
  
  rownames = T,
  colnames = F,
  bordered = T,
  hover = T
  
  )
  
  ## Plot of mean curves
  output$MeanCurve1 <- renderImage({
      
    KM <- clusterInput1()
    k <- length(KM$size)
    filename <- normalizePath(file.path('./www', paste('Chl529-K', k, '.png', sep='')))
    list(src = filename, alt = paste('Image K =', k))
  },
  deleteFile = FALSE
  )
  # outputOptions(output, 'MeanCurve1', suspendWhenHidden = TRUE)  
  
  output$MeanCurve2 <- renderImage({
    
    # width  <- session$clientData$output_MeanCurve2_width
    # height <- session$clientData$output_MeanCurve2_height
      
    KM <- clusterInput2()
    k <- length(KM$size)
    filename <- normalizePath(file.path('./www', paste('Trend531-K', k, '.png', sep='')))
    list(src = filename, alt = paste('Image K =', k))
  },
  deleteFile = FALSE
  )
  
  output$MeanCurve3 <- renderImage({
      
    KM <- clusterInput3()
    k <- length(KM$size)
    filename <- normalizePath(file.path('./www', paste('Season531-K', k, '.png', sep='')))
    list(src = filename, alt = paste('Image K =', k))
  },
  deleteFile = FALSE
  )
  
  
  ## ----------------------------
  ## Outputs for Tab 2
  ## ----------------------------
  ## Cluster map
  output$Allmap1 <- renderImage({
    
    KM <- clusterInput1()
    k <- length(KM$size)
    filename <- normalizePath(file.path('./www', paste('Chl529-map', k, '.png', sep='')))
    list(src = filename, width = '87%', alt = 'Please select a cluster number.') 
  },
  deleteFile = FALSE
  )
  
  output$Allmap2 <- renderImage({
    
    KM <- clusterInput2()
    k <- length(KM$size)
    filename <- normalizePath(file.path('./www', paste('Trend531-map', k, '.png', sep='')))
    list(src = filename, width = '87%', alt = 'Please select a cluster number.')
  },
  deleteFile = FALSE
  )
  
  output$Allmap3 <- renderImage({
    
    KM <- clusterInput3()
    k <- length(KM$size)
    filename <- normalizePath(file.path('./www', paste('Season531-map', k, '.png', sep='')))
    list(src = filename, width = '87%', alt = 'Please select a cluster number.')
  },
  deleteFile = FALSE
  )
  
  ## Detailed maps
  output$ClusterMap1 <- renderImage({
    
    KM <- clusterInput1()
    k <- length(KM$size)
    lab <- KM$cluster
    
    col.dot <- lab
    for (i in 1:k) {
      tp <- order(KM$size, decreasing=T)[i]
      col.dot[lab == tp] <- Colours[i]
    }
    
    lon <- attrInput()[[4]]   # Attr$ChlLon
    lat <- attrInput()[[5]]   # Attr$ChlLat
    
    group <- input$Group
    gr <- as.numeric(substr(group, start=9, stop=10))
    gr2 <- order(KM$size, decreasing=T)[gr]
    lake.id <- lab == gr2
    ldata <- data.frame(cbind(x=lon[lake.id], y=lat[lake.id]))
    
    if(gr <= k) {
      png('mapfile', width = 900, height = 500)
      par(mfrow=c(1,1), mar=c(4.5,3,1.5,1.5), cex=1.4)
      plot(lon[lake.id], lat[lake.id], col=Colours[gr], pch=19, xlim=c(-180, 180), ylim=c(-80, 90),
           xlab=paste('Map of cluster', gr), ylab='', main='')
      plot(coastsCoarse, add=T, col='gray40')
      dev.off()
    } 
    
    list(src = 'mapfile',
         contentType = 'image/png', width = '87%',
         alt = paste('Sorry, but there are only', k, 'clusters!'))
    
  }, deleteFile = TRUE)

  
  output$ClusterMap2 <- renderImage({
    
    KM <- clusterInput2()
    k <- length(KM$size)
    lab <- KM$cluster
    
    col.dot <- lab
    for (i in 1:k) {
      tp <- order(KM$size, decreasing=T)[i]
      col.dot[lab == tp] <- Colours[i]
    }

    lon <- attrInput()[[4]]   # Attr$TrendLon
    lat <- attrInput()[[5]]   # Attr$TrendLat
    
    group <- input$Group
    gr <- as.numeric(substr(group, start=9, stop=10))
    gr2 <- order(KM$size, decreasing=T)[gr]
    lake.id <- lab == gr2
    ldata <- data.frame(cbind(x=lon[lake.id], y=lat[lake.id]))
    
    if(gr <= k) {
      png('mapfile', width = 900, height = 500)
      par(mfrow=c(1,1), mar=c(4.5,3,1.5,1.5), cex=1.4)
      plot(lon[lake.id], lat[lake.id], col=Colours[gr], pch=19, xlim=c(-180, 180), ylim=c(-80, 90),
           xlab=paste('Map of cluster', gr), ylab='', main='')
      plot(coastsCoarse, add=T, col='gray40')
      dev.off()
    } 
    
    list(src = 'mapfile',
         contentType = 'image/png', width = '87%',
         alt = paste('Sorry, but there are only', k, 'clusters!'))
    
  }, deleteFile = TRUE)
  
  
  output$ClusterMap3 <- renderImage({
    
    KM <- clusterInput3()
    k <- length(KM$size)
    lab <- KM$cluster
    
    col.dot <- lab
    for (i in 1:k) {
      tp <- order(KM$size, decreasing=T)[i]
      col.dot[lab == tp] <- Colours[i]
    }
    
    lon <- attrInput()[[4]]   # Attr$SeasonLon
    lat <- attrInput()[[5]]   # Attr$SeasonLat
    
    group <- input$Group
    gr <- as.numeric(substr(group, start=9, stop=10))
    gr2 <- order(KM$size, decreasing=T)[gr]
    lake.id <- lab == gr2
    ldata <- data.frame(cbind(x=lon[lake.id], y=lat[lake.id]))
    
    if(gr <= k) {
      png('mapfile', width = 900, height = 500)
      par(mfrow=c(1,1), mar=c(4.5,3,1.5,1.5), cex=1.4)
      plot(lon[lake.id], lat[lake.id], col=Colours[gr], pch=19, xlim=c(-180, 180), ylim=c(-80, 90),
           xlab=paste('Map of cluster', gr), ylab='', main='')
      plot(coastsCoarse, add=T, col='gray40')
      dev.off()
    } 
    
    list(src = 'mapfile',
         contentType = 'image/png', width = '87%',
         alt = paste('Sorry, but there are only', k, 'clusters!'))
    
  }, deleteFile = TRUE)
  
  
  
  # output$Lakelist <- renderTable({
  output$Lakelist1 <- DT::renderDataTable({
    
    KM <- clusterInput1()
    k <- length(KM$size)
    
    group <- input$Group
    gr <- as.numeric(substr(group, start=9, stop=10))
      
    if(gr <= k){
      gr2 <- order(KM$size, decreasing=T)[gr]
      lake.id <- KM$cluster == gr2
      
      lakeID <- attrInput()[[1]]   # Attr$ChlID
      lakename <- attrInput()[[2]]   # Attr$ChlName
      # curve <- attrInput()[[3]]   # Attr$SmoothCurve
      
      lakeID2 <- lakeID[lake.id]
      name2 <- lakename[lake.id]
      chl2 <- Curve[CurveID %in% lakeID2, ]
      mean1 <- apply(chl2, MARGIN=1, FUN=mean, na.rm=T)
      mean2 <- round(mean1, 2)
      
      # tab2 <- cbind(lakeID2, name2, mean2)
      # colnames(tab2) <- c('Lake ID', 'Lake name', 'Average Chl')
      
      lake.data <- cbind(lakeID2, name2, mean2)
      tab2 <- DT::datatable(lake.data, colnames=c('Lake ID', 'Lake name', 'Average Chl'),
                        options = list(dom = 'p', pageLength = 20)
        # lengthMenu = c(15, 20, 25)
      )
      
      return(tab2)
      
    } else {
      lake.data <- cbind(NA, NA, NA)
      tab2 <- DT::datatable(lake.data, colnames=c('Lake ID', 'Lake name', 'Average Chl'),
                            options = list(dom = 'p', pageLength = 20), 
                            caption=paste('Sorry, but there are only', k, 'clusters!'))
      return(tab2)
    }
   
  })
  
  output$Lakelist2 <- DT::renderDataTable({
    
    KM <- clusterInput2()
    k <- length(KM$size)
    
    group <- input$Group
    gr <- as.numeric(substr(group, start=9, stop=10))
    
    if(gr <= k){
      gr2 <- order(KM$size, decreasing=T)[gr]
      lake.id <- KM$cluster == gr2
      
      lakeID <- attrInput()[[1]]   # Attr$ChlID
      lakename <- attrInput()[[2]]   # Attr$ChlName
      # curve <- attrInput()[[3]]   # Attr$SmoothCurve
      
      lakeID2 <- lakeID[lake.id]
      name2 <- lakename[lake.id]
      chl2 <- Curve[CurveID %in% lakeID2, ]
      mean1 <- apply(chl2, MARGIN=1, FUN=mean, na.rm=T)
      mean2 <- round(mean1, 2)
      
      # tab2 <- cbind(lakeID2, name2, mean2)
      # colnames(tab2) <- c('Lake ID', 'Lake name', 'Average Chl')
      
      lake.data <- cbind(lakeID2, name2, mean2)
      tab2 <- DT::datatable(lake.data, colnames=c('Lake ID', 'Lake name', 'Average Chl'),
                            options = list(dom = 'p', pageLength = 20)
                            # lengthMenu = c(15, 20, 25)
      )
      
      return(tab2)
      
    } else {
      lake.data <- cbind(NA, NA, NA)
      tab2 <- DT::datatable(lake.data, colnames=c('Lake ID', 'Lake name', 'Average Chl'),
                            options = list(dom = 'p', pageLength = 20), 
                            caption=paste('Sorry, but there are only', k, 'clusters!'))
      return(tab2)
    }
    
  })
  
  output$Lakelist3 <- DT::renderDataTable({
    
    KM <- clusterInput3()
    k <- length(KM$size)
    
    group <- input$Group
    gr <- as.numeric(substr(group, start=9, stop=10))
    
    if(gr <= k){
      gr2 <- order(KM$size, decreasing=T)[gr]
      lake.id <- KM$cluster == gr2
      
      lakeID <- attrInput()[[1]]   # Attr$ChlID
      lakename <- attrInput()[[2]]   # Attr$ChlName
      # curve <- attrInput()[[3]]   # Attr$SmoothCurve
      
      lakeID2 <- lakeID[lake.id]
      name2 <- lakename[lake.id]
      chl2 <- Curve[CurveID %in% lakeID2, ]
      mean1 <- apply(chl2, MARGIN=1, FUN=mean, na.rm=T)
      mean2 <- round(mean1, 2)
      
      # tab2 <- cbind(lakeID2, name2, mean2)
      # colnames(tab2) <- c('Lake ID', 'Lake name', 'Average Chl')
      
      lake.data <- cbind(lakeID2, name2, mean2)
      tab2 <- DT::datatable(lake.data, colnames=c('Lake ID', 'Lake name', 'Average Chl'),
                            options = list(dom = 'p', pageLength = 20)
                            # lengthMenu = c(15, 20, 25)
      )
      
      return(tab2)
      
    } else {
      lake.data <- cbind(NA, NA, NA)
      tab2 <- DT::datatable(lake.data, colnames=c('Lake ID', 'Lake name', 'Average Chl'),
                            options = list(dom = 'p', pageLength = 20), 
                            caption=paste('Sorry, but there are only', k, 'clusters!'))
      return(tab2)
    }
    
  })
  
  
 
}


###############################################################################################


## (4) Finish the app script
shinyApp(ui = ui, server = server)


