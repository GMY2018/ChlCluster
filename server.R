## ---------------------------------
## Server function for ChlCluster
## ---------------------------------


library(shiny)
library(rworldmap)
# library(ggplot2)
library(DT)
library(mgcv)


## Read in the essential data
Chl529 <- readRDS('data/Chl529.rds')
ClusterChl <- readRDS('data/ClusterChl.rds')
Trend531 <- readRDS('data/Trend531.rds')
ClusterTrend <- readRDS('data/ClusterTrend.rds')
Season531 <- readRDS('data/Season531.rds')
ClusterSeason <- readRDS('data/ClusterSeason.rds')

Chl535 <- readRDS('data/Chl535.rds')   # not the smoothed version
curve535 <- Chl535$ChlCurve   # For GAM fitting
ID535 <- Chl535$ChlID
name535 <- Chl535$ChlName
label535 <- paste(ID535, name535)
country535 <- Chl535$ChlCountry
lon535 <- round(Chl535$ChlLon, 2)
lat535 <- round(Chl535$ChlLat, 2)
# tabAll <- data.frame(cbind(ID535, name535, country535, lon535, lat535))
# colnames(tabAll) <- c('Lake_ID', 'Lake_name', 'Country', 'Longitude', 'Latitude')

Time <- Chl529$Time

Colours <- c('royalblue4', 'dodgerblue2', 'lightseagreen', 'forestgreen', 
             'olivedrab3', 'goldenrod1','darkorange', 'salmon', 'rosybrown2', 
             'violetred3', 'red3', 'tan4', 'navajowhite4', 'gray20')

##################################################################################################


function(input, output, session) {
  
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
  # The reason we use renderImage here is that the partitions of the canvas 
  # are different due to different cluster numbers
  output$MeanCurve1 <- renderImage({
    
    KM <- clusterInput1()
    k <- length(KM$size)
    filename <- normalizePath(file.path('./www', paste('Chl529-K', k, '.png', sep='')))
    list(src = filename, alt = paste('Image K =', k))
  },
  deleteFile = FALSE
  )
  
  output$MeanCurve2 <- renderImage({
    
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
  
  output$Allmap1 <- renderPlot({
    KM <- clusterInput1()
    k <- length(KM$size)
    col.dot <- KM$cluster
    for (i in 1:k) {
      tp <- order(KM$size, decreasing=T)[i]
      col.dot[KM$cluster == tp] <- Colours[i]
    }
    
    lon <- attrInput()[[4]]  # Attr$ChlLon
    lat <- attrInput()[[5]]  # Attr$ChlLat
    
    par(mar=c(1.5, 1.5, 1.5, 1.5), bg=NA)
    plot(lon, lat, col=col.dot, pch=19, xlim=c(-180, 180), ylim=c(-80, 90),
         xlab='', ylab='', axes=FALSE)
    plot(coastsCoarse, add=T, col='gray40')
    legend(x=-170, y=20, legend=paste('Cluster', 1:k), pch=rep(19, 10), col=Colours[1:k], cex=0.8)
    
  })
  
  output$Allmap2 <- renderPlot({
    
    KM <- clusterInput2()
    k <- length(KM$size)
    col.dot <- KM$cluster
    for (i in 1:k) {
      tp <- order(KM$size, decreasing=T)[i]
      col.dot[KM$cluster == tp] <- Colours[i]
    }
    
    lon <- attrInput()[[4]]
    lat <- attrInput()[[5]]
    
    par(mar=c(1.5, 1.5, 1.5, 1.5), bg=NA)
    plot(lon, lat, col=col.dot, pch=19, xlim=c(-180, 180), ylim=c(-80, 90),
         xlab='', ylab='', axes=FALSE)
    plot(coastsCoarse, add=T, col='gray40')
    legend(x=-170, y=20, legend=paste('Cluster', 1:k), pch=rep(19, 10), col=Colours[1:k], cex=0.8)
  })
  
  output$Allmap3 <- renderPlot({
    
    KM <- clusterInput3()
    k <- length(KM$size)
    col.dot <- KM$cluster
    for (i in 1:k) {
      tp <- order(KM$size, decreasing=T)[i]
      col.dot[KM$cluster == tp] <- Colours[i]
    }
    
    lon <- attrInput()[[4]]
    lat <- attrInput()[[5]]
    
    par(mar=c(1.5, 1.5, 1.5, 1.5), bg=NA)
    plot(lon, lat, col=col.dot, pch=19, xlim=c(-180, 180), ylim=c(-80, 90),
         xlab='', ylab='', axes=FALSE)
    plot(coastsCoarse, add=T, col='gray40')
    legend(x=-170, y=20, legend=paste('Cluster', 1:k), pch=rep(19, 10), col=Colours[1:k], cex=0.8)
  })
  
  
  ## Detailed maps
  output$ClusterMap1 <- renderPlot({
    
    KM <- clusterInput1()
    k <- length(KM$size)
    lab <- KM$cluster
    
    col.dot <- lab
    for (i in 1:k) {
      tp <- order(KM$size, decreasing=T)[i]
      col.dot[lab == tp] <- Colours[i]
    }
    
    lon <- attrInput()[[4]]
    lat <- attrInput()[[5]]
    
    group <- input$Group
    gr <- as.numeric(substr(group, start=9, stop=10))
    gr2 <- order(KM$size, decreasing=T)[gr]
    lake.id <- lab == gr2
    ldata <- data.frame(cbind(x=lon[lake.id], y=lat[lake.id]))
    
    if(gr <= k) {
      par(mar=c(1.5, 1.5, 1.5, 1.5), bg=NA)
      plot(lon[lake.id], lat[lake.id], col=Colours[gr], pch=19, xlim=c(-180, 180), ylim=c(-80, 90),
           xlab=paste('Map of cluster', gr), ylab='', main='', axes=FALSE)
      plot(coastsCoarse, add=T, col='gray40')
    } 
  })
  
  
  output$ClusterMap2 <- renderPlot({
    
    KM <- clusterInput2()
    k <- length(KM$size)
    lab <- KM$cluster
    
    col.dot <- lab
    for (i in 1:k) {
      tp <- order(KM$size, decreasing=T)[i]
      col.dot[lab == tp] <- Colours[i]
    }
    
    lon <- attrInput()[[4]]
    lat <- attrInput()[[5]]
    
    group <- input$Group
    gr <- as.numeric(substr(group, start=9, stop=10))
    gr2 <- order(KM$size, decreasing=T)[gr]
    lake.id <- lab == gr2
    ldata <- data.frame(cbind(x=lon[lake.id], y=lat[lake.id]))
    
    if (gr <= k) {
      par(mar=c(1.5, 1.5, 1.5, 1.5), bg=NA)
      plot(lon[lake.id], lat[lake.id], col=Colours[gr], pch=19, xlim=c(-180, 180), ylim=c(-80, 90),
           xlab=paste('Map of cluster', gr), ylab='', main='', axes=FALSE)
      plot(coastsCoarse, add=T, col='gray40')
    } 
    else {
      par(mar=c(1.5, 1.5, 5, 1.5), bg=NA)
      plot(0, 1, xlab="", ylab="", xaxt="n", yaxt="n", bty="n",  # type="n", 
           main=paste('Sorry, but there are only', gr, 'clusters!'))
    }
    
  })
  
  
  output$ClusterMap3 <- renderPlot({
    
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
      par(mar=c(1.5, 1.5, 1.5, 1.5), bg=NA)
      plot(lon[lake.id], lat[lake.id], col=Colours[gr], pch=19, xlim=c(-180, 180), ylim=c(-80, 90),
           xlab=paste('Map of cluster', gr), ylab='', main='', axes=FALSE)
      plot(coastsCoarse, add=T, col='gray40')
    } 
  })
  
  
  
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
      
      lakeID2 <- lakeID[lake.id]
      name2 <- lakename[lake.id]
      chl2 <- curve535[ID535 %in% lakeID2, ]
      mean1 <- apply(chl2, MARGIN=1, FUN=mean, na.rm=T)
      mean2 <- round(mean1, 2)
      
      # tab2 <- cbind(lakeID2, name2, mean2)
      # colnames(tab2) <- c('Lake ID', 'Lake name', 'Average Chl')
      
      lake.data <- cbind(lakeID2, name2, mean2)
      tab2 <- DT::datatable(lake.data, colnames=c('Lake ID', 'Lake name', 'Average Chl'),
                            options = list(dom = 'p', pageLength = 18)
                            # lengthMenu = c(15, 20, 25)
      )
      return(tab2)
    } 
    
    # else {
    #   lake.data <- cbind(NA, NA, NA)
    #   tab2 <- DT::datatable(lake.data, colnames=c('Lake ID', 'Lake name', 'Average Chl'),
    #                         options = list(dom = 'p', pageLength = 18), 
    #                         caption=paste('Sorry, but there are only', k, 'clusters!'))
    #   return(tab2)
    # }
    
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
      
      lakeID2 <- lakeID[lake.id]
      name2 <- lakename[lake.id]
      chl2 <- curve535[ID535 %in% lakeID2, ]
      mean1 <- apply(chl2, MARGIN=1, FUN=mean, na.rm=T)
      mean2 <- round(mean1, 2)
      
      lake.data <- cbind(lakeID2, name2, mean2)
      tab2 <- DT::datatable(lake.data, colnames=c('Lake ID', 'Lake name', 'Average Chl'),
                            options = list(dom = 'p', pageLength = 18)
      )
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
      
      lakeID2 <- lakeID[lake.id]
      name2 <- lakename[lake.id]
      chl2 <- curve535[ID535 %in% lakeID2, ]
      mean1 <- apply(chl2, MARGIN=1, FUN=mean, na.rm=T)
      mean2 <- round(mean1, 2)
      
      lake.data <- cbind(lakeID2, name2, mean2)
      tab2 <- DT::datatable(lake.data, colnames=c('Lake ID', 'Lake name', 'Average Chl'),
                            options = list(dom = 'p', pageLength = 18)
      )
      return(tab2)
    } 
    
  })
  
  
  ## ---------------------------
  ## Output for Tab 4
  ## ---------------------------

  # Fit the model and give the output
  GamData <- eventReactive(input$gam, {
    id <- which(label535 %in% input$gamLake)
    dataCurve <- curve535[id, ]
    dataAttr <- c(label535[id], country535[id], lon535[id], lat535[id])
    # tsData <- list(dataCurve, dataAttr)
    Chl <- dataCurve
    Decimal_month <- Time - floor(Time)
    data_mat <- data.frame(Chl, Time, Decimal_month)
    fitGam <- gam(Chl ~ s(Time, bs='ps') + s(Decimal_month, bs='cc'), data=data_mat)
    
    data <- list(dataCurve, dataAttr, fitGam)
  })

  output$GamSummary <- renderPrint({
    attrGam <- GamData()[[2]]
    fitGam <- GamData()[[3]]
    intro <- paste0('Lake ', attrGam[1], ' is in ', attrGam[2], ', with longitude ', attrGam[3], 
             ' and latitude ', attrGam[4], '.')
    print(intro)
    print('Here is the fitted additive model.')
    summary(fitGam)
  })

  output$GamPlot <- renderPlot({
    fitGam <- GamData()[[3]]
    par(mfrow=c(1,2), cex=1.3)
    if (!is.null(fitGam)) {
      plot(fitGam, ylab='', lwd=2)
    }
    else {plot(0, 1, type="n", xlab="", ylab="", xaxt="n", yaxt="n", bty="n")}
  })
  
}





