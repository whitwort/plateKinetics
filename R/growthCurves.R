
#Filter Functions, return logical vectors where FALSE indicates bad data
default.lagFilter <- function(od, lag.window = 3) {
  return(od > (median(od[1:lag.window]) * 2))
}

default.plateauFilter <- function(od, plateau.cutoff = 0.85) {
  return(od < plateau.cutoff)
}

bubbleFilter <- function(od, bubble.tolerance = 3, bubble.neighborhoodSize = 3) {
  
  #Calculate pair-wise point-to-point value changes (deltas)
  localDelta <- vectorDeltas(od)
  
  #Compare each pair-wise delta to the average delta in its neighborhood
  neighborhoodDelta <- vector("numeric", length(localDelta))
  for (i in 1:length(localDelta)) {
    neighborhoodDelta[i] <- abs( 
      localDelta[i] - median(
        localDelta[max(1,i-bubble.neighborhoodSize):min(length(localDelta),i+bubble.neighborhoodSize)])
      )
  }
  
  #Caclulate a neighborhood cutoff value based on the given tolerance parameter
  cutoff <- mean(neighborhoodDelta) * (1 + bubble.tolerance)
  
  #Keep only points with neighborhood deltas below the cutoff
  goodPoints <- (neighborhoodDelta < cutoff)
  
  #Return the final set of points passing the filter
  return( goodPoints )
  
}

#Analysis functions
doublingTime <- function(table, 
                         filters        = c(default.lagFilter, default.plateauFilter), 
                         number.format  = round
                         ) {
  
  calculateDT <- function(col) {
    
    #Apply each filter to the column of OD data
    filter <- c(TRUE)
    for (f in filters) { filter <- filter & f(col) }
    
    #Use ODs and times that pass the filters
    od <- col[filter]
    time <- table$time[filter]

    #Check to ensure we haven't filtered all points
    if (length(od)<1) { return(NA) }
    
    #Fit a linear model to the log transformed ODs versus time
    fit <- lm(log(od) ~ time)
    
    #Calculate the doubling time
    return( number.format(log(2) / coef(fit)["time"] / 60 ) )
  }
  
  return( apply(table[2:length(table)], MARGIN = 2, FUN = calculateDT) )
  
}

#Visualizations
makeODPlots <- function(table, savePath, 
                        annotations = data.frame(),
                        plateLabels = default.plate.96, 
                        filters     = c(default.lagFilter, default.plateauFilter),
                        individual  = TRUE,
                        composite   = c(300,350)
                        ) {
  
  #Calculate a linear series of labels
  wellLabels <- c(t(plateLabels))
  
  #Calculate a universal set of x- and y- axis scales
  xlim <- c( min(table$time), max(table$time) ) / 60
  ylim <- c( min(table[2:length(table)]), max(table[2:length(table)]) )
  
  #od Plotting function (used to make both individual and aggregated graphs)
  odPlot <- function(well, 
                     main     = NULL, 
                     xlab     = NULL, 
                     ylab     = NULL, 
                     ul.label = NULL, 
                     lr.label = NULL
                     ) {
    
    #Apply each filter to the column of OD data
    filter <- c(TRUE)
    for (f in filters) { filter <- filter & f(table[[well]]) }
    
    #Plot the good points in blue
    convertedTimes <- table$time[filter] / 60
    plot(convertedTimes, table[[well]][filter], 
         main = main,
         xlab = xlab, 
         ylab = ylab, 
         xlim = xlim, 
         ylim = ylim,
         col  = 4,
         pch  = 20,
         )
    
    #Plot the filtered points in black
    points(table$time[!filter] / 60, table[[well]][!filter], pch = 20, col = "grey45")
    
    #If a doubling time is saved on annotations, plot the doubling time function
    if (!is.null(annotations$doublingTime)) {
      dt <- annotations[well,'doublingTime']
      if (!is.na(dt)) {
        
        startOD <- min(table[[well]][filter])
        logStartTime <- convertedTimes[1]
        
        model <- function(t) { startOD * ( 2^(t/dt) ) }
        lines(table$time/60, model((table$time/60)-logStartTime), col=4)
        
      }
    }
    
    #If we were given an upper left annotation label
    if (!is.null(ul.label)) {  
      #Put the annotation text in the upper lefthand corner of the graph
      text(x = xlim[1], y = ylim[2], labels = ul.label, adj = c(0,1))
    }
    
    #If we were given an upper left annotation label
    if (!is.null(lr.label)) {
      #Put the annotation text in the lower righthand corner of the graph
      text(x = xlim[2], y = ylim[1], labels = ul.label, adj = c(1,0))
    }
    
  }
  
  #Make a plot of each individual OD series versus time
  if (individual) {
    for (well in wellLabels) {
      
      #If we were passed annotations, write them to the graph as the upper left label
      if (length(annotations)>0) {
        ul.label <- paste(
          names(annotations), ": ", annotations[well,], sep = "", collapse = "\n" 
          )
      } else { 
        ul.label = NULL
      }
      
      #Start a new JPEG
      jpeg(paste(savePath, paste(well, ".jpg", sep=""), sep="/"))
      
      #Make the individual plot
      odPlot(well, main = well, xlab = "Time (minutes)", ylab = "OD", ul.label = ul.label)
      
      #Save the image
      dev.off()
      
    }
  }
  
  #Make a composite plot of all series
  if (composite[1]) {
    
    #Calculate a size for the composite image and open a file for writing
    imageDim <- composite * dim(plateLabels) #c(row, col)
    png(filename=paste(savePath, "composite.png", sep = "/"), width = imageDim[2], height = imageDim[1])
    
    #Save the starting par values, so that we can restore later
    original.par = par(
      mfrow   = dim(plateLabels),
      mar     = c(0.3,0.3,0.3,0.3),
      oma     = c(4,4,4,4), 
      yaxt    = "n",
      xaxt    = "n",
      cex     = 1
      )
    
    #For each plot
    for (well in wellLabels) {
      
      #If we were passed annotations, write them to the graph as the upper left label
      if (length(annotations)>0) {
        ul.label <- paste(c(well, annotations[well,]), sep = "", collapse = "\n")
      } else { 
        ul.label = NULL
      }
      
      #Make the plot
      odPlot(well, xlab="", ylab="", ul.label=ul.label)
      
    }
    
    #Save the image
    dev.off()
    
    #Restore the original plot parameters
    par(original.par)
    
  }
    
}

#Utility functions
vectorDeltas <- function(v) { return( abs( v[-1] - v[-length(v)] ) ) }
aveVectorDelta <- function(v, ave = mean) { return( ave(vectorDeltas(v)) ) }
