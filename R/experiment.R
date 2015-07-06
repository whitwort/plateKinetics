## API ##

#' Load experimental design and source data.
#' 
#' This function loads an experimental design and source data files described by
#' that design.  Both are checked for validity and consistency.  It returns an
#' experiment object (a list), which is used as the input for all downstream
#' analysis functions.
#' 
#' @param path A path to a directory where source files and (optionally) a
#'   design file can be found.  Defaults to the current working directory.
#' @param design A design object, usually created by \code{\link{loadDesign}}. 
#'   By default an experimental design will be loaded from the first file found
#'   in the \code{projectPath} that has a name containing 'design.yaml'.
#' @param findFiles Boolean.  If TRUE, the paths to the design file and data
#'   files can be complete file names or patterns to use to find a file in the
#'   project path; if no exact file name match is found then the first file
#'   which fits the pattern is picked.
#' 
#' @details Several vignettes are included with this package to fully document
#'   the options for input files.  Run \code{vignette('design-files')} for a
#'   description of the design file format.  Run \code{vignette('source-files')}
#'   for a description of supported input file formats and loader functions.
#' 
#' @return An experiment object that serves as input to downstream
#'   analysis functions.
#'
#' @seealso \code{\link{loadDesign}} for finer control over loading a design
#'   from a file or a list, \code{\link{newDesign}} for creating a template
#'   design.yaml file.
#'
#' @export
loadExperiment <- function( path      = getwd()
                          , design    = loadDesign(path, "design.yaml", findFiles = findFiles)
                          , findFiles = TRUE
                          ) {
  
  data    <- mergeData(path, design)
  factors <- data.frame( design$factors
                       , row.names = design$wells
                       , stringsAsFactors = TRUE
                       )
  
  list( design  = design
        
      # Source data structures
      , data    = data
      , factors = factors
      
      # Mutated by analysis procedures
      , map     = cbind(factors[data$well,], data)
      , reduce  = factors
      
      )
  
}

#' Load a design from a file or R list
#' 
#' This function should always be used to load design information, whether from
#' a yaml file or R list structure, because it performs validation checks and
#' injects reasonable defaults for missing parameters.
#' 
#' Usually there is no need to call this function directly; use
#' \code{\link{loadExperiment}} instead.
#' 
#' @param path A path to a directory where source files and (optionally) a
#'   design file can be found.  Not used if a design is given.
#' @param file File name (or pattern if findFiles = TRUE) for the yaml design
#'   file. Not used if a design is given.
#' @param design A preliminary design data structure to be validated.
#' @param default A design object to use as a source of default values to fill
#'   in missing entries in \code{design}.
#' @param findFiles Boolean.  If TRUE, the paths to the design file and data
#'   files can be complete file names or patterns to use to find a file in the
#'   project path; if no exact file name match is found then the first file
#'   which fits the pattern is picked.
#'
#' @details Several vignettes are included with this package to fully document 
#'   the options for input files.  Run \code{vignette('source-files')} for a
#'   description of supported input file formats and loader functions.
#'
#' @return A validated design object to be used by \code{\link{loadExperiment}}.
#' 
#' @seealso See \code{\link{newDesign}} for creating a template design.yaml
#'   file.
#'  
#'  @export
loadDesign <- function( path       = getwd()
                      , file       = "design.yaml"
                      , design     = yaml::yaml.load_file(fullPath(path, file, findFiles))
                      , default    = yaml::yaml.load(newDesign(file = NULL))
                      , findFiles  = TRUE
                      ) {
  
  if (is.null(design$loader)) {
    message("Design is missing a 'loader:' definition; using default: ", default$loader)
    design$loader <- default$loader
  }
  
  if (length(find(design$loader)) < 1) {
    stop("The data file loader `", design$loader, "` does not match any function currently in scope.")
  }
  
  if (is.null(design$platform)) {
    message("Design is missing a 'platform:' definition; using default:", default$platform)
    design$platform <- default$platform
  }
  
  if (!design$platform %in% names(platforms)) {
    platform <- eval(parse(text = design$platform))
  } else {
    platform <- platforms[[as.character(design$platform)]]
  }
  design$platform <- platform
  
  if (is.null(design$wells)) {
    message("Design is missing a 'wells:' definition; using default: whole platform")
    design$wells <- as.vector(design$platform)
  }
  design$wells <- expandWells(design$wells, design$platform)
  
  if (is.null(design$channels)) {
    stop("Design is missing a 'channels:' definition. No data can be loaded.")
  }
  
  for (name in names(design$channels)) {
    file <- fullPath(path, design$channels[[name]], findFiles)
    if (!file.exists(file)) {
      stop("No source file could be found for channel `", name, "` with name/pattern `", design$channels[name], "`.")
    } else {
      design$channels[name] <- file
    }
  }
  
  if (is.null(design$factors)) {
    stop("Design is missing a 'factors:' definition. No annotations can be added")
  }
  
  factorNames    <- names(design$factors)
  design$factors <- lapply( factorNames
                          , function(name) { 
                              fact <- expandFactor(design$factor[[name]], design$platform)
                              
                              missing <- !(design$wells %in% names(fact))
                              if (any(missing)) {
                                stop("Factor `", name, "` missing values for wells: ", design$wells[missing])
                              }
                              
                              fact[design$wells]
                            }
                          )
  names(design$factors) <- factorNames
  
  design
}

#' Load, validate, and reformat (if necessary) source data.
#' 
#' This function loads data using the given reader and then validates that it
#' conforms with the expectations of the experimental design and reformats it as
#' needed.
#' 
#' Usually there is no need to call this function directly; use
#' \code{\link{loadExperiment}} instead.
#' 
#' @param path Source file to load.
#' @param loader Function to use to load the source file.
#' @param design Design object, usually created with \code{\link{loadDesign}}.
#' 
#' @details Several vignettes are included with this package to fully document 
#'   the options for input files.  Run \code{vignette('source-files')} for a
#'   description of supported input file formats and loader functions.
#'
#' @return A validated data.frame with a 'time', 'well', and 'value' column.
#' 
loadData   <- function(path, loader, design) {
  x <- loader(path)
  
  wellCount <- length(design$wells)
  
  # spread format no header
  spreadNames <- c('time', design$wells)
  if ( identical(colnames(x), paste("V", 1:(wellCount + 1), sep = "")) ) {
    colnames(x) <- spreadNames
  }
  
  # gathered format no header
  gatheredNames <- c("time", "well", "value")
  if ( identical(colnames(x), paste("V", 1:3, sep = "")) ) {
    colnames(x) <- gatheredNames
  }
  
  dostop <- function(...) {
    stop("Reading source file `", path, "` with loader `", substitute(loader), "` failed.  ", ...)
  }
  
  if ( all(spreadNames %in% colnames(x)) ) {
    # reformat spread to gather
    df <- tidyr::gather(x[spreadNames], "well", "value", 2:(ncol(x)))
    colnames(df)[1] <- "time"
    
  } else if ( all(gatheredNames %in% colnames(x)) ) {
    # gathered format check
    df <- x[gatheredNames]
    
    missing <- !(design$wells %in% x$well)
    if ( any(missing) ) {
      dostop("The data.frame is missing values for wells: ", design$wells[missing])
    }
    
  } else {
    
    dostop("The data.frame does not conform to any accepted format.  See 'source-files' vignette for details.")
    
  }
 
  df$well  <- as.character(df$well)
  df$value <- as.numeric(df$value)
  rownames(df) <- NULL
  
  df
}

#' Load and merge data for all channels.
#' 
#' This function loads and merges source data for all channels in an experiment.
#' 
#' Usually there is no need to call this function directly; use
#' \code{\link{loadExperiment}} instead.
#' 
#' @param path A path to a directory where source files can be found.
#' @param design Design object, usually created with \code{\link{loadDesign}}.
#' 
#' @details Several vignettes are included with this package to fully document 
#'   the options for input files.  Run \code{vignette('source-files')} for a
#'   description of supported input file formats and loader functions.
#'
#' @return A data.frame containing a 'well', 'time' column and value columns for
#'   each channel.
#' 
mergeData <- function(path, design) {
  
  loader   <- eval(parse(text = design$loader))
  dataList <- lapply( design$channels
                    , function(file) { 
                        loadData(file, loader, design) 
                      }
                    )
  
  c1     <- dataList[[1]]
  values <- lapply( dataList
                  , function(df) {
                    
                      # check count of measurements for each well
                      if ( !identical(df$well, c1$well) ) {
                        stop("Each channel must have the same number of time measurements for a given well.  Check source data.")
                      }
                      
                      # check time point consistency
                      if ( !identical(df$time, c1$time) ) {
                        message("This package assumes that data for all channels for a given well are collected at the same time points.  This does not appear to be the case. Processing will continue but will use ONLY the time points found in the source file for the FIRST channel to represent data in ALL channels. Non-identical time points are:")
                        not.eq <- !(df$time == c1$time)
                        print(unique(c1$time[not.eq]))
                        print(unique(df$time[not.eq]))
                      }
                      
                      df$value 
                    }
                  )
  
  data.frame( well = c1$well
            , time = c1$time
            , values
            , stringsAsFactors = FALSE
            )
  
}

#' Write a new design object out to a yaml file.
#' 
#' This function serializes a design object out to a yaml text file or character
#' vector.  It is useful for writing a design object out to a file for future
#' use or for creating a template design file.
#' 
#' @param design A optional design object.  Defaults to a sample experimental
#'   design.
#' @param file An optional file path to use for creating a yaml text file.
#'   containing \code{design}.  Defaults to 'design.yaml' in the current working
#'   directory.
#' 
#' @return A character vector containing yaml text.
#' 
#' @export
newDesign  <- function( design = list( loader   = "read.table"
                                     , platform = '96'
                                     , wells    = "A1 -> H12"
                                     , channels = list(channel1 = "data1.txt", channel2 = "data2.txt")
                                     , factors  = list( factor1 = list(`A1->H6`  = "A", `A7->H12` = "B")
                                                      , factor2 = list(`A1->D12` = "C", `E1->H12` = "D")
                                                      )
                                     )
                       , file   = "design.yaml"
                       ) {
  
  s <- yaml::as.yaml(design)
  if (!is.null(file)) { write(s, file) }
  s
  
}

#' Create well labels for a multiwell plate platform
#' 
#' Utility function that generates standard well labels for standard multiwell 
#' plate formats.
#' 
#' @param nrow Number of rows.
#' @param ncol Number of columns.
#' @param rowLabels A character vector containing  row labes. Defaults to the
#'   most common convention of using A..Z first, followed by two letter series: 
#'   AA..AZ, BA..BZ, etc.
#'   
#' @return A character matrix with well labels.
#' 
#' @export
platformLabels <- function( nrow
                          , ncol
                          , rowLabels = c( LETTERS
                                         , paste( rep(LETTERS, each = length(LETTERS))
                                                , LETTERS
                                                , sep = ""
                                                )
                                         )
                          ) {
  
  matrix( data  = paste(rep(rowLabels[1:nrow], each = ncol), 1:ncol, sep = "")
        , nrow  = nrow
        , ncol  = ncol
        , byrow = TRUE
        )
}

#' Standard multiwell plate platforms
#' 
#' Built-in data structure containing default platforms, which are character
#' matrixes containing well lables for standard multiwell plate formats.
#' 
#' Designs files can directly reference names on this list for the
#' \code{platform} parameter (for example:  '96' or '384').  Run
#' \code{vignette('design-files')} for details.
#' 
#' @seealso \code{\link{platformLabels}} for creating additional platform 
#'   definitions.
#' 
#' @export
platforms <- list( "4"    = platformLabels( 2,  2)
                 , "6"    = platformLabels( 2,  3)
                 , "24"   = platformLabels( 4,  6)
                 , "96"   = platformLabels( 8, 12)
                 , "384"  = platformLabels(16, 24)
                 , "1536" = platformLabels(32, 48)
                 , "6144" = platformLabels(64, 96)
                 )

## internals ##
expandFactor <- function(factor, platform) { 
  c( mapply( expandValues
           , names(factor)
           , factor
           , MoreArgs = list(platform = platform)
           , SIMPLIFY = FALSE
           , USE.NAMES = FALSE
           )
   , recursive = TRUE
   )
}

expandValues <- function(wells, value, platform) {
  wells  <- expandWells(wells, platform)
  values <- rep(value, length(wells))
  names(values) <- wells
  
  values
}

expandWells <- function(wells, platform) {
  
  wells <- gsub("\\s", "", wells)
  
  if (grepl(",", wells)) {
    
    wells <-  sapply( strsplit(wells, ",")[[1]]
                    , function(s) { expandWells(s, platform) }
                    )
    
    wells <- c(wells, recursive = TRUE)
    
  } else if (grepl("->", wells)) {
    range <- strsplit(wells, "->", fixed = TRUE)
    
    startWell <- range[[1]][1]
    endWell   <- range[[1]][2]

    start <- which(platform == startWell, arr.ind = TRUE)
    end   <- which(platform == endWell,   arr.ind = TRUE)
    
    wells <- t(platform[start[1]:end[1], start[2]:end[2]])
    
  }

  wells <- as.vector(wells)
  checkWell(wells, platform)
  
  wells
  
}

checkWell <- function(wells, platform) {
  missing <- !(wells %in% platform)
  
  if (any(missing)) {
    print(wells)
    print(missing)
    
    stop("The following wells are not in the given platform:  ", wells[missing])
  }
}

fullPath <- function(path, file, findFiles) {
  filePath <- file.path(path, file)
  if (!findFiles || file.exists(filePath)) {
    filePath
  } else {
    list.files(path, pattern = file, full.names = TRUE)[1]
  }
}
