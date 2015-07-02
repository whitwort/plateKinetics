library(yaml)
library(reshape2)

## API ##

#' Load an experimental design and data files from a directory
#' @export
loadExperiment <- function( projectPath = getwd()
                          , design = loadDesign(pickFile(projectPath, "design.yaml"))
                          ) {
  
  parser <- eval(parse(text = design$parser))
  data   <- lapply( design$channels
                  , function(file) {
                      parser( pickFile(projectPath, file)
                            , design = design
                            )
                    }
                  )
  
  list( design  = design
      , factors = data.frame( design$factors
                            , row.names = design$wells
                            )
      , data    = data.frame( well = data[[1]]$variable
                            , time = data[[1]]$time
                            , lapply(data, function(df) { df$value })
                            , stringsAsFactors = FALSE
                            )
      , analysis = list()
      )
  
}

loadDesign <- function( path
                      , design  = yaml.load_file(path)
                      , default = list( parser   = "read.table"
                                      , platform = "96"
                                      )
                      ) {
  
  if(is.null(design$parser)) {
    warning("Design is missing a 'parser:' definition; using default: ", default$parser)
    design$parser <- default$parser
  }
  
  if (length(find(design$parser)) < 1) {
    stop("The data file parser `", design$parser, "` does not match any function currently in scope.")
  }
  
  if (is.null(design$platform)) {
    warning("Design is missing a 'platform:' definition; using default:", default$platform)
    design$platform <- default$platform
  }
  design$platform <- platforms[[as.character(design$platform)]]
  
  if (is.null(design$wells)) {
    warning("Design is missing a 'wells:' definitio; using default: whole platform")
    design$wells <- as.vector(design$platform)
  }
  design$wells <- expandWells(design$wells, design$platform)
  
  if (is.null(design$factors)) {
    stop("Design is missing a 'factors:' definition.")
  }
  
  factorNames <- names(design$factors)
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

# Parses a .asc text file produced by Tecan's Magellan software saved with the
# following settings:  'Direction' : 'Horizontal', 'Result' : 'Table (well data in columns)', 'Add kinetic time stamps' : checked.
read.magellan <- function(filePath, design) {
  
  df <- read.table( filePath
                  , sep              = "\t"
                  , header           = FALSE
                  , stringsAsFactors = FALSE
                  )
  
  df[,1]        <- as.numeric(gsub("s", "", df[,1]))
  df[,ncol(df)] <- NULL
  
  if (ncol(df) - 1 != length(design$wells)) {
    warning("Input file `", filePath, "` does not match the well layout in the current design.")
  }
  colnames(df) <- c("time", design$wells)
  
  melt(df, id.vars = "time", )
}


## implementation ##

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
    
  } else if (grepl("->", wells)) {
    range <- strsplit(wells, "->", fixed = TRUE)
    
    startWell <- range[[1]][1]
    endWell   <- range[[1]][2]
    
    start <- which(platform == startWell, arr.ind = TRUE)
    end   <- which(platform == endWell,   arr.ind = TRUE)
    
    wells <- platform[start[1]:end[1], start[2]:end[2]]
    
  }

  wells <- as.vector(wells)
  checkWell(wells, platform)
  
  wells
  
}

checkWell <- function(wells, platform) {
  missing <- !(wells %in% platform)
  
  if (any(missing)) {
    stop("The following wells are not in the given platform:  ", wells[missing])
  }
}

pickFile <- function(path, file) {
  filePath <- file.path(path, file)
  if (file.exists(filePath)) {
    filePath
  } else {
    list.files(path, pattern = file, full.names = TRUE)[1]
  }
  
}

platformLabels <- function( nrow
                          , ncol
                          
                          # The most common row label convention is to first use
                          # letters A..Z and then to use a two letter series AA..AZ,
                          # BA..BZ, etc.
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

#' Platforms
#' @export
platforms <- list( "6"    = platformLabels( 2,  3)
                   , "24"   = platformLabels( 4,  6)
                   , "96"   = platformLabels( 8, 12)
                   , "384"  = platformLabels(16, 24)
                   , "1536" = platformLabels(32, 48)
                   , "6144" = platformLabels(64, 96)
)
