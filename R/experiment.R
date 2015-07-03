#' @import yaml

#' Load experimental design and source data.
#' 
#' This function loads an experimental design and source data files described by
#' that design.  Both are checked for validity and consistency.  It returns an
#' experiment object (a list), which is used as the input for all downstream
#' analysis functions.
#' 
#' See the \link[=doc/design.html]{Design file} vignette for complete 
#' documentation of the structure of a design \href{http://www.yaml.org/}{yaml}
#' file (or R list structure).
#' 
#' @param projectPath A path to a directory where source files and (optionally)
#'   a design file can be found.  Defaults to the current working directory.
#' @param design A design object, usually created by \code{\link{loadDesign}}. 
#'   By default an experimental design will be loaded from the first file found
#'   in the \code{projectPath} that has a name ending in 'design.yaml'.
#' 
#' @return An experiment object (list) that serves as input to downstream
#'   analysis functions.
#' 
#' @seealso \code{\link{loadDesign}} for finer control over loading a design
#'   from a file or a list, \code{\link{writeDesign}} for creating a template
#'   design.yaml file.
#' 
#' @export
loadExperiment <- function( projectPath = getwd()
                          , design      = loadDesign(pickFile(projectPath, "design.yaml"))
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


#' Load a design from a file or R list
#' 
#' This function should always be used to load design information, whether from
#' a yaml file or R list structure, because it performs various validation and
#' consistency checks.  It also injects reasonable defaults for missing
#' parameters.
#' 
#' @param path Path to a yaml design file.  Only used if \code{design} is not
#'   given directly.
#' @param design A preliminary design data structure (See the
#'   \link[=doc/design.html]{Design file} vignette for complete documentation of
#'   the structure of a design).
#' @param default A design object to use as a source of default values to fill
#'   in missing keys in \code{design}.
#' 
loadDesign <- function( path
                      , design  = yaml.load_file(path)
                      , default = writeDesign(file = NULL)
                      ) {
  
  if(is.null(design$parser)) {
    message("Design is missing a 'parser:' definition; using default: ", default$parser)
    design$parser <- default$parser
  }
  
  if (length(find(design$parser)) < 1) {
    stop("The data file parser `", design$parser, "` does not match any function currently in scope.")
  }
  
  if (is.null(design$platform)) {
    message("Design is missing a 'platform:' definition; using default:", default$platform)
    design$platform <- default$platform
  }
  design$platform <- platforms[[as.character(design$platform)]]
  
  if (is.null(design$wells)) {
    message("Design is missing a 'wells:' definition; using default: whole platform")
    design$wells <- as.vector(design$platform)
  }
  design$wells <- expandWells(design$wells, design$platform)
  
  if (is.null(design$factors)) {
    stop("Design is missing a 'factors:' definition.")
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

writeDesign  <- function( design = list( parser   = "read.table"
                                       , platform = '96'
                                       , wells    = "A1 -> H12"
                                       , channels = list(channel1 = "data1.txt", channel2 = "data2.txt")
                                       , factors  = list( factor1 = list(`A1->H6` = "A", `A7->H12` = "B")
                                                        , factor2 = list(`A1->D12` = "C", `E1->H12` = "D")
                                                        )
                                       )
                        , file   = "design.yaml"
                        ) {
  
  s <- as.yaml(design)
  
  if (!is.null(file)) {
    write(s, file)
  }
  
  s
  
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


