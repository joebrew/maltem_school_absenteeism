
# Define function for reading in xls for phase 1 data
phase_1_read_data <- function(file_name = 'Form A core.xlsx'){
  message(paste0('Reading and cleaning ', file_name))
  # Read in the data
  if(grepl('.xlsx', file_name)){
    temp <- read_excel(paste0('data/', file_name))
  } else {
    if(file_name == 'Form D directed.csv'){
      # fix weird issue with character being converted to numeric
      temp <- read_csv(paste0('data/', file_name))
      class_column <- read.csv(paste0('data/', file_name))
      temp$CLASS <- as.character(class_column$CLASS)
      rm(class_column)
    } else {
      temp <- read_csv(paste0('data/', file_name))
    }
  }
  # Remove empty rows
  bad_rows <- rep(FALSE, nrow(temp))
  for (row in 1:nrow(temp)){
    empty <- c()
    for (column in 1:ncol(temp)){
      empty[column] <- is.na(temp[row, column])
    }
    if(all(empty)){
      bad_rows[row] <- TRUE
    }
  }
  temp$bad_row <- bad_rows
  temp <- temp[!temp$bad_row,]
  temp$bad_row <- NULL
  # Assign to an appropriate name
  object_name <- 
    gsub(' ', '_', gsub('.xlsx|.csv', '', tolower(file_name)))
  assign(object_name,
         temp,
         envir = .GlobalEnv)
}



# Add 0s to numbers if needed
pre_zero <- function(var, n = 2){
  var <- as.character(var)
  var <- ifelse(is.na(var), '0', var)
  for(i in 1:length(var)){
    while(nchar(var[i]) < n){
      var[i] <- paste0('0', var[i])
    }
  }
  return(var)
}

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Collect and disconnect
#' 
#' Collect a database table connection into an in-memory dataframe and disconnect from that connection
#' @param table_name The name of the table connection
#' @export

collect_and_disconnect <- function(table_name){
  require(dplyr)
  require(RPostgreSQL)
  temp <- dplyr::collect(table_name)
  RPostgreSQL::dbDisconnect(table_name$src$con)
  return(temp)
}

read_yaml <- function(yaml_file = '../credentials.yaml'){
  require(yaml)
  yaml.load_file(yaml_file)
  
}
