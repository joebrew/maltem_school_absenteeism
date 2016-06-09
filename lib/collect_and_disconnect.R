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
