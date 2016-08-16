#' Get a list of tabels from Statistics Denmarks
#' @description Retrieves list of datasets. Filtering by subject is done by using the codes from the "subjects"-call.
#' @param discontinued logical; FALSE (default) - Include discontinued tables
#' @return Returns a Data Frame
#' @export

StatBank.GetTabels <- function(discontinued = FALSE) {

  require(httr)
  require(jsonlite)

  url <- paste0("http://api.statbank.dk/v1/tables?", ifelse(discontinued, "includeInactive=true&", ""), "format=JSON")

  data <- httr::GET(url)
  textdata <- httr::content(data, as="text")
  outputdata <- jsonlite::fromJSON(textdata)

  outputdata$updated <- as.POSIXct(outputdata$updated, tz="CET")

  return(outputdata)

}
