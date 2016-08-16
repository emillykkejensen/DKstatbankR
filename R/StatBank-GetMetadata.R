#' Get metadata for a tabel from Statistics Denmarks
#' @description Retrieves metadata about a dataset. The result contains eg. information about variables and their values used when extracting data.
#' @param TableId character("TableId"); Table id to extract metadata from - get a list of TableId using \code{\link{StatBank.GetTabels}}
#' @param lang character("DA", "ENG); Language to export Danish "DA" (default) or English ENG
#' @return Returns a list
#' @export

StatBank.GetMetadata <- function(TableId, lang = "DA") {

  STOPTableIdmissin <- "TableId is missing - use StatBank.GetTabels() to get a list of all TableIds or browse http://www.statistikbanken.dk/"
  if(!exists("TableId")) stop(STOPTableIdmissin)
  if(length(TableId) == 0) stop(STOPTableIdmissin)

  if(!is.character(TableId)) stop("TableId must be of class character.")

  lang <- match.arg(lang, c("DA", "ENG"))

  require(httr)
  require(jsonlite)

  url <- paste0("http://api.statbank.dk/v1/tableinfo/", TableId, ifelse(lang == "ENG", "?lang=en", ""), ifelse(lang == "ENG", "&format=JSON", "?format=JSON"))

  data <- httr::GET(url)
  textdata <- httr::content(data, as="text")
  outputdata <- jsonlite::fromJSON(textdata)

  outputdata$updated <- as.POSIXct(outputdata$updated, tz="CET")

  return(outputdata)

}
