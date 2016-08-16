#' Get Data from Statistics Denmarks
#' @description Retrieves metadata about a dataset. The result contains eg. information about variables and their values used when extracting data.
#' @param TableId character("TableId"); Table id to extract metadata from - get a list of TableId using \code{\link{StatBank.GetTabels}}
#' @param lang character("DA", "ENG); Language to export Danish "DA" (default) or English ENG
#' @param variables list(Variableid = c(Value-ids)); list of variable or variables to include in the API call - see details
#' @return Returns a Data Frame
#' @details To include variables to the API call build a list, where each object in the list is named with the Variableid and value added as character or character list. Use \code{\link{StatBank.GetMetadata}} to get a list of variables or browse http://www.statistikbanken.dk/.
#' @examples
#' #Building a list to get variables indto
#' #the call - note that * includes all values
#' #of that variable
#'
#' variablelist <- list(
#'    UDDANNELSE = c("H40", "H4020", "H402015", "H4024"),
#'    FSTATUS = "*")
#'
#' StatBank.GetData("UDDAKT40", variables = variablelist)
#' @export

StatBank.GetData <- function(TableId, lang = "DA", variables = NULL) {

  STOPTableIdmissin <- "TableId is missing - use StatBank.GetTabels() to get a list of all TableIds or browse http://www.statistikbanken.dk/"
  if(!exists("TableId")) stop(STOPTableIdmissin)
  if(length(TableId) == 0) stop(STOPTableIdmissin)

  if(!is.character(TableId)) stop("TableId must be of class character.")

  lang <- match.arg(lang, c("DA", "ENG"))

  require(httr)
  require(readr)
  require(jsonlite)

  if(!is.null(variables)){
    if(length(variables) == 1){
      variablescall <- paste0("&", names(variables), "=", paste(variables[[1]], collapse = "%2C"))
    } else {
      for(v in seq(variables)){
        if(v == 1) {
          variablescall <- paste0(names(variables[v]), "=", paste(variables[[v]], collapse = "%2C"))
        } else {
          variablescall <- c(variablescall, paste0(names(variables[v]), "=", paste(variables[[v]], collapse = "%2C")))
        }
      }
      variablescall <- paste0("&", paste0(variablescall, collapse = "&"))
    }
  }

  url <- paste0("http://api.statbank.dk/v1/data/", TableId, "/CSV?delimiter=Semicolon", ifelse(lang == "ENG", "&lang=en", ""), ifelse(is.null(variables), "", variablescall))

  data <- httr::GET(url)

  if(data$status_code != 200) {
    textdata <- httr::content(data, as="text")
    outputdata <- jsonlite::fromJSON(textdata)
    stop(outputdata$message)
  } else {
    textdata <- httr::content(data, as="text")
    outputdata <- readr::read_csv2(textdata)
  }

  return(outputdata)

}
