#' Provide registration delay for NGER
#'
#' Provide registration delay in median number of days grouped by years
#'
#' @param years integer vector with years for results and grouping
#' @return data frame with registry name and values for each year
#' @export

NGERregDealy <- function(years) {

  # get data
  NGERdelayData <- NGERRegDelayData()

  # make data frame
  medianDelay <- data.frame(regName = "NGER", stringsAsFactors = FALSE)
  for (i in years) {
    ind <- which(NGERdelayData$year == i)
    medianDelay[[as.character(i)]] = median(NGERdelayData$daysDiff[ind])
    medianDelay[[paste0("N", as.character(i)]] = length(ind)
  }

  return(medianDelay)
}
