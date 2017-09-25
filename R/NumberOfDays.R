NumberOfDays <- function(date) {

  m <- format(date, format="%m")
  while (format(date, format="%m") == m)
  {
    date <- date + 1
  }
  nday <- as.integer(format(date - 1, format="%d"))
  return(nday)
}
