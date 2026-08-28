#' Fetch and read microdata files from SISCAN DataSUS
#'
#' Function to download microdata files from SISCAN (DBC or CSV) from SISCAN DataSUS and reads them.
#'
#'@param year_start numeric.Start year of files in the format yyyy.
#'@param year_end numeric.End year of files in the format yyyy.
#'@param month_start numeric. Start month of files in the format mm.
#'@param end_start numeric. End month of files in the format mm.
#'@param uf an optional string or a vector of strings. By default all UFs ("Unidades Federativas") are download. See Details.
#'@param information_system	string. an optional string of the abbreviation of the SISCAN information system to be accessed. See Details.
#'@param vars an optional string or a vector of strings. By default, all variables read and stored. See Details
#'@param stop_on_error logical. If TRUE, the download process will be stopped if an error occurs.
#'@param timeout numeric (seconds). Sets a timeout tolerance for downloads, usefull on large files and/or slow connections. Defaults to 240 seconds.
#'@param track_source logical. If TRUE, adds a column called source with the downloaded file name.
#'@details The SISCAN SUS database has two XXXXXXX, the early one a XXXXXX data of breast cancer and uterin cancer (REVER INGLES).....
#'
#'@returns pool function return a pool class object with Species label, Abundance and per capita fecundity parameter w. Such object also record richness and initial parameters in attributes
#'@seealso process_siscan
#'@examples
#'

#'@export
function (year_start, month_start = NULL, year_end, month_end = NULL,
          uf = "all", information_system = NULL, vars = NULL, stop_on_error = FALSE,
          timeout = 240, track_source = FALSE)
{
  original_time_option <- getOption("timeout")
  on.exit(options(timeout = original_time_option))
  options(timeout = timeout)

  available_information_system <- c("SISCAN","SISCOLO","SISMAMA")
  checkmate::assert_choice(x = information_system, choices = available_information_system)
  checkmate::assert_numeric(x = year_start, lower = 1996,
                            null.ok = FALSE)
  checkmate::assert_numeric(x = year_end, lower = 1996, null.ok = FALSE)
  checkmate::assert_numeric(x = month_start, lower = 1, upper = 12,
                            null.ok = TRUE)
  checkmate::assert_numeric(x = month_end, lower = 1, upper = 12,
                            null.ok = TRUE)
  #AJUSTE DA DATA DE ACORDO A BASE DE DADOS
  if (substr(information_system, 4, 7) == "COLO" |
      substr(information_system, 4, 7) == "MAMA") {
    date_start <- as.Date(paste0(year_start, "-", formatC(month_start,
                                                          width = 2, format = "d", flag = "0"), "-", "01"))
    date_end <- as.Date(paste0(year_end, "-", formatC(month_end,
                                                      width = 2, format = "d", flag = "0"), "-", "01"))
  }else if (substr(information_system, 4, 6) == "CAN" | year_start > 2015 | year_end > 2015 ) {
    if (year_start < 2013 && year_end > 2015) {
      cli::cli_abort(message = "The range of years is in differents database, repeat operation in small range interval. SISCAN 2013 - 2025. SICOLO 2006 - 2015. SISMAMA 2009 - 2015")
    }
    date_start <- as.Date(paste0(year_start, "-01-01"))
    date_end <- as.Date(paste0(year_end, "-01-01"))
  }
  if (date_start > date_end) {
    cli::cli_abort(message = "Start date must be greather than end date.")
  }
  ufs <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES",
           "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE",
           "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE",
           "TO")
  checkmate::assert_subset(x = uf, choices = c("all", ufs))
  if (information_system == "SISCAN" & uf[1] != "all") {
    cli::cli_alert_info("SISCAN files are not available per UF. Ignoring argument 'uf' and downloading data.")
  }
  local_internet <- curl::has_internet()
  if (local_internet == TRUE) {
    cli::cli_alert_info("Your local Internet connection seems to be ok.")
  }else {
    cli::cli_alert_warning("It appears that your local Internet connection is not working. Can you check?")
    return(NULL)
  }
  datasus_ftp_connection <- RCurl::url.exists("ftp.datasus.gov.br",
                                              .opts = list(timeout = timeout))
  if (datasus_ftp_connection == TRUE) {
    cli::cli_alert_info("DataSUS FTP server seems to be up and reachable.")
    cli::cli_alert_info("Starting download...")
  }else {
    cli::cli_alert_warning("It appears that DataSUS FTP is down or not reachable.")
    return(NULL)
  }

  if (substr(information_system, 4, 7) == "COLO" |
      substr(information_system, 4, 7) == "MAMA") {
                    dates <- seq(date_start, date_end, by = "month")
                    dates <- paste0(substr(lubridate::year(dates), 3, 4),
                              formatC(lubridate::month(dates), width = 2, format = "d",
                              flag = "0"))
    }else{
    dates <- seq(date_start, date_end, by = "year")
    dates <- lubridate::year(dates)
  }
  if (uf[1] == "all") {
    lista_uf <- ufs
  }else {
    lista_uf = uf
  }
##### continuar daqui

}#end
#'@export
print.pool<-function(x){
  cat("number of species:",richness(x$abundance),"\n")
  if(nrow(x)>10){
    print(head(as.data.frame(x),3))
    cat(rep("\t.\n",3))
    print(tail(as.data.frame(x),3))
  }else{
    print.data.frame(x)
  }
  invisible(x)
}
