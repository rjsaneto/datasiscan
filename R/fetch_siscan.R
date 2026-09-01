#' Fetch and read microdata files from SISCAN DataSUS
#'
#' Function to download microdata files from SISCAN (DBC or CSV) from SISCAN DataSUS and reads them.
#'
#'@param year_start numeric.Start year of files in the format yyyy.
#'@param year_end numeric.End year of files in the format yyyy.
#'@param month_start numeric. Start month of files in the format mm.
#'@param end_start numeric. End month of files in the format mm.
#'@param uf an optional string or a vector of strings. By default all UFs ("Unidades Federativas") are download. See Details.
#'@param information_system	string an optional string of the abbreviation of the SISCAN information system to be accessed. See Details.
#'@param exam string of the three possible examination in SISCAN database: "CITO" - screening cytopathologic tests, "HISTO" - Diagnostic HISTOpathologic tests,"MMG" - screening mammography.
#'@param PACNT logical. If TRUE, extract data of PACNT files in SISCAN database.
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
fetch_siscan<-function (year_start, month_start = NULL, year_end, month_end = NULL,
                        uf = "all", information_system = "SISCAN",tissue="COLO",exam = "CITO",PACNT=F, vars = NULL, stop_on_error = FALSE,
                        timeout = 240, track_source = FALSE)
{

  original_time_option <- getOption("timeout")
  on.exit(options(timeout = original_time_option))
  options(timeout = timeout)

  if (year_start < 2013 && year_end > 2015) {
    cli::cli_abort(message = "For this range of years is necessary use several database, repeat operation in small range interval. SISCAN 2013 - 2025. SICOLO 2006 - 2015. SISMAMA 2009 - 2015")
  }

  available_information_system <- c("SISCAN","SISCOLO","SISMAMA")
  available_tissues <- c("COLO","MAMA")
  available_exam <- c("CITO","HISTO","MMG")

  checkmate::assert_choice(x = information_system, choices = available_information_system)
  checkmate::assert_choice(x = tissue, choices = available_tissues)
  checkmate::assert_choice(x = exam, choices = available_exam)

  if(information_system=="SISMAMA"){
    checkmate::assert_numeric(x = year_start, lower = 2009,upper = 2015,null.ok = FALSE)
    checkmate::assert_numeric(x = year_end, lower = 2009,upper = 2015, null.ok = FALSE)
  }
  if(information_system=="SISCOLO"){
    checkmate::assert_numeric(x = year_start, lower = 2006,upper = 2015,null.ok = FALSE)
    checkmate::assert_numeric(x = year_end, lower = 2006,upper = 2015, null.ok = FALSE)
  }
  if(information_system=="SISCAN"){
    checkmate::assert_numeric(x = year_start, lower = 2013,null.ok = FALSE)
    checkmate::assert_numeric(x = year_end, lower = 2013, null.ok = FALSE)
  }

  checkmate::assert_numeric(x = month_start, lower = 1, upper = 12, null.ok = TRUE,)
  checkmate::assert_numeric(x = month_end, lower = 1, upper = 12, null.ok = TRUE)

  if(is.null(month_start)|is.null(month_end)){
    date_start <- as.Date(paste0(year_start, "-01-01"))
    date_end <- as.Date(paste0(year_end, "-01-01"))
  }else{
    date_start <- as.Date(paste0(year_start, "-", formatC(month_start, width = 2, format = "d", flag = "0"), "-", "01"))
    date_end <- as.Date(paste0(year_end, "-", formatC(month_end, width = 2, format = "d", flag = "0"), "-", "01"))
  }
  if (date_start > date_end) {
    cli::cli_abort(message = "Start date must be greather than end date.")
  }
  dates <- seq(date_start, date_end, by = "month")

  if(information_system=="SISCOLO" && (tissue=="MAMA" | exam=="MMG")){
    cli::cli_abort(message = "Tissue or Test not available in SISCOLO database")
  }

  if(information_system=="SISMAMA" && (tissue=="COLO")){
    cli::cli_abort(message = "Tissue or Test not available in SISMAMA database")
  }

  if(tissue=="COLO" && exam=="MMG"){
    cli::cli_abort(message = "Test not available for uterine cervix database")
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
    dates <- paste0(substr(lubridate::year(dates), 3, 4),
                    formatC(lubridate::month(dates), width = 2, format = "d",
                            flag = "0"))
    if (uf[1] == "all") {
      lista_uf <- ufs
    }else {
      lista_uf = uf
    }
    if(substr(information_system, 4, 7) == "COLO"){
      geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISCOLO4/Dados/"
    }else{
      geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISMAMA/Dados/"
    }
    avail_geral <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(
      url = geral_url, ftp.use.epsv = TRUE, dirlistonly = TRUE),
      split = "\n")),
      start = 5, stop = 8))
    if (!all(dates %in% avail_geral)) {
      cli::cli_alert(paste0("The following dates are not availabe at DataSUS: ",
                            paste0(dates[!dates %in% c(avail_geral, avail_prelim)],
                                   collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% avail_geral]
    files_list <- if (any(valid_dates %in% avail_geral)) {
      paste0(geral_url, substr(exam,1,1),substr(tissue,1,1), as.vector(sapply(lista_uf,
                                                                              paste0, valid_dates[valid_dates %in% avail_geral],
                                                                              ".dbc")))
    }
  }else{
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISCAN/"
    avail_geral <- unlist(strsplit(x = RCurl::getURL(
      url = geral_url, ftp.use.epsv = TRUE, dirlistonly = TRUE),
      split = "\r\n"))
    if(length(avail_geral)<=1){
      avail_geral <- unlist(strsplit(x = avail_geral,
        split = "\n"))
    }

    dates <- unique(lubridate::year(dates))
    prefix<-paste0(information_system,"_")
    if(exam=="MMG"){
      prefix<-paste0(prefix,"MAMOGRAFIA","_")
    }else{
      prefix<-paste0(prefix,exam,"_",tissue,"_")
    }
    if(PACNT){
      prefix<-paste0(prefix,"PACNT","_")
    }
    files_list <- paste0(prefix, dates,".csv")
    if (!any(files_list %in% avail_geral)) {
      cli::cli_abort(paste0("The datas are not availabe at DataSUS"))
    }
    if (!all(files_list %in% avail_geral)) {
      cli::cli_alert(paste0("The following datas are not availabe at DataSUS: ",
                            paste0(files_list[!files_list %in% avail_geral],
                                   collapse = ", "), ". Only the available datas will be downloaded."))
    }

    files_list<-paste0(geral_url,files_list[files_list %in% avail_geral])


  }
  ##### Aqui faz o download
  data <- NULL
  for (f in files_list) {
    temp <- tempfile()
    partial <- data.frame()
    tryCatch({
      utils::download.file(f, temp, mode = "wb", method = "libcurl")
      if(information_system=="SISCAN"){
        partial <- read.csv(f, header = T,sep=";")
      }else{

        partial <- read.dbc::read.dbc(temp, as.is = TRUE)
      }
      file.remove(temp)
    }, error = function(cond) {
      cli::cli_alert_info(paste("Something went wrong with this URL:",
                                f))
      cli::cli_alert("This can be a problem with the Internet or the file does not exist yet.")
      cli::cli_alert("If the file is too big, try to increase the timeout argument value.")
      if (stop_on_error == TRUE) {
        cli::cli_abort("Stopping download.")
      }
    })
    if (nrow(partial) > 0) {
      if (track_source == TRUE) {
        partial$source <- basename(file)
      }
      if (!all(vars %in% names(partial)))
        cli::cli_abort("One or more variables names are unknown. Typo?")
      if (is.null(vars)) {
        data <- dplyr::bind_rows(data, partial)
      }else {
        data <- dplyr::bind_rows(data, subset(partial,
                                              select = vars))
      }
    }
  }
  class(data)<-c("fetch_siscan","data.frame")
  return(data)
}#end
#'@export
print.fetch_siscan<-function(x){
  cat("number of records:",nrow(x),"\n")
  nomes<-names(x)
  ordem<-order(nomes)
  for(i in 1:length(nomes)){
    cat("number of classes of ",names(x)[ordem[i]],":",length(unique(x[,ordem[i]])),"\n")
  }
  invisible(x)
}
