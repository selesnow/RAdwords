function (clientCustomerId, google_auth, statement, apiVersion = "201809", 
                       transformation = TRUE, changeNames = TRUE, includeZeroImpressions = FALSE, 
                       verbose = FALSE) 
{
  access <- google_auth$access
  credlist <- google_auth$credentials
  if (as.numeric(Sys.time()) - 3600 >= access$timeStamp) {
    access <- refreshToken(google_auth)
  }
  google.auth <- paste(access$token_type, access$access_token)
  url <- paste("https://adwords.google.com/api/adwords/reportdownload/v", 
               apiVersion, sep = "")
  header <- c(Authorization = google.auth, developerToken = credlist$auth.developerToken, 
              clientCustomerId = clientCustomerId, includeZeroImpressions = includeZeroImpressions)
  if (attributes(statement)$compressed) {
    data <- RCurl::getBinaryURL(url, httpheader = header, 
                                postfields = statement, verbose = verbose, ssl.verifypeer = TRUE)
    
    
    # ####
    # check for Error - Alexey Seleznev
    xml_data  <- xml2::read_xml(data)
    xml_error <- xml_find_all(xml_data, "ApiError") 
    
    if ( length(xml_error) > 0 ) {
      
      stop(xml_text(xml_error))
      
    }
    # ####
    
    tmp <- tempfile()
    if (.Platform$OS.type == "unix" && file.exists("/dev/shm") && 
        file.info("/dev/shm")$isdir) {
      tmp <- tempfile(tmpdir = "/dev/shm")
    }
    on.exit(unlink(tmp), add = TRUE)
    writeBin(data, con = tmp)
    data <- paste(readLines(con <- gzfile(tmp)), collapse = "\n")
    close(con)
  }
  else {
    data <- RCurl::getURL(url, httpheader = header, postfields = statement, 
                          verbose = verbose, ssl.verifypeer = TRUE)
    
  }
  valid <- grepl(attr(statement, "reportType"), data)
  if (transformation & valid) {
    data <- transformData(data, report = attributes(statement)$reportType, 
                          apiVersion = apiVersion)
    if (changeNames) {
      data <- changeNames(data)
    }
  }
  
  data
}
