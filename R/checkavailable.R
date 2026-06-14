#' Checks availability of resources
#'
#' Checks availability of resources.
#'
#' @param what a string.
#'
#' @returns a logical value.
#' @noRd

checkavailable <- function(what = c("github","iea")){
  
  
  what <- what[1L]
  
  if (!capabilities("http/ftp")) {
    return(FALSE)
  }
  
  
  tmis <- getOption("timeout")
  options(timeout = 4)
  on.exit(options(timeout = tmis))
  
  
  if(what=="github"){
    urli <- "https://www.github.com"
  }else{
    urli <- "https://www.iea.nl"
  }
  
  isup <- tryCatch({
    req <- httr2::request(urli) |> 
      httr2::req_timeout(4) |> 
      httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) UserCheck") |> 
      httr2::req_method("HEAD")
    
    resp <- httr2::req_perform(req)
    
    httr2::resp_status(resp) < 400
  }, error = function(e) {
    message("Error encountered: ", e$message)
    FALSE
  })
  
  return(isup)
}

checkavailable("github")