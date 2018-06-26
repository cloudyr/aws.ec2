#' gets a parameter from the parameter store
#'
#' @param name The parameter name
#' @param with_decryption Specifies if the parameter should be decripted. Defaults to \code{TRUE}
#' @param ... Additional arguments passed to \code{ec2HTTP}
#'
#' @return The value of the parameter
#' @export
get_parameter <- function(name, with_decryption=TRUE, ...) {
  query <- list(Action = "GetParameter")
  if (missing(name)) {
    stop('Parameter name missing')
  }
  names(name) <- "Name"
  
  with_decryption <- if( with_decryption ) "true" else "false"
  names(with_decryption) <- "WithDecryption"
  
  query <- c(query, name, with_decryption)
  r <- ec2HTTP(version="2014-11-06", query = query, service="ssm", ...)
  return(r$GetParameterResult$Parameter)
}