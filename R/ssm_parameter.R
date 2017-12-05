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
  return(fromJSON(r)$GetParameterResponse$GetParameterResult$Parameter)
}