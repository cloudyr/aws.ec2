#' @title EC2 API Requests
#' @description This is the workhorse function to execute calls to the EC2 API.
#' @param query A named list of query string parameters.
#' @param headers A list of headers to pass to the HTTP request.
#' @param dry_run An optional logical specifying whether to execute a
#'   consequence-free \dQuote{dry run} of the request. When this parameter is
#'   \code{TRUE}, the return of this function is either \code{TRUE} or an error.
#' @param verbose A logical indicating whether to be verbose. Default is given
#'   by `options("verbose")`.
#' @param region A character string specifying an AWS region. See
#'   \code{\link[aws.signature]{locate_credentials}}.
#' @param key A character string specifying an AWS Access Key. See
#'   \code{\link[aws.signature]{locate_credentials}}.
#' @param secret A character string specifying an AWS Secret Key. See
#'   \code{\link[aws.signature]{locate_credentials}}.
#' @param session_token Optionally, a character string specifying an AWS
#'   temporary Session Token to use in signing a request. See
#'   \code{\link[aws.signature]{locate_credentials}}.
#' @param version A character string specifying an API version. Default is
#'   \dQuote{2016-11-15}. Note: For future-proofing, this parameter is not
#'   validated.
#' @param clean_response A logical indicating whether to remove line breaks from
#'   the response. This is useful for returning a clean response object, but may
#'   not be appropriate if the XML-formatted API response contains meaningful
#'   linebreaks (e.g., in a keypair).
#' @param \dots Additional arguments passed to \code{\link[httr]{GET}}.
#' @return A list.
#' @importFrom aws.signature signature_v4_auth
#' @importFrom aws.signature locate_credentials
#' @importFrom httr add_headers headers content warn_for_status http_status
#'   http_error GET
#' @importFrom xml2 read_xml as_list
#' @export
ec2HTTP <- function(query = list(),
                    headers = list(),
                    dry_run = FALSE,
                    verbose = getOption("verbose", FALSE),
                    region = Sys.getenv("AWS_DEFAULT_REGION", "us-east-1"),
                    key = NULL,
                    secret = NULL,
                    session_token = NULL,
                    version = "2016-11-15",
                    clean_response = TRUE,
                    ...) {
  # Set things up for aws.signature.
  host <- if (region == "us-east-1") {
    "ec2.amazonaws.com" 
  } else {
    paste0("ec2.", region, ".amazonaws.com")
  }
  if (isTRUE(dry_run)) {
    query$DryRun <- "true"
  }
  query$Version <- version
  
  # locate and validate credentials
  credentials <- aws.signature::locate_credentials(
    key = key, secret = secret, session_token = session_token, 
    region = region, verbose = verbose
  )
  key <- credentials[["key"]]
  secret <- credentials[["secret"]]
  session_token <- credentials[["session_token"]]
  region <- credentials[["region"]]
  
  d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  headers[["host"]] <- host
  headers[["x-amz-date"]] <- d_timestamp
  
  # generate request signature
  Sig <- aws.signature::signature_v4_auth(
    datetime = d_timestamp,
    region = region,
    service = "ec2",
    verb = "GET",
    action = "/",
    query_args = query,
    canonical_headers = headers,
    request_body = "",
    key = key, 
    secret = secret,
    session_token = session_token,
    verbose = verbose)
  headers[["Authorization"]] <- Sig[["SignatureHeader"]]
  if (!is.null(session_token) && session_token != "") {
    headers[["x-amz-security-token"]] <- session_token
  }
  H <- do.call(httr::add_headers, headers)

  url <- paste0("https://", host)
  
  verbosity <- if (isTRUE(verbose)) {
    httr::verbose() 
  } else {
    NULL
  }
  
  # execute request. Note that query *always* has length > 0 for this api.
  r <- httr::GET(url, H, query = query, ..., verbosity)
  
  if (httr::http_error(r)) {
    tmp <- gsub("\n\\s*", "", httr::content(r, "text", encoding = "UTF-8"))
    x <- try(xml2::as_list(xml2::read_xml(tmp)), silent = TRUE)
    msg <- paste0(.parse_errors(x), collapse = "\n")
    expected_dry_run_error <- paste(
      "DryRunOperation : \n",
      "- Request would have succeeded, but DryRun flag is set."
    )
    if (isTRUE(dry_run) && msg == expected_dry_run_error) {
      return(TRUE) 
    } else stop(msg, call. = FALSE)
  } else {
    if (isTRUE(clean_response)) {
      tmp <- gsub("\n\\s*", "", httr::content(r, "text", encoding = "UTF-8"))
    } else {
      tmp <- httr::content(r, "text")
    }
    out <- try(xml2::as_list(xml2::read_xml(tmp)), silent = TRUE)
    if (inherits(out, "try-error")) {
      out <- structure(httr::content(r, "text", encoding = "UTF-8"))
    }
  }
  out <- out[[1]]
  return(out)
}

.parse_errors <- function(error) {
  messages <- if (!is.null(error$Errors)) error$Errors else error$Response$Errors
  unlist(lapply(messages, function(z) {
    paste(z$Code[[1]], z$Message[[1]], sep = " : \n - ", collapse = "\n")
  }))
}
