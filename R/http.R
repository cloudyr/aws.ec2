#' @title EC2 API Requests
#' @description Execute an EC2 API Request
#' @param query A named list of query string parameters.
#' @param dryrun An optional logical specifying whether to execute a consequence-free \dQuote{dry run} of the request.
#' @param region A character string specifying an AWS region. See \code{\link[aws.signature]{locate_credentials}}.
#' @param key A character string specifying an AWS Access Key. See \code{\link[aws.signature]{locate_credentials}}.
#' @param secret A character string specifying an AWS Secret Key. See \code{\link[aws.signature]{locate_credentials}}.
#' @param session_token Optionally, a character string specifying an AWS temporary Session Token to use in signing a request. See \code{\link[aws.signature]{locate_credentials}}.
#' @param version A character string specifying an API version. Default is \dQuote{2015-10-01}.
#' @param clean_response A logical indicating whether to remove line breaks from the response. This is useful for returning a clean response object, but may not be appropriate if the XML-formatted API response contains meaningful linebreaks (e.g., in a keypair).
#' @param ... Additional arguments passed to \code{\link[httr]{GET}}.
#' @return A list
#' @importFrom aws.signature signature_v4_auth
#' @importFrom httr add_headers headers content warn_for_status http_status http_error GET
#' @importFrom xml2 read_xml as_list
#' @export
ec2HTTP <- function(query = list(), 
                    dryrun, 
                    region = Sys.getenv("AWS_DEFAULT_REGION","us-east-1"), 
                    key = NULL, 
                    secret = NULL, 
                    session_token = NULL,
                    version = "2015-10-01",
                    clean_response = TRUE,
                    ...) {
    if (!missing(dryrun)) {
        query$DryRun <- tolower(as.character(dryrun))
    }
    query$Version <- version
    if (region == "us-east-1") {
        url <- paste0("https://ec2.amazonaws.com")
    } else {
        url <- paste0("https://ec2.",region,".amazonaws.com")
    }
    d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    Sig <- signature_v4_auth(
           datetime = d_timestamp,
           region = region,
           service = "ec2",
           verb = "GET",
           action = "/",
           query_args = query,
           canonical_headers = list(host = if (region == "us-east-1") "ec2.amazonaws.com" else paste0("ec2.",region,".amazonaws.com"),
                                    `X-Amz-Date` = d_timestamp),
           request_body = "",
           key = key, 
           secret = secret,
           session_token = session_token)
    headers <- list()
    headers[["x-amz-date"]] <- d_timestamp
    headers[["Authorization"]] <- Sig[["SignatureHeader"]]
    if (!is.null(session_token) && session_token != "") {
        headers[["x-amz-security-token"]] <- session_token
    }
    H <- do.call(add_headers, headers)
    
    if (length(query)) {
        r <- GET(url, H, query = query, ...)
    } else {
        r <- GET(url, H, ...)
    }
    if (http_error(r)) {
        tmp <- gsub("\n\\s*", "", content(r, "text"))
        x <- try(as_list(read_xml(tmp)), silent = TRUE)
        stop(paste0(parse_errors(x), collapse = "\n"))
    } else {
        if (isTRUE(clean_response)) {
            tmp <- gsub("\n\\s*", "", content(r, "text"))
        } else {
            tmp <- content(r, "text")
        }
        out <- try(as_list(read_xml(tmp)), silent = TRUE)
        if (inherits(out, "try-error")) {
            out <- structure(content(r, "text"))
        }
    }
    return(out)
}

parse_errors <- function(error) {
    unname(unlist(lapply(error$Errors, function(z) {
        paste0(z$Code[[1]], ": ", z$Message[[1]], "\n")
    })))
}
