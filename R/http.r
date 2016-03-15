#' @title EC2 API Requests
#' @description Execute an EC2 API Request
#' @param query A named list of query string parameters.
#' @param dryrun An optional logical specifying whether to execute a consequence-free \dQuote{dry run} of the request.
#' @param region A character string containing the AWS region. If missing, defaults to \dQuote{us-east-1}.
#' @param key A character string containing an AWS Access Key ID. If missing, defaults to value stored in environment variable \dQuote{AWS_ACCESS_KEY_ID}.
#' @param secret A character string containing an AWS Secret Access Key.  If missing, defaults to value stored in environment variable \dQuote{AWS_SECRET_ACCESS_KEY}.
#' @param version A character string specifying an API version. Default is \dQuote{2015-10-01}.
#' @param ... Additional arguments passed to \code{\link[httr]{GET}}.
#' @return A list
#' @importFrom aws.signature signature_v4_auth
#' @importFrom httr add_headers headers content warn_for_status http_status GET
#' @importFrom xml2 read_xml as_list
#' @export
ec2HTTP <- function(query = list(), 
                    dryrun, 
                    region = Sys.getenv("AWS_DEFAULT_REGION","us-east-1"), 
                    key = Sys.getenv("AWS_ACCESS_KEY_ID"), 
                    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"), 
                    version = "2015-10-01",
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
    if (key == "") {
        H <- add_headers(`X-Amz-Date` = d_timestamp)
    } else {
        S <- signature_v4_auth(
               datetime = d_timestamp,
               region = region,
               service = "ec2",
               verb = "GET",
               action = "/",
               query_args = query,
               canonical_headers = list(host = if (region == "us-east-1") "ec2.amazonaws.com" else paste0("ec2.",region,".amazonaws.com"),
                                        `X-Amz-Date` = d_timestamp),
               request_body = "",
               key = key, secret = secret)
        H <- add_headers(`X-Amz-Date` = d_timestamp, 
                         Authorization = S$SignatureHeader)
    }
    if (length(query)) {
        r <- GET(url, H, query = query, ...)
    } else {
        r <- GET(url, H, ...)
    }
    if (http_status(r)$category == "client error") {
        tmp <- gsub("\n\\s*", "", content(r, "text"))
        x <- try(as_list(read_xml(tmp)), silent = TRUE)
        warn_for_status(r)
        h <- headers(r)
        out <- structure(x, headers = h, class = "aws_error")
        attr(out, "request_canonical") <- S$CanonicalRequest
        attr(out, "request_string_to_sign") <- S$StringToSign
        attr(out, "request_signature") <- S$SignatureHeader
    } else {
        tmp <- gsub("\n\\s*", "", content(r, "text"))
        out <- try(as_list(read_xml(tmp)), silent = TRUE)
        if (inherits(out, "try-error")) {
            out <- structure(content(r, "text"))
        }
    }
    return(out)
}
