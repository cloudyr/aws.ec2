#' @title Account Attributes
#' @description Retrieve Account Attributes
#' @param attribute Optionally a character string specifying one or more of: \dQuote{supported-platforms}, \dQuote{vpc-max-security-groups-per-interface}, \dQuote{max-elastic-ips}, \dQuote{max-instances}, \dQuote{vpc-max-elastic-ips}, \dQuote{default-vpc}. If missing, all are returned.
#' @template dots
#' @return A list
#' @examples
#' \dontrun{
#' account_attrs()
#' }
#' @export
account_attrs <- function(attribute, ...) {
    query <- list(Action = "DescribeAccountAttributes")
    if (!missing(attribute)) {
        attribute <- as.list(attribute)
        names(attribute) <- paste0("AttributeName.", 1:length(attribute))
        query <- c(query, attribute)
    }
    r <- ec2HTTP(query = query, ...)
    out <- lapply(r$accountAttributeSet, function(z) {
        unname(unlist(z$attributeValueSet))
    })
    names(out) <- lapply(r$accountAttributeSet, function(z) z[["attributeName"]][[1]])
    return(out)
}
