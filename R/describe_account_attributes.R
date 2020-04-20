#' @title Describe Account Attributes
#' @description Retrieve Account Attributes
#' @param attribute_names A character vector specifying the attributes to
#'   include. One or more of \code{supported_platforms}, \code{default_vpc},
#'   \code{vpc_max_security_groups_per_interface}, \code{max_elastic_ips}, or
#'   \code{vpc_max_elastic_ips}.
#' @template dots
#' @return A list of the requested attributes.
#' @references
#' \href{https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAccountAttributes.html}{API
#' Reference: DescribeAccountAttributes}
#' @examples
#' \dontrun{
#' describe_account_attributes()
#' }
#' @export
describe_account_attributes <- function(attribute_names = c(
  "supported_platforms", 
  "default_vpc",
  "vpc_max_security_groups_per_interface", 
  "max_elastic_ips",
  "vpc_max_elastic_ips"), ...) {
  query <- list(Action = "DescribeAccountAttributes")
  
  # For argument matching, we want the "_" form.
  attribute_names <- .make_syntactic(attribute_names)
  if ("max_instances" %in% attribute_names) {
    alert_func <- ifelse(
      length(attribute_names) > 1,
      warning,
      stop
    )
    alert_func("max_instances is no longer supported")
    attribute_names <- setdiff(attribute_names, "max_instances")
  }
  attribute_names <- match.arg(attribute_names, several.ok = TRUE)

  # For the call, we need the "-" form.
  attribute_names <- .match_api_parameters(attribute_names)
  attribute_names <- as.list(attribute_names)
  names(attribute_names) <- paste0("AttributeName.", 1:length(attribute_names))
  query <- c(query, attribute_names)
  
  r <- ec2HTTP(query = query, ...)
  out <- lapply(r$accountAttributeSet, function(z) {
    unname(unlist(z$attributeValueSet))
  })
  names(out) <- lapply(
    r$accountAttributeSet,
    function(z) z[["attributeName"]][[1]]
  )
  names(out) <- .make_syntactic(names(out))
  return(out)
}
