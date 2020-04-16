#' @title Describe Regions
#' @description Retrieve Region Details
#' @param region Optionally a character string specifying the name of one or more AWS regions. If missing, details about all are returned.
#' @param filter \dots
#' @template dots
#' @return A list
#' @examples
#' \dontrun{
#' decribe_regions()
#' }
#' @seealso [describe_zones()]
#' @export
describe_regions <-
function(
  region = NULL,
  filter = NULL,
  ...
) {
    query <- list(Action = "DescribeRegions")
    if (!is.null(region)) {
        region <- as.list(region)
        names(region) <- paste0("RegionName.", 1:length(region))
        query <- c(query, region)
    }
    if (!is.null(filter)) {
        vfilter <- c("endpoint", "region-name")
        if (any(!names(filter) %in% vfilter)) {
            stop("'filter' must be one or more of: ", paste0(vfilter, collapse = ", "))
        }
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(unname(lapply(r$regionInfo, function(z) {
        structure(flatten_list(z), class = "ec2_region")
    })))
}
