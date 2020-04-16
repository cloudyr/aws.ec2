#' @title Describe Zones
#' @description Retrieve Zone Details
#' @param zone Optionally a character string specifying the name of one or more AWS zones. If missing, details about all are returned.
#' @param filter \dots
#' @template dots
#' @return A list
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html>
#' @seealso [describe_regions()]
#' @export
describe_zones <-
function(
  zone = NULL,
  filter = NULL,
  ...
) {
    query <- list(Action = "DescribeAvailabilityZones")
    if (!is.null(zone)) {
        zone <- as.list(zone)
        names(zone) <- paste0("ZoneName.", 1:length(zone))
        query <- c(query, zone)
    }
    if (!is.null(filter)) {
        vfilter <- c("message", "region-name", "state", "zone-name")
        if (any(!names(filter) %in% vfilter)) {
            stop("'filter' must be one or more of: ", paste0(vfilter, collapse = ", "))
        }
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(unname(lapply(r$availabilityZoneInfo, function(z) {
        structure(flatten_list(z), class = "ec2_zone")
    })))
}
