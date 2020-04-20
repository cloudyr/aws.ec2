#' @rdname placement_groups
#' @title Placement Groups
#' @description EC2 Cluster \dQuote{Placement Groups}
#' @param group A character string specifying a placement group.
#' @param strategy A character string specifying \dQuote{cluster}. No other values are currently allowed.
#' @template filter
#' @template dots
#' @return For `describe_placements`, a list of objects of class \dQuote{ec2_placement_group}. Otherwise, a logical.
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using_cluster_computing.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribePlacementGroups.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreatePlacementGroup.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeletePlacementGroup.html>
#' @examples
#' \dontrun{
#' pg <- create_placement("examplepg")
#' describe_placements()
#' delete_placement("examplepg")
#' }
#' @export
create_placement <- function(group, strategy = "cluster", ...) {
    query <- list(Action = "CreatePlacementGroup", 
                  GroupName = group,
                  Strategy = strategy)
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1L]] == "true") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' @rdname placement_groups
#' @export
delete_placement <- function(group, ...) {
    query <- list(Action = "DeletePlacementGroup", 
                  GroupName = get_pgname(group))
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' @rdname placement_groups
#' @export
describe_placements <- function(group = NULL, filter = NULL, ...) {
    query <- list(Action = "DescribePlacementGroups")
    if (!is.null(group)) {
        if (inherits(group, "ec2_image")) {
            group <- list(get_pgname(group))
        } else if (is.character(group)) {
            group <- as.list(get_pgname(group))
        } else {
            group <- lapply(group, get_pgname)
        }
        names(group) <- paste0("GroupName.", 1:length(group))
        query <- c(query, group)
    }
    if (!is.null(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(unname(lapply(r$placementGroupSet, function(z) {
        structure(flatten_list(z), class = "ec2_placement_group")
    })))
}
