create_placement <- function(group, strategy = "cluster", ...) {
    query <- list(Action = "CreatePlacementGroup", 
                  GroupName = group,
                  Strategy = strategy)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

delete_placement <- function(group, ...) {
    query <- list(Action = "DeletePlacementGroup", 
                  GroupName = group)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

describe_placements <- function(group, filter, ...) {
    query <- list(Action = "DescribePlacementGroup")
    if(!missing(group)) {
        group <- as.list(group)
        names(group) <- paste0("InstanceId.", 1:length(group))
        query <- c(query, group)
    }
    if(!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}
