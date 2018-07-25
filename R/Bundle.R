bundle_instance <-
function(
  instance,
  bucket,
  ...
) {
    query <- list(Action = "BundleInstance", 
                  InstanceId = instance,
                  Storage = bucket)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

cancel_bundle <-
function(
  bundle,
  ...
) {
    query <- list(
      Action = "CancelBundleTask",
      BundleId = bundle
    )
    r <- ec2HTTP(query = query, ...)
    return(r)
}

describe_bundle <-
function(
  bundle = NULL,
  filter = NULL,
  ...
) {
    query <- list(Action = "DescribeBundleTask")
    if (!is.null(bundle)) {
        bundle <- as.list(bundle)
        names(bundle) <- paste0("BundleId.", 1:length(bundle))
        query <- c(query, bundle)
    }
    if (!is.null(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

