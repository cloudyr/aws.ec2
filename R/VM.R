# import

import_instance <- function() {}

import_volume <- function() {}

describe_conversion <- function(conversion, filter, ...) {
    query <- list(Action = "DescribeConversionTasks")
    if(!missing(conversion)) {
        conversion <- as.list(conversion)
        names(conversion) <- paste0("ConversionTaskId.", 1:length(conversion))
        query <- c(query, conversion)
    }
    if(!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

cancel_conversion <- function(conversion, reason, ...) {
    query <- list(Action = "CancelConversionTask", 
                  ConversionTaskId = conversion)
    if(!missing(reason))
        query$ReasonMessage <- reason
    r <- ec2HTTP(query = query, ...)
    return(r)
}


# export

create_export <- function(instance, description, targetenv, s3, ...) {
    query <- list(Action = "CreateInstanceExportTask", 
                  InstanceId = instance)
    if(!missing(description))
        query$Description <- description
    if(!missing(targetenv)) {
        venv <- c("citrix", "vmware", "microsoft")
        if(!targetenv %in% venv)
            stop("'targetenv' must be one of: ", paste0(venv, collapse = ", "))
    }
    if(!missing(s3)) {
        names(s3) <- paste0("ExportToS3.", names(s3))
        query <- c(query, s3)
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

cancel_export <- function(export, ...) {
    query <- list(Action = "CancelExportTask", 
                  ExportTaskId = export)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

describe_exports <- function(export, ...) {
    query <- list(Action = "DescribeExportTasks")
    if(!missing(export)) {
        export <- as.list(export)
        names(export) <- paste0("ExportTaskId.", 1:length(export))
        query <- c(query, export)
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}
