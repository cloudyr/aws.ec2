get_instance_attr <- function(instance, attribute, ...) {
    query <- list(Action = "DescribeInstanceAttribute", 
                  InstanceId = instance,
                  Attribute = attribute)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

set_instance_attr <- function(instance, ...) {
    query <- list(Action = "ModifyInstanceAttribute", InstanceId = instance)
    # ADD ATTRIBUTES
    r <- ec2HTTP(query = query, ...)
    return(r)
}

reset_instance_attr <- function(instance, attribute, ...) {
    query <- list(Action = "ResetInstanceAttribute", 
                  InstanceId = instance,
                  Attribute = attribute)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

get_console_output <- function(instance, ...) {
    query <- list(Action = "GetConsoleOutput", InstanceId = instance)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

get_password_data <- function() {
    query <- list(Action = "GetPasswordData", InstanceId = instance)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

run_instances <- function() {

    # NEED TO DO THIS

}

start_instances <- function(instance, info, ...) {
    query <- list(Action = "StartInstances")
    if(!missing(instance)) {
        instance <- as.list(instance)
        names(instance) <- paste0("InstanceId.", 1:length(instance))
        query <- c(query, instance)
    }
    if(!missing(info))
        query$AdditionalInfo <- info
    r <- ec2HTTP(query = query, ...)
    return(r)
}

stop_instances <- function(instance, force, ...) {
    query <- list(Action = "StopInstances")
    if(!missing(instance)) {
        instance <- as.list(instance)
        names(instance) <- paste0("InstanceId.", 1:length(instance))
        query <- c(query, instance)
    }
    if(!missing(force))
        query$Force <- tolower(as.character(force))
    r <- ec2HTTP(query = query, ...)
    return(r)
}

terminate_instances <- function(instance, ...) {
    query <- list(Action = "TerminateInstances")
    if(!missing(instance)) {
        instance <- as.list(instance)
        names(instance) <- paste0("InstanceId.", 1:length(instance))
        query <- c(query, instance)
    }
    if(!missing(dryrun))
        query$DryRun <- tolower(as.character(dryrun))
    r <- ec2HTTP(query = query, ...)
    return(r)
}

describe_instances <- function(instance, filter, n, token, ...) {
    query <- list(Action = "DescribeInstances")
    if(!missing(instance)) {
        instance <- as.list(instance)
        names(instance) <- paste0("InstanceId.", 1:length(instance))
        query <- c(query, instance)
    }
    if(!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    if(!missing(n)) {
        if(n > 1000) {
            warning("'n' coerced to 1000 (the maximum)")
            n <- 1000
        }
        query$MaxResults <- n
    }
    if(!missing(token))
        query$NextToken <- token
    r <- ec2HTTP(query = query, ...)
    return(r)
}

instance_status <- function(instance, filter, runningonly, n, token, ...) {
    query <- list(Action = "DescribeInstanceStatus")
    if(!missing(instance)) {
        instance <- as.list(instance)
        names(instance) <- paste0("InstanceId.", 1:length(instance))
        query <- c(query, instance)
    }
    if(!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    if(!missing(n)) {
        if(n > 1000) {
            warning("'n' coerced to 1000 (the maximum)")
            n <- 1000
        }
        query$MaxResults <- n
    }
    if(!missing(token))
        query$NextToken <- token
    if(!missing(runningonly))
        query$IncludeAllInstances <- !runningonly
    r <- ec2HTTP(query = query, ...)
    return(r)
}

monitor_instances <- function(instance, ...) {
    query <- list(Action = "MonitorInstances")
    if(!missing(instance)) {
        instance <- as.list(instance)
        names(instance) <- paste0("InstanceId.", 1:length(instance))
        query <- c(query, instance)
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

unmonitor_instances <- function(instance, ...) {
    query <- list(Action = "UnmonitorInstances")
    if(!missing(instance)) {
        instance <- as.list(instance)
        names(instance) <- paste0("InstanceId.", 1:length(instance))
        query <- c(query, instance)
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

reboot_instances <- function() {
    query <- list(Action = "RebootInstances")
    if(!missing(instance)) {
        instance <- as.list(instance)
        names(instance) <- paste0("InstanceId.", 1:length(instance))
        query <- c(query, instance)
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}
