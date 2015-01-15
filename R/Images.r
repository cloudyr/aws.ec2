copy_image <- function(name, image, region, description, token, ...) {
    query <- list(Action = "CopyImage", 
                  Name = name,
                  SourceImageId = image,
                  SourceRegion = region)
    if(!missing(description))
        query$Description <- description
    if(!missing(token))
        query$ClientToken <- token
    r <- ec2HTTP(query = query, ...)
    return(r)
}

create_image <- function(name, instance, description, noreboot, mapping, ...) {
    query <- list(Action = "CreateImage", 
                  Name = name,
                  InstanceId = instance,
                  SourceRegion = region)
    if(!missing(description))
        query$Description <- description
    if(!missing(noreboot))
        query$NoReboot <- tolower(as.character(noreboot))
    if(!missing(mapping)) {
        mapping <- as.list(mapping)
        names(mapping) <- paste0("BlockDeviceMapping.", 1:length(mapping))
        query <- c(query, mapping)
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

register_image <- function() {
    
    # NEED TO DO THIS
    
}

deregister_image <- function(image, ...) {
    query <- list(Action = "DeregisterImage", 
                  ImageId = image)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

get_image_attr <- function(image, attribute, ...) {
    query <- list(Action = "DescribeImageAttribute", 
                  ImageId = image,
                  Attribute = attribute)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

set_image_attr <- 
function(image, 
         attribute, 
         value,
         description, 
         operationtype, 
         launchpermission, 
         usergroup,
         userid, ...) {
    query <- list(Action = "DescribeImageAttribute", 
                  ImageId = image)
    
    # NEED TO HANDLE THIS
    
    
    r <- ec2HTTP(query = query, ...)
    return(r)
}

reset_image_attr <- function() {
    query <- list(Action = "ResetImageAttribute", 
                  ImageId = image,
                  Attribute = attribute)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

describe_images <- function(image, filter, availableto, owner, ...) {
    query <- list(Action = "DescribeImages")
    if(!missing(avialableto)) {
        avialableto <- as.list(avialableto)
        names(avialableto) <- paste0("ExecutableBy.", 1:length(avialableto))
        query <- c(query, avialableto)
    }
    if(!missing(owner)) {
        owner <- as.list(owner)
        names(owner) <- paste0("Owner.", 1:length(owner))
        query <- c(query, owner)
    }
    if(!missing(image)) {
        image <- as.list(image)
        names(image) <- paste0("ImageId.", 1:length(image))
        query <- c(query, image)
    }
    if(!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}
