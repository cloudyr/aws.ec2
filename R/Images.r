#' @title Copy AMI
#' @description Create a copy of an AMI
#' @param name A character string specifying a name for the new AMI
#' @template image
#' @param description A character string containing a description of the AMI.
#' @param region A character string specifying the AWS region to create the AMI in. The default is \dQuote{us-east-1}.
#' @template token
#' @template dots
#' @return A list.
#' @examples
#' \dontrun{
#' copy_image("example-rstudio-ami", "ami-7f9dc615", "This is a description")
#' 
#' }
#' @export
copy_image <- function(name, image, description, region = getOption("AWS_DEFAULT_REGION", "us-east-1"), token, ...) {
    query <- list(Action = "CopyImage", 
                  Name = name,
                  SourceImageId = get_imageid(image),
                  SourceRegion = region)
    if (!missing(description)) {
        query$Description <- description
    } 
    if (!missing(token)) {
        query$ClientToken <- token
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

create_image <- function(name, instance, region, description, noreboot, mapping, ...) {
    query <- list(Action = "CreateImage", 
                  Name = name,
                  InstanceId = instance,
                  SourceRegion = region)
    if (!missing(description)) {
        query$Description <- description
    }
    if (!missing(noreboot)) {
        query$NoReboot <- tolower(as.character(noreboot))
    }
    if (!missing(mapping)) {
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
                  ImageId = get_imageid(image))
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname image_attrs
#' @title AMI Attributes
#' @description Get, set, and reset AMI attributes
#' @template image
#' @param attribute A character string specifying one of: \dQuote{description}, \dQuote{kernel}, \dQuote{ramdisk}, \dQuote{launchPermission}, \dQuote{productCodes}, \dQuote{blockDeviceMapping}, \dQuote{sriovNetSupport}
#' @param value \dots
#' @param description \dots
#' @param operationtype \dots
#' @param launchpermission \dots
#' @param usergroup \dots
#' @param userid \dots
#' @template dots
#' @return A list
#' @examples
#' \dontrun{
#' # RStudio AMIs from: http://www.louisaslett.com/RStudio_AMI/
#' get_image_attr("ami-7f9dc615", "description")
#' }
#' @export
get_image_attr <- function(image, attribute, ...) {
    val <- c("description", "kernel", "ramdisk", "launchPermission", "productCodes", "blockDeviceMapping", "sriovNetSupport")
    if (!attribute %in% val) {
        stop(paste0("'attribute' must be one of: ", paste(val, sep = ", ")))
    }
    query <- list(Action = "DescribeImageAttribute", 
                  ImageId = get_imageid(image),
                  Attribute = attribute)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname image_attrs
#' @export
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
                  ImageId = get_imageid(image))
    
    # NEED TO HANDLE THIS
    stop("This function is not currently implemented.")
    
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname image_attrs
#' @export
reset_image_attr <- function(image, attribute, ...) {
    query <- list(Action = "ResetImageAttribute", 
                  ImageId = get_imageid(image),
                  Attribute = attribute)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @title Describe AMI(s)
#' @description Search/Describe AMI(s)
#' @template image
#' @template filter
#' @param availableto \dots
#' @param owner \dots
#' @template dots
#' @return A list
#' @examples
#' \dontrun{
#' # RStudio AMIs from: http://www.louisaslett.com/RStudio_AMI/
#' describe_images("ami-7f9dc615")
#' }
#' @export
describe_images <- function(image, filter, availableto, owner, ...) {
    query <- list(Action = "DescribeImages")
    if (!missing(availableto)) {
        avialableto <- as.list(avialableto)
        names(avialableto) <- paste0("ExecutableBy.", 1:length(avialableto))
        query <- c(query, avialableto)
    }
    if (!missing(owner)) {
        owner <- as.list(owner)
        names(owner) <- paste0("Owner.", 1:length(owner))
        query <- c(query, owner)
    }
    if (!missing(image)) {
        if (inherits(image, "ec2_image")) {
            image <- list(get_imageid(image))
        } else if (is.character(image)) {
            image <- as.list(get_imageid(image))
        } else {
            image <- lapply(image, get_imageid)
        }
        names(image) <- paste0("ImageId.", 1:length(image))
        query <- c(query, image)
    }
    if (!missing(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(setNames(lapply(r$imagesSet, `class<-`, "ec2_image"), NULL))
}

print.ec2_image <- function(x, ...) {
    cat("imageId:      ", x$imageId[[1]], "\n")
    cat("name:         ", x$name[[1]], "\n")
    cat("creationDate: ", x$creationDate[[1]], "\n")
    cat("description:  ", strwrap(x$description[[1]], width = 72, prefix = "  "), sep = "\n")
    cat("Public?", if(x$isPublic[[1]] == "true") "TRUE" else "FALSE", "\n")
    invisible(x)
}

get_imageid <- function(x) {
    if (inherits(x, "ec2_image")) {
        return(x$imageId[[1]])
    } else if (is.character(x)) {
        return(x)
    } 
}
