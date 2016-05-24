#' @rdname create_ami
#' @title Create AMI
#' @description Copy an AMI or create AMI from instance
#' @template instance
#' @param name A character string specifying a name for the new AMI
#' @template image
#' @param description A character string containing a description of the AMI.
#' @param region A character string specifying the AWS region to create the AMI in. The default is \dQuote{us-east-1}.
#' @param mapping \dots
#' @param noreboot A logical. From the AWS documentation: \dQuote{By default, this parameter is set to false, which means Amazon EC2 attempts to shut down the instance cleanly before image creation and then reboots the instance. When the parameter is set to true, Amazon EC2 doesn't shut down the instance before creating the image. When this option is used, file system integrity on the created image can't be guaranteed.}
#' @template token
#' @template dots
#' @return A list.
#' @references
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CopyImage.html}
#' \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateImage.html}
#' @examples
#' \dontrun{
#' copy_image("example-rstudio-ami", "ami-7f9dc615", "This is a description")
#' 
#' }
#' @keywords images
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

#' @rdname create_ami
create_image <- function(instance, name, description, 
                         region = getOption("AWS_DEFAULT_REGION", "us-east-1"), 
                         mapping, noreboot = FALSE, ...) {
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

# \url{http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami.html}
# \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RegisterImage.html}
#' @keywords images
register_image <- function() {
    
    # NEED TO DO THIS
    
}

deregister_image <- function(image, ...) {
    query <- list(Action = "DeregisterImage", 
                  ImageId = get_imageid(image))
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
#' @seealso \url{http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeImages.html}
#' @examples
#' \dontrun{
#' # RStudio AMIs from: http://www.louisaslett.com/RStudio_AMI/
#' describe_images("ami-7f9dc615")
#' 
#' # Amazon Linux AMI from: http://aws.amazon.com/amazon-linux-ami/
#' describe_images("ami-08111162")
#' }
#' @keywords images
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
    return(unname(lapply(r$imagesSet, function(z) {
        structure(flatten_list(z), class = "ec2_image")
    })))
}

print.ec2_image <- function(x, ...) {
    cat("imageId:      ", x$imageId[[1]], "\n")
    cat("name:         ", x$name[[1]], "\n")
    cat("creationDate: ", x$creationDate[[1]], "\n")
    cat("description:  ", strwrap(x$description[[1]], width = 72, prefix = "  "), sep = "\n")
    cat("Public?", if(x$isPublic[[1]] == "true") "TRUE" else "FALSE", "\n")
    invisible(x)
}
