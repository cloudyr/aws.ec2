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
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CopyImage.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateImage.html>
#' @examples
#' \dontrun{
#' copy_image("example-rstudio-ami", "ami-7f9dc615", "This is a description")
#' 
#' }
#' @keywords images
#' @export
create_image <- 
function(
  instance,
  name,
  description = NULL,
  region = getOption("AWS_DEFAULT_REGION", "us-east-1"),
  mapping = NULL,
  noreboot = FALSE,
  ...
) {
    query <- list(Action = "CreateImage", 
                  Name = name,
                  InstanceId = instance,
                  SourceRegion = region)
    if (!is.null(description)) {
        query$Description <- description
    }
    if (!is.null(noreboot)) {
        query$NoReboot <- tolower(as.character(noreboot))
    }
    if (!is.null(mapping)) {
        mapping <- as.list(mapping)
        names(mapping) <- paste0("BlockDeviceMapping.", 1:length(mapping))
        query <- c(query, mapping)
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname create_ami
#' @export
copy_image <-
function(
  name,
  image,
  description = NULL,
  region = getOption("AWS_DEFAULT_REGION", "us-east-1"),
  token = NULL,
  ...
) {
    query <- list(Action = "CopyImage", 
                  Name = name,
                  SourceImageId = get_imageid(image),
                  SourceRegion = region)
    if (!is.null(description)) {
        query$Description <- description
    } 
    if (!is.null(token)) {
        query$ClientToken <- token
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
