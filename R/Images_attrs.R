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
