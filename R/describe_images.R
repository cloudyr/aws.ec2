#' @title Describe AMI(s)
#' @description Search/Describe AMI(s)
#' @template image
#' @template filter
#' @param availableto Scopes the images by users with explicit launch
#'   permissions. Specify an AWS account ID, `"self"` (the sender of the
#'   request), or `"all"`` (public AMIs).
#' @param owner Filters the images by the owner. Specify an AWS account ID,
#'   `"self"` (owner is the sender of the request), or an AWS owner alias (valid
#'   values are `"amazon"` | `"aws-marketplace"` | `"microsoft"`). Omitting this
#'   option returns all images for which you have launch permissions, regardless
#'   of ownership.
#' @template dots
#' @return A list
#' @examples
#' \dontrun{
#' # RStudio AMIs from: http://www.louisaslett.com/RStudio_AMI/
#' describe_images("ami-7f9dc615")
#'
#' # Amazon Linux AMI from: http://aws.amazon.com/amazon-linux-ami/
#' describe_images("ami-08111162")
#' }
#' @keywords images
#' @references
#' https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeImages.html
#'
#' @export
describe_images <- function(image, filter, availableto, owner, ...) {
    query <- list(Action = "DescribeImages")
    if (!missing(availableto)) {
        availableto <- as.list(availableto)
        names(availableto) <- paste0("ExecutableBy.", seq_along(availableto))
        query <- c(query, availableto)
    }
    if (!missing(owner)) {
        owner <- as.list(owner)
        names(owner) <- paste0("Owner.", seq_along(owner))
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
    cat("Public?", if (x$isPublic[[1]] == "true") "TRUE" else "FALSE", "\n")
    invisible(x)
}
