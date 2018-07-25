#' @title Run an EC2 Instance
#' @description Run/launch a new EC2 Instance
#' @template image
#' @param type A character string specifying the type of EC2 instance to use. See <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html> for details of types and available options.
#' @param min An integer specifying a minimum number of instances to launch. Defaults to 1.
#' @param max An integer specifying a minimum number of instances to launch. Defaults to `min`.
#' @template keypair
#' @template subnet
#' @template sgroup
#' @param userdata Optionally, a character string specifying a script to run during launch.
#' @param shutdown A character string specifying either \dQuote{stop} or \dQuote{terminate}, to control the behavior of a shutdown action taken from within the instance.
#' @template token
#' @param tags A named list of tags.
#' @param spot_options Optionally, an empty `list()` to request a spot instance with API defaults, or list of spot-market options. See <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotMarketOptions.html> for available options
#' @param query_extra Optionally, additional query parameters to be passed to the RunInstances API. See <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html> for available parameters.
#' @param launch_template LaunchTemplate
#' @template dots
#' @return A list
#' @references
#' <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EC2_GetStarted.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html>
#' @examples
#' \dontrun{
#' # RStudio AMIs from: http://www.louisaslett.com/RStudio_AMI/
#' describe_images("ami-7f9dc615")
#' s <- describe_subnets()
#' g <- create_sgroup("my_security_group", "a security group", vpc = s[[1]])
#' i <- run_instances(image = "ami-7f9dc615",
#'                    type = "t2.micro",
#'                    subnet = s[[1]],
#'                    sgroup = g[[1]])
#'
#' stop_instances(i[[1]])
#' terminate_instances(i[[1]])
#' }
#' @seealso [describe_instances()], [start_instances()], [terminate_instances()]
#' @keywords instances
#' @export
run_instances <- 
function(
  image,
  type,
  min = 1,
  max = min,
  keypair,
  subnet,
  sgroup,
  userdata,
  shutdown = c("stop", "terminate"),
  token,
  tags,
  spot_options,
  query_extra = list(),
  launch_template,
  ...
) {
    query <- list(Action = "RunInstances", 
                  ImageId = get_imageid(image),
                  InstanceType = type,
                  MinCount = min,
                  MaxCount = max)
    if (!missing(keypair)) {
        query$KeyName <- get_keypairname(keypair)
    }
    if (!missing(subnet)) {
        query$SubnetId <- get_subnetid(subnet)
    }
    if (!missing(sgroup)) {
        if (inherits(sgroup, "ec2_security_group")) {
            sgroup <- list(get_sgid(sgroup))
        } else if (is.character(sgroup)) {
            sgroup <- as.list(get_sgid(sgroup))
        } else {
            sgroup <- lapply(sgroup, get_sgid)
        }
        names(sgroup) <- paste0("SecurityGroupId.", seq_along(sgroup))
        query <- c(query, sgroup)
    }
    if (!missing(userdata)) {
        query$UserData <- userdata
    }
    if (!missing(shutdown)) {
        query$InstanceInitiatedShutdownBehavior <- match.arg(shutdown)
    }
    if (!missing(token)) {
        query$ClientToken <- token
    }
    if (!missing(spot_options) && !is.null(spot_options)) {
        query$InstanceMarketOptions.MarketType <- "spot"
        if (length(spot_options)) {
            names(spot_options) <- paste0("InstanceMarketOptions.SpotOptions.",names(spot_options))
            query <- c(query, spot_options) 
        }
    }
    if (!missing(launch_template) && !is.null(launch_template)) {
        names(launch_template) <- "LaunchTemplate"
        query <- c(query, launch_template)
    }
    if (!missing(tags) && !is.null(tags)) {
        tags <- .tag_specification(resource_type = "instance", tags)
        query <- c(query, tags)
    }
    query <- c(query, query_extra)
    r <- ec2HTTP(query = query, ...)
    return(lapply(r$instancesSet, `class<-`, "ec2_instance"))
}

print.ec2_instance <- function(x, ...) {
    cat("instanceId    : ", x$instanceId[[1]], "\n")
    cat("imageId       : ", x$imageId[[1]], "\n")
    cat("instanceType  : ", x$instanceType[[1]], "\n")
    cat("instanceState : ", paste0(x$instanceState$name[[1]], " (", x$instanceState$code[[1]], ")"), "\n")
    cat("subnetId      : ", x$subnetId[[1]], "\n")
    cat("vpcId         : ", x$vpcId[[1]], "\n")
    # cat("ipAddress: ", x$ipAddress, "\n")
    invisible(x)
}
