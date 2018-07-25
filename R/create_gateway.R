#' @rdname gateway_create
#' @title Create/Delete Gateways
#' @description Create and delete Network Gateways for a VPC
#' @template gateway
#' @template dots
#' @references
#' <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateInternetGateway.html>
#' <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteInternetGateway.html>
#' @seealso [create_gateway()], [describe_gateways()]
#' @keywords security
#' @export
create_gateway <- function(...) {
    query <- list(Action = "CreateInternetGateway")
    r <- ec2HTTP(query = query, ...)
    return(structure(flatten_list(r$internetGateway), class = "ec2_internet_gateway", requestId = r$requestId))
}

#' @rdname gateway_create
#' @export
delete_gateway <- function(gateway, ...) {
    query <- list(Action = "DeleteInternetGateway",
                  InternetGatewayId = gateway)
    r <- ec2HTTP(query = query, ...)
    if (r$return[[1]] == "true") {
        return(TRUE)
    } else { 
        return(FALSE)
    }
}
