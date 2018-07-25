#' @rdname ebs
#' @title Elastic Block Store Volumes
#' @description Manage Elastic Block Store (EBS) volumes for EC2
#' @template volume
#' @param size Size of the volume in gigabytes. See API documentation for allowed values.
#' @param type The volume type. Defaults to "Standard".
#' @param iops The number of I/O operations per second (IOPS) to provision for the volume, ranging from 100 to 32000.
#' @template instance
#' @template snapshot
#' @param device \dots
#' @param attribute \dots
#' @param auto_enable \dots
#' @param force \dots
#' @template filter
#' @template dots
#' @return \dots
#' @references
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateVolume.html>
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteVolume.html>
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyVolume.html>
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_AttachVolume.html>
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DetachVolume.html>
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeVolumes.html>
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeVolumeStatus.html>
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeVolumeAttribute.html>
#' <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyVolumeAttribute.html>
#' @seealso [run_instances()], [create_snapshot()]
#' @export
create_volume <-
function(
  size = NULL,
  type = NULL,
  iops = NULL,
  snapshot = NULL,
  ...
) {
    query <- list(
      Action = "CreateVolume"
    )
    if (!is.null(snapshot)) {
        query$SnapshotId <- snapshot
    } else if (!is.null(size)) {
        query$Size <- size
    }
    if (!is.null(type)) {
        query$VolumeType <- match.arg(type, c("standard", "io1", "gp2", "sc1", "st1"))
    }
    if (!is.null(iops)) {
        query$Iops <- iops
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname ebs
#' @export
modify_volume <-
function(
  volume,
  size = NULL,
  type = NULL,
  iops = NULL,
  ...
) {
    query <- list(Action = "ModifyVolume")
    query$VolumeId <- get_volumeid(volume)
    if (!is.null(type)) {
        query$VolumeType <- match.arg(type, c("standard", "io1", "gp2", "sc1", "st1"))
    }
    if (!is.null(size)) {
        query$Size <- size
    }
    if (!is.null(iops)) {
        query$Iops <- iops
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname ebs
#' @export
delete_volume <-
function(
  volume,
  ...
) {
    query <- list(Action = "DeleteVolume")
    query$VolumeId <- get_volumeid(volume)
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname ebs
#' @export
attach_volume <-
function(
  volume,
  instance,
  device,
  ...
) {
    query <- list(
        Action = "AttachVolume",
        VolumeId = get_volumeid(volume),
        InstanceId = get_instanceid(instance),
        Device = device
    )
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname ebs
#' @export
detach_volume <-
function(
  volume,
  instance,
  device = NULL,
  force = FALSE,
  ...
) {
    query <- list(
        Action = "DetachVolume",
        VolumeId = get_volumeid(volume),
        InstanceId = get_instanceid(instance)
    )
    if (!is.null(device)) {
        query$Device <- device
    }
    if (isTRUE(force)) {
        query$Force <- TRUE
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname ebs
#' @export
enable_volume_io <-
function(
  volume,
  ...
) {
    query <- list(
        Action = "EnableVolumeIO",
        VolumeId = get_volumeid(volume)
    )
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname ebs
#' @export
get_volume_attr <-
function(
  volume,
  attribute = c("autoEnableIO", "productCodes"),
  ...
) {
    query <- list(
        Action = "DescribeVolumeAttribute",
        VolumeId = get_volumeid(volume),
        Attribute = match.arg(attribute)
    )
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname ebs
#' @export
set_volume_attr <-
function(
  volume,
  auto_enable,
  ...
) {
    query <- list(
        Action = "ModifyVolumeAttribute",
        VolumeId = get_volumeid(volume),
        AutoEnableIO = auto_enable
    )
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname ebs
#' @export
describe_volumes <-
function(
  volume = NULL,
  filter = NULL,
  ...
) {
    query <- list(Action = "DescribeVolumes")
    if (!is.null(volume)) {
        if (inherits(volume, "ec2_image")) {
            volume <- list(get_volumeid(volume))
        } else if (is.character(volume)) {
            volume <- as.list(get_volumeid(volume))
        } else {
            volume <- lapply(volume, get_imageid)
        }
        names(volume) <- paste0("VolumeId.", 1:length(volume))
        query <- c(query, volume)
    }
    if (!is.null(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname ebs
#' @export
volume_status <-
function(
  volume = NULL,
  filter = NULL,
  ...
) {
    query <- list(Action = "DescribeVolumeStatus")
    if (!is.null(volume)) {
        if (inherits(volume, "ec2_image")) {
            volume <- list(get_volumeid(volume))
        } else if (is.character(volume)) {
            volume <- as.list(get_volumeid(volume))
        } else {
            volume <- lapply(volume, get_imageid)
        }
        names(volume) <- paste0("VolumeId.", 1:length(volume))
        query <- c(query, volume)
    }
    if (!is.null(filter)) {
        query <- c(query, .makelist(filter, type = "Filter"))
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}
