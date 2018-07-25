#' @rdname ebs_snapshots
#' @title Elastic Block Store Snapshots
#' @description Manage Elastic Block Store (EBS) volumes for EC2
#' @template volume
#' @template snapshot
#' @param description \dots
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
#' @seealso [run_instances()], [create_volume()]
#' @export
create_snapshot <-
function(
  volume,
  description = NULL,
  ...
) {
    query <- list(
      Action = "CreateSnapshot",
      VolumeId = get_volumeid(volume)
    )
    if (!is.null(description)) {
        query$Description <- description
    }
    r <- ec2HTTP(query = query, ...)
    return(r)
}

#' @rdname ebs_snapshots
#' @export
delete_snapshot <-
function(
  snapshot,
  ...
) {
    query <- list(
      Action = "DeleteSnapshot",
      SnapshotId = get_snapshotid(snapshot)
    )
    r <- ec2HTTP(query = query, ...)
    return(r)
}

copy_snapshot <-
function(
) {
}

get_snapshot_attr <-
function(
) {
}

set_snapshot_attr <-
function(
) {
}

reset_snapshot_attr <-
function(
) {
}

describe_snapshots <-
function(
) {
}

