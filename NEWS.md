# aws.ec2 0.1.15

* Added functions `get_instance_public_ip()` and `get_instance_private_ip()` to obtain IP addresses for one or more instances. (#41)

# aws.ec2 0.1.14

* Updated README
* Exposed `revoke_egress() and `revoke_ingress()` functions.

# aws.ec2 0.1.13

* Merge major PR (#38) from Andrie de Vries.
* Bumped **aws.signature** dependency to v0.4.0.

# aws.ec2 0.1.12

* Default to EC2 API version 2016-11-15.
* Add functionality to start spot instances (#9, h/t Peter Foley).

# aws.ec2 0.1.11

* Export `release_ip()` function (h/t Sean Davis)
* Bump **aws.signature** version to 0.3.4 to use `locate_credentials()`.
* Updated README

# aws.ec2 0.1.10

* Suggest **aws.efs**.

# aws.ec2 0.1.9

* `describe_keypairs()` now returns a named list, where names reflect keypair names. (#18, h/t Colin Gillespie)
* `create_keypair()` gains a `path` argument to save a .pem file. (#17, h/t Colin Gillespie)
* Noted that `instance` arguments can accept a single instance or vector/list of instances. (#19, h/t Colin Gillespie)
* Fixed edge case error in `describe_instances()`. (#10, h/t Peter Foley)

# aws.ec2 0.1.7

* Implemented VPCs and Network ACLs.
* Expanded documentation, especially cross-references.

# aws.ec2 0.1.6

* Several breaking API changes and substantial changes to return values of various functions to increase consistency across the package.
* Considerable expansion of documentation.

# aws.ec2 0.1.1

* Initial release.
