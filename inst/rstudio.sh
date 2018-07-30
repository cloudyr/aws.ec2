#!/bin/bash

# install R
yum install -y R

# install full awspack suite
R -e "install.packages('awspack', repos='https://cloud.r-project.org/')"

# install RStudio-Server (1.1.456)
# latest version number is in: https://download2.rstudio.org/current.ver
wget https://download2.rstudio.org/rstudio-server-rhel-1.1.456-x86_64.rpm
yum install -y --nogpgcheck rstudio-server-rhel-1.1.456-x86_64.rpm
rm rstudio-server-rhel-1.1.456-x86_64.rpm

# add user(s)
useradd ec2user
echo ec2user:ec2password | chpasswd
