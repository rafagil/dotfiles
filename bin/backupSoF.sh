#!/bin/bash
echo "Backup starting at `date`"

export PATH=/usr/local/share/python/:/sbin:/usr/local/bin/:$PATH
export HOME_DIR=/Users/channing
export DOCUMENTS=$HOME_DIR/Documents
export ARCHIVE=$DOCUMENTS/Archive/Code

#www.stateofflow.com
echo "Dumping stateofflow database"
ssh raedan@www.stateofflow.com 'rm stateofflow.sql.bz2; mysqldump --host=mysql.positive-internet.com -u raedan -pjelly177 raedan > stateofflow.sql; bzip2 -9 stateofflow.sql'
echo "Rsyncing www.stateofflow.com"
rsync -avz --exclude 'cache*' -e ssh raedan@www.stateofflow.com: $ARCHIVE/www.stateofflow.com
