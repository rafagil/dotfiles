#!/bin/bash

echo
echo "**************** Backup starting at `date` ****************"

/Users/channing/bin/backupRepos.sh
/Users/channing/bin/mirror.sh

echo
echo "---------------- Backup complete at `date` ----------------"
