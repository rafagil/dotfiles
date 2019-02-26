#!/bin/bash

echo
echo "**************** Backup starting at `date` ****************"

echo "WARNING: Skipping SoF and CM"
#/Users/channing/bin/backupSoF.sh
/Users/channing/bin/backupRepos.sh
/Users/channing/bin/mirror.sh

echo
echo "---------------- Backup complete at `date` ----------------"
