#!/bin/sh
echo "Running mirror..."
 
RSYNC="/usr/local/bin/rsync"
 
SRC="/Users/channing/"
DST="10.0.1.3::Mirror/channing"
 
# rsync options
# -v increase verbosity
# -a turns on archive mode (recursive copy + retain attributes)
# -x don't cross device boundaries (ignore mounted volumes)
# -E preserve executability
# -S handle spare files efficiently
# --delete deletes any files that have been deleted locally
# --exclude-from reference a list of files to exclude
 
echo "Start rsync $(date)"
 
$RSYNC -qaxEzv -S --timeout=3600 --progress --human-readable --force --delete-excluded --delete --verbose --include-from='/Users/channing/bin/mirror.include'  "$SRC" "$DST"
 
echo "End rsync $(date)"
  
exit 0
