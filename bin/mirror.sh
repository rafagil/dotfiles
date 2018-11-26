#!/bin/sh
echo "Running mirror..."
 
RSYNC="/usr/local/bin/rsync"
 
SRC="/Users/channing/"
DST="10.0.1.3::Mirror/channing"
 
FILTER="/Users/channing/bin/mirror.filter"
  
# rsync options
# -v increase verbosity
# -a turns on archive mode (recursive copy + retain attributes)
# -x don't cross device boundaries (ignore mounted volumes)
# -E preserve executability
# -S handle spare files efficiently
# --delete deletes any files that have been deleted locally
# --exclude-from reference a list of files to exclude
 
echo "Start rsync $(date)"
 
$RSYNC -vaxEz -S --timeout=3600 --progress --human-readable --force --delete-excluded --delete --verbose --prune-empty-dirs --include-from='/Users/channing/bin/mirror.include'  "$SRC" "$DST"
 
echo "End rsync $(date)"
  
exit 0
