#!/bin/sh
echo "Running mirror..."
 
RSYNC="/usr/local/bin/rsync"
 
SRC="/Users/channing"
DST="10.0.1.3::Mirror"
 
EXCLUDES="/Users/channing/bin/mirror.excludes"
  
# rsync options
# -v increase verbosity
# -a turns on archive mode (recursive copy + retain attributes)
# -x don't cross device boundaries (ignore mounted volumes)
# -E preserve executability
# -S handle spare files efficiently
# --delete delete deletes any files that have been deleted locally
# --exclude-from reference a list of files to exclude
 
echo "Start rsync $(date)"
 
$RSYNC -vaxEzq  -S --timeout=3600 --progress --human-readable --force --delete-excluded --delete --exclude-from=$EXCLUDES "$SRC" "$DST"
 
echo "End rsync $(date)"
  
exit 0
