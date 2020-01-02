#!/bin/sh
echo "Running mirror..."
 
RSYNC="/usr/local/bin/rsync"
 
SRC="/Users/channing/"
DST="192.168.1.2::Mirror/channing"

echo "Checking for unfriendly files ..."
find ${SRC}Documents -name '*[<>:"/\\|?*]*'
find ${SRC}Dropbox -name '*[<>:"/\\|?*]*'
find ${SRC}Pictures -name '*[<>:"/\\|?*]*'

# rsync options
# -v increase verbosity
# -a turns on archive mode (recursive copy + retain attributes)
# -x don't cross device boundaries (ignore mounted volumes)
# -E preserve executability
# -S handle spare files efficiently
# --delete deletes any files that have been deleted locally
# --exclude-from reference a list of files to exclude
 
echo "Start rsync $(date)"
 
$RSYNC -axEzq -S --timeout=3600 --human-readable --force --delete-excluded --delete --prune-empty-dirs --include-from='/Users/channing/bin/mirror.include' --exclude="*" "$SRC" "$DST"
 
echo "End rsync $(date)"
  
exit 0
