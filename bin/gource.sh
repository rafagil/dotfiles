#!/bin/zsh
 
# For Macs, get gource with HomeBrew:
#   brew install gource
#   brew install ffmpeg
 
if (( !($# == 3) ))
then
    echo "Usage:"
    echo $0 "<gravatar directory> <output file base name> <seconds per day>"
    exit 1
fi
 
gravatar_dir=$1
output_file=$2
seconds_per_day=$3
 
git log --pretty=format:"%ae|%an" | sort | uniq | while read -r line
do parts=("${(s/|/)line}")
   
   username=$parts[2]
   gravatar_file=$gravatar_dir/${username}.jpg
   
   if [[ ! (-f $gravatar_file) ]]
   then email=$parts[1]
        email_hash=`md5 -qs $email`
        curl --output $gravatar_dir/${username}.jpg http://www.gravatar.com/avatar/${email_hash}.jpg
   fi
done
 
gource --user-image-dir $gravatar_dir --seconds-per-day $seconds_per_day --hide files,filenames,dirnames,usernames -1280x720 -o - | ffmpeg -y -r 60 -f image2pipe -vcodec ppm -i - -vcodec libx264 -preset ultrafast -pix_fmt yuv420p -crf 1 -threads 0 -bf 0 ${output_file}.mp4

