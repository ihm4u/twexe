#!/bin/sh

#
# Update the version in the index.html file
#

DIR=`dirname "$1"`
VERSION=`cat $DIR/rel/VERSION`

if [ -z "$1" ]; then
   echo Please specify wiki file to update.
   exit 1;
fi
perl -0777 -pi -e 's#(<div.*?title="Version">\s*<pre>).*?(</pre>\s*</div>)#${1}'$VERSION'${2}#s' "$1"
echo Updated "$1" to version $VERSION.
