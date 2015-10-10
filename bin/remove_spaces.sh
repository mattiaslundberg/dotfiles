#!/bin/sh

set -e

remove_spaces() {
	# Remove extraneous spaces inside parentheses
	sed --in-place "s/( /(/g" $1
	sed --in-place "s/ )/)/g" $1

	# Remove extraneous spaces inside brackets
	sed --in-place "s/\[ /\[/g" $1
	sed --in-place "s/ \]/\]/g" $1

	# Remove extraneous spaces inside braces
	sed --in-place "s/{ /{/g" $1
	sed --in-place "s/ }/}/g" $1

	# Remove trailing whitespace
	sed --in-place "s/[ \t]*$//" $1
}

if [ "$1" = "-r" ]; then
	shift
	files=`find $1 -type f`
else
	files=$@
fi

for f in $files; do
	echo "Fixing $f"
	remove_spaces $f
done
