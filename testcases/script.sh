# for FILENAME in *; do
#   awk '{ sub("\r$", ""); print }' $FILENAME > new_$FILENAME
# done

# for FILENAME in *; do
#   newname="$(echo "$FILENAME" | cut -c5-)"
#   mv "$FILENAME" "$newname"
# done
