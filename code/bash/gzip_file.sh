#!/bin/bash

FILE=${1}
OUTPUT_DIR=${2:-data/derived}
GIT_ROOT=$( git rev-parse --show-toplevel )
full_path=${GIT_ROOT}/${OUTPUT_DIR}
echo $full_path
echo "Will produce gzipped file in '${full_path}'."

gzip -k ${FILE}
chmod u+w ${FILE}.gz
mv ${FILE}.gz ${full_path}
echo "Finished."

