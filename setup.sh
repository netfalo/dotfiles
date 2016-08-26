#!/bin/bash

echo "Creating symbolic links in ${HOME}"

for FILE in $(find files -type f | sed 's/files\///g'); do
    ln -s "$(pwd)/files/$FILE" "${HOME}/.$FILE"
done