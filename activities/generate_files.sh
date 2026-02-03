#!/bin/bash

# Create a new directory and an inner directory
folder_name="random_files"
inner_folder="$folder_name/secret_folder"
mkdir -p "$inner_folder"

# List of sample words and phrases for the outer folder
phrases=("apple" "banana" "cat" "dog" "sunshine" "moonlight" "rainbow" "programming" "code" "bash" "keyboard" "laptop" "coffee" "travel" "mountain" "ocean" "sky" "happy" "learning" "adventure")

# Secret phrase to be split across files
secret_phrase="This is the secret phrase hidden inside text files"

# Split the secret phrase into individual words
secret_words=($secret_phrase)

# Loop to create 20 files in the outer folder
for i in {1..20}; do
    # Randomly pick a phrase for the outer folder files
    random_phrase="${phrases[$RANDOM % ${#phrases[@]}]}"
    
    # Generate a file with the random phrase in the outer folder
    echo "$random_phrase" > "$folder_name/file_$i.txt"
done

# Distribute the secret phrase into separate text files in the inner folder
for i in "${!secret_words[@]}"; do
    # Create a file in the inner folder and add part of the secret phrase
    echo "${secret_words[$i]}" > "$inner_folder/secret_part_$((i+1)).txt"
done

echo "20 files created in '$folder_name', and the secret phrase has been distributed in '$inner_folder'."
