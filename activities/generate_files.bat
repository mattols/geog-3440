@echo off
setlocal enabledelayedexpansion

:: Create a new directory and an inner directory
set folder_name=random_files
set inner_folder=%folder_name%\secret_folder

mkdir "%folder_name%"
mkdir "%inner_folder%"

:: List of sample words and phrases for the outer folder
set phrases=apple banana cat dog sunshine moonlight rainbow programming code bash keyboard laptop coffee travel mountain ocean sky happy learning adventure

:: Secret phrase to be split across files
set secret_phrase=This is the secret phrase hidden inside text files

:: Split the secret phrase into individual words
for /f "tokens=1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19" %%a in ("%secret_phrase%") do (
    set secret_words[1]=%%a
    set secret_words[2]=%%b
    set secret_words[3]=%%c
    set secret_words[4]=%%d
    set secret_words[5]=%%e
    set secret_words[6]=%%f
    set secret_words[7]=%%g
    set secret_words[8]=%%h
    set secret_words[9]=%%i
    set secret_words[10]=%%j
    set secret_words[11]=%%k
    set secret_words[12]=%%l
    set secret_words[13]=%%m
    set secret_words[14]=%%n
    set secret_words[15]=%%o
    set secret_words[16]=%%p
    set secret_words[17]=%%q
    set secret_words[18]=%%r
    set secret_words[19]=%%s
)

:: Loop to create 20 files in the outer folder
for /l %%i in (1,1,20) do (
    :: Randomly pick a phrase from the list of phrases
    set /a random_index=!random! %% 19
    for /f "tokens=%random_index%" %%x in ("%phrases%") do set random_phrase=%%x
    
    :: Create a file with the random phrase in the outer folder
    echo !random_phrase! > "%folder_name%\file_%%i.txt"
)

:: Distribute the secret phrase into separate text files in the inner folder
for /l %%i in (1,1,19) do (
    :: Create a file in the inner folder and add part of the secret phrase
    echo !secret_words[%%i]! > "%inner_folder%\secret_part_%%i.txt"
)

echo 20 files created in "%folder_name%", and the secret phrase has been distributed in "%inner_folder%"!
