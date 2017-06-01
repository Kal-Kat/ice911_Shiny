#!/bin/sh

unzip fwdfwfiles1.zip
unzip fwdfwfiles2.zip 
unzip fwdfwfiles3.zip 
unzip fwdfwfiles4.zip
unzip fwdfwfiles5.zip
# add newline at the end of each .txt file lines
sed -i '' -e '$a\' 0*.txt
# combine .txt files into a .csv file
cat 0*.txt > sd_data.csv
