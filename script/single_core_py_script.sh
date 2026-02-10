#!/bin/bash -l

# Request wallclock time (format hours:minutes:seconds).
#$ -l h_rt=48:0:0

# Request 160 gigabyte of RAM
#$ -l mem=160G

# Request TEPDIR space
#$ -l tmpfs=50G

# Request 10 threads
#$ -pe smp 1

# Set the name of the job
#$ -N en_wales_poi

# Set the working directory
#$ -wd /myriadfs/home/ucfnpje/Scratch/enhance

# email when begin, end, aborted, or suspended
#$ -m beas

# Load the Python module
module load python3/recommended

# Activate the virtual environment
source ~/myenv/bin/activate


# Run the Python script
python /home/ucfnpje/Scratch/enhance/script/poi_import.py > en_wales_poi.txt

