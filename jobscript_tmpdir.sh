#!/bin/bash -l

# Request wallclock time (format hours:minutes:seconds).
#$ -l h_rt=1:0:0

# Request 10 gigabyte of RAM (must be an integer followed by M, G, or T)
#$ -l mem=10G

# Request 100 gigabytes of TMPDIR space per node
#$ -l tmpfs=100G

# Set the name of the job
#$ -N package_test

# Set the working directory to somewhere in your scratch space
#$ -wd /home/ucfnpje/Scratch/enhance

# email when begin, end, aborted, or suspended
#$ -m beas

# Load the R module
module -f unload compilers mpi gcc-libs
module load r/recommended

# Copy input data and script to TMPDIR for faster access
cp /lustre/home/ucfnpje/Scratch/enhance/data/pois.rds $TMPDIR/pois.rds
cp /home/ucfnpje/Scratch/enhance/script/test.R $TMPDIR/test.R

# Change working directory to TMPDIR
cd $TMPDIR

# Run the R script and store logs
R --no-save < test.R > test.txt

# Move output files from TMPDIR back to permanent storage
mv test.txt /home/ucfnpje/Scratch/enhance/

# Clean up TMPDIR to free space
rm test.R
rm pois.rds

# Run 'dos2unix' before submitting the jobscript