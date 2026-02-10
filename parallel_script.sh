#!/bin/bash -l

# Request wallclock time (format hours:minutes:seconds).
#$ -l h_rt=6:0:0

# Request 60 gigabyte of RAM (must be an integer followed by M, G, or T)
#$ -l mem=60G

# Request 1000 gigabytes of TMPDIR space per node
#$ -l tmpfs=1000G

# Request 24 threads
#$ -pe smp 24

# Set the name of the job
#$ -N en_wales_lts

# Set the working directory to somewhere in your scratch space
#$ -wd /home/ucfnpje/Scratch/enhance

# email when begin, end, aborted, or suspended
#$ -m beas

# Load the R module
module -f unload compilers mpi gcc-libs
module load r/recommended


# Copy input data and script to TMPDIR for faster access
# cp /lustre/home/ucfnpje/Scratch/enhance/output/hex_10_nodes_visited.rds $TMPDIR/hex_10_nodes_visited.rds
# cp /home/ucfnpje/Scratch/enhance/data/lts_data_no_na.gpkg $TMPDIR/lts_data_no_na.gpkg
# cp /home/ucfnpje/Scratch/enhance/script/test.R $TMPDIR/test.R

# Change working directory to TMPDIR
# cd $TMPDIR

# Run the R script and store logs
R --no-save < /home/ucfnpje/Scratch/enhance/script/lts_parallel.R > en_wales_lts.txt

# Move output files from TMPDIR back to permanent storage
# mv hex_edge.txt /home/ucfnpje/Scratch/enhance/

# Clean up TMPDIR to free space
# rm edges_extraction.R
# rm hex_10_nodes_visited.rds
# rm lts_data_no_na.gpkg
