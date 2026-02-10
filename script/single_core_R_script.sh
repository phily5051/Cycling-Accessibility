#!/bin/bash -l

# Request wallclock time (format hours:minutes:seconds).
#$ -l h_rt=3:0:0

# Request maximum 160 gigabyte of RAM (must be an integer followed by M, G, or T)
#$ -l mem=160G

# Request TEPDIR space
#$ -l tmpfs=1500G

# Set the name of the job
#$ -N nx_setup

# Set the working directory to somewhere in your scratch space
#$ -wd /myriadfs/home/ucfnpje/Scratch/enhance

# email when begin, end, aborted, or suspended
#$ -m beas

# Load the R module
module -f unload compilers mpi gcc-libs
module load r/recommended


# Copy input data and script to TMPDIR for faster access
#cp /lustre/home/ucfnpje/Scratch/enhance/output/hex_10_nodes_visited.rds $TMPDIR/hex_10_nodes_visited.rds
#cp /home/ucfnpje/Scratch/enhance/data/lts_data_no_na.gpkg $TMPDIR/lts_data_no_na.gpkg
#cp /home/ucfnpje/Scratch/enhance/script/centrality.R $TMPDIR/centrality.R

# Change working directory to TMPDIR
#cd $TMPDIR

# Run the R script and store logs
R --no-save < /myriadfs/home/ucfnpje/Scratch/enhance/script/network_setup.R > nx_setup.txt

# Move output files from TMPDIR back to permanent storage
#mv centrality.txt /home/ucfnpje/Scratch/enhance/

# Clean up TMPDIR to free space
#rm centrality.R
#rm hex_10_nodes_visited.rds
#rm lts_data_no_na.gpkg


