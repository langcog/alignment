#!/bin/bash 
#imb4.sbatch, a sample slurm job  
#
#all commands that start with SBATCH contain commands that are just used by SLURM for scheduling  
#################
#set a job name  
#SBATCH --job-name=childes_wordbank50
#################  
#a file for job output, you can check job progress
#SBATCH --output=childes_wordbank50.out
#################
# a file for errors from the job
#SBATCH --error=childes_wordbank50.errors
#################
#time you think you need; default is one hour
#in minutes in this case
#SBATCH --time=04:00:00
#################
#quality of service; think of it as job priority
#SBATCH --qos=normal
#################
#number of nodes you are requesting
#SBATCH --nodes=1
#################
#memory per node; default is 4000 MB per CPU
#SBATCH --mem=4000
#you could use --mem-per-cpu; they mean what we are calling cores
#################
#task to run per node; each node has 16 cores, "task" is "core" really
#SBATCH --ntasks-per-node=1
#don't use --cpus-per-task on this cluster
#################
#get emailed about job BEGIN, END, and FAIL
#SBATCH --mail-type=ALL
#################
#who to send email to; please change to your email
#SBATCH  --mail-user=gdoyle@stanford.edu
#################
#now run normal batch commands
./childes_wordbank50.sh