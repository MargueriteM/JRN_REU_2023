# Code to import and work with Biomet USJo1 data files

# set working directory to location where data is saved
setwd("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/Ameriflux_USJo1/Biomet2_20201227")

# import data files one by one, use header = TRUE to assign first row as column names
biomet2010 <- read.csv("Biomet_USJo1_wide_2010_.csv", header=TRUE)
biomet2011 <- read.csv("Biomet_USJo1_wide_2011_.csv", header=TRUE)

# combine multiple files
biomet.10.11 <- rbind(biomet2010, biomet2010)

# read all files and combine them together
biometfiles <- list.files(path="/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Bahada/Tower/Ameriflux_USJo1/Biomet2_20201227", full.names=TRUE) 
# read files and bind them into one file. fill=TRUE because of the missing columns in 2011
biomet_all <- do.call("rbind", lapply(biometfiles, header = TRUE, fread, sep=","))
