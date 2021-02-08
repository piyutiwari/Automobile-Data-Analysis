# Intial_Setup.R
# Creates the intial directpru structure
# 
# Args :
#       No Arguments required
# Returns :
#       Nothing returns
#       

# Create Directory

dir.create("data")
dir.create("code")

download.file("http://www.fueleconomy.gov/feg/epadata/vehicles.csv.zip",destfile = "data/vehicles.csv.zip")
unzip("data/vehicles.csv.zip",exdir = "data")
