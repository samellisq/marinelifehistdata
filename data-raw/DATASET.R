## code to prepare `DATASET` dataset goes here
## Run this to reload data from local folders. Not included in package, just for me

##https://r-pkgs.org/data.html

rm(list = ls())
require(Matrix)
require(tidyverse)

# usethis::use_data(DATASET, overwrite = TRUE)

current.wd = here::here()
local.data.wd = paste(stringr::str_split(current.wd, "Methods/", simplify = TRUE)[1], "Comparative/Projects/Menopause Evolution/Data Library/Data", sep = "")
setwd(local.data.wd)

### Data Key

# require(readxl) # will change eventually, easiest to have it as xlsx for now (freeze panes etc)
marine.lifehist.datakey = readxl::read_excel("0 Data Key v1.xlsx")
marine.lifehist.datakey = select(marine.lifehist.datakey, -notes, -from, - details.checked)

## References
bibliography = readxl::read_excel(paste(current.wd, "/data-raw/references/refs as CSV.xlsx", sep=  "")) # as excel becasue then it importas the non latin letters


## The data

marine.lifehist.data = list()
for(i in 1:nrow(marine.lifehist.datakey)){
  dataset = read_csv(paste(marine.lifehist.datakey$dataset[i], ".csv",sep = ""))

  if(ncol(dataset) == 2 &marine.lifehist.datakey$data[i] == "age-structure"){
    dataset = data.frame(ages = rep(dataset$age, dataset$n))
  }
  dataset$dataset = i
  names(dataset) = ifelse(names(dataset) == "ages", "age", names(dataset))
  marine.lifehist.data[[i]] = dataset
}
names(marine.lifehist.data) = seq(1, length(marine.lifehist.data), 1)

## Dataset extras
setwd(local.data.wd)
marine.lifehist.datasetextras = list()

biasdetails = read_csv("../Extra Data/datasets_extras.csv")
marine.lifehist.datasetextras[[1]] = biasdetails
names(marine.lifehist.datasetextras)[1] = "datasetsextras_bias.details"

pop.ids = read_csv("../Extra Data/datasets_population.identifiers.csv")
marine.lifehist.datasetextras[[2]] = pop.ids
names(marine.lifehist.datasetextras)[2] = "datasetsextras_populations.key"

### Species.level data
setwd(local.data.wd)
marine.lifehist.speciesdata = list()

#1 species names data
names.key = read_csv("../Extra Data/species_whale.names.csv")
marine.lifehist.speciesdata[[1]] = names.key
names(marine.lifehist.speciesdata)[1] = "species_names.key"

#2 ages at matruity
agemat.df = read_csv("../Extra Data/species_age.maturity.csv")
marine.lifehist.speciesdata[[2]] = agemat.df
names(marine.lifehist.speciesdata)[2] = "species_age.maturity"

#3
sizedat.raw = read_csv("../Extra Data/species_size.csv")
marine.lifehist.speciesdata[[3]] = sizedat.raw
names(marine.lifehist.speciesdata)[3] = "species_raw.sizes"

#4
lengthdat.processed = read_csv("../../Analysis/Menopause Evolution II/size_length.estimates.csv")
marine.lifehist.speciesdata[[4]] = lengthdat.processed
names(marine.lifehist.speciesdata)[4] = "species_length"

#5
weightdat.procesed = read_csv("../../Analysis/Menopause Evolution II/size_weight.estimates.csv")
marine.lifehist.speciesdata[[5]] = weightdat.procesed
names(marine.lifehist.speciesdata)[5] = "species_mass"


#### EXPORT to DATA folder
setwd(current.wd)
usethis::use_data(marine.lifehist.datakey, overwrite = TRUE)
usethis::use_data(marine.lifehist.data, overwrite = TRUE)
usethis::use_data(marine.lifehist.speciesdata, overwrite = TRUE)
usethis::use_data(bibliography, overwrite = TRUE)
usethis::use_data(marine.lifehist.datasetextras, overwrite = TRUE)
