### The main file to be called before any other file.


## Required packages
library(conflicted)
library(flextable) # Package for neat tables
library(tidyverse) # A group of packages for data analysis
library(magrittr)
library(dplyr)     # Package for data manipulation
library(ggplot2)   # Package for plots
library(ggstance)  # Package for horizontal stacked bar plots


## Loading main excel coded data into a data frame
excel_data <- read.csv("/mnt/LDisk-E/Albert Einstein/Books & Resources/Sociology/Gender Mainstreaming/Megha/stat/megha-dissertation-data-coded-data.csv")

## Loading the codebook into a dataframe
codebook <- read.csv("/mnt/LDisk-E/Albert Einstein/Books & Resources/Sociology/Gender Mainstreaming/Megha/stat/megha-dissertation-data-codebook.csv")

## Removing certain columns to only have questions and councillors

coded_data <- subset(excel_data, select = c(-X,-X.1,-X.2,-X.3,-X.4,-X.4,-X.5,-X.6,-X.7,-X.8,-Notes))

## Vector of councils
council_labels <- c("Kishangarh", "Makrana", "Tonk", "Nagaur", "Bhilwara", "Beawar")

## Vector of question codes
q_codes <- colnames(coded_data[-1])

## Subsets per council, each one of these is a data frame.

kishangarh <- coded_data[1:22,]
beawar <- coded_data[142:161,]
bhilwara <- coded_data[113:138,]
nagaur <- coded_data [62:86,]
tonk <- coded_data[92:108,]
makrana <- coded_data[35:54,]

## Adding an identifier column to each council dataframe
kishangarh$council <- "Kishangarh"
beawar$council <- "Beawar"
bhilwara$council <- "Bhilwara"
nagaur$council <- "Nagaur"
tonk$council <- "Tonk"
makrana$council <- "Makrana"

## Binding all councils
all_councils <- bind_rows(kishangarh, makrana, beawar, bhilwara, nagaur, tonk)

## Socio-economic profile

socio_economic <- q_codes[1:36]

## Dimensions of Idicators indexed from `q_codes'.
## Each one of these is a vector too.
awareness <- q_codes[37:54]
perc_gender <- q_codes[55:69]
gend_sens_considerations <- q_codes[70:78]
effect_national <-q_codes[81:102]
part_dec<- q_codes[103:166]
cap_build <- q_codes[c(79:80,202,158:160)]
critic_exp <- q_codes[167:169]
work_exp <- q_codes[c(170:174,179)]
gend_dynamics <- q_codes[175:178]
chal_pow_exec <- q_codes[180:183]
chal_family <- q_codes[184:188]
sup_field <- q_codes[189:191]
res_alloc <- q_codes[192:195]
scope_growth <- q_codes[197:206]


## This function takes two arguments:
## - council
## - indicator
## and then creates a subset data frame that contains information concerning the particular indicator. It's a wrapper around `subset' function so that we don't have to use it continuously a lot.

ind <- function(x,y) {
    c_ind <- subset(x, select = y)
    c_ind$council <- x$council[1]
    ## So that every created data frame also has an identifier of
    ## the council it creates the data frame of.
    return(c_ind)
}
