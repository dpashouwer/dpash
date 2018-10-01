# Load packages - first pacman, installing if necessary, then others
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(devtools, readxl, janitor, tidyverse, tidymodels, lubridate) # add more here as needed

if(!require("dpash")) devtools::install_github("dpashouwer/dpash", force = TRUE); p_load(dpash)


url <- "https://tntp.me/fresno/wp-json/frm/v2/forms/8/"
my_username <- "tntpformidableuser"
my_password <- "fXJ4kSnWXmfEXTE4KVMuRPIO"

dpash::pull_formidable_entries(url, my_username, my_password)
