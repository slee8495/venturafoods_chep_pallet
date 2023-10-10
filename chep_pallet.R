library(tidyverse)
library(magrittr)
library(openxlsx)
library(readxl)
library(writexl)
library(reshape2)
library(skimr)
library(janitor)
library(lubridate)


##################################################################################################################################################################
gtl_raw <- read_csv("C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 24/CHEP Pallet/GTL.5018077265_20230812_6100788687_20230813_124450.csv")
as400_raw <- read_csv("C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 24/CHEP Pallet/EF065565.csv")

##################################################################################################################################################################


# gtl (CHEP data) clean
gtl_raw[-1:-13, ] -> gtl
colnames(gtl) <- gtl[1, ]

gtl %>% 
  dplyr::slice(-1) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(dplyr::across(contains("date"), ~as.Date(.x, format="%d.%m.%Y"))) -> gtl


# as400 data clean (This could be little different with the real raw data)
colnames(as400_raw) <- as400_raw[1, ]
as400_raw %>% 
  dplyr::slice(-1) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(dplyr::across(contains("date"), ~as.Date(.x, format="%m/%d/%Y"))) -> as400


##################################################################################################################################################################







