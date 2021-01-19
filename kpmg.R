setwd("C:/Users/SANTHOSH/Downloads")
library(readxl)
library(tidyverse)


kpmg.transaction <- read_xlsx("KPMG_VI_New_raw_data_update_final (1).xlsx",sheet = 2, skip = 1)
kpmg.newcustomer <- read_xlsx("KPMG_VI_New_raw_data_update_final (1).xlsx",sheet = 3, skip = 1)
kpmg.demographic <-read_xlsx("KPMG_VI_New_raw_data_update_final (1).xlsx",sheet = 4, skip = 1)
kpmg.cusaddress <-read_xlsx("KPMG_VI_New_raw_data_update_final (1).xlsx",sheet = 5, skip = 1)


#ADDRESS

str(kpmg.cusaddress)

summary(kpmg.cusaddress)

is.na(kpmg.cusaddress)

kpmg.cusaddress[!complete.cases(kpmg.cusaddress),]

complete.cases(kpmg.cusaddress)

#DEMOGRAPHIC

str(kpmg.demographic)

summary(kpmg.demographic)

is.na(kpmg.demographic)

kpmg.demographic[!complete.cases(kpmg.demographic),-11]

complete.cases(kpmg.cusaddress)

as.tibble(kpmg.demographic[!complete.cases(kpmg.demographic$deceased_indicator),1])

#NEW CUSTOMER

as.tibble(kpmg.newcustomer[!complete.cases(kpmg.newcustomer$address),1])

edit(kpmg.newcustomer %>% arrange(DOB))

str(kpmg.newcustomer)
summary(kpmg.newcustomer)

kpmg.newcustomer$DOB <- as.Date(kpmg.newcustomer$DOB)



