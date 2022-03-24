library(CRSSIO)
# ?crssi_change_nf_start_date() 
# ?changeStartDate() #old function name?

crssi_change_nf_start_date(nTrace = 107,folder = "C:/Users/cfelletter/Documents/crss.offc/dmi/NFSinput_2012",start_year = 2020)

# recordToUse = full record
crssi_create_dnf_files(iFile = "C:/Users/cfelletter/Documents/natflowsaltmodel/results/NaturalFlows1906-2017_3.18.2019.xlsx",oFolder = "C:/Users/cfelletter/Documents/natflowsalt working/2017/CRSSIO",startYear = 2020, endYear = 2060,oFiles = nf_file_names(), recordToUse = NA, overwriteFiles = FALSE)

?crssi_create_dnf_files

library(zoo)

# recordToUse = stress test  
crssi_create_dnf_files(iFile = "C:/Users/cfelletter/Documents/natflowsaltmodel/results/NaturalFlows1906-2017_3.18.2019.xlsx",oFolder = "C:/Users/cfelletter/Documents/natflowsalt working/2017/CRSSIO",startYear = 2020, endYear = 2040,oFiles = nf_file_names(), recordToUse = c(as.yearmon("1988-01"),as.yearmon("2017-12")), overwriteFiles = TRUE)

# recordToUse = 1931-2016 
crssi_create_dnf_files(nTrace = 86,iFile = "C:/Users/cfelletter/Downloads/NaturalFlows1906-2016_withExtensions_9.28.2018.xlsx",oFolder = "C:/Users/cfelletter/Documents/natflowsalt working/2016/19312016",startYear = 2020, endYear = 2060,oFiles = nf_file_names(), recordToUse = c(as.yearmon("1931-01"),as.yearmon("2016-12")), overwriteFiles = TRUE)



#salt files 
crssi_change_nf_start_date(nTrace = 113,folder = "C:/Users/cfelletter/Documents/natflowsalt working/2018 NFS/Salt/1906-2018 reg 89-18 oldBLM/RWfiles - 2021 start",start_year = 2021)
