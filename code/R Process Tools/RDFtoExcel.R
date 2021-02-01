# Input -----------------------------------

## rdf to excel exe 
### MyPC
rdf2excel <- "C:/Users/cfelletter/Documents/Tools/RdfToExcel/RdfToExcelExecutable.exe" 
### BA
rdf2excel <- "C:/Program Files/CADSWES/RdfToExcel/RdfToExcelExecutable.exe"

## file paths 
### CRSS - MTOM
folderpath <- "Z:/felletter/CRSS_MTOM_scens/Stress" # manoa
folderpath <- "C:/Users/cfelletter/Documents/CRSS_MTOM/testbedanalysis/data/Scenario" #myPC
scen <- "NF_pluvialRem_2000,2016Dems_v1.6,CRSS_VerificationModel_9015,IGDCP.v4.4.0.9004"
scen <- c("NF_pluvialRem_2000,2016Dems_v1.6,CRSS_VerificationModel_9015,IGDCP.v4.4.0.9004",
          "NF_StressTest_2000,2016Dems_v1.6,CRSS_VerificationModel_9015,IGDCP.v4.4.0.9004",
          "NF_Full_2000,2016Dems_v1.6,CRSS_VerificationModel_9015,IGDCP.v4.4.0.9004")

### if you previously ran Move and Rename
folderpath <- current.folder
### from BA CRSS results 
folderpath <- "C:/Users/fellette/Documents/GIT/CRSS/Scenario" #BA
scen <- list.files(folderpath) #only if want all scenarios 

###if you have a scens list from other code 
scen <- scens 

###always check this before proceeding 

scen 

## which rdf files 
rdf <- c('SystemConditions') #c('UBRch')
## CRSS-MTOM files
rdf <- c("SystemConditions","Res","UBRes","xtraRes")  #"UBRch.rdf"

## GREAT files
rdf <- c("DailyFlows","Res","UBRes","UBDO","Drought")  #"UBRch.rdf"

### setup files 
rdfs <- rdf #if you previously ran Move and Rename.R
xlsx <- rdfs
rdfs <- paste0(rdfs, ".rdf")
xlsx <- paste0(xlsx, ".xlsx")
rdfs
xlsx

# Run RdfToExcelExecutable -----------------------------------

stopifnot(file.exists(rdf2excel))

for(j in 1:length(scen)){
  
  # j=1
  fpath <- file.path(folderpath,scen[j])
  message("Starting: ", scen[j])
  
  for (i in seq_along(rdfs)) {
    ifile <- file.path(fpath, rdfs[i])
    message("Starting: ", rdfs[i])
    
    if (!file.exists(ifile)) {
      message(ifile, "\nDoes not exist.")
    } else {
      ofile <- file.path(fpath, xlsx[i])
      
      cmd <- c("-i", ifile, "-o", ofile)
      
      system2(rdf2excel, args = cmd)
    }
  }
}
# TODO: parse the log file. Delete it if it ends in 
# "RdfToExcel: Workbook write successfully completed", otherwise keep it.
