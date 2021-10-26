rm(list=ls()) #clear the enviornment

library(RWDataPlyr)

CRSSDIR <- Sys.getenv("CRSS_DIR")

# # where scenarios are folder are kept
scen_dir <- file.path(CRSSDIR,"Scenario")
# #containing the sub folders for each ensemble

results_dir <- file.path(CRSSDIR,"results") 


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. User Input ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#all scens 
scens <- list( ### don't comment these out use keepscens variabile #### 
               # "OffcAugFixed" = "Aug2021_2022,ISM1988_2019,2016Dems,IG_DCPnoUBDRO,CRMMS_Most_Fix",
               "RW8.3.1" = "Aug2021_2022_RW831,ISM1988_2019,2016Dems,IG_DCPnoUBDRO,CRMMS_Most",
               "OffcReRun" = "Aug2021_2022.9000.NvjElVol,ISM1988_2019,2016Dems,IG_DCPnoUBDRO,CRMMS_Most"
)

rdf <- 'KeySlots.rdf'
r1 <- read_rdf(file.path(scen_dir,scens[1],rdf))
r2 <- read_rdf(file.path(scen_dir,scens[2],rdf))
all.equal(r1, r2)
# its okay if it says [1] "Component “meta”: Component “owner”: 1 string mismatch"    

rdf <- 'UBRes.rdf'
r1 <- read_rdf(file.path(scen_dir,scens[1],rdf))
r2 <- read_rdf(file.path(scen_dir,scens[2],rdf))
all.equal(r1, r2)

