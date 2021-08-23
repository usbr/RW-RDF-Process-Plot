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
               "Jun9005" = "Jun9005,ISM1988_2019,2016Dems,IG_DCP,CRMMS_Most",
               "Jun9004" = "Jun9004,ISM1988_2019,2016Dems,IG_DCP,CRMMS_Most"#,
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

