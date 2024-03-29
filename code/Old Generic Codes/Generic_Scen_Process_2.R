# ##############################################################################
# #This script processes annual and monthly rdf files to compare two CRSS runs
# 
# #DEVELOPMENT IS ON GOING ON THIS
# 
#   Created by C. Felletter 8/2018
#   Updated by CF on 10/2018 to be a function
#   Built to process multiple slots in the same rdf by CF on 6/15/2020
# ##############################################################################


generic_scen_process_2 <- function(scen_dir,scens,timestep,same_rdf) { 
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1. Set Up ##
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #Additional plotting functions and libraries 
  library('tidyverse') #ggplot2,dplyr,tidyr
  library('devtools')
  library(RWDataPlyr)
  source('code/Stat_emp_ExcCrv.r')
  source('code/stat-boxplot-custom.r')
  library('scales') #need this for scale_x_date()
  
  # # some sanity checks that UI is correct:
  # if(!(mainScenGroup %in% names(scens))) 
  #   stop(mainScenGroup, ' is not found in scens.')
  # 
  # # check folders
  # if(!file.exists(file.path(scen_dir, scens[1])) 
  #    | !file.exists(file.path(scen_dir, scens[2])))
  #   stop('scens folder(s) do not exist or scen_dir is set up incorrectly. 
  #        Please ensure scens is set correctly.')
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3. Process Results 
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if(timestep == "annual" | timestep == "monthly"){
    
    for (i in 1:length(same_rdf)) {
      rdffile <- rdffiles[same_rdf[i]] 
      variable <- variables[same_rdf[i]] 
      floworpe <- floworpes[same_rdf[i]] 
      cyorwy <- cyorwys[same_rdf[i]] 
      timestep <- timesteps[same_rdf[i]] 
      
      
      if (floworpe == "pe"){
        if (cyorwy == "cy"){
          peperiod = "eocy"
        } else {
          peperiod = "eowy"
        }
      }
      
      df <- data.frame(
        file = rdffile,
        slot = variable, 
        period = 
          if(timestep == "annual"){
            if (floworpe == "flow"){
              cyorwy
            } else {peperiod}
          } else {"asis"}
        ,  
        summary = 
          if(timestep == "annual"){
            if (floworpe == "flow"){
              "sum"
            } else {NA}
          } else {NA}
        ,
        eval = NA,
        t_s = NA,
        variable = variable,
        stringsAsFactors = FALSE)
      
      if (i == 1) {
        df2 <- df
      } else {
        df2 <- rbind.data.frame(df2,df)
      }
      
      
    }
    
    rwa1 <- rwd_agg(df2)
    
    # message(paste("Processing ",timesteps[same_rdf]," ",variables[same_rdf]," in ",names(scens[1])," & ",names(scens[2])))
    
    #rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
    scen_res <- rw_scen_aggregate(
      scens,
      agg = rwa1,
      scen_dir = scen_dir
    ) 
    
    # unique(scen_res$Variable) #check variable names
    # unique(scen_res$TraceNumber) #check trace numbers 
    
    if (timestep == "monthly"){
      #get everything on a date 
      scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
      ######## use YrMon in Zoo library and x scale YrMon object #########
      #get a numeric month number
      scen_res$MonthNum = as.numeric(format.Date(scen_res$MonthNum, format = "%m"))
    }
    
    if (model == "MTOM"){
      test <- scen_res %>% 
        # filter out Most,Min,Max
        dplyr::filter(
          (Scenario == names(scens[1]) && TraceNumber >= first_ensemble[1]) |
            (Scenario == names(scens[2]) && TraceNumber >= first_ensemble[2])
        ) 
      
      message(paste('Filtering out trace',first_ensemble[1],'from',names(scens[1]),"and",first_ensemble[2],'from',names(scens[2])))
    }
    
    return(scen_res)
    
  } #end of ann/mon process 
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3. Process Results - Daily ----- NOT TESTED
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  else if (timestep == "daily"){
    

    title <- variable

    #Special rw scen agg since RWDataPlyr doesn't support daily
    for (i in 1:length(scens)) {

      scen_res_x <- file.path(scen_dir,scens[i],rdffile) %>% #this means "pipe" the data to the next function
        rdf_to_rwtbl2() %>%
        filter(ObjectSlot == variable)

      #add on Scenario since rdf_to_rwtbl2 doesn't add it
      scen_res_x <- cbind.data.frame(
        scen_res_x,
        Scenario = rep(names(scens)[i], Times = length(scen_res_x$Timestep))
      )

      #convert Timestep chr to POSIXct
      scen_res_x$Timestep <- as.POSIXct(strptime(scen_res_x$Timestep,"%Y-%m-%d %H:%M"))
      scen_res_x$Timestep <- as.Date(scen_res_x$Timestep)
      #first entry is 2019-1-31 24:00 which gets converted to 2019-02-01, is that okay?????

      if(i == 1){
        scen_res = scen_res_x
      } else {
        scen_res = rbind.data.frame(scen_res,scen_res_x)
      }

    } #close i Scenario loop

    # unique(scen_res$ObjectSlot) #check variable names
    # unique(scen_res$Scenario) #check Scenario names

    #get everything on a date
    scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
    #get a numeric month number
    scen_res$MonthNum = as.numeric(format.Date(scen_res$Timestep, format = "%m"))
    scen_res$DayNum = as.numeric(format.Date(scen_res$Timestep, format = "%d"))

    if (model == "MTOM"){
      test <- scen_res %>%
        # filter out Most,Min,Max
        dplyr::filter(
          (Scenario == names(scens[1]) && TraceNumber >= first_ensemble[1]) |
            (Scenario == names(scens[2]) && TraceNumber >= first_ensemble[2])
        )
      message(paste('Filtering out trace',first_ensemble[1],'from',names(scens[1]),"and",first_ensemble[2],'from',names(scens[2])))
    }

    return(scen_res)

  } else {
    stop(paste0("Process type ",timestep," not supported"))
  }
  
} #end function



