#### how to get depletions information ########

library(xml2)
# 
Attributes <- read_xml(x="C:/Users/cfelletter/Documents/CRSS working/DemandUseVerificationRun/Attributes.xml")
source("get_demand_atts.R") 
summary(df)


# cowu <- rdf_to_rwtbl2(file.path(results_dir,"COWU.rdf"))
#####read in all WUs ########### 
allWU <- rbind.data.frame(rdf_to_rwtbl2(file.path(results_dir,"COWU.rdf")),
                          rdf_to_rwtbl2(file.path(results_dir,"NMWU.rdf")),
                          rdf_to_rwtbl2(file.path(results_dir,"WYWU.rdf")),
                          rdf_to_rwtbl2(file.path(results_dir,"UTWU.rdf")),
                          rdf_to_rwtbl2(file.path(results_dir,"AZWU.rdf")))

#split off agg div
tmp <- str_split(allWU$ObjectSlot, ":", n = 2, simplify = TRUE)
allWU['agg_div'] = tmp[,1]

#split off object.slot
# tmp2 <- str_split(allWU$ObjectSlot, ".", n = 2, simplify = TRUE) #doesn't work IDK why
tmp2 <- str_split(allWU$ObjectSlot, ".D", n = 2, simplify = TRUE)
#### this might be messing up some objects
# unique(tmp2[,2])
# head(tmp2)

allWU['object'] = tmp2[,1]


# test <- allWU[1:50,]
# test %>% 
#   full_join(lookup_wu, by="object")


#join on Node and Tributary Attributes
allWU <- allWU %>% 
  full_join(lookup_div, by="agg_div")

#join on WU Attributes 
allWU <- allWU %>% 
  full_join(lookup_wu, by="object")



#get everything on a date 
allWU$Date = as.Date(paste0(allWU$Year,allWU$Month,"01"), format = "%Y%B%d")
#get a numeric month number
allWU$MonthNum = as.numeric(format.Date(allWU$Date, format = "%m"))

# nodes <- sort(unique(df$Node)) #sort these better 
nodes <- c("1 Glenwood","2 Cameo","4 Blue Mesa","6 Grand Junction","7 Dolores",
           "8 CO River at Cisco","9 Fontenelle","10 Green River WY","11 Greendale",
           "12 Yampa","13 Little Snake","14 Duchesne","15 White River","16 Green River UT",
           "17 San Rafael","18 Archuleta","19 Bluff","20 Lee's Ferry")

# sectors <- unique(allWU$Sector)
onlythesesectors <- c("Agriculture","Evaporation","Energy","Exports","M & I")        #"Minerals", "Environmental","Lease" ,"Fish & Wildlife"
    



i=1
j=1


pdf(file.path(results_dir,"WUbyCPbySector.pdf"), width=9, height=6)

for (i in 1:length(nodes)) {
# for (i in 2:4) {
  
  print(paste("Node",nodes[i]))
  
  
  test <- allWU %>%
    #filter out all but one node
    dplyr::filter(Node == nodes[i])
  
  sectors <- unique(test$Sector) #only plot sectors that exisit in CP
  #limit which sectors are plotted 
  sectors = sectors[which(sectors %in% onlythesesectors)]
  
  if(length(sectors) > 0){ #have some sector of interest 
  for (j in 1:length(sectors)) {
    
    print(paste("Sector",sectors[j]))
    
    
    x <- test %>%
      #filter out all but one node
      dplyr::filter(Sector == sectors[j]) 
    
    user <- unique(x$object)
    
    ####what to do if user has multiple users? E.g. "EnergyAndMandIUsesAbvCameo:Energy"   "EnergyUsesShoshonePowerPlant:Energy"
        ##### DEV IMPROVEMENT SUM UP across sector ##########
    ##########
    ##########
    ##########
    
    if (length(user) != 1 ){
      
      for (k in 1:length(user)) {
        
        user1 = user[k]
        
        request <- paste0(user1,".Depletion Requested")
        shortage <- paste0(user1,".Depletion Shortage")
        
        requested <- x %>%
          dplyr::filter(ObjectSlot == request)
        depleted <- x %>%
          dplyr::filter(ObjectSlot == shortage)
        
        depleted$Value = requested$Value - depleted$Value   
        depleted$ObjectSlot = rep(paste0(user1,".Depletion "),times = length(depleted$ObjectSlot))
        
        xx <- rbind.data.frame(requested,depleted)
        
        #annual  
        p <- xx %>% 
          group_by(ObjectSlot,Year) %>%
          summarise(Value = sum(Value)) %>%
          # group_by(ObjectSlot)  %>%
          ggplot(aes(x = Year, y = Value, color = ObjectSlot)) +
          geom_line() +
          # geom_point() +
          labs(title = paste(nodes[i],sectors[j]), y = "Depletions (AF/yr)")
        print(p)
        
        #monthly 
        p <- xx %>%
          group_by(ObjectSlot,Date) %>%
          ggplot(aes(x = Date, y = Value, color = ObjectSlot)) +
          geom_line() +
          labs(title = paste(nodes[i],sectors[j]), y = "Depletions (AF/mo)")
        print(p)
        
      } #end if multiple users   

      
    } else {
    
    request <- paste0(user,".Depletion Requested")
    shortage <- paste0(user,".Depletion Shortage")
    
    requested <- x %>%
      dplyr::filter(ObjectSlot == request)
    depleted <- x %>%
      dplyr::filter(ObjectSlot == shortage)
    
    depleted$Value = requested$Value - depleted$Value   
    depleted$ObjectSlot = rep(paste0(user,".Depletion "),times = length(depleted$ObjectSlot))
    
    xx <- rbind.data.frame(requested,depleted)
    
    #annual  
    p <- xx %>% 
      group_by(ObjectSlot,Year) %>%
      summarise(Value = sum(Value)) %>%
      # group_by(ObjectSlot)  %>%
      ggplot(aes(x = Year, y = Value, color = ObjectSlot)) +
      geom_line() +
      # geom_point() +
      labs(title = paste(nodes[i],sectors[j]), y = "Depletions (AF/yr)")
    print(p)
    
    #monthly 
    p <- xx %>%
      group_by(ObjectSlot,Date) %>%
      ggplot(aes(x = Date, y = Value, color = ObjectSlot)) +
      geom_line() +
      labs(title = paste(nodes[i],sectors[j]), y = "Depletions (AF/mo)")
    print(p)
    
    } #end else one users 
    
  } #end if no data 

    
  } #end sector loop 
    
    


} #end node loop

dev.off()
