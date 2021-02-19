setwd("C:/Users/cfelletter/Documents/Tools/Kens24MSVerificationCode")

a= read.csv("24MS MRIDs 08-Nov19.csv", header = F)

paste(a, sep="_", collapse=",")



#### list of cell names to convert to excel cell formula ##### 
setwd("C:/Users/cfelletter/Documents/natflowsalt working/PR 26 Update")

a= read.csv("CellNames.csv", header = F)

paste(t(a), sep="_", collapse="+")


