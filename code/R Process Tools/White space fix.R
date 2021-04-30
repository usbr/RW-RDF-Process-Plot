# if you have a \\s space that is not a " " go back into excel and fix


# white space check 
# xnodes <- unique(allCUL$Node)
# for (i in 1:length(nodes)) {
#   print(paste(nodes[i],xnodes[i]== nodes[i]))
#   # for (i in 2:4) {
# }



# str_remove(unique(allCUL$Node)," ")
# str_remove(unique(allCUL$Node),"\\s")

str_remove(x," ")  
str_remove(y,"\\s")     


str_replace_all(allCUL,unique(allCUL$Node)[1],nodes[i])     
