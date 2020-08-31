library(xml2)
library(dplyr)
library(stringr)
dit_atts <- read_xml("Attributes.xml")

# get all children
sim_objs <- xml_children(dit_atts)

df <- data.frame()

# loop through all children
for (i in seq_along(sim_objs)) {
  # get object name
  obj_name <- sim_objs[i] %>% xml_attrs()
  obj_name <- unname(obj_name[[1]])
  
  tmp_df <- data.frame(object = obj_name, stringsAsFactors = FALSE)
  
  # get object children (attributes)
  obj_atts <- sim_objs[i] %>% xml_children()
  if (length(obj_atts) > 0) {
    # get the attributes out and get them out of list
    tmp_atts <- data.frame(
      t(simplify2array(xml_attrs(obj_atts))), 
      stringsAsFactors = FALSE
    )
    
    # now convert to a data frame where the columns are the attribute key and
    # rows are the values
    t2 <- matrix(tmp_atts$value, nrow = 1)
    colnames(t2) <- tmp_atts$name
    t2 <- data.frame(t2, stringsAsFactors = FALSE)
    
    # and combine with other df
    tmp_df <- cbind(tmp_df, t2)
  }
  
  df <- bind_rows(df, tmp_df)
}

# df now has all objects and all possible attributes
# need to filter off objects that have no attributes (reaches)

cc <- colnames(df)
cc <- cc[cc != "object"]

df <- df %>% 
  filter_at(vars(cc), any_vars(!is.na(.)))

# now split on ':' to seperate object from water user
tmp <- str_split(df$object, ":", n = 2, simplify = TRUE)
df$agg_div <- tmp[,1]
df$water_user <- tmp[,2]

cc <- colnames(df)
cc <- cc[cc != "object"]

##########
View(df)

# need to filter off agg div 
df2 <- df %>% 
  filter_at(vars("Node"), any_vars(!is.na(.)))
lookup_div <- df2[,c("Node","Tributary","agg_div")]

# need to filter off WUs
df3 <- df %>% 
  filter_at(vars("Sector"), any_vars(!is.na(.)))
lookup_wu <- df3[,c("object","Depletion.Rate","Sector","State","Tribe" )]

