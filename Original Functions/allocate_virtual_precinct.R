allocate_virtual_precinct <- function(x, id_col, county_name = NA, county_stem = NA, ev_name){

# A function to reallocate votes cast to early/virtual districts to remaining districts in a county
# Created by Peter R. Licari. Inspired by code by Brian Amos. Produced for Voting and Elections Science Team (VEST)  
  
  
# Accepts datafames where each column is the number of votes for an electoral outcome (e.g., number of democratic votes for governor,
# number of people voting yes on an amendment, etc) and each row is a precinct. 
# 
# Returns a dataframes where each precinct vote number in the specified counties is adjusted in proportion to that precinct's
# overall vote share for that particular race within its county. 
  
# x: the dataframe in the aforementioned format (columns are specific race outcomes, row are specific precincts)
# id_col: the name of the column with the precinct identifiers. 
# county_name: the string, or vector of strings, containing the name(s) of the count(y/ies) to be affected. 
# county_stem: If the dataframe does not have a dedicated column for county names, but the Precinct IDs have stems that ID the county
# (for example "SEM0001" and "ORA0001" for "Seminole" and "Orange" counties, respectively), you can insert the stems
# ("SEM" and "ORA") into county_stem instead.
#  ev_name: String, or vector of strings, of the early vote / virtual precinct(s). 
  

  
##########################################  
##########################################    

# Sets an empty dataframe (important when adjusting multiple counties at once)    
  a4<-data.frame()

# Determining if it is using county name or stem  
if(is.na(county_name)){
  county_identification <- county_stem
  } 
  else if(is.na(county_stem)){
  county_identification <- county_name
  
} 
  else if (is.na(county_stem) & is.na(county_name)){
  stop("Error: Both county_stem and county_name cannot be NA")
}

# Going through multiple counties     
for (cntystm in county_identification){

  #filters out county currently being adjusted from the main dataframe. 
  a <-x %>% 
    filter(grepl(cntystm,x[,c(id_col)])) 
  #main dataframe sans county that is being adjusted
  x <- x %>%
    filter(!(grepl(cntystm,x[,c(id_col)])))
  

# %in% helps if there are multiple precincts in ev_name--and if there are more than 1 per county  
  a2<- a %>%
  filter(!(a[,c(id_col)] %in% c(ev_name)))



#Iterative loop to tackle multiple early/virtual precincts in single county    
  for (ev in ev_name) {
# The "if" speeds up performance if you're dealing with multiple counties    
    if(ev %in% a[,c(id_col)]){
      #Just precinct ids for that county (sans those identified as virtual or early)
      a3 <- a2 %>%
        select(id_col) 
 #Just the particular early/virtual precinct for this loop. (Helps with doing multiple contests at once.)
     a1 <- a %>%
    filter(grepl(ev,a[,c(id_col)]))
  

b<-a2
#Where the magic happens...

#Iterates through every race and applies the amount from the virtual/early precinct to each precinct in proportion
#to said precincts' proportion of the overall share for the race. This is then rounded to the nearest whole integer.
#The sum of the rounded integers added are compared to the original total. The difference is then taken (or added)
#to the precinct with the largest total vote share. (This is to minimize overall error). Future iterations of this 
#program may work to ensure that the error is not larger than the total apportioned for the largest precinct. 
    for (i in 2:ncol(a)) {
      b <- a2 %>%
      select(c(id_col),i) %>%
        mutate(b1  = round((b[,2]/sum(b[,2]))*as.numeric(a1[,2]))) %>%
        arrange(desc(.[[2]]))
      b[2]<-b[2]+b["b1"]
      error = sum(b["b1"])-a1[,2]
      b[1,2]<-b[1,2]-error
      b<-b[1:2]
      a3 <- merge(x=a3, y=b, by = id_col)
      a3
    }
  a2<-a3
  a2
  
  

    }else{next} 
    
    


  }
  #For when dealing with multiple counties (and/or multiple early vote within them). 
  a4<-rbind(a4,a2)
  a4
} 
  x<-rbind(x,a4)
  x   



}
  
