library(tidyverse)
library(stringi)
library(stringr)
library(data.table)
library(foreign)
library(broom)
library(sf)
full<-data.frame()

cty.names<-c("ALA","BAY", "BRA", "BRE", "CAL","CHA","CIT","CLA","CLL", "IND",
             "CLM","DAD","DES","DIX","DUV","ESC","FLA","FRA","GAD", "GLA","GUL",
             "HAM","HEN","HER","HIG","HIL","HOL","JAC","JEF","LAF","LAK","LEE",
             "LEO","LEV","LIB","MAD","MAN","MRN","MRT","NAS","OKA","OKE","ORA",
             "OSC","PAL","PAS","PIN","POL","SAN","SAR","STJ","STL","SUM",
             "SUW","TAY","VOL","WAK","WAL","WAS","PUT","HAR","BAK","SEM","BRO","GIL")


for(cty in cty.names){
  file <- paste("C:/Users/prlic/Downloads/precinctlevelelectionresults2018gen/",
                cty,"_PctResults20181106.txt",sep = "") 
  cty<-read.csv2(file,sep = "\t",header = F, na.strings = " ", colClasses = c("V13"="character",
                                                                              "V16"="character")) 
  cty <- cty %>%
    # Rename the base variables
    rename(county.code = V1, name = V2, el.number = V3, el.date = V4,
           el.name = V5, prec.id = V6, prec.loc = V7, tot.reg = V8,
           tot.reg.rep = V9, tot.reg.dem = V10, tot.reg.oth = V11,
           contest = V12, district = V13, contest.code = V14, candidate= V15,
           cand.pty.code = V16, cand.vtr.id = V17, doe.num = V18, total.votes = V19)%>%
    # Create a precinct ID based on county and precinct name.
    mutate(prec.id2 = paste(county.code,prec.id,sep=""),
           contest = tolower(contest),
           prec.id2 = case_when(
             str_length(prec.id) == 1 ~ paste(county.code,"000",prec.id,sep = ""),
             str_length(prec.id) == 2 ~ paste(county.code,"00",prec.id,sep = ""),
             str_length(prec.id) == 3 ~ paste(county.code,"0",prec.id,sep = ""),
             str_length(prec.id) >= 4 ~ paste(county.code, prec.id,sep = "")
           )) %>%
    # Remove statewide offices and blank precincts
    filter(!(candidate == "OverVotes" | candidate == "UnderVotes" | candidate == "WriteinVotes"),
           !(prec.id==0), !(contest == "united states senator" | contest == "governor"|
                             contest == "attorney general" | contest == "chief financial officer" | contest == "commissioner of agriculture" |
                             grepl("amend",contest)))

  
  full <- rbind(full,cty) 
}

#UNI 

uni<-read.csv2("C:/Users/prlic/Downloads/precinctlevelelectionresults2018gen/UNI_PctResults20181106.txt",sep = "\t",header = F, na.strings = " ", colClasses = c("V13"="character",
                                                                                                                                                                 "V16"="character")) 
uni1<- uni %>%
  rename(county.code = V1, name = V2, el.number = V3, el.date = V4,
         el.name = V5, prec.id = V6, prec.loc = V7, tot.reg = V8,
         tot.reg.rep = V9, tot.reg.dem = V10, tot.reg.oth = V11,
         contest = V12, district = V13, contest.code = V14, candidate= V15,
         cand.pty.code = V16, cand.vtr.id = V17, doe.num = V18, total.votes = V19) %>%
  mutate(prec.id = ifelse(grepl("&",prec.id, fixed = T),substr(prec.id,1,2),as.character(prec.id)))

cty <- uni1

cty <- cty %>%
  mutate(prec.id2 = paste(county.code,prec.id,sep=""),
         contest = tolower(contest),
         prec.id2 = case_when(
           str_length(prec.id) == 1 ~ paste(county.code,"000",prec.id,sep = ""),
           str_length(prec.id) == 2 ~ paste(county.code,"00",prec.id,sep = ""),
           str_length(prec.id) == 3 ~ paste(county.code,"0",prec.id,sep = ""),
           str_length(prec.id) >= 4 ~ paste(county.code, prec.id,sep = "")
         )) %>%
  filter(!(candidate == "OverVotes" | candidate == "UnderVotes" | candidate == "WriteinVotes"),
         !(prec.id==0), !(contest == "united states senator" | contest == "governor"|
                           contest == "attorney general" | contest == "chief financial officer" | contest == "commissioner of agriculture" |
                           grepl("amend",contest))) 

full <- rbind(full,cty)

#For Monroe and Washington, the counties sent a different precinct ID for the dbf than they sent
#to the state. I had to manually adjust these.I chose to change state data rather than dbf.

#Monroe 
mon<-read.csv2("C:/Users/prlic/Downloads/precinctlevelelectionresults2018gen/MON_PctResults20181106.txt",sep = "\t",header = F, na.strings = " ", colClasses = c("V13"="character",
                                                                                                                                                                 "V16"="character")) 
mon1a<-mon
mon2<-data.frame()
i<-0
mon.prec.state<-c(1,2,3,4,5,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,28,29,30,31,32,35,36,37,38,41)
mon.prec.dbf  <-c(1,2,3,4,5,6,7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33)

#Manual adjust function
for(y in mon.prec.state){
  i <- i+1
  mon1<-mon1a %>%
    mutate(V6 = ifelse(V6 == y,mon.prec.dbf[i],V6))%>%
    filter(V6 == mon.prec.dbf[i])
  mon1a<-mon1a %>%
    filter(!(V6==y))
  mon2<-rbind(mon2,mon1)
}


cty <- mon2

cty <- cty %>%
  rename(county.code = V1, name = V2, el.number = V3, el.date = V4,
         el.name = V5, prec.id = V6, prec.loc = V7, tot.reg = V8,
         tot.reg.rep = V9, tot.reg.dem = V10, tot.reg.oth = V11,
         contest = V12, district = V13, contest.code = V14, candidate= V15,
         cand.pty.code = V16, cand.vtr.id = V17, doe.num = V18, total.votes = V19)%>%
  mutate(prec.id2 = paste(county.code,prec.id,sep=""),
         contest = tolower(contest),
         prec.id2 = case_when(
           str_length(prec.id) == 1 ~ paste(county.code,"000",prec.id,sep = ""),
           str_length(prec.id) == 2 ~ paste(county.code,"00",prec.id,sep = ""),
           str_length(prec.id) == 3 ~ paste(county.code,"0",prec.id,sep = ""),
           str_length(prec.id) >= 4 ~ paste(county.code, prec.id,sep = "")
         )) %>%
  filter(!(candidate == "OverVotes" | candidate == "UnderVotes" | candidate == "WriteinVotes"),
         !(prec.id==0), !(contest == "united states senator" | contest == "governor"|
                           contest == "attorney general" | contest == "chief financial officer" | contest == "commissioner of agriculture" |
                           grepl("amend",contest))) 


full <- rbind(full,cty)

#Washington County

was<-read.csv2("C:/Users/prlic/Downloads/precinctlevelelectionresults2018gen/WAS_PctResults20181106.txt",sep = "\t",header = F, na.strings = " ", colClasses = c("V13"="character",
                                                                                                                                                                 "V16"="character")) 
was1a<-was
was2<-data.frame()
i<-0
was.prec.state<- c(1,2,3,4,5,6,7,8,9,10,11,12)
was.prec.dbf   <- c(1,2,3,4,5,6,7,8,9,11,12,15)


for(y in was.prec.state){
  i <- i+1
  was1<-was1a %>%
    mutate(V6 = ifelse(V6 == y,was.prec.dbf[i],V6))%>%
    filter(V6 == was.prec.dbf[i])
  was1a<-was1a %>%
    filter(!(V6==y))
  was2<-rbind(was2,was1)
}


cty <- was2

cty <- cty %>%
  rename(county.code = V1, name = V2, el.number = V3, el.date = V4,
         el.name = V5, prec.id = V6, prec.loc = V7, tot.reg = V8,
         tot.reg.rep = V9, tot.reg.dem = V10, tot.reg.oth = V11,
         contest = V12, district = V13, contest.code = V14, candidate= V15,
         cand.pty.code = V16, cand.vtr.id = V17, doe.num = V18, total.votes = V19)%>%
  mutate(prec.id2 = paste(county.code,prec.id,sep=""),
         contest = tolower(contest),
         prec.id2 = case_when(
           str_length(prec.id) == 1 ~ paste(county.code,"000",prec.id,sep = ""),
           str_length(prec.id) == 2 ~ paste(county.code,"00",prec.id,sep = ""),
           str_length(prec.id) == 3 ~ paste(county.code,"0",prec.id,sep = ""),
           str_length(prec.id) >= 4 ~ paste(county.code, prec.id,sep = "")
         )) %>%
  filter(!(candidate == "OverVotes" | candidate == "UnderVotes" | candidate == "WriteinVotes"),
         !(prec.id==0), !(contest == "united states senator" | contest == "governor"|
                           contest == "attorney general" | contest == "chief financial officer" | contest == "commissioner of agriculture" |
                           grepl("amend",contest))) 
full <- rbind(full,cty)

full <- full %>%
  mutate(prec.id2 = ifelse(prec.id2 == "CAL201/201C","CAL0201",paste(prec.id2)))





### Lower State House

#Read in shapefile that dealt with split precincts.
FL.2018.lh <- st_read("H:/Research/2019 Research/TampaBayTimes/2018_Precincts/2018LowerHousePrecinctGeneral/2018LowerHousePrecinctGeneral.shp")

full.lower.state<- full %>%
  #Dealing with state data
  filter(grepl("state rep",contest)) %>%
  group_by(prec.id2, contest, district, cand.pty.code,candidate,tot.reg) %>%
  summarise(total.votes = sum(total.votes)) %>%
  #renaming contest to be able to make a merge variable
  #A bit superfluous but it works.
  mutate(contest1 = ifelse((contest == "state representative"),paste("state house",district,sep = ""),paste(contest)),
         contest1 = ifelse((contest == "state senator"),paste("state senate",district,sep = ""),paste(contest1)),
         contest1 = ifelse((contest == 'representative in congress'),paste("congressional",district,sep = ""),paste(contest1))) %>%
  ungroup() %>%
  #Converting to wide from long
  dcast(prec.id2 + contest1 +tot.reg~ cand.pty.code, value.var = "total.votes", fun.aggregate=sum ) %>%
  #Creation of merge variable
  mutate(prec.id3 = paste(prec.id2,str_to_lower(contest1),sep=""),
         prec.id3 = stri_replace_all_fixed(prec.id3," ",""))


FL.2018.lh <- FL.2018.lh %>%
  #Creation of merge variable in shapefile
  mutate(prec.id3 = paste(Pct_std, str_to_lower(NAMELSAD),sep = ""),
         prec.id3 = str_replace_all(prec.id3, fixed(" "),"")) 

#Merge
FL2018LowerHouse <- sp::merge(x= FL.2018.lh, y =full.lower.state, by = "prec.id3", all.x = T)

#Dealing
FL2018LowerHouse <- FL2018LowerHouse %>% 
  replace(., is.na(.),-9)%>%
  mutate(district = str_sub(GEOID,3,6)) %>%
  select(-c(STATEFP, INTPTLAT, INTPTLON, SLDLST, NAMELSAD, LSY, MTFCC, FUNCSTAT, ALAND, AWATER, Pct_std, prec.id3,prec.id2,contest1, GEOID)) %>%
  rename(G18SLHREP = REP,
         G18SLHDEM = DEM,
         G18SLHGRE = GRE,
         G18SLHLPF = LPF,
         G18SLHNPA = NPA)

#SLH = State Lower House

st_write(FL2018LowerHouse, "H:/Research/2019 Research/TampaBayTimes/2018_Precincts/2018LowerHousePrecinctGeneral/FL2018GenLowerHouse.shp" )


### State Senate
FL.2018.uh <- st_read("H:/Research/2019 Research/TampaBayTimes/2018_Precincts/2018UpperHousePrecinctGeneral/2018UpperHousePrecinctMergeGeneral.shp")

full.upper.state<- full %>%
  filter(grepl("state sen",contest)) %>%
  group_by(prec.id2, contest, district, cand.pty.code,candidate,tot.reg) %>%
  summarise(total.votes = sum(total.votes)) %>%
  mutate(contest1 = ifelse((contest == "state representative"),paste("state house",district,sep = ""),paste(contest)),
         contest1 = ifelse((contest == "state senator"),paste("state senate",district,sep = ""),paste(contest1)),
         contest1 = ifelse((contest == 'representative in congress'),paste("congressional",district,sep = ""),paste(contest1))) %>%
  ungroup() %>%
  dcast(prec.id2 + contest1 +tot.reg~ cand.pty.code, value.var = "total.votes", fun.aggregate=sum ) %>%
  mutate(prec.id3 = paste(prec.id2,str_to_lower(contest1),sep=""),
         prec.id3 = stri_replace_all_fixed(prec.id3," ",""))


FL.2018.uh <- FL.2018.uh %>%
  mutate(prec.id3 = paste(Pct_std, str_to_lower(NAMELSAD_2),sep = ""),
         prec.id3 = str_replace_all(prec.id3, fixed(" "),"")) 

FL2018UpperHouse <- sp::merge(x= FL.2018.uh, y =full.upper.state, by = "prec.id3", all.x = T)
FL2018UpperHouse <- FL2018UpperHouse %>% 
  replace(., is.na(.),-9)%>%
  select(c( County, Precinct,  District = SLDUST, tot.reg, G18SUHDEM = DEM, G18SUHREP = REP, G18SUHNPA = NPA, G18SUHLPF = LPF, geometry ))

#SUH = State Upper House
  

st_write(FL2018UpperHouse, "H:/Research/2019 Research/TampaBayTimes/2018_Precincts/2018UpperHousePrecinctGeneral/FL2018GenUpperHouse.shp" )


### Federal House

FL.2018.fh <- st_read("H:/Research/2019 Research/TampaBayTimes/2018_Precincts/2018FederalHousePrecinctGeneral/2018FederalHousePrecinctGeneral.shp")

full.lower.federal<- full %>%
  filter(grepl("congress",contest)) %>%
  group_by(prec.id2, contest, district, cand.pty.code,candidate,tot.reg) %>%
  summarise(total.votes = sum(total.votes)) %>%
  mutate(contest1 = ifelse((contest == "state representative"),paste("state house",district,sep = ""),paste(contest)),
         contest1 = ifelse((contest == "state senator"),paste("state senate",district,sep = ""),paste(contest1)),
         contest1 = ifelse((contest == 'representative in congress'),paste("congressional",district,sep = ""),paste(contest1))) %>%
  ungroup() %>%
  dcast(prec.id2 + contest1 +tot.reg~ cand.pty.code, value.var = "total.votes", fun.aggregate=sum ) %>%
  mutate(prec.id3 = paste(prec.id2,str_to_lower(contest1),sep=""),
         prec.id3 = stri_replace_all_fixed(prec.id3," ",""))


FL.2018.fh <- FL.2018.fh %>%
  mutate(prec.id3 = paste(Pct_std, "congressionaldistrict", str_to_lower(as.character(DISTRICT)),sep = ""),
         prec.id3 = str_replace_all(prec.id3, fixed(" "),"")) 

FL2018FederalHouse <- sp::merge(x= FL.2018.fh, y =full.lower.federal, by = "prec.id3", all.x = T)
FL2018FederalHouse <- FL2018FederalHouse %>% 
  select(c(County, Precinct, District = DISTRICT, tot.reg, G18USHDEM = DEM, G18USHREP = REP, G18USHNPA = NPA, geometry)) %>%
  replace(., is.na(.), -9)

#USH = US House


st_write(FL2018FederalHouse, "H:/Research/2019 Research/TampaBayTimes/2018_Precincts/2018FederalHousePrecinctGeneral/FL2018GenFederalHouse.shp" )




