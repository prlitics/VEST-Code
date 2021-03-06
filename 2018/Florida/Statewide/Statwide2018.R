library(tidyverse)
library(stringi)
library(stringr)
library(data.table)
library(foreign)
library(broom)
library(data.table)
library(sf)
full<-data.frame()

cty.names<-c("ALA","BAY", "BRA", "BRE", "CAL","CHA","CIT","CLA","CLL", "IND",
             "CLM","DAD","DES","DIX","DUV","ESC","FLA","FRA","GAD", "GLA","GUL",
             "HAM","HEN","HER","HIG","HIL","HOL","JAC","JEF","LAF","LAK","LEE",
             "LEO","LEV","LIB","MAD","MAN","MRN","MRT","NAS","OKA","OKE","ORA",
             "OSC","PAL","PAS","PIN","POL","SAN","SAR","STJ","STL","SUM",
             "SUW","TAY","VOL","WAK","WAL","PUT","HAR","BAK","SEM","BRO","GIL")


for(cty in cty.names){
  file <- paste("C:/Users/prlic/Downloads/precinctlevelelectionresults2018gen/",
                cty,"_PctResults20181106.txt",sep = "") 
  cty<-read.csv2(file,sep = "\t",header = F, na.strings = " ", colClasses = c("V13"="character",
                                                                              "V16"="character")) 
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
           !(prec.id==0), (contest == "united states senator" | contest == "governor"|
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
         !(prec.id==0), (contest == "united states senator" | contest == "governor"|
                           contest == "attorney general" | contest == "chief financial officer" | contest == "commissioner of agriculture" |
                           grepl("amend",contest)))

full <- rbind(full,cty)


#Monroe 
mon<-read.csv2("C:/Users/prlic/Downloads/precinctlevelelectionresults2018gen/MON_PctResults20181106.txt",sep = "\t",header = F, na.strings = " ", colClasses = c("V13"="character",
                                                                                                                                                                 "V16"="character")) 
mon1a<-mon
mon2<-data.frame()
i<-0
mon.prec.state<-c(1,2,3,4,5,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,28,29,30,31,32,35,36,37,38,41)
mon.prec.dbf  <-c(1,2,3,4,5,6,7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33)


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
         !(prec.id==0), (contest == "united states senator" | contest == "governor"|
                           contest == "attorney general" | contest == "chief financial officer" | contest == "commissioner of agriculture" |
                           grepl("amend",contest)))

full <- rbind(full,cty)

#Washington County

was<-read.csv2("C:/Users/prlic/Downloads/precinctlevelelectionresults2018gen/WAS_PctResults20181106.txt",sep = "\t",header = F, na.strings = " ", colClasses = c("V13"="character",
                                                                                                                                                                 "V16"="character")) 
was2<-was %>%
  mutate(V6 = case_when(
    V6 == 12 ~ 15,
    V6 == 11 ~ 12,
    V6 == 10 ~ 11,
    T ~ as.double(V6)
  ))


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
         !(prec.id==0), (contest == "united states senator" | contest == "governor"|
                           contest == "attorney general" | contest == "chief financial officer" | contest == "commissioner of agriculture" |
                           grepl("amend",contest)))
full <- rbind(full,cty)

full <- full %>%
  mutate(prec.id2 = ifelse(prec.id2 == "CAL201/201C","CAL0201",paste(prec.id2)))


full.amends<- full %>%
  filter(grepl("amend",contest))%>%
  mutate(contest = str_replace_all(contest, fixed(" "),""),
         candidate = case_when(
           candidate == "Yes for Approval" ~ "Yes",
           candidate == "No for Rejection" ~ "No"
         ),
         contest = case_when(
           grepl("no.10", contest) ~ "Am.10",
           grepl("no.11", contest) ~ "Am.11",
           grepl("no.12", contest) ~ "Am.12",
           grepl("no.13", contest) ~ "Am.13",
           grepl("no.2", contest) ~ "Am.2",
           grepl("no.3", contest) ~ "Am.3",
           grepl("no.4", contest) ~ "Am.4",
           grepl("no.5", contest) ~ "Am.5",
           grepl("no.6", contest) ~ "Am.6",
           grepl("no.7", contest) ~ "Am.7",
           grepl("no.9", contest) ~ "Am.9",
           grepl("no.1", contest) ~ "Am.1",
         )) %>%
  dcast(prec.id2 + contest ~ candidate, value.var = "total.votes", fun.aggregate=sum) %>%
  mutate(per.yes = Yes / (No + Yes),
         per.no = No / (No + Yes),
         total.cast = No + Yes) %>%
  pivot_wider(names_from = contest, values_from = c("No","Yes","per.yes","per.no","total.cast"))



full.people <- full %>%
  filter(!grepl("amend",contest)) %>%
  mutate(contest = str_replace_all(contest, fixed(" "),"")) %>%
  dcast(prec.id2 + contest ~ cand.pty.code, value.var = "total.votes", fun.aggregate=sum)%>%
  mutate(prec.R.percent = REP/(DEM+NPA+REP+REF),
         prec.D.percent = DEM/(DEM+NPA+REP+REF),
         prec.NPA.percent = NPA/(DEM+NPA+REP+REF),
         total.cast = (DEM+NPA+REP+REF),
          prec.R.percent = ifelse(is.nan(prec.R.percent),-9,prec.R.percent),
         prec.D.percent = ifelse(is.nan(prec.D.percent),-9,prec.D.percent),
         prec.NPA.percent = ifelse(is.nan(prec.NPA.percent),-9,prec.NPA.percent)) %>%
  pivot_wider(names_from = contest, values_from = c("DEM","REP","REF","NPA","total.cast","prec.R.percent","prec.D.percent","prec.NPA.percent"))




full.results <- merge(x =full.amends, y = full.people, by="prec.id2",all = T)

full.regis <-  full %>%
  group_by(prec.id2) %>%
  summarise(total.reg = mean(tot.reg))



full.results <- merge(x = full.results, y = full.regis, by = "prec.id2" )

FL.2018.Precs <- st_read("H:/Research/2019 Research/TampaBayTimes/2018_Precincts/Precincts2018General/Precincts2018General.shp")
FL.2018.Precs<-st_zm(FL.2018.Precs, what = "ZM")






FL.2018.Precs.Test <- full.results %>%
  select(
    prec.id2,
    G18AM1No = No_Am.1,                                  
    G18AM10No = No_Am.10,                                 
    G18AM11No= No_Am.11,                                 
    G18AM12No= No_Am.12,                                 
    G18AM13No= No_Am.13,                                  
    G18AM2No= No_Am.2,                                  
    G18AM3No= No_Am.3,                                 
    G18AM4No=No_Am.4,                                   
    G18AM5No=No_Am.5,                                  
    G18AM6No= No_Am.6,                                 
    G18AM7No=No_Am.7,                                   
    G18AM9No= No_Am.9,                                  
    G18AM1Yes= Yes_Am.1,                                 
    G18AM10Yes=Yes_Am.10,                                
    G18AM11Yes= Yes_Am.11,                               
    G18AM12Yes=Yes_Am.12,                                
    G18AM13Yes=Yes_Am.13,                                
    G18AM2Yes=Yes_Am.2,                                 
    G18AM3Yes=Yes_Am.3,                                
    G18AM4Yes=Yes_Am.4,                                  
    G18AM5Yes=Yes_Am.5,                                 
    G18AM6Yes= Yes_Am.6,                                
    G18AM7Yes= Yes_Am.7,                               
    G18AM9Yes= Yes_Am.9,                              
    G18AM1Sum= total.cast_Am.1,                          
    G18AM10Sum=total.cast_Am.10,                          
    G18AM11Sum= total.cast_Am.11,                         
    G18AM12Sum= total.cast_Am.12,                        
    G18AM13Sum= total.cast_Am.13,                        
    G18AM2Sum=  total.cast_Am.2,                        
    G18AM3Sum= total.cast_Am.3,                         
    G18AM4Sum= total.cast_Am.4,                         
    G18AM5Sum= total.cast_Am.5,                         
    G18AM6Sum= total.cast_Am.6,                         
    G18AM7Sum= total.cast_Am.7,                         
    G18AM9Sum=total.cast_Am.9,                          
    G18ATGDSha= DEM_attorneygeneral,                     
    G18CFODRin= DEM_chieffinancialofficer,               
    G18AGRDFri= DEM_commissionerofagriculture,            
    G18GOVDGil= DEM_governor,                              
    G18SENDNel= DEM_unitedstatessenator,                 
    G18ATGRMoo= REP_attorneygeneral,                      
    G18CFORPat= REP_chieffinancialofficer,                
    G18AGRRCal= REP_commissionerofagriculture,           
    G18GOVRDeS= REP_governor,                             
    G18SENRSco= REP_unitedstatessenator,                            
    G18AGRRfRi= REF_governor,                                               
    G18ATGISis= NPA_attorneygeneral,                              
    G18ATGNPA= NPA_governor ,                            
    G18SENNPA= NPA_unitedstatessenator ,                
    G18ATGSum= total.cast_attorneygeneral,               
    G18CFOSum= total.cast_chieffinancialofficer,         
    G18AGRSum= total.cast_commissionerofagriculture,     
    G18GOVSum= total.cast_governor,                     
    G18SENSum= total.cast_unitedstatessenator,         
    SumRegis = total.reg)%>%
  #The next function can be found https://github.com/prlitics/VEST-Code/blob/master/Original%20Functions/allocate_virtual_precinct.R
  allocate_virtual_precinct(id_col = "prec.id2", county_stem = c("SEM", "BRE"), ev_name = c("SEM00EV","BRE0999"))

FL.2018.Precs.Test <- sp::merge(y = full.results, x = FL.2018.Precs, by.y = "prec.id2", by.x = "Pct_std", all.x = T)
FL.2018.Precs.Test <- FL.2018.Precs.Test %>%
  select(1:25,52:88,104) %>%
  select(-c(57,55,54,52,50,49,48))


st_write(FL.2018.Precs.Test, "H:/Research/2019 Research/TampaBayTimes/2018_Precincts/Precincts2018General/VEST18Gen.shp")


