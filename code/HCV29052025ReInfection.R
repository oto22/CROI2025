rm(list=ls())  # remove all data
#dev.off()
## packages
## load packages that needed
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c("installr", "RPostgreSQL", "dplyr","lubridate",
              "ggplot2","tidyr", "stringr", "tibble",
              "reshape2", "tidytext", "gtsummary", 
              "forcats", "knitr","readr", "readxl"
              
              # "pdftools", "tabulizer",  "gtools", "flextable",  "shiny",
              # "emayili","linelist", "pacman", "parsedate", "AMR", "cleaner"                  
)

ipak(packages)
### set wirking directory
workwd <- "D:/OTO/Data_Qdb/CROI2025"
homewd <- "/Users/oto/Documents/OTO/Data_Qdb/CROI2025"
if(isTRUE(file.info(workwd) [1,"isdir"])){
  setwd(workwd);#dir() # dir.create is used to creatge not existing directory
}else{
  (isTRUE(file.info(homewd)[1,"isdir"]))
  setwd(homewd);#dir()
}
getwd() # check working directory

## make tables smaller gtsummary
theme_gtsummary_compact()

### import from postgreslq
# con<-dbConnect(PostgreSQL(), user="oto", password="oto2213", dbname="aidshis22092023")
# patient<-dbGetQuery(con, "select * from patient;", na.strings = c("", "NA"))  
# vln<-dbGetQuery(con, "select * from visit;", na.strings = c("", "NA"))  
# dbDisconnect(con)  # close connection to posgreslq

### import from postgreslq
source(paste0(getwd(), "/code/Connect_to_PGSQL.R")) # load data from postgresql, you should enter database name

###> import NCDC HCV HIV checked data status 0 means not in theyr DB and we need to check those patients and
# ###> to enter into HCV DB.
# hcvncdcscrnall <- read_excel("data/HCVdataHIVsfromKvicha.xlsx",  sheet = "HCVScreeningALL")
# hcvncdcscrnpos <- read_excel("data/HCVdataHIVsfromKvicha.xlsx",  sheet = "HCVsrcinPOZ")
# hcvncdcconfall <- read_excel("data/HCVdataHIVsfromKvicha.xlsx",  sheet = "HCVconALL")
# hcvncdcconfpoz <- read_excel("data/HCVdataHIVsfromKvicha.xlsx",  sheet = "HCVconfPOZ")
# hcvncdcscrn6month <- read_excel("data/HCVdataHIVsfromKvicha.xlsx",  sheet = "HCVscrinL6month")
hcvdbfromkhvicha <- read_excel("data/dbHCVGetia.xlsx",  sheet = "hcvdbkhvicha")

####### start data management and exploration ######
### str_pad("24493", width = 7, side = "left", pad = "0") # add pads (0 before regnum) to values requires // library(strngr)

### Make a list of HCV rapid test codes
hcvcoderapidt <- investigations %>% 
  filter(investigationid %in% c(234, 235, 237, 239, 240))
hcvcoderapidt

#### Make a list of HCV confirmatory test codes 
hcvcodeconfirmationt <- investigations %>% 
  filter(investigationid %in% c(376, 378, 379, 380))
hcvcodeconfirmationt

#### Any HCV tests to avoid missig those who have HCV PCR and not HCV screening###
hcvtests <- patient %>% 
  filter(!is.na(regnum)) %>% 
  inner_join(dball, by = c("regnum"="id")) %>% 
  filter(!is.na(rslttxt), #(is.na(deathfixdate) | deathfixdate < '2015-12-31'), nchar(pin) == 11,
         #regionid != 1, !is.na(rslttxt), regdate >= '2014-01-01', regdate < '2019-01-01', hcvdrcd == '1',
         vid %in% c(234, 235, 237, 239, 240, 376, 378, 379, 380) ## Added all test to exclude cases when we have only PCR resutls
  ) %>% # , rslttxt %in% c("Pozitive", "Negative") | rslttxt > 1 
  mutate(id = regnum, 
         sex = ifelse(genderid == 4, "M", "F"),
         #pln = ifelse(nchar(pin) == 11, "correct", "incorrect"),
         # hcvd = case_when(is.na(rslttxt) ~ "Noanswer",
         #                  as.numeric(str_detect(rslttxt, "-")) == 0 ~ "Y",
         #                  as.numeric(str_detect(rslttxt, "-")) == 1 ~ "N",
         #                  as.numeric(str_detect(rslttxt, "Negative")) == 1 ~ "N"),
         # hcvdrcd = recode_factor(hcvd,'1' = "0", '0' = "1"),
         rrt = recode_factor(transferid, '47' = "IDU", '48' = "HETERO", '49' = "HOMO", 
                             .default = "hhh", .missing = "gg"),
         rstlcon = str_squish(rslttxt),
         #rsnmb = gsub('\\D','', rslttxt), ## extract numbers only 
         rstlcon1 = str_replace_all(rstlcon, " |,", ""),
         rstlcon2 = tolower(rstlcon1), # make all to lower
         rstlcon3 = str_replace_all(rstlcon2, "1b", ""), # make all to lower
         hcvirslt1 = ifelse(str_detect(rstlcon3, "ml"),  as.character(str_extract_all(rstlcon3, "\\d+")), rstlcon1),
         rslthcv = case_when(str_detect(hcvirslt1, "po") ~ "Pozitive",
                             str_detect(hcvirslt1, "დად") ~ "Pozitive",
                             str_detect(hcvirslt1, "პოზ") ~ "Pozitive",
                             as.numeric(hcvirslt1) > 25 ~ "Pozitive",
                             str_detect(hcvirslt1, "\\+") ~ "Pozitive",
                             #as.numeric(str_extract_all(hcvirslt1, "\\d+")) > 25 ~ "Pozitive",
                             TRUE ~ "Negative" )
  ) %>% 
  # str_detect(vnm, "HCV") == TRUE) %>%
  arrange(id, labdate) %>% 
  group_by(id) %>%
  summarise(pinf = first(pin), rrt = last(rrt), gender = last(sex), lddate =  last(deathdate),
            fvid = first(vid), mlbdt = min(labdate), fnrslt = first(rslthcv), patregdate = last(regdate),
            lvid = last(vid), lsrslt = last(rslthcv), maxlbdt = max(labdate), lbcntr = last(labcenter),
            ever_positive = ifelse(any(rslthcv == "Pozitive"), "Pozitive", "Negative")
  ) %>%
  mutate(hcvbecomepozitive = ifelse(fnrslt == "Negative" & lsrslt == "Pozitive", "BecamePoz", fnrslt),
         hcvbecomenegative = ifelse(fnrslt == "Pozitive" & lsrslt == "Negative", "BecameNeg", fnrslt)
  ) %>% 
  select(id, pinf, gender, rrt, fvid, mlbdt, fnrslt, lvid, maxlbdt, lsrslt, hcvbecomenegative, hcvbecomepozitive, ever_positive, patregdate, lddate, lbcntr) 

#check
hcvtests %>% 
  count(fvid)

## ever positive
hcvtests %>% 
  count(ever_positive)

#### HCV COnfirmation all####

hcvConfAllrslts <- patient %>% 
  filter(!is.na(regnum)) %>%
  inner_join(dball, by = c("regnum" = "id")) %>%
  left_join(hcvdbfromkhvicha, by = c("pin" = "PID")) %>%
  filter(labdate < '2024-12-31',
         vid %in% c(376, 378, 379, 380)) %>%
  
  mutate(
    id = regnum,
    sex = if_else(genderid == 4, "M", "F"),
    age = year("2024-12-31") - year(birthdate),
    agegr = cut(as.integer(age), breaks = c(0, 39, 49, 59, 1000), 
                labels = c("<40", "40-49", "50-59", "60+")),
    rrt = recode_factor(transferid, '47' = "IDU", '48' = "HETERO", '49' = "HOMO", 
                        .default = "hhh", .missing = "gg"),
    
    # Clean and normalize result strings
    rstlcon = str_squish(rslttxt),
    rstlcon1 = str_replace_all(rstlcon, " |,", ""),
    rstlcon2 = tolower(rstlcon1),
    rstlcon3 = str_replace_all(rstlcon2, "1b", ""),
    
    hcvirslt1 = if_else(str_detect(rstlcon3, "ml|\\d+"), 
                        str_extract(rstlcon3, "\\d+"), 
                        rstlcon3),
    
    rslthcv = case_when(
      str_detect(hcvirslt1, "po|დად|პოზ|\\+") ~ "Pozitive",
      suppressWarnings(as.numeric(hcvirslt1)) > 25 ~ "Pozitive",
      TRUE ~ "Negative"
    ),
    
    HCVtrtEnddateF = coalesce(EndDateEarly, EndDateLate, End_Date),
    weeks_post_trt = as.numeric(difftime(labdate, HCVtrtEnddateF, units = "weeks")),
    is_svr12 = case_when(
      !is.na(weeks_post_trt) & weeks_post_trt >= 12 & rslthcv == "Negative" ~ TRUE,
      TRUE ~ FALSE
    ),
    post_treatment = labdate > HCVtrtEnddateF
  ) %>%
  
  # Flag SVR12 dates before summarising
  arrange(id, labdate) %>% 
  group_by(id) %>%
  mutate(SVR12_date = ifelse(is_svr12, labdate, as.Date(NA))) %>%
  mutate(SVR12_date = as.Date(ifelse(!is.na(SVR12_date), SVR12_date, NA))) %>%
  summarise(
    pinf = first(pin),
    rrt = last(rrt),
    gender = last(sex),
    agegrl = last(agegr),
    age = last(age),
    mlbdt = min(labdate),
    fnrslt = first(rslthcv),
    lsrslt = last(rslthcv),
    lsrslt1 = last(hcvirslt1),
    fsrslt1 = first(hcvirslt1),
    maxlbdt = max(labdate),
    lbcntr = last(labcenter),
    ever_positive = if_else(any(rslthcv == "Pozitive", na.rm = TRUE), "Pozitive", "Negative"),
    SVR12 = if_else(any(is_svr12, na.rm = TRUE), "SVR12", "No SVR12"),
    SVR12_date = min(SVR12_date, na.rm = TRUE),
    REinftested = if_else(any(maxlbdt > SVR12_date, na.rm = TRUE), "REinftested", "Not reinftested"),
    .groups = "drop"
  ) %>%
  
  mutate(
    hcvbecomepozitive = if_else(fnrslt == "Negative" & lsrslt == "Pozitive", "BecamePoz", fnrslt),
    hcvbecomenegative = if_else(fnrslt == "Pozitive" & lsrslt == "Negative", "BecameNeg", fnrslt)
  ) %>%
  
  select(
    id, pinf, gender, rrt, mlbdt, fnrslt, maxlbdt, age,
    lsrslt, hcvbecomenegative, hcvbecomepozitive, agegrl,
    ever_positive, SVR12, SVR12_date, lbcntr, lsrslt1, REinftested
  )

## count SVR12
hcvConfAllrslts %>% 
  count(REinftested) 


### combined final database ####
HCVdbfinalDB <- hcvtests %>% 
  left_join(hcvConfAllrslts, by = c("id"), suffix = c("_scrn", "_conf") ) %>% 
  left_join(hcvdbfromkhvicha, by = c("pinf_scrn"="PID")) %>%
  #anti_join(lftcntr, by = c("id" = "id")) %>% ## remove left country
  mutate(
         ever_positive_all = ifelse(ever_positive_scrn == "Pozitive" | EverPos == "Yes", "Pozitive", "Negative"),
         ever_viremic_all = ifelse(ever_positive_conf == "Pozitive" | EverViremic == "Yes", "Pozitive", "Negative"),
         SRV_Final = ifelse(SVRCheckLate == "SVR Achieved" | hcvbecomenegative_conf == "BecameNeg", "SVR", "Negative"),
         HCVtrt_Final = ifelse(CompletedTreatmentLate == "Complete" | CompletedTreatmentEarly == "Complete", "Complete", "NotComplete"),
         HCVtrtStdateF = coalesce(TreatmentStartDateEarly, TreatmentStartDateLate),
         HCVtrtEnddateF = coalesce(EndDateEarly, EndDateLate, End_Date),
         HCVSVR12beanch = HCVtrtEnddateF + weeks(12),
         followup_time_years = round(time_length(interval(SVR12_date, maxlbdt_conf), unit = "years"), 1)
      #   HCVtrtEndandLastVLdateCompr = diff.Date(HCVtrtEnddateF < maxlbdt_conf, "Correct", "notCorrect"),
      # Calculate weeks between treatment end and VL test
  ) %>% 
  filter(SVR12 == "SVR12") %>% 
  select(id, SVR12, HCVtrtEnddateF, HCVSVR12beanch, maxlbdt_conf, followup_time_years, HCVtrt_Final, lsrslt_conf, sex = gender_scrn, trrt = rrt_scrn, agegrl, age, REinftested)


# HCVdbfinalDB %>%
#   filter(ever_positive_all == "Pozitive", 
#          ever_positive_scrn != "Pozitive") %>% 
#   select(id)

## save final hcv cascade db in csv
write.csv(HCVdbfinalDB,  paste0(getwd(), "/outcomes/HCVdbfinalDB", Sys.Date(), ".csv", sep = ""), row.names=F, na="")


### data analysis ########
### Re infection is defined as have detectable HCV VL among patients after confirmed SVR ####

HCVdbfinalDB %>% 
  filter(REinftested == "REinftested") %>% ### filter tested on reinfection
 # mutate(ty = year(HCVtrtEnddateF)) %>% 
  #filter(trrt == "HETERO", sex == "M", lsrslt_conf == "Pozitive")
 # group_by(lsrslt_conf) %>% ## group by  reinfection variable
      count(sex)

## under 15
patient %>% 
  mutate(age = year("2024-12-31") - year(birthdate)) %>% 
  select(regnum, fname, lname, birthdate, age) %>% 
  arrange(birthdate) %>% 
  filter(age < 18, !is.na(regnum))

## age median among screened for reinfection
agedt <- HCVdbfinalDB %>% 
  mutate(ty = year(HCVtrtEnddateF)) %>% 
  mutate(agemdn = median(age), ageIQR = IQR(age)) 

## median follow-up from screened reinfection
medianflwpsscreened <- HCVdbfinalDB %>% 
  mutate(reinfmdnflwpd = median(followup_time_years), reinfflwpIQR = IQR(followup_time_years)) 

# median followup from reinfected people
medianflwpsReinfected <- HCVdbfinalDB %>% 
  filter(lsrslt_conf == "Pozitive") %>% 
  mutate(reinfmdnflwpd = median(followup_time_years), reinfflwpIQR = IQR(followup_time_years)) 


# 
# reinfectionHCV <- HCVdbfinalDB %>% 
#   mutate(reinhcv = , ) %>% 
#   summarize()
#   

## calcualte incidence rate for HCV re infection

# Total events (reinfections or new infections)
reinfectionHCV <- HCVdbfinalDB %>% 
  #filter(REinftested == "REinftested") %>% 
  mutate(total_events = sum(lsrslt_conf == "Pozitive"), # assuming event is 1 for infection/reinfection

# Total person-years
  total_person_years = sum(followup_time_years, na.rm = TRUE),

# Incidence Rate per 100 person-years
 incidence_rate = (total_events / total_person_years) * 100 )

## save final hcv cascade db in csv
write.csv(reinfectionHCV,  paste0(getwd(), "/outcomes/reinfectionHCV", Sys.Date(), ".csv", sep = ""), row.names=F, na="")

### median followup time

# Total events (reinfections or new infections)
OnlyreinfectionHCV <- HCVdbfinalDB %>% 
  filter(REinftested == "REinftested") %>% 
  mutate(total_events = sum(lsrslt_conf == "Pozitive"), # assuming event is 1 for infection/reinfection
         
         # Total person-years
         total_person_years = sum(followup_time_years, na.rm = TRUE),
         
         # Incidence Rate per 100 person-years
         incidence_rate = (total_events / total_person_years) * 100 )

###
write.csv(OnlyreinfectionHCV,  paste0(getwd(), "/outcomes/OnlyreinfectionHCV", Sys.Date(), ".csv", sep = ""), row.names=F, na="")

