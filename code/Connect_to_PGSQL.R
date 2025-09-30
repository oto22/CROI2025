library(DBI)
library(RPostgres)

### import from postgreslq
#con<-dbConnect(PostgreSQL(), user="oto", password="oto2213", dbname="aidshis20082022")  # connect to postgresql local db aidshis##date

con <- DBI::dbConnect(RPostgres::Postgres(), 
                      dbname = rstudioapi::askForSecret("dbname"), 
                      host = "localhost", 
                      port = 5432, 
                      user = "postgres", # rstudioapi::askForSecret("username"), 
                      password = "ototata" ) # rstudioapi::askForSecret("dbpassword")) ## - use this code for dialog window# "oto2213"

# con<-dbConnect(PostgreSQL(), user=userr, password=psw, dbname=dbnm)  # connect to postgresql local db aidshis##date
# 
# if(isTRUE(con)){
#   con; # dir.create is used to creatge not existing directory
# }else{
#   (isTRUE(conn))
#   con <- conn;
# }



dbGetQuery(con, "SHOW CLIENT_ENCODING") # get cline encoding
dbListTables(con) 
patient<-dbGetQuery(con, "select * from patient;", na.strings = c("", "NA"))  # select patient table
personllist<-dbGetQuery(con, "select * from personell")
investigations <-dbGetQuery(con, "select * from investigation")
code<-dbGetQuery(con, "select * from code ;", na.strings = c("", "NA"))  # select patient table
region<-dbGetQuery(con, "select * from region;", na.strings = c("", "NA"))  # select patient table
disctict<-dbGetQuery(con, "select * from discrit;", na.strings = c("", "NA"))  # select patient table
visit <- dbGetQuery(con, "select distinct p.regnum as id,
                    date(v.visitdate) as vdate,
                    v.patstatustid as vstatus,
                    v.visitcenterid  as vcntr,
                    v.tocenterid  as tocntr,
                    patient_weight as pweight,
                    patient_heigth as pheight,
                    v.doctorid as vdoctor,
                    v.createdate,
                    v.personellid
                    from
                    patient as p
                    inner JOIN visit as v ON(p.patientid=v.patientid)
                    --where
                    --  v.patstatustid != 251
                    --v.visitdate < '2019-01-01' and p.regdate < '2019-01-01'
                    ORDER BY
                    vdate desc;", na.strings = c("", "NA"))

dball<-dbGetQuery(con, "select distinct p.regnum as id,
                  p.pin as pn,
                  date(vst.startdate) as labdate,
                  vst.result_num as rslt,
                  vst.result as rslttxt,
                  vst.investigationid as vid,
                   inv.name as vnm,
                  vst.centerid as labcenter
                  --CURRENT_DATE as d_date
                  from
                  patient as p
                  LEFT JOIN visit as v ON(p.patientid=v.patientid)
                  LEFT JOIN visitstandard as vst ON(v.visitid=vst.visitid)
                  INNER JOIN investigation as inv ON(vst.investigationid=inv.investigationid)
                  -- INNER JOIN code as cod ON(p.genderid=cod.codeid)
                  -- INNER JOIN center as c  ON(vst.centerid=c.centerid)
                  --where
                  --   vst.investigationid in(151, 157) and vst.result_num is not null and vst.startdate < '2019-02-01' and p.regdate < '2019-01-01' -- and p.regnum <10002
                  ORDER BY
                  labdate desc;", na.strings = c("", "NA"), as.is=TRUE)
dbarv<-dbGetQuery(con, "select p.regnum as id, cd.code as comb,
                  date(arv.arvgetdate) as arvgdate,
                  arv.createdate as arvcrdate, 
                  arv.arvstatusid as arvst, 
                  arv.personellid as prscode,
                  arv.arvcombinationid as cid,
                  by_effects as rigi,
                  docvisittypeid as arvcenter
                  from
                  patient as p
                  inner JOIN arvpatient as arv ON(p.patientid=arv.patientid)
                  inner JOIN code cd ON(cd.codeid = arv.arvcombinationid);",
                  na.strings = c("", "NA"))
scrvacc<-dbGetQuery(con,"select distinct p.regnum,  vscr.screeningdate::date as scrd, vscr.result, vscr.vaccinationid
                  from
                  patient p
                  inner join visit v ON(p.patientid=v.patientid)
                  inner join visitscreening vscr ON(vscr.visitid=v.visitid)
                  inner join code cd ON(cd.codeid = vscr.vaccinationid)
                  where vscr.vaccinationid = 149 
                  order by
                  regnum, scrd", na.strings = c("", "NA"))
scrvacc<-dbGetQuery(con,"select * from pregnancy ", na.strings = c("", "NA"))
                    
### get ill data ##
dbill<-dbGetQuery(con, "select * from
                  visitill;")
dbDisconnect(con)  # clode connection to posgreslq
# status <- if(exists(c("patient","visit","dball","dbarv","scrvacc", "dbill"))){print("DLS:(patient,visit , dball,dbarv,scrvacc, dbill, - are ready!!!)")}
# status
print("Dataframes are loaded:")
print(names(which(unlist(eapply(.GlobalEnv,is.data.frame)))))


