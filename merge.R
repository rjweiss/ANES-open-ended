

setwd("/Users/Rebecca/Dropbox/research/ANES/data")

anes = read.csv("anes_timeseries_2008_rawdata.txt", sep = "|")
# crosswalk = read.csv("Crosswalk Final.csv")
crosswalk = read.xlsx("Crosswalk Final.xlsx", 1, encoding = "UTF-8")
#occupation = read.xlsx("Occupation - Full Reconciled coding.xlsx", 1, encoding = "UTF-8")
terrorism = read.xlsx("Terrorists - Complete reconciled coding.xlsx", 1, encoding = "UTF-8")

demo = subset(anes, select = c(
  "V080001",
  "V083215a", #birthyear
  "V083215b", #birthmonth
  "V083215c", #birthday
  "V083215x", #age
  "V083216a", #marital, ver. m.
  "V083216b", #marital, ver. n.
  "V083216x", #marital summary
  "V083217", #highest year of ed completed
  "V083218a", #hs diploma 
  "V083218b", #highest degree if highest year = 13+
  "V083218x", #educational attainment
  "V083219", #spouse
  "V083220a", #spouse hs diploma
  "V083220b", #spouse highest degree
  "V083220x", #spouse educational attainment
  "V083221", #served in military
  "V083222", #employment status
  "V083222x", #employment status 2-digit
  "V083245", #marital 
  "V083248", #household income
  "V083251a", #race mention 1
  "V083251b", #race mention 2
  "V083251c", #race mention 3
  "V083251d", #race mention 4
  "V083251e", #race mention 5
  "V083265a", #number children in house <10
  "V083265b", #number children in house >10 <18
  "V083311", #gender
  "V082252", #interviewer gender
  "V082254" #interviewer race
  ))

names(crosswalk) = c("V080001", "interviewer_code", "transcript_code")
#names(occupation) = c("V080001", "verbatim_occ", "codes_occ")
names(terrorism) = c("V080001", "verbatim_terr", "code1", "code2", "code3", "code4", "code5", "code6", "code7", "code8", "code9", "code10", "code11")

demo_x = join(demo, crosswalk)
demo_x_terr = join(demo_x, terrorism)

#demo_x_occ = join(demo_x, occupation)
#demo_x_occ_terr = join(demo_x_occ, terrorism)

test = sqldf("select * FROM crosswalk JOIN anes USING(V080001) LIMIT 10")