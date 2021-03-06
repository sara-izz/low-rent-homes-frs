require("haven")
require("data.table")
require("dplyr")
#library(sqldf)

source("./FRS_tools/column_selector.R")


######Function definition to create flatfile for each year of data 
make.flatfile <- function(data_dir,dat_year){
  ###Get column names
  col_vector <- column.selector(dat_year)
  childcols <- col_vector[[1]]
  adultcols <- col_vector[[2]]
  bencols <- col_vector[[3]]
  hcols <- col_vector[[4]]
  hbaicols <- col_vector[[5]]
  rentcols <- col_vector[[6]]
  ###Add columns from chdcare to chld
  if(dat_year != "1213"){chldcare <- read_spss(paste0(data_dir,"Family Resources Survey/",dat_year,"/spss19/chldcare.sav"))}
  if(dat_year == "1213"){chldcare <- read_spss(paste0(data_dir,"Family Resources Survey/",dat_year,"/spss19/childcare.sav"))}
  child <- read_spss(paste0(data_dir,"Family Resources Survey/",dat_year,"/spss19/child.sav"))
  
  #Convert dataframes to datatables
  CHLDCARE = data.table(chldcare)
  CHILD = data.table(child)
  #Set keys for datatables (i.e. row identifiers)
  keycols = c("SERNUM","BENUNIT","PERSON")
  setkeyv(CHILD, keycols)
  #Add CHLOOK to CHILD datatable
  CHILD[CHLDCARE, CHLOOK := i.CHLOOK]
  #Add flag to CHILD to indicate PERSON is a child
  CHILD$IsChild <- TRUE
  #Check everything's ok
  head(CHILD)
  
  #Keep only needed variables
  
  CHILD[ , .SD, .SDcols = childcols]
  
  ###Add CHILD to adult dataset
  adult <- read_spss(paste0(data_dir,"Family Resources Survey/",dat_year,"/spss19/adult.sav"))
  ADULT = data.table(adult)
  setkeyv(ADULT, keycols)
  ADULT <- merge(ADULT, CHILD[ , .SD, .SDcols = childcols], by = keycols, all = TRUE, suffixes = c("a","c"))
  # print("1")
  # print(ADULT[SERNUM == '17470'])
  #Turn IsChild NAs in to false
  # ADULT %>% mutate_each(funs(replace(., is.na(.), F)), IsChild)
  # ADULT %>% mutate_each(funs(replace(., is.na(.), 0)), COHABITa)
  # ADULT %>% mutate_each(funs(replace(., is.na(.), 0)), COHABITc)
  
  ADULT[is.na(IsChild), IsChild := FALSE]
  
  #Add adult variables to list of columns to keep
  remove <- c("COHABIT","SEX","AGE","CURQUAL")
  cols<-childcols[! childcols %in% remove]
  if (dat_year == "1213" | dat_year == "1314" | dat_year == "1415" ){
    cols <- c(cols, "COHABITa","SEXa","AGEa","CURQUALa","COHABITc","SEXc","AGEc","CURQUALc",adultcols)
  } else if (dat_year == "1112" | dat_year == "1011" | dat_year == "0910"){
    cols <- c(cols, "COHABITa","SEXa","AGEa","COHABITc","SEXc","AGEc",adultcols) 
  }
  
  ADULT[is.na(COHABITa), COHABITa := 999]
  ADULT[is.na(COHABITc), COHABITc := 999]
  ADULT[is.na(SEXa), SEXa := 999]
  ADULT[is.na(SEXc), SEXc := 999]
  ADULT[is.na(AGEc), AGEc := 999]
  if(dat_year != "1112" & dat_year != "1011" & dat_year != "0910"){
    ADULT[is.na(CURQUALa), CURQUALa := 999]
    ADULT[is.na(CURQUALc), CURQUALc := 999]
  }
  
  #recode potentially missing variables
  
  ###Add benefit unit info to ADULT
  benu <- read_spss(paste0(data_dir,"Family Resources Survey/",dat_year,"/spss19/benunit.sav"))
  BENU = data.table(benu)
  keycols = c("SERNUM","BENUNIT")
  setkeyv(BENU, keycols )
  setkeyv(ADULT, keycols)
  #columns to pull from benefit unit

  
  #merge those columns in to ADULT
  ADULT <- merge(ADULT, BENU[ , .SD, .SDcols = bencols], by = keycols, all.x = TRUE, suffixes = c("a","b"))
  # print("2")
  # print(ADULT[SERNUM == '17470'])
  #add benunit columns to overall columns to keep
  #take out SERNUM, BENUNIT to avoid overlap
  remove <- c("SERNUM", "BENUNIT")
  bencols<-bencols[! bencols %in% remove]
  cols <- c(cols, bencols)
  
  ###Add household info into ADULT
  hhold <- read_spss(paste0(data_dir,"Family Resources Survey/",dat_year,"/spss19/househol.sav"))
  HHOLD = data.table(hhold)
  keycols = c("SERNUM")  
  setkeyv(HHOLD, keycols)
  setkeyv(ADULT, keycols)
  #Columns to take from HHOLD

  ADULT <- merge(ADULT, HHOLD[ , .SD, .SDcols = hcols], by = keycols, all.x = TRUE, suffixes = c("a","h"))
  
  if(dat_year != "1112" & dat_year != "1011" & dat_year != "0910"){
    ADULT[, GROSS4a := NULL]
    setnames(ADULT, "GROSS4h", "GROSS4")
  }

  
  #add hhold columns to overall columns to keep
  remove <- c("SERNUM")
  hcols<-hcols[! hcols %in% remove]
  cols <- c(cols, hcols)
  #
  # print("3")
  # print(ADULT[SERNUM == '17470'])
  ###Add care info to ADULT
  care <- read_spss(paste0(data_dir,"Family Resources Survey/",dat_year,"/spss19/care.sav"))
  CARE = data.table(care)
  
  cacols <- c("WHOLOO01", "WHOLOO02", "WHOLOO03", "WHOLOO04", "WHOLOO05", "WHOLOO06", "WHOLOO07", "WHOLOO08", "WHOLOO09","WHOLOO10","WHOLOO11",
              "WHOLOO12","WHOLOO13","WHOLOO14","WHOLOO15","WHOLOO16","WHOLOO17","WHOLOO18","WHOLOO19","WHOLOO20")
  keycols <- c("SERNUM", "BENUNIT")
  
  setkeyv(ADULT, keycols)
  setkeyv(CARE, keycols)
  
  ADULT[CARE, ':=' (WHOLOO01=i.WHOLOO01, WHOLOO02=i.WHOLOO02, WHOLOO03=i.WHOLOO03, WHOLOO04=i.WHOLOO04, WHOLOO05=i.WHOLOO05, WHOLOO06=i.WHOLOO06,
                    WHOLOO07=i.WHOLOO07, WHOLOO08=i.WHOLOO08, WHOLOO09=i.WHOLOO09, WHOLOO10=i.WHOLOO10, WHOLOO11=i.WHOLOO11, WHOLOO12=i.WHOLOO12,
                    WHOLOO13=i.WHOLOO13, WHOLOO14=i.WHOLOO14, WHOLOO15=i.WHOLOO15, WHOLOO16=i.WHOLOO16, WHOLOO17=i.WHOLOO17, WHOLOO18=i.WHOLOO18,
                    WHOLOO19=i.WHOLOO19, WHOLOO20=i.WHOLOO20, FREQ=i.FREQ)]
  
  #add care columns to overall columns to keep
  cols <- c(cols, cacols)
  
  
  ### Add renter info to ADULT
  renter <- read_spss(paste0(data_dir,"Family Resources Survey/",dat_year,"/spss19/renter.sav"))
  RENTER = data.table(renter)
  keycols = c("SERNUM")  
  setkeyv(RENTER, keycols)
  setkeyv(ADULT, keycols)
  #Columns to take from HHOLD
  
  ADULT <- merge(ADULT, RENTER[ , .SD, .SDcols = rentcols], by = keycols, all.x = TRUE, suffixes = c("a","r"))
  
  #add renter columns to overall columns to keep
  remove <- c("SERNUM")
  rentcols<-rentcols[! rentcols %in% remove]
  cols <- c(cols, rentcols)
  
  ### New data table that is slimmed down ADULT to variables needed

  AFF <- ADULT[, .SD, .SDcols = cols]
  if (dat_year == "1112" | dat_year == "1011" | dat_year == "0910"){
    setnames(AFF, "gross4", "GROSS4")
  }
  #####Add in HBAI variables
  hbai <- read_spss(paste0(data_dir,"HBAI/1995-2015/spss19/hbai", dat_year,"_g4.sav")) #year,
  HBAI = data.table(hbai)
  keycols = c("SERNUM", "BENUNIT")
  setkeyv(HBAI, keycols)
  # #columns to pull from benefit unit
  # 
  # 
  # #merge those columns in to ADULT
  AFF <- merge(AFF, HBAI[ , .SD, .SDcols = hbaicols], by = keycols, all.x = TRUE, suffixes = c("a","hbai"))
  # affdt_1415 <- merge(affdt_1415, HBAI[ , .SD, .SDcols = hbaicols], by = "SERNUM", all.x = TRUE, suffixes = c("a","hbai"))
  # print("6")
  # print(AFF[SERNUM == '17470'])
  
  AFF[ , year := dat_year]
  return(AFF)
}


