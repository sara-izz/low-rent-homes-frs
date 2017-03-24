#Calculate minimum income for family
#Currently only applicable to 1 benefit unit households - need to think how to handle multiple ben unit households

append.mis <- function(data_table, mis_file){
  
  mis <- read.csv(mis_file, stringsAsFactors = FALSE)
  MIS <- data.table(mis)
  
  #Re-code missing FAMTYPBU and BUKIDS in main dataset
  data_table[ is.na(FAMTYPBU), FAMTYPBU := 999]
  
  data_table[ (FAMTYPBU == 6 |  FAMTYPBU == 4 |  FAMTYPBU == 2 |  FAMTYPBU == 1) , BUKIDS := 0]
  
  
  mis_keycols = c("FAMTYPBU", "BUKIDS")
  MIS_cols = c("FAMTYPBU", "BUKIDS", "ahc_mis_15")
  
  setkeyv(data_table, mis_keycols)
  setkeyv(MIS, mis_keycols)
  
  data_table <- merge(data_table , MIS[ , .SD, .SDcols = MIS_cols], by = mis_keycols, all.x = TRUE)
  
  return(data_table)
  
}