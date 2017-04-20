#Code could be tidied by a for loop but I'm scared of them in R!

assign.percentile <- function(data_DT, pc_reg_data_DT, dist_var, perc_width){
  if(perc_width == "5"){
    #quote get(dist_var) for evaluation in data.table environment later 
    #dist_var <- as.character(dist_var)
    #grab region variable name from percentile dataset
    region = colnames(pc_reg_data_DT)[1]
    region_var = quote(colnames(pc_reg_data_DT)[1])
    
    
    setkeyv(pc_reg_data_DT, region)
    setkeyv(data_DT, region)
    
    data_DT <- merge(data_DT, pc_reg_data_DT[ , .SD, .SDcols = c(region,"pc5_reg", "pc10_reg", "pc15_reg", 
                                                                 "pc20_reg", "pc25_reg", "pc30_reg", "pc35_reg",
                                                                 "pc40_reg", "pc45_reg", "pc50_reg", "pc55_reg", 
                                                                 "pc60_reg","pc65_reg", "pc70_reg", "pc75_reg", 
                                                                 "pc80_reg", "pc85_reg", "pc90_reg", "pc95_reg")], 
                     by = region, all.x = TRUE)
    
    
    #Merge percentiles in to data set - bit of a hack, looking up value would be better.
    data_DT[ , pc5_nat := pc_reg_data_DT[get(region) == "England", "pc5_reg"]]
    data_DT[ , pc10_nat := pc_reg_data_DT[get(region) == "England", "pc10_reg"]]
    data_DT[ , pc15_nat := pc_reg_data_DT[get(region) == "England", "pc15_reg"]]
    data_DT[ , pc20_nat := pc_reg_data_DT[get(region) == "England", "pc20_reg"]]
    data_DT[ , pc25_nat := pc_reg_data_DT[get(region) == "England", "pc25_reg"]]
    data_DT[ , pc30_nat := pc_reg_data_DT[get(region) == "England", "pc30_reg"]]
    data_DT[ , pc35_nat := pc_reg_data_DT[get(region) == "England", "pc35_reg"]]
    data_DT[ , pc40_nat := pc_reg_data_DT[get(region) == "England", "pc40_reg"]]
    data_DT[ , pc45_nat := pc_reg_data_DT[get(region) == "England", "pc45_reg"]]
    data_DT[ , pc50_nat := pc_reg_data_DT[get(region) == "England", "pc50_reg"]]
    data_DT[ , pc55_nat := pc_reg_data_DT[get(region) == "England", "pc55_reg"]]
    data_DT[ , pc60_nat := pc_reg_data_DT[get(region) == "England", "pc60_reg"]]
    data_DT[ , pc65_nat := pc_reg_data_DT[get(region) == "England", "pc65_reg"]]
    data_DT[ , pc70_nat := pc_reg_data_DT[get(region) == "England", "pc70_reg"]]
    data_DT[ , pc75_nat := pc_reg_data_DT[get(region) == "England", "pc75_reg"]]
    data_DT[ , pc80_nat := pc_reg_data_DT[get(region) == "England", "pc80_reg"]]
    data_DT[ , pc85_nat := pc_reg_data_DT[get(region) == "England", "pc85_reg"]]
    data_DT[ , pc90_nat := pc_reg_data_DT[get(region) == "England", "pc90_reg"]]
    data_DT[ , pc95_nat := pc_reg_data_DT[get(region) == "England", "pc95_reg"]]
    #Create factor variable showing what 5% wide percentile household is in
    
    data_DT[ , pc5position_reg := ordered(NA, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))]
    
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= 0 & get(dist_var) < pc5_reg), 1, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc5_reg & get(dist_var) < pc10_reg), 2, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc10_reg & get(dist_var) < pc15_reg), 3, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc15_reg & get(dist_var) < pc20_reg), 4, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc20_reg & get(dist_var) < pc25_reg), 5, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc25_reg & get(dist_var) < pc30_reg), 6, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc30_reg & get(dist_var) < pc35_reg), 7, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc35_reg & get(dist_var) < pc40_reg), 8, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc40_reg & get(dist_var) < pc45_reg), 9, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc45_reg & get(dist_var) < pc50_reg), 10, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc50_reg & get(dist_var) < pc55_reg), 11, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc55_reg & get(dist_var) < pc60_reg), 12, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc60_reg & get(dist_var) < pc65_reg), 13, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc65_reg & get(dist_var) < pc70_reg), 14, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc70_reg & get(dist_var) < pc75_reg), 15, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc75_reg & get(dist_var) < pc80_reg), 16, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc80_reg & get(dist_var) < pc85_reg), 17, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc85_reg & get(dist_var) < pc90_reg), 18, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc90_reg & get(dist_var) < pc95_reg), 19, pc5position_reg )]
    data_DT[ , pc5position_reg := ifelse((get(dist_var) >= pc95_reg & get(dist_var) <= max(get(dist_var))), 20, pc5position_reg )]
    
    data_DT[ , pc5_reg := NULL]
    data_DT[ , pc10_reg := NULL]
    data_DT[ , pc15_reg := NULL]
    data_DT[ , pc20_reg := NULL]
    data_DT[ , pc25_reg := NULL]
    data_DT[ , pc30_reg := NULL]
    data_DT[ , pc35_reg := NULL]
    data_DT[ , pc40_reg := NULL]
    data_DT[ , pc45_reg := NULL]
    data_DT[ , pc50_reg := NULL]
    data_DT[ , pc55_reg := NULL]
    data_DT[ , pc60_reg := NULL]
    data_DT[ , pc65_reg := NULL]
    data_DT[ , pc70_reg := NULL]
    data_DT[ , pc75_reg := NULL]
    data_DT[ , pc80_reg := NULL]
    data_DT[ , pc85_reg := NULL]
    data_DT[ , pc90_reg := NULL]
    data_DT[ , pc95_reg := NULL]
    
    
    data_DT[ , pc5position_nat := ordered(NA, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))]
    
    
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= 0 & get(dist_var) < pc5_nat), 1, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc5_nat & get(dist_var) < pc10_nat), 2, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc10_nat & get(dist_var) < pc15_nat), 3, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc15_nat & get(dist_var) < pc20_nat), 4, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc20_nat & get(dist_var) < pc25_nat), 5, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc25_nat & get(dist_var) < pc30_nat), 6, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc30_nat & get(dist_var) < pc35_nat), 7, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc35_nat & get(dist_var) < pc40_nat), 8, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc40_nat & get(dist_var) < pc45_nat), 9, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc45_nat & get(dist_var) < pc50_nat), 10, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc50_nat & get(dist_var) < pc55_nat), 11, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc55_nat & get(dist_var) < pc60_nat), 12, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc60_nat & get(dist_var) < pc65_nat), 13, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc65_nat & get(dist_var) < pc70_nat), 14, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc70_nat & get(dist_var) < pc75_nat), 15, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc75_nat & get(dist_var) < pc80_nat), 16, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc80_nat & get(dist_var) < pc85_nat), 17, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc85_nat & get(dist_var) < pc90_nat), 18, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc90_nat & get(dist_var) < pc95_nat), 19, pc5position_nat )]
    data_DT[ , pc5position_nat := ifelse((get(dist_var) >= pc95_nat & get(dist_var) <= max(get(dist_var))), 20, pc5position_nat )]
    
    data_DT[ , pc5_nat := NULL]
    data_DT[ , pc10_nat := NULL]
    data_DT[ , pc15_nat := NULL]
    data_DT[ , pc20_nat := NULL]
    data_DT[ , pc25_nat := NULL]
    data_DT[ , pc30_nat := NULL]
    data_DT[ , pc35_nat := NULL]
    data_DT[ , pc40_nat := NULL]
    data_DT[ , pc45_nat := NULL]
    data_DT[ , pc50_nat := NULL]
    data_DT[ , pc55_nat := NULL]
    data_DT[ , pc60_nat := NULL]
    data_DT[ , pc65_nat := NULL]
    data_DT[ , pc70_nat := NULL]
    data_DT[ , pc75_nat := NULL]
    data_DT[ , pc80_nat := NULL]
    data_DT[ , pc85_nat := NULL]
    data_DT[ , pc90_nat := NULL]
    data_DT[ , pc95_nat := NULL]
    
    return(data_DT)
  }
  
  else if(perc_width == "10"){
    #quote get(dist_var) for evaluation in data.table environment later 
    #dist_var <- as.character(dist_var)
    #grab region variable name from percentile dataset
    region = colnames(pc_reg_data_DT)[1]
    region_var = quote(colnames(pc_reg_data_DT)[1])
    
    
    setkeyv(pc_reg_data_DT, region)
    setkeyv(data_DT, region)
    
    data_DT <- merge(data_DT, pc_reg_data_DT[ , .SD, .SDcols = c(region, "pc10_reg", "pc20_reg", "pc30_reg", 
                                                                 "pc40_reg", "pc50_reg", "pc60_reg", "pc70_reg", "pc80_reg", "pc90_reg")], 
                     by = region, all.x = TRUE)
    
    
    #Merge percentiles in to data set - bit of a hack, looking up value would be better.
    data_DT[ , pc10_nat := pc_reg_data_DT[get(region) == "England", "pc10_reg"]]
    data_DT[ , pc20_nat := pc_reg_data_DT[get(region) == "England", "pc20_reg"]]
    data_DT[ , pc30_nat := pc_reg_data_DT[get(region) == "England", "pc30_reg"]]
    data_DT[ , pc40_nat := pc_reg_data_DT[get(region) == "England", "pc40_reg"]]
    data_DT[ , pc50_nat := pc_reg_data_DT[get(region) == "England", "pc50_reg"]]
    data_DT[ , pc60_nat := pc_reg_data_DT[get(region) == "England", "pc60_reg"]]
    data_DT[ , pc70_nat := pc_reg_data_DT[get(region) == "England", "pc70_reg"]]
    data_DT[ , pc80_nat := pc_reg_data_DT[get(region) == "England", "pc80_reg"]]
    data_DT[ , pc90_nat := pc_reg_data_DT[get(region) == "England", "pc90_reg"]]
    
    #Create factor variable showing what 5% wide percentile household is in
    
    data_DT[ , pc10position_reg := ordered(NA, levels = c(1,2,3,4,5,6,7,8,9,10))]
    
    data_DT[ , pc10position_reg := ifelse((get(dist_var) >= 0 & get(dist_var) < pc10_reg), 1, pc10position_reg )]
    data_DT[ , pc10position_reg := ifelse((get(dist_var) >= pc10_reg & get(dist_var) < pc20_reg), 2, pc10position_reg )]
    data_DT[ , pc10position_reg := ifelse((get(dist_var) >= pc20_reg & get(dist_var) < pc30_reg), 3, pc10position_reg )]
    data_DT[ , pc10position_reg := ifelse((get(dist_var) >= pc30_reg & get(dist_var) < pc40_reg), 4, pc10position_reg )]
    data_DT[ , pc10position_reg := ifelse((get(dist_var) >= pc40_reg & get(dist_var) < pc50_reg), 5, pc10position_reg )]
    data_DT[ , pc10position_reg := ifelse((get(dist_var) >= pc50_reg & get(dist_var) < pc60_reg), 6, pc10position_reg )]
    data_DT[ , pc10position_reg := ifelse((get(dist_var) >= pc60_reg & get(dist_var) < pc70_reg), 7, pc10position_reg )]
    data_DT[ , pc10position_reg := ifelse((get(dist_var) >= pc70_reg & get(dist_var) < pc80_reg), 8, pc10position_reg )]
    data_DT[ , pc10position_reg := ifelse((get(dist_var) >= pc80_reg & get(dist_var) < pc90_reg), 9, pc10position_reg )]
    data_DT[ , pc10position_reg := ifelse((get(dist_var) >= pc90_reg & get(dist_var) <= max(get(dist_var))), 10, pc10position_reg )]
    

    data_DT[ , pc10_reg := NULL]
    data_DT[ , pc20_reg := NULL]
    data_DT[ , pc30_reg := NULL]
    data_DT[ , pc40_reg := NULL]
    data_DT[ , pc50_reg := NULL]
    data_DT[ , pc60_reg := NULL]
    data_DT[ , pc70_reg := NULL]
    data_DT[ , pc80_reg := NULL]
    data_DT[ , pc90_reg := NULL]
    
    
    data_DT[ , pc10position_nat := ordered(NA, levels = c(1,2,3,4,5,6,7,8,9,10))]
    
    #print(data_DT[ ifelse((get(dist_var) >= 0 & get(dist_var) < pc10_nat), 1, pc10position_nat ), ])
    
    
    data_DT[ , pc10position_nat := ifelse((get(dist_var) >= 0 & get(dist_var) < pc10_nat), 1, pc10position_nat )]
    data_DT[ , pc10position_nat := ifelse((get(dist_var) >= pc10_nat & get(dist_var) < pc20_nat), 2, pc10position_nat )]
    data_DT[ , pc10position_nat := ifelse((get(dist_var) >= pc20_nat & get(dist_var) < pc30_nat), 3, pc10position_nat )]
    data_DT[ , pc10position_nat := ifelse((get(dist_var) >= pc30_nat & get(dist_var) < pc40_nat), 4, pc10position_nat )]
    data_DT[ , pc10position_nat := ifelse((get(dist_var) >= pc40_nat & get(dist_var) < pc50_nat), 5, pc10position_nat )]
    data_DT[ , pc10position_nat := ifelse((get(dist_var) >= pc50_nat & get(dist_var) < pc60_nat), 6, pc10position_nat )]
    data_DT[ , pc10position_nat := ifelse((get(dist_var) >= pc60_nat & get(dist_var) < pc70_nat), 7, pc10position_nat )]
    data_DT[ , pc10position_nat := ifelse((get(dist_var) >= pc70_nat & get(dist_var) < pc80_nat), 8, pc10position_nat )]
    data_DT[ , pc10position_nat := ifelse((get(dist_var) >= pc80_nat & get(dist_var) < pc90_nat), 9, pc10position_nat )]
    data_DT[ , pc10position_nat := ifelse((get(dist_var) >= pc90_nat & get(dist_var) <= max(get(dist_var))), 10, pc10position_nat )]
    
    data_DT[ , pc10_nat := NULL]
    data_DT[ , pc20_nat := NULL]
    data_DT[ , pc30_nat := NULL]
    data_DT[ , pc40_nat := NULL]
    data_DT[ , pc50_nat := NULL]
    data_DT[ , pc60_nat := NULL]
    data_DT[ , pc70_nat := NULL]
    data_DT[ , pc80_nat := NULL]
    data_DT[ , pc90_nat := NULL]
    return(data_DT)
  }
}