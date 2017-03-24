#Calculates percentile values for a continuous FRS variable 
#Only works if calculate.logical.flags and calculate.weights run on dataset first
#Distribution variable must be input as a function (i.e. beginning with ~)
calculate.percentile.thresholds <- function( DT, distr_var_form, region_form, perc_width){
  
  if(perc_width == "5"){
    #survey design using FRS gross hhld weights
    my_des <- svydesign(ids = ~1, weights = ~grossweight, data = DT[is_HRP & (is.na(grossweight) == F), ])
    
    
    pc_reg_dt <- as.data.table(svyby(distr_var_form, by = region_form, design = my_des, FUN = svyquantile,
                                     quantiles = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6,
                                                   0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95), ci = TRUE))
    
    
    pc_eng_dt <- as.data.table(svyquantile(distr_var_form, design = my_des,
                                           quantiles = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6,
                                                         0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95), na.rm = TRUE))
    
    
    region_name<-unlist(strsplit(as.character(region_form), ",", fixed = TRUE))
    region = region_name[2]
    
    pc_eng_dt[ , c(region) := "England"]
    
    l <- list(pc_reg_dt, pc_eng_dt)
    pc_reg_dt <- rbindlist(l, fill = TRUE)
    
    
    colnames(pc_reg_dt)[2] <- "pc5_reg"
    colnames(pc_reg_dt)[3] <- "pc10_reg"
    colnames(pc_reg_dt)[4] <- "pc15_reg"
    colnames(pc_reg_dt)[5] <- "pc20_reg"
    colnames(pc_reg_dt)[6] <- "pc25_reg"
    colnames(pc_reg_dt)[7] <- "pc30_reg"
    colnames(pc_reg_dt)[8] <- "pc35_reg"
    colnames(pc_reg_dt)[9] <- "pc40_reg"
    colnames(pc_reg_dt)[10] <- "pc45_reg"
    colnames(pc_reg_dt)[11] <- "pc50_reg"
    colnames(pc_reg_dt)[12] <- "pc55_reg"
    colnames(pc_reg_dt)[13] <- "pc60_reg"
    colnames(pc_reg_dt)[14] <- "pc65_reg"
    colnames(pc_reg_dt)[15] <- "pc70_reg"
    colnames(pc_reg_dt)[16] <- "pc75_reg"
    colnames(pc_reg_dt)[17] <- "pc80_reg"
    colnames(pc_reg_dt)[18] <- "pc85_reg"
    colnames(pc_reg_dt)[19] <- "pc90_reg"
    colnames(pc_reg_dt)[20] <- "pc95_reg"
    
    
    return(pc_reg_dt)
  }
  
  else if(perc_width == "10"){
    #survey design using FRS gross hhld weights
    my_des <- svydesign(ids = ~1, weights = ~grossweight, data = DT[is_HRP & (is.na(grossweight) == F), ])
    
    
    pc_reg_dt <- as.data.table(svyby(distr_var_form, by = region_form, design = my_des, FUN = svyquantile,
                                     quantiles = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), ci = TRUE))
    
    
    pc_eng_dt <- as.data.table(svyquantile(distr_var_form, design = my_des,
                                           quantiles = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm = TRUE))
    
    
    region_name<-unlist(strsplit(as.character(region_form), ",", fixed = TRUE))
    region = region_name[2]
    
    pc_eng_dt[ , c(region) := "England"]
    
    l <- list(pc_reg_dt, pc_eng_dt)
    pc_reg_dt <- rbindlist(l, fill = TRUE)
    
    
    
    colnames(pc_reg_dt)[2] <- "pc10_reg"
    colnames(pc_reg_dt)[3] <- "pc20_reg"
    colnames(pc_reg_dt)[4] <- "pc30_reg"
    colnames(pc_reg_dt)[5] <- "pc40_reg"
    colnames(pc_reg_dt)[6] <- "pc50_reg"
    colnames(pc_reg_dt)[7] <- "pc60_reg"
    colnames(pc_reg_dt)[8] <- "pc70_reg"
    colnames(pc_reg_dt)[9] <- "pc80_reg"
    colnames(pc_reg_dt)[10] <- "pc90_reg"
    
    
    return(pc_reg_dt)
  }

}