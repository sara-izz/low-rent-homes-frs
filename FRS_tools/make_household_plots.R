#Wrapper function that spits out plots and tables of households characteristics
#Needs all data table prep functions to be run first and data table and survey design of the sub group to be made
source("./FRS_tools/calculate_percentile.R")

make.household.plots <- function(my_design, my_dt, subset, results_dir, my_region = "GVTREGN") {

  #This should work but doesn't :/ - dir.create(results_dir)
  
  #Number of group by all regions
  t_regions<- as.data.frame(svytotal(~GVTREGN, design = my_design, ci = TRUE, na.rm = TRUE))
  write.csv( t_regions, file = paste0(results_dir, "/number_by_region.csv"))
  
  #incomes
  inc_p <- ggplot(my_dt[ ], 
                  aes(x = hh_grossinc, weight = grossweight)) +
    #geom_freqpoly( aes(group = factor(aff_shared_noincben)))
    geom_freqpoly() + 
    theme( legend.position = "top", panel.background = element_rect(fill = "white"),
           panel.grid.major = element_line(colour = "grey85")) +
    ggtitle("Gross household income: ") +
    xlim(-100, 80000)
  ggsave(filename = paste0(results_dir,"/Gross household income.png"), plot = inc_p)
  

  t_grossinc <- calculate.percentile.thresholds(my_dt, ~hh_grossinc, ~GVTREGN, "10")
  write.csv( t_grossinc, file = paste0(results_dir, "/hh_grossinc_percentiles.csv"))
 
  t_grossincnoincben <- calculate.percentile.thresholds(my_dt, ~hh_grossinc_noincben, ~GVTREGN, "10") 
  write.csv( t_grossincnoincben, file = paste0(results_dir, "/hh_grossinc_noincben_percentiles.csv"))
  
  t_grossinc_earn<- calculate.percentile.thresholds(my_dt, ~hh_grossinc_earn, ~GVTREGN, "10")
  write.csv( t_grossinc_earn, file = paste0(results_dir, "/hh_grossinc_earn_percentiles.csv"))

  t_grossinc_nohb<- calculate.percentile.thresholds(my_dt, ~hh_grossinc_nohb, ~GVTREGN, "10")
  write.csv( t_grossinc_nohb, file = paste0(results_dir, "/hh_grossinc_nohb_percentiles.csv"))
  
  t_grossinc_grp <- calculate.percentile.thresholds(my_dt, ~hh_grossinc, ~grpd_region1, "10")
  write.csv( t_grossinc_grp, file = paste0(results_dir, "/hh_grossinc_percentiles_grpdreg1.csv"))
  
  t_grossincnoincben_grp <- calculate.percentile.thresholds(my_dt, ~hh_grossinc_noincben, ~grpd_region1, "10") 
  write.csv( t_grossincnoincben_grp, file = paste0(results_dir, "/hh_grossinc_noincben_percentiles_grpdreg1.csv"))
  
  t_grossincnohb_grp <- calculate.percentile.thresholds(my_dt, ~hh_grossinc_nohb, ~grpd_region1, "10")
  write.csv( t_grossinc_grp, file = paste0(results_dir, "/hh_grossincnohb_percentiles_grpdreg1.csv"))
  
  t_grossinceq <- calculate.percentile.thresholds(my_dt, ~hh_grossinc_eq, ~GVTREGN, "10") 
  write.csv( t_grossinceq, file = paste0(results_dir, "/hh_grossinceq_percentiles.csv"))
  
  t_grossinc_earneq<- calculate.percentile.thresholds(my_dt, ~hh_grossinc_earn_eq, ~GVTREGN, "10")
  write.csv( t_grossinc_earneq, file = paste0(results_dir, "/hh_grossinc_earneq_percentiles.csv"))
  
  #Calculate some income means
  plot.survey.grpdregion(my_design, results_dir, "hh_grossinc", p_title = "hh_grossinc_mean", region = "GVTREGN", p_type = "none")
  
  # Age
  my_design <- update(my_design, HDAGE = factor(HDAGE))
  plot.survey.grpdregion(my_design, results_dir, "HDAGE", var_levels = c("HDAGE1", "HDAGE2", "HDAGE3", "HDAGE4", "HDAGE5", "HDAGE6"),
                         var_labels = c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65+"), 
                         p_title = "Age", region = my_region)
  
  plot.survey.grpdregion(my_design, results_dir, "HDAGE", var_levels = c("HDAGE1", "HDAGE2", "HDAGE3", "HDAGE4", "HDAGE5", "HDAGE6"),
                                            var_labels = c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65+"), 
                                            p_title = "Age bar", region = my_region, p_type = "bar")
  
  # Self-reported employment status of head of household
  my_design <- update(my_design, SELFDEMP = factor(SELFDEMP))
  plot.survey.grpdregion(my_design, results_dir, "SELFDEMP", var_levels = c("SELFDEMP1", "SELFDEMP2", "SELFDEMP3", "SELFDEMP4", "SELFDEMP5",
                                                                           "SELFDEMP6", "SELFDEMP7", "SELFDEMP8", "SELFDEMP9", "SELFDEMP10"),
                         var_labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
                                        "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other"), 
                         p_title = "Self-reported situation", region = my_region)
  
  # Employment of whole household
  my_design <- update(my_design, ECOBU = factor(ECOBU))
  plot.survey.grpdregion(my_design, results_dir, "ECOBU", var_levels = c("ECOBU1", "ECOBU2", "ECOBU3", "ECOBU4", "ECOBU5", "ECOBU6", "ECOBU7",
                                                                                             "ECOBU8"),
                                              var_labels = c("1+ self employed", "Sing/couple all FT", 
                                                             "Couple, 1 FT, 1 PT",  "Couple, 1 FT, 1 not working", 
                                                             "No FT, 1+ PT", "Workless, head/spouse aged 60+", 
                                                             "Workless, head/spouse unemployed", "Workless, inactive" ), 
                                              p_title = "Employment of household", region = my_region, p_type = "hbar")
  
  # #SELFDEMP of head of households for inactive households
  # plot.survey.grpdregion(my_design = subset(my_design, ECOBU == 8), results_dir, "SELFDEMP", var_levels = c("SELFDEMP1", "SELFDEMP2", "SELFDEMP3", "SELFDEMP4", "SELFDEMP5",
  #                                                                                                          "SELFDEMP6", "SELFDEMP7", "SELFDEMP8", "SELFDEMP9", "SELFDEMP10"),
  #                        var_labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
  #                                       "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other"), 
  #                        p_title = "Self-reported situation, inactive", region = my_region)
  
  #ILO employment of head of household
  my_design <- update(my_design, DVIL04A = factor(DVIL04A))
  plot.survey.grpdregion(my_design, results_dir, "DVIL04A", var_levels = c("DVIL04A1", "DVIL04A2", "DVIL04A3", "DVIL04A4"),
                         var_labels = c("Employed", "Family worker", "Unemployed", "Inactive"),
                         p_title = "HRP employment (ILO)", region = my_region)

  
  
  # School age kids
  my_design <- update(my_design, has_kids16 = factor(has_kids16))
  plot.survey.grpdregion(my_design, results_dir, "has_kids16", var_levels = c("has_kids160", "has_kids161"),
                         var_labels = c("No school age kids", "Has school age kids"), 
                         p_title = "Has school age kids", region = my_region)
  
  # Number of bedrooms needed
  my_design <- update(my_design, bedrooms_needed = factor(bedrooms_needed))
  plot.survey.grpdregion(my_design, results_dir, "bedrooms_needed", 
                         var_levels = c("bedrooms_needed1", "bedrooms_needed2", "bedrooms_needed3", "bedrooms_needed4", 
                                        "bedrooms_needed5", "bedrooms_needed6"),
                         var_labels = c("1", "2", "3", "4", "5", "6"), 
                         p_title = "Number of bedrooms needed", region = my_region)
  
  # Number of bedrooms needed
  my_design <- update(my_design, FAMTYPBU = factor(FAMTYPBU))
  plot.survey.grpdregion(my_design, results_dir, "FAMTYPBU", 
                         var_levels = c("FAMTYPBU0", "FAMTYPBU1", "FAMTYPBU2", "FAMTYPBU3", "FAMTYPBU4", 
                                        "FAMTYPBU5", "FAMTYPBU6"),
                         var_labels = c("Other", "Pen couple", "Pen single", "Couple + kids", "Couple", "Lone parent", "Single"), 
                         p_title = "Family type", region = my_region)
  
  # Single parents
  my_design <- update(my_design, single_parent = factor(single_parent))
  plot.survey.grpdregion(my_design, results_dir, "single_parent", var_levels = c("single_parent0", "single_parent1"),
                         var_labels = c("Not Single parent head", "Single parent head "), 
                         p_title = "Single parent", region = my_region)
  
  # disabled adult or child
  my_design <- update(my_design, has_disabled = ifelse((has_disabledad ==1 | has_disabledch == 1), 1, 0))
  my_design <- update(my_design, has_disabled = factor(has_disabled))
  plot.survey.grpdregion(my_design, results_dir, "has_disabled", var_levels = c("has_disabled0", "has_disabled1"),
                         var_labels = c("No disabled", "Has disabled person"), 
                         p_title = "Has disabled adult or child", region = my_region)
  
  # Someone in household gives care to someone else in household
  my_design <- update(my_design, has_carer = factor(has_carer))
  plot.survey.grpdregion(my_design, results_dir, "has_carer", var_levels = c("has_carerFALSE", "has_carerTRUE"),
                         var_labels = c("No carer", "Has carer"), 
                         p_title = "Has carer", region = my_region)
  
  # Someone in household gives care to someone outside household
  my_design <- update(my_design, GIVEHELP = factor(GIVEHELP))
  plot.survey.grpdregion(my_design, results_dir, "GIVEHELP", var_levels = c("GIVEHELP1", "GIVEHELP2"),
                         var_labels = c("Helps outside", "Doesn't"), 
                         p_title = "Help given outside hhld", region = my_region)
  
  
  #savings
  t_savings<- as.data.frame(svyby(~TOTCAPB3, by = as.formula(paste("~", my_region)), design = my_design,
                                      FUN = svyquantile,
                                      quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

  t_nosavings <- as.data.frame(svyby(~(TOTCAPB3==0), by =as.formula(paste("~", my_region)), design = my_design,
                                         FUN = svymean, na.rm = TRUE))
  
  write.csv( t_savings, file = paste0(results_dir, "/household_savings_percentiles.csv"))
  write.csv( t_nosavings, file = paste0(results_dir, "/household_nosavings.csv"))
  
  # Can save ?10 a month?
  my_design <- update(my_design, ADDMON = factor(ADDMON))
  plot.survey.grpdregion(my_design, results_dir, "ADDMON", var_levels = c("ADDMON1", "ADDMON2", "ADDMON3", "ADDMON4"),
                         var_labels = c("Do this", "Like to but can't afford", "Don't want to",  "Does not apply" ),
                         p_title = "Can save £10 a month", region = my_region)
  
}