#Wrapper function that spits out plots and tables of households characteristics
#Needs all data table prep functions to be run first and data table and survey design of the sub group to be made
make.household.plots <- function(my_design, my_dt, subset, results_dir, my_region = "GVTREGN") {

  #dir.create(results_dir)
  inc_p <- ggplot(my_dt[ ], 
                  aes(x = hh_grossinc_nohb, weight = grossweight)) +
    #geom_freqpoly( aes(group = factor(aff_shared_noincben)))
    geom_freqpoly() + 
    theme( legend.position = "top", panel.background = element_rect(fill = "white"),
           panel.grid.major = element_line(colour = "grey85")) +
    ggtitle("Gross household income - HB: ") +
    xlim(-100, 80000)
  ggsave(filename = paste0(results_dir,"/Gross household income minus HB.png"), plot = inc_p)
  
  t_income<- as.data.frame(svyby(~hh_grossinc_nohb, by = as.formula(paste("~", my_region)), design = my_design,
                                  FUN = svyquantile,
                                  quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))
  write.csv( t_income, file = paste0(results_dir, "/household_income_percentiles.csv"))
  
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
  
  #SELFDEMP of head of households for inactive households
  plot.survey.grpdregion(my_design = subset(my_design, ECOBU == 8), results_dir, "SELFDEMP", var_levels = c("SELFDEMP1", "SELFDEMP2", "SELFDEMP3", "SELFDEMP4", "SELFDEMP5",
                                                                                                           "SELFDEMP6", "SELFDEMP7", "SELFDEMP8", "SELFDEMP9", "SELFDEMP10"),
                         var_labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
                                        "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other"), 
                         p_title = "Self-reported situation, inactive", region = my_region)
  
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
  
  # Can save £10 a month?
  my_design <- update(my_design, ADDMON = factor(ADDMON))
  plot.survey.grpdregion(my_design, results_dir, "ADDMON", var_levels = c("ADDMON1", "ADDMON2", "ADDMON3", "ADDMON4"),
                         var_labels = c("Do this", "Like to but can't afford", "Don't want to",  "Does not apply" ),
                         p_title = "Can save £10 a month", region = my_region)
  
}