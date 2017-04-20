########## JAM analysis###########
### Defining and understanding who is 'Just About Managing' ### 
### Uses Family Resources Survey and Households Below Average Income #####
### Author: sara_mahmoud@shelter.org.uk####

#set wd to where this is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#If packages not found, do install.packages("foo") - only necessary first time/if something not working
install.packages("devtools")
require("devtools")
install.packages("sqldf")
#install.packages("data.table")
install_github("Rdatatable/data.table") #Use dev version on Github
install.packages("sjPlot")
install.packages("reshape2")
install.packages("survey")

#Do this on each re-load
require(sjPlot)
require(ggplot2)
require(RColorBrewer)
library("reshape2")
require("survey")


### Source and define calculating functions ####

## Make flatfiles for each year
source("./FRS_tools/make_flatfile.R")
##Logical flags for later analysis #
source("./FRS_tools/calculate_logical_flags.R")
## Calculate deflated household incomes 
source("./FRS_tools/calculate_household_incomes.R")
source("./FRS_tools/calculate_weights.R")
## Tidy up tenure 
source("./FRS_tools/tidy_tenure.R")
## Tidy up regions 
source("./FRS_tools/tidy_regions.R")
## Region comparison plotting function
source("./FRS_tools/plot_survey_grpdregion.R")
## Calculate equivalising weights
source("./FRS_tools/calculate_equivalise.R")
## Calculate percentile values for a continuous distribution
source("./FRS_tools/calculate_percentile.R")
## Calculate percentile values for a continuous distribution
source("./FRS_tools/assign_percentile.R")
## Append 2015 MIS amounts
source("./FRS_tools/append_mis.R")
#make plots of household charactersitics
source("./FRS_tools/make_household_plots.R")
#compare charactersitics of JAM groups
source("./FRS_tools/compare_JAM_plots.R")
#compare charactersitics of all LIPR groups
source("./FRS_tools/compare_LIPR_plots.R")
source("./FRS_tools/compare_LIPR_plots_refine.R")
source("./FRS_tools/calculate_bedrooms.R")

flag.shared.ownership <- function (data_table){
  shared_thresh <- read.csv("./shared_ownership_13141415thresholds.csv", header = TRUE, 
                            colClasses = c("double", "double", "double", "character"), stringsAsFactors = FALSE)
  shared_thresh <- data.table(shared_thresh)
  
  #return(shared_thresh)
   shared_keycols = c("GVTREGN", "year")
   shared_cols = c("GVTREGN", "shared", "shared_deposit", "year" )
  
   setkeyv(data_table, shared_keycols)
   setkeyv(shared_thresh, shared_keycols)

   data_table <- merge(data_table , shared_thresh[ , .SD, .SDcols = shared_cols], by = shared_keycols, all.x = TRUE)

   data_table[ , aff_shared_noincben := ifelse( hh_grossinc_noincben >= shared, 1, 0 )]
  
   data_table[ , cant_shareddep := ifelse( (TOTCAPB3 < (shared_deposit - 1000)) & (ADDMON == 2 | behind_debts==1), 1, 0 )]
  
   #flag for shared ownership situation
   data_table[ , affanddep_shared := 0]
   data_table[ , affanddep_shared := ifelse(aff_shared_noincben==1 & cant_shareddep==1, 1 , 0)]
   data_table[ , affanddep_shared := ifelse(aff_shared_noincben==1 & cant_shareddep==0, 2 , affanddep_shared)]
   data_table[ , affanddep_shared := ifelse(aff_shared_noincben==0 , 0 , affanddep_shared)]
  
  
   return(data_table)

}


### Make flatfiles ####
survey_dir <- "S:/@Communications, Policy and Campaigns/Research/STATS & INFO/Statistics/Household Surveys/"

dt_1415 <- make.flatfile(survey_dir, "1415")
dt_1314 <- make.flatfile(survey_dir, "1314")
dt_1415[ , year := "1415"]
dt_1314[ , year := "1314"]

l <- list(dt_1314, dt_1415)
dt_1315 <- rbindlist(l, fill = TRUE)
rm(l)

#Prep 1315 data set ####
dt_1315 <- calculate.logical.flags(dt_1315, "1315")
dt_1315 <- calculate.weights(dt_1315, 1)
dt_1315 <- tidy.tenure(dt_1315)
#Calculate equivalisation weights (according to HBAI spec)
dt_1315<- calculate.equivalise(dt_1315)
#calculate income, including equivalised
dt_1315 <- calculate.household.incomes(dt_1315, "FRS", TRUE)
dt_1315 <- flag.shared.ownership(dt_1315)
dt_1315 <- calculate.bedrooms(dt_1315)
dt_1315 <- tidy.regions(dt_1315, "1315")

dt_1315[ , not_1stbenu := ifelse(BENUNIT >1 , 1, 0)]
dt_1315[ , multiple_benu := ifelse( (sum(not_1stbenu) > 0), 1, 0 ), by = .(SERNUM, year) ]

#LIPR definition flags
#more than 20% of household's gross income from benefits (excluding tax credits) - same as Resolution Foundation definition
dt_1315[ , over20gross_allben := ifelse(((HBENINC - HHTXCRED )/(HHINC)) > 0.2, 1, 0)]

#more than 20% of household income-HB comes from non-HB benefits, modified RF definition
dt_1315[ , over20grossnohb_nohbben := ifelse(!is.na(hh_hbweekly), 
                                             (ifelse((HBENINC-hh_hbweekly - HHTXCRED)/(HHINC-hh_hbweekly) > 0.2, 1, 0)),
                                             (ifelse((HBENINC- HHTXCRED)/(HHINC) > 0.2, 1, 0)))]

#remove households from sample with shared status and lodgers
dt1315_noshare <- dt_1315[ (has_lodger == 0 & HHSTAT == 1), ]
#select households with only 1 benefit unit - necessary for current shared ownership/MIS definitions of low income
dt1315_noshare_1bu <- dt1315_noshare[ multiple_benu == 0, ]

#Append AHC Minimum Income Standard for households - only works for 1 benefit unit households atm
dt1315_noshare_1bu <- append.mis(dt1315_noshare_1bu, "./MIS_2015.csv")

#Add for if AHC income is less than AHC MIS
dt1315_noshare_1bu[ , under_ahc_mis := ifelse(hh_gross_ahc < ahc_mis*52, 1, 0)]

#Restrict to households not headed by a pensioner 
dt1315_noshare_1bu_nopen <- dt1315_noshare_1bu[ head_pensioner == 0 & is_HRP ==1 & (is.na(grossweight) == F), ]

des1315_nosharenopen <- svydesign(ids = ~1, weights = ~grossweight, data = dt1315_noshare_1bu_nopen[is_HRP & (is.na(grossweight) == F), ])

#Characteristics by region - Less than AHC MIS private renters  ######


#Characteristics by grouped region for 2014/15
make.household.plots(my_design = subset(des1315_nosharenopen, grpd_tenure == 1 & under_ahc_mis == 1 & year == "1415"), 
                     my_dt = dt1315_noshare_1bu_nopen[ grpd_tenure == 1 & under_ahc_mis == 1 & year == "1415"], 
                     "PRS, no pensioner HRP, under MIS AHC", 
                     results_dir = "./Results/prs_nopen_underMIS_1415", my_region = "grpd_region1" )

#check sample size
t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & under_ahc_mis == 1 & year == "1415", .N, by=.(GVTREGN)]
write.csv( t_samplesize, file = "./Results/prs_nopen_underMIS_1415/subsample_size.csv")

t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & under_ahc_mis == 1 & year == "1415", .N, by=.(grpd_region1)]
write.csv( t_samplesize, file = "./Results/prs_nopen_underMIS_1415/subsample_size_grpdregion1.csv")

#total number of households, % of working age, % of all households
svytotal(~(grpd_tenure == 1 & under_ahc_mis == 1), design = subset(des1315_nosharenopen, year == "1415"), na.rm = TRUE)
svymean(~(grpd_tenure == 1 & under_ahc_mis == 1), design = subset(des1315_nosharenopen, year == "1415"), na.rm = TRUE)

#Characteristics by grouped region for 2013/14
make.household.plots(my_design = subset(des1315_nosharenopen, grpd_tenure == 1 & under_ahc_mis == 1 & year == "1314"), 
                     my_dt = dt1315_noshare_1bu_nopen[ grpd_tenure == 1 & under_ahc_mis == 1 & year == "1314"], 
                     "PRS, no pensioner HRP, under MIS AHC", 
                     results_dir = "./Results/prs_nopen_underMIS_1314", my_region = "grpd_region1" )

t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & under_ahc_mis == 1 & year == "1314", .N, by=.(GVTREGN)]
write.csv( t_samplesize, file = "./Results/prs_nopen_underMIS_1314/subsample_size.csv")

t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & under_ahc_mis == 1 & year == "1314", .N, by=.(grpd_region1)]
write.csv( t_samplesize, file = "./Results/prs_nopen_underMIS_1314/subsample_size_grpdregion1.csv")

#Characteristics by region - Renting households who can't afford shared ownership ####

#Characteristics by grouped region for 2014/15
make.household.plots(my_design = subset(des1315_nosharenopen, grpd_tenure == 1 & aff_shared_noincben ==0 & year == "1415"), 
                     my_dt = dt1315_noshare_1bu_nopen[ grpd_tenure == 1 & aff_shared_noincben ==0 & year == "1415"], 
                     "PRS, no pensioner HRP, can't afford shared ownership", 
                     results_dir = "./Results/prs_nopen_noshared_1415", my_region = "grpd_region1" )

# Additional can't afford shared ownership stats
des1315_nosharenopen <- update(des1315_nosharenopen, BURDEN = factor(BURDEN))
plot.survey.grpdregion(my_design = subset(des1315_nosharenopen, 
                                          aff_shared_noincben ==0 & grpd_tenure == 1 & year == "1415"), 
                       "./Results/prs_nopen_noshared_1415", "BURDEN", 
                       var_levels = c("BURDEN1", "BURDEN2", "BURDEN3"),
                       var_labels = c("Heavy burden", "Slight burden", "Not a burden"),
                       p_title = "Housing costs a burden", region = "grpd_region1")

#no savings all england
svymean(~(TOTCAPB3==0), design = subset(des1315_nosharenopen, 
                                          aff_shared_noincben ==0 & grpd_tenure == 1 & year == "1415"), na.rm = TRUE)
svytotal(~(TOTCAPB3==0), design = subset(des1315_nosharenopen, 
                                        aff_shared_noincben ==0 & grpd_tenure == 1 & year == "1415"), na.rm = TRUE)
#Behind on rent
svymean(~(DEBTFRE1==1 |DEBTFRE1==2), design = subset(des1315_nosharenopen, 
                                        aff_shared_noincben ==0 & grpd_tenure == 1 & year == "1415"), na.rm = TRUE)

#Behind on utilities

svymean(~(DEBTFRE2==1 |DEBTFRE2==2), design = subset(des1315_nosharenopen, 
                                                     aff_shared_noincben ==0 & grpd_tenure == 1 & year == "1415"), na.rm = TRUE)


#Behind on other loan repayments
svymean(~(DEBTFRE3==1 |DEBTFRE3==2), design = subset(des1315_nosharenopen, 
                                                     aff_shared_noincben ==0 & grpd_tenure == 1 & year == "1415"), na.rm = TRUE)

#Additional CAN afford shared ownership stats
svyquantile(~TOTCAPB3, design = subset(des1315_nosharenopen, 
                                       aff_shared_noincben ==1 & grpd_tenure == 1 & year == "1415"), 
            quantiles = 0.5, na.rm = TRUE, ci = TRUE)

#Behind on rent
svymean(~(DEBTFRE1==1 |DEBTFRE1==2), design = subset(des1315_nosharenopen, 
                                                     aff_shared_noincben ==1 & grpd_tenure == 1 & year == "1415"), na.rm = TRUE)
svytotal(~(DEBTFRE1==1 |DEBTFRE1==2), design = subset(des1315_nosharenopen, 
                                                     aff_shared_noincben ==1 & grpd_tenure == 1 & year == "1415"), na.rm = TRUE)

#Behind on utilities

svymean(~(DEBTFRE2==1 |DEBTFRE2==2), design = subset(des1315_nosharenopen, 
                                                     aff_shared_noincben ==1 & grpd_tenure == 1 & year == "1415"), na.rm = TRUE)
svytotal(~(DEBTFRE2==1 |DEBTFRE2==2), design = subset(des1315_nosharenopen, 
                                                      aff_shared_noincben ==1 & grpd_tenure == 1 & year == "1415"), na.rm = TRUE)


#Behind on other loan repayments
svymean(~(DEBTFRE3==1 |DEBTFRE3==2), design = subset(des1315_nosharenopen, 
                                                     aff_shared_noincben ==1 & grpd_tenure == 1 & year == "1415"), na.rm = TRUE)
svytotal(~(DEBTFRE3==1 |DEBTFRE3==2), design = subset(des1315_nosharenopen, 
                                                      aff_shared_noincben ==1 & grpd_tenure == 1 & year == "1415"), na.rm = TRUE)

#Ability to save �10 a month
des1315_nosharenopen <- update(des1315_nosharenopen, ADDMON = factor(ADDMON))
svymean(~ADDMON, design = subset(des1315_nosharenopen, 
                                                     aff_shared_noincben ==1 & grpd_tenure == 1 & year == "1415"), na.rm = TRUE)


#Characteristics by grouped region for 2013/14
make.household.plots(my_design = subset(des1315_nosharenopen, grpd_tenure == 1 & aff_shared_noincben ==0  & year == "1314"), 
                     my_dt = dt1315_noshare_1bu_nopen[ grpd_tenure == 1 & aff_shared_noincben ==0 & year == "1314"], 
                     "PRS, no pensioner HRP, can't afford shared ownership", 
                     results_dir = "./Results/prs_nopen_noshared_1314", my_region = "grpd_region1" )
#check sample size
t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & aff_shared_noincben ==0 & year == "1314", .N, by=.(GVTREGN)]
write.csv( t_samplesize, file = "./Results/prs_nopen_noshared_1314/subsample_size.csv")

#Characteristics by region - Renting households who can't afford shared ownership and have <20% from non-HB benefits####

#Characteristics by grouped region for 2014/15
make.household.plots(my_design = subset(des1315_nosharenopen, 
                                        grpd_tenure == 1 & aff_shared_noincben ==0 & over20grossnohb_nohbben == 0 & year == "1415"), 
                     my_dt = dt1315_noshare_1bu_nopen[ grpd_tenure == 1 & aff_shared_noincben ==0 
                                                       & over20grossnohb_nohbben == 0 & year == "1415"], 
                     "PRS, no pensioner HRP, can't afford shared ownership", 
                     results_dir = "./Results/prs_nopen_nosharedbenlim_1415", my_region = "grpd_region1" )

#check sample size
t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & aff_shared_noincben ==0 & over20grossnohb_nohbben == 0 & year == "1415",
                                         .N, by=.(GVTREGN)]
write.csv( t_samplesize, file = "./Results/prs_nopen_nosharedbenlim_1415/subsample_size.csv")

t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & aff_shared_noincben ==0 & over20grossnohb_nohbben == 0 & year == "1415",
                                         .N, by=.(grpd_region1)]
write.csv( t_samplesize, file = "./Results/prs_nopen_nosharedbenlim_1415/subsample_size_grpdregion1.csv")

#Characteristics by grouped region for 2013/14
make.household.plots(my_design = subset(des1315_nosharenopen, 
                                        grpd_tenure == 1 & aff_shared_noincben ==0 & over20grossnohb_nohbben == 0 & year == "1314"), 
                     my_dt = dt1315_noshare_1bu_nopen[ grpd_tenure == 1 
                                                       & aff_shared_noincben ==0 & over20grossnohb_nohbben == 0 & year == "1314"], 
                     "PRS, no pensioner HRP, can't afford shared ownership", 
                     results_dir = "./Results/prs_nopen_nosharedbenlim_1314", my_region = "grpd_region1" )
#check sample size
t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & aff_shared_noincben ==0 
                                         & over20grossnohb_nohbben == 0 & year == "1314", .N, by=.(GVTREGN)]
write.csv( t_samplesize, file = "./Results/prs_nopen_nosharedbenlim_1314/subsample_size.csv")

t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & aff_shared_noincben ==0 
                                         & over20grossnohb_nohbben == 0 & year == "1314", .N, by=.(grpd_region1)]
write.csv( t_samplesize, file = "./Results/prs_nopen_nosharedbenlim_1314/subsample_size_grpdregion1.csv")



#Prop and total renting households who can't afford shared ownership (no sharers, 1 BU only)
# svymean(~(aff_shared_noincben == 0 & grpd_tenure == 1), design = subset(des1315_noshare_1bu, head_pensioner == 0), na.rm=TRUE)
# svytotal(~(aff_shared_noincben == 0 & grpd_tenure == 1), design = subset(des1315_noshare_1bu, head_pensioner == 0), na.rm=TRUE)
# 
# svymean(~aff_shared_noincben == 0, design = subset(des1315_noshare_1bu, grpd_tenure == 1 &  head_pensioner == 0), na.rm=TRUE)
# svytotal(~aff_shared_noincben == 0, design = subset(des1315_noshare_1bu, grpd_tenure == 1 &  head_pensioner == 0), na.rm=TRUE)
# 



#Characteristics by region - Renting households who can't afford shared ownership and HRP is working####

#Characteristics by grouped region for 2014/15
make.household.plots(my_design = subset(des1315_nosharenopen, 
                                        grpd_tenure == 1 & aff_shared_noincben ==0 & hrp_nowork == FALSE & year == "1415"), 
                     my_dt = dt1315_noshare_1bu_nopen[ grpd_tenure == 1 & aff_shared_noincben ==0 
                                                       & hrp_nowork == FALSE & year == "1415"], 
                     "PRS, no pensioner HRP, can't afford shared ownership", 
                     results_dir = "./Results/prs_nopen_nosharedhrpwork_1415", my_region = "grpd_region1" )

#check sample size
t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & aff_shared_noincben ==0 & hrp_nowork == FALSE & year == "1415",
                                         .N, by=.(GVTREGN)]
write.csv( t_samplesize, file = "./Results/prs_nopen_nosharedhrpwork_1415/subsample_size.csv")

t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & aff_shared_noincben ==0 & hrp_nowork == FALSE & year == "1415",
                                         .N, by=.(grpd_region1)]
write.csv( t_samplesize, file = "./Results/prs_nopen_nosharedhrpwork_1415/subsample_size_grpdregion1.csv")

#Characteristics by grouped region for 2013/14
make.household.plots(my_design = subset(des1315_nosharenopen, 
                                        grpd_tenure == 1 & aff_shared_noincben ==0 & hrp_nowork == FALSE & year == "1314"), 
                     my_dt = dt1315_noshare_1bu_nopen[ grpd_tenure == 1 
                                                       & aff_shared_noincben ==0 & hrp_nowork == FALSE& year == "1314"], 
                     "PRS, no pensioner HRP, can't afford shared ownership", 
                     results_dir = "./Results/prs_nopen_nosharedhrpwork_1314", my_region = "grpd_region1" )
#check sample size
t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & aff_shared_noincben ==0 
                                         & hrp_nowork == FALSE & year == "1314", .N, by=.(GVTREGN)]
write.csv( t_samplesize, file = "./Results/prs_nopen_nosharedhrpwork_1314/subsample_size.csv")

t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & aff_shared_noincben ==0 
                                         & hrp_nowork == FALSE & year == "1314", .N, by=.(grpd_region1)]
write.csv( t_samplesize, file = "./Results/prs_nopen_nosharedhrpwork_1314/subsample_size_grpdregion1.csv")





#Characteristics by region - Renting households who can't afford shared ownership and households is not workless ####
# This is the definition used to calculate typical incomes for LIPR##
#check sample size
t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & aff_shared_noincben ==0 & ECOBU!=6 & ECOBU!=7 & ECOBU!=8 & year == "1415",
                                         .N, by=.(GVTREGN)]
write.csv( t_samplesize, file = "./Results/prs_nopen_nosharednotworkless_1415/subsample_size.csv")

t_samplesize <- dt1315_noshare_1bu_nopen[grpd_tenure == 1 & aff_shared_noincben ==0 & ECOBU!=6 & ECOBU!=7 & ECOBU!=8 & year == "1415",
                                         .N, by=.(grpd_region1)]
write.csv( t_samplesize, file = "./Results/prs_nopen_nosharednotworkless_1415/subsample_size_grpdregion1.csv")

#Characteristics by grouped region for 2014/15
make.household.plots(my_design = subset(des1315_nosharenopen, 
                                        grpd_tenure == 1 & aff_shared_noincben ==0 & ECOBU!=6 & ECOBU!=7 & ECOBU!=8 & year == "1415"), 
                     my_dt = dt1315_noshare_1bu_nopen[ grpd_tenure == 1 & aff_shared_noincben ==0 
                                                       & ECOBU!=6 & ECOBU!=7 & ECOBU!=8 & year == "1415"], 
                     "PRS, no pensioner HRP, can't afford shared ownership", 
                     results_dir = "./Results/prs_nopen_nosharednotworkless_1415", my_region = "grpd_region1" )

#Characteristics by grouped region for 2013/14
make.household.plots(my_design = subset(des1315_nosharenopen, 
                                        grpd_tenure == 1 & aff_shared_noincben ==0 & ECOBU!=6 & ECOBU!=7 & ECOBU!=8 & year == "1314"), 
                     my_dt = dt1315_noshare_1bu_nopen[ grpd_tenure == 1 
                                                       & aff_shared_noincben ==0 & ECOBU!=6 & ECOBU!=7 & ECOBU!=8 & year == "1314"], 
                     "PRS, no pensioner HRP, can't afford shared ownership", 
                     results_dir = "./Results/prs_nopen_nosharednotworkless_1314", my_region = "grpd_region1" )





#More LIPR compare plots, this time cf to all working age households ####
des1315_nosharenopen <- update(des1315_nosharenopen, GVTREGN = factor(GVTREGN ))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./Results/nopen_lowincprs_morecompare/", variable = "GVTREGN", 
                          var_levels = c("GVTREGNNorth East", "GVTREGNNorth West", "GVTREGNYorks & Humber", "GVTREGNEast Midlands", 
                                         "GVTREGNWest Midlands", "GVTREGNEast", "GVTREGNLondon", "GVTREGNSouth East", "GVTREGNSouth West"),
                          var_labels = c("North East", "North West", "Yorks & Humber", "East Midlands", 
                                         "West Midlands", "East", "London", "South East", "South West"),
                          p_title = "Region - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, HDAGE = factor(HDAGE))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./Results/nopen_lowincprs_morecompare/", variable = "HDAGE", 
                          var_levels = c("HDAGE1", "HDAGE2", "HDAGE3", "HDAGE4", "HDAGE5", "HDAGE6"),
                          var_labels = c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65+"),
                          p_title = "Age - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, DVIL04A = factor(DVIL04A))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./Results/nopen_lowincprs_morecompare/", variable = "DVIL04A", 
                          var_levels = c("DVIL04A1", "DVIL04A2", "DVIL04A3", "DVIL04A4"),
                          var_labels = c("Employed", "Family worker", "Unemployed", "Inactive"), 
                          p_title = "HRP employment (ILO) - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, ECOBU = factor(ECOBU))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./Results/nopen_lowincprs_morecompare/", variable = "ECOBU", 
                          var_levels = c("ECOBU1", "ECOBU2", "ECOBU3", "ECOBU4", "ECOBU5", "ECOBU6", "ECOBU7", "ECOBU8"),
                          var_labels = c("1+ self employed", "Sing/couple all FT", 
                                         "Couple, 1 FT, 1 PT",  "Couple, 1 FT, 1 not working", 
                                         "No FT, 1+ PT", "Workless, head/spouse aged 60+", 
                                         "Workless, head/spouse unemployed", "Workless, inactive" ),
                          p_title = "Employment of household - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, SELFDEMP = factor(SELFDEMP))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./Results/nopen_lowincprs_morecompare/", variable = "SELFDEMP", 
                          var_levels = c("SELFDEMP1", "SELFDEMP2", "SELFDEMP3", "SELFDEMP4", "SELFDEMP5","SELFDEMP6", "SELFDEMP7",
                                         "SELFDEMP8", "SELFDEMP9", "SELFDEMP10"),
                          var_labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
                                         "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other"),
                          p_title = "Self-reported HRP situation - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, has_kids16 = factor(has_kids16))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./Results/nopen_lowincprs_morecompare/", variable = "has_kids16", 
                          var_levels = c("has_kids160", "has_kids161"),
                          var_labels = c("No school age kids", "Has school age kids"), 
                          p_title = "Has children - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, bedrooms_needed = factor(bedrooms_needed))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./Results/nopen_lowincprs_morecompare/", variable = "bedrooms_needed", 
                          var_levels = c("bedrooms_needed1", "bedrooms_needed2", "bedrooms_needed3", "bedrooms_needed4", 
                                         "bedrooms_needed5", "bedrooms_needed6"),
                          var_labels = c("1", "2", "3", "4", "5", "6"), 
                          p_title = "Bedrooms needed", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, FAMTYPBU = factor(FAMTYPBU))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./Results/nopen_lowincprs_morecompare/", variable = "FAMTYPBU", 
                          var_levels = c("FAMTYPBU0", "FAMTYPBU1", "FAMTYPBU2", "FAMTYPBU3", "FAMTYPBU4", 
                                         "FAMTYPBU5", "FAMTYPBU6"),
                          var_labels = c("Other", "Pen couple", "Pen single", "Couple + kids", "Couple", "Lone parent", "Single"), 
                          p_title = "Family type", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, single_parent = factor(single_parent))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./Results/nopen_lowincprs_morecompare/", variable = "single_parent", 
                          var_levels = c("single_parent0", "single_parent1"),
                          var_labels = c("Not Single parent head", "Single parent head "),
                          p_title = "Is single parent - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, has_disabled = ifelse((has_disabledad ==1 | has_disabledch == 1), 1, 0))
des1315_nosharenopen <- update(des1315_nosharenopen, has_disabled = factor(has_disabled))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./Results/nopen_lowincprs_morecompare/", variable = "has_disabled", 
                          var_levels = c("has_disabled0", "has_disabled1"),
                          var_labels = c("No disabled", "Has disabled person"), 
                          p_title = "Has disabled adult or child - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, no_savings = ifelse(TOTCAPB3==0, 1, 0))
des1315_nosharenopen <- update(des1315_nosharenopen, no_savings = factor(no_savings))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./Results/nopen_lowincprs_morecompare/", variable = "no_savings", 
                          var_levels = c("no_savings0", "no_savings1"),
                          var_labels = c("No savings", "Has savings"), 
                          p_title = "Has savings - no pensioner HRP", plot_mode = "var_compare")







## JAM group define: 2-5th percentile of different income distributions, for all non-pension HRP, non-shared households ####

#rank households - 10% percentiles, gross equivalised household income #
pc10val_hhgreqinc_nosharenopen <- calculate.percentile.thresholds(DT = dt1315_noshare_1bu_nopen[year == "1415", ], 
                                                                  distr_var = ~hh_grossinc_eq, 
                                                                  region_form = ~GVTREGN, perc_width = "10")
dt1315_noshare_1bu_nopen <- assign.percentile(dt1315_noshare_1bu_nopen[year == "1415", ], 
                                              pc10val_hhgreqinc_nosharenopen, 
                                              "hh_grossinc_eq", perc_width = "10")
setnames(dt1315_noshare_1bu_nopen, "pc10position_reg", "pc10position_hhgreqinc_reg")
setnames(dt1315_noshare_1bu_nopen, "pc10position_nat", "pc10position_hhgreqinc_nat")

#rank households - 10% percentiles, gross equivalised household income -hb, equivalised
pc10val_hhgreqinc_nohb_nosharenopen <- calculate.percentile.thresholds(DT = dt1315_noshare_1bu_nopen[year == "1415", ], 
                                                                       distr_var = ~hh_grossinc_nohb_eq, 
                                                                       region_form = ~GVTREGN, perc_width = "10")
dt1315_noshare_1bu_nopen <- assign.percentile(dt1315_noshare_1bu_nopen[year == "1415", ], 
                                              pc10val_hhgreqinc_nohb_nosharenopen, 
                                              "hh_grossinc_nohb_eq", perc_width = "10")
setnames(dt1315_noshare_1bu_nopen, "pc10position_reg", "pc10position_hhgreqincnohb_reg")
setnames(dt1315_noshare_1bu_nopen, "pc10position_nat", "pc10position_hhgreqincnohb_nat")

#Create flags for JAM definition groups #
dt1315_noshare_1bu_nopen[ , JAMbase_greqinc := ifelse(pc10position_hhgreqinc_nat >= 2 & pc10position_hhgreqinc_nat <= 5, 1, 0 )]
dt1315_noshare_1bu_nopen[ , JAMrf_greqinc := ifelse((pc10position_hhgreqinc_nat >= 2 & pc10position_hhgreqinc_nat <= 5) 
                                               & over20gross_allben == 0, 1, 0 )]
dt1315_noshare_1bu_nopen[ , JAMnohb_greqinc := ifelse((pc10position_hhgreqincnohb_nat >= 2 & pc10position_hhgreqincnohb_nat <= 5), 1, 0 )]
dt1315_noshare_1bu_nopen[ , JAMnohbrf_greqinc := ifelse((pc10position_hhgreqincnohb_nat >= 2 & pc10position_hhgreqincnohb_nat <= 5) 
                                                      & over20grossnohb_nohbben == 0, 1, 0 )]

dt1315_noshare_1bu_nopen[ , JAMnohbwork_greqinc := ifelse((pc10position_hhgreqincnohb_nat >= 2 & pc10position_hhgreqincnohb_nat <= 5) 
                                                   & hrp_nowork == FALSE, 1, 0 )]

### Sample sizes of JAM groups
t_samplesize <- dt1315_noshare_1bu_nopen[ JAMbase_greqinc == 1 & year == "1415",
                                         .N, by=.(GVTREGN)]
write.csv( t_samplesize, file = "./Results/nopen_JAMs_1415/nopenJAMbase_subsample_size.csv")

t_samplesize <- dt1315_noshare_1bu_nopen[ JAMrf_greqinc == 1 & year == "1415",
                                          .N, by=.(GVTREGN)]
write.csv( t_samplesize, file = "./Results/nopen_JAMs_1415/nopenJAMrf_subsample_size.csv")

t_samplesize <- dt1315_noshare_1bu_nopen[ JAMnohb_greqinc == 1 & year == "1415",
                                          .N, by=.(GVTREGN)]
write.csv( t_samplesize, file = "./Results/nopen_JAMs_1415/nopenJAMnohb_subsample_size.csv")

t_samplesize <- dt1315_noshare_1bu_nopen[ JAMnohbrf_greqinc == 1 & year == "1415",
                                          .N, by=.(GVTREGN)]
write.csv( t_samplesize, file = "./Results/nopen_JAMs_1415/nopenJAMnohbrf_subsample_size.csv")

t_samplesize <- dt1315_noshare_1bu_nopen[ JAMnohbwork_greqinc == 1 & year == "1415",
                                          .N, by=.(GVTREGN)]
write.csv( t_samplesize, file = "./Results/nopen_JAMs_1415/nopenJAMnohbwork_subsample_size.csv")

#preferred JAM definition sample size
t_samplesize <- dt1315_noshare_1bu_nopen[ JAMnohbrf_greqinc == 1 & year == "1415",
                                          .N, by=.(grpd_region1)]
write.csv( t_samplesize, file = "./Results/nopen_JAMs_1415/nopenJAMnohbrf_subsample_size_grpd.csv")

#preferred JAM definition prs sample size

t_samplesize <- dt1315_noshare_1bu_nopen[ JAMnohbrf_greqinc == 1 & grpd_tenure == 1 & year == "1415",
                                          .N, by=.(GVTREGN)]
write.csv( t_samplesize, file = "./Results/nopen_JAMs_1415/nopenJAMnohbrfprs_subsample_size.csv")

t_samplesize <- dt1315_noshare_1bu_nopen[ JAMnohbrf_greqinc == 1 & grpd_tenure == 1 & year == "1415",
                                          .N, by=.(grpd_region1)]
write.csv( t_samplesize, file = "./Results/nopen_JAMs_1415/nopenJAMnohbrfprs_subsample_size_grpd.csv")


des1315_nosharenopen <- svydesign(ids = ~1, weights = ~grossweight, 
                                  data = dt1315_noshare_1bu_nopen[is_HRP & (is.na(grossweight) == F), ])

#Prop of each group under AHC MIS private renters #### 
# des1315_nosharenopen <- update(des1315_nosharenopen, under_MIS_prs = factor(under_MIS_prs))
# compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "under_MIS_prs", 
#                   p_title = "Prop of JAM who are private renters under MIS - no pensioner HRP", plot_mode = "proportion")
# 
# des1315_nosharenopen <- update(des1315_nosharenopen, no_shared_prs = ifelse(aff_shared_noincben == 0 & grpd_tenure == 1, 1, 0 ))
# #Above should be done in the dataset, not in survey design
# des1315_nosharenopen <- update(des1315_nosharenopen, no_shared_prs = factor(no_shared_prs))
# compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "no_shared_prs", 
#                   p_title = "Prop of JAM who are private renters and can't afford shared ownership - no pensioner HRP", plot_mode = "proportion")
# 
# compare.JAM.plots(my_design = subset(des1315_nosharenopen, grpd_tenure == 1), results_dir = "./nopen_JAMdefs/", variable = "under_MIS_prs", 
#                   p_title = "Prop of private renting JAM who are private renters under MIS - no pensioner HRP", plot_mode = "proportion")


### what % of under AHC MIS/can't afford shared ownership is in the JAM group? ####
des1315_nosharenopen <- update(des1315_nosharenopen, JAMbase_greqinc = factor(JAMbase_greqinc))
svymean(~JAMbase_greqinc, 
        design = subset(des1315_nosharenopen, year == "1415" & grpd_tenure == 1 & under_ahc_mis == 1), na.rm = TRUE)
svytotal(~JAMbase_greqinc, 
        design = subset(des1315_nosharenopen, year == "1415" & grpd_tenure == 1 & under_ahc_mis == 1), na.rm = TRUE)

des1315_nosharenopen <- update(des1315_nosharenopen, JAMnohbrf_greqinc = factor(JAMnohbrf_greqinc))
svymean(~JAMnohbrf_greqinc, 
        design = subset(des1315_nosharenopen, year == "1415" & grpd_tenure == 1 & under_ahc_mis == 1), na.rm = TRUE)
svytotal(~JAMnohbrf_greqinc, 
         design = subset(des1315_nosharenopen, year == "1415" & grpd_tenure == 1 & under_ahc_mis == 1), na.rm = TRUE)


svymean(~JAMbase_greqinc, design = subset(des1315_nosharenopen, 
                                          year == "1415" & grpd_tenure == 1 & aff_shared_noincben == 0), na.rm = TRUE)
svymean(~JAMnohbrf_greqinc, design = subset(des1315_nosharenopen, 
                                            year == "1415" & grpd_tenure == 1 & aff_shared_noincben == 0), na.rm = TRUE)




### financial strain of 'low-to-middle income renters' ####
des1315_nosharenopen <- update(des1315_nosharenopen, OAEXPNS = factor(OAEXPNS))
plot.survey.grpdregion(my_design = subset(des1315_nosharenopen, 
                                          JAMbase_greqinc == 1 & grpd_tenure == 1 & year == "1415"), 
                       "./Results/nopen_JAMs_1415", "OAEXPNS", 
                       var_levels = c("OAEXPNS1", "OAEXPNS2"),
                       var_labels = c("Could afford", "Couldn't afford"),
                       p_title = "Could afford a £200 expense", region = "grpd_region1")

des1315_nosharenopen <- update(des1315_nosharenopen, ADDMON = factor(ADDMON))
plot.survey.grpdregion(my_design = subset(des1315_nosharenopen, 
                                          JAMbase_greqinc == 1 & grpd_tenure == 1 & year == "1415"), 
                       "./Results/nopen_JAMs_1415", "ADDMON", 
                       var_levels = c("ADDMON1", "ADDMON2", "ADDMON3", "ADDMON4"),
                       var_labels = c("Do this", "Like to but can't afford", "Don't want to",  "Does not apply" ),
                       p_title = "Can save £10 a month", region = "grpd_region1")

plot.survey.grpdregion(my_design = subset(des1315_nosharenopen, 
                                          JAMbase_greqinc == 1 & grpd_tenure == 1 & year == "1415"), 
                       "./Results/nopen_JAMs_1415", "ADDMON", 
                       var_levels = c("ADDMON1", "ADDMON2", "ADDMON3", "ADDMON4"),
                       var_labels = c("Do this", "Like to but can't afford", "Don't want to",  "Does not apply" ),
                       p_title = "Can save £10 a month - numbers", svy_fun = svytotal, region = "grpd_region1")


#median gross incomes for prs under AHC MIS, can't afford shared ownership and JAM groups ####

svyquantile(~hh_grossinc, design = subset(des1315_nosharenopen, 
                                          year == "1415" & grpd_tenure == 1 & aff_shared_noincben == 0), 
            quantiles = 0.5, na.rm=TRUE)
svyquantile(~hh_grossinc, design = subset(des1315_nosharenopen, 
                                          year == "1415" & grpd_tenure == 1 & under_ahc_mis == 1 ), 
            quantiles = 0.5, na.rm=TRUE)
svyquantile(~hh_grossinc, design = subset(des1315_nosharenopen, 
                                          year == "1415" & grpd_tenure == 1 & JAMbase_greqinc == 1 ), 
            quantiles = 0.5, na.rm=TRUE)
svyquantile(~hh_grossinc, design = subset(des1315_nosharenopen, 
                                          year == "1415" & grpd_tenure == 1 & JAMnohbrf_greqinc == 1 ), 
            quantiles = 0.5, na.rm=TRUE)






