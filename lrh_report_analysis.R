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

flag.shared.ownership <- function (data_table){
  shared_thresh <- read.csv("./shared_ownership_13141415thresholds_1.csv", header = TRUE, 
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
dt_1315 <- tidy.regions(dt_1315, "1315")

dt_1315[ , not_1stbenu := ifelse(BENUNIT >1 , 1, 0)]
dt_1315[ , multiple_benu := ifelse( (sum(not_1stbenu) > 0), 1, 0 ), by = .(SERNUM, year) ]


#remove households from sample with shared status and lodgers
dt1315_noshare <- dt_1315[ (has_lodger == 0 & HHSTAT == 1), ]


#Append AHC Minimum Income Standard for households - only works for 1 benefit unit households atm

dt1315_noshare_1bu <- dt1315_noshare[ multiple_benu == 0, ]

dt1315_noshare_1bu <- append.mis(dt1315_noshare_1bu, 
                                 "./MIS_2015.csv")

#Add for if AHC income is less than AHC MIS
dt1315_noshare_1bu[ , under_ahc_mis := ifelse(hh_gross_ahc < ahc_mis_15*52, 1, 0)]


#LIPR definition flags
#more than 20% of household's gross income from benefits (excluding tax credits) - same as Resolution Foundation definition
dt_1315[ , over20gross_allben := ifelse(((HBENINC - HHTXCRED )/(HHINC)) > 0.2, 1, 0)]

#more than 20% of household income-HB comes from non-HB benefits, modified RF definition
dt_1315[ , over20grossnohb_nohbben := ifelse(!is.na(hh_hbweekly), 
                                             (ifelse((HBENINC-hh_hbweekly - HHTXCRED)/(HHINC-hh_hbweekly) > 0.2, 1, 0)),
                                              (ifelse((HBENINC- HHTXCRED)/(HHINC) > 0.2, 1, 0)))]

dt_1315[ , not_1stbenu := ifelse(BENUNIT >1 , 1, 0)]
dt_1315[ , multiple_benu := ifelse( (sum(not_1stbenu) > 0), 1, 0 ), by = .(SERNUM, year) ]
#remove households from sample with shared status and lodgers
dt1315_noshare <- dt_1315[ (has_lodger == 0 & HHSTAT == 1), ]


#Append AHC Minimum Income Standard for households - only works for 1 benefit unit households atm ####

dt1315_noshare_1bu <- dt1315_noshare[ multiple_benu == 0, ]

dt1315_noshare_1bu <- append.mis(dt1315_noshare_1bu, "./MIS_2015.csv")

#Add for if AHC income is less than AHC MIS
dt1315_noshare_1bu[ , under_ahc_mis := ifelse(hh_gross_ahc < (ahc_mis_15*52), 1, 0)]

des1315_noshare_1bu <- svydesign(ids = ~1, weights = ~grossweight, data = dt1315_noshare_1bu[is_HRP & (is.na(grossweight) == F), ])

#Look at characteristics of less than AHC MIS ######


#proportions, tenure for all under AHC cf no pensioners under AHC
des1315_noshare_1bu <- update(des1315_noshare_1bu, under_ahc_mis = factor(under_ahc_mis))
svymean(~under_ahc_mis, des1315_noshare_1bu, na.rm=TRUE)
svymean(~under_ahc_mis, design = subset(des1315_noshare_1bu, head_pensioner == 0), na.rm=TRUE)
svytotal(~under_ahc_mis, design = subset(des1315_noshare_1bu, head_pensioner == 0), na.rm=TRUE)

svymean(~(under_ahc_mis==1 & grpd_tenure == 1), design = subset(des1315_noshare_1bu, head_pensioner == 0), na.rm=TRUE)
svytotal(~(under_ahc_mis==1 & grpd_tenure == 1), design = subset(des1315_noshare_1bu, head_pensioner == 0), na.rm=TRUE)

svymean(~under_ahc_mis, design = subset(des1315_noshare_1bu, head_pensioner == 0 & grpd_tenure == 1), na.rm=TRUE)
svytotal(~under_ahc_mis, design = subset(des1315_noshare_1bu, head_pensioner == 0 & grpd_tenure == 1), na.rm=TRUE)


des1315_noshare_1bu <- update(des1315_noshare_1bu, grpd_tenure = factor(grpd_tenure, labels = c("PRS", "Social", "Outright", "Mortgaged") ))
t_tenure_allunderMIS <- as.data.frame(svymean(~grpd_tenure, design = subset(des1315_noshare_1bu, under_ahc_mis ==1), na.rm=TRUE))
t_tenure_nopenunderMIS <- as.data.frame(svymean(~grpd_tenure, 
                                                design = subset(des1315_noshare_1bu, under_ahc_mis ==1 & head_pensioner == 0), na.rm=TRUE))


#get rid of factors to be able to look at prop of pensioners
des1315_noshare_1bu <- svydesign(ids = ~1, weights = ~grossweight, data = dt1315_noshare_1bu[is_HRP & (is.na(grossweight) == F), ])
des1315_noshare_1bu <- update(des1315_noshare_1bu, head_pensioner = factor(head_pensioner))
svymean(~head_pensioner, design= subset(des1315_noshare_1bu, (under_ahc_mis == 1)), na.rm = TRUE )
svymean(~head_pensioner, design= subset(des1315_noshare_1bu, (under_ahc_mis == 1 & grpd_tenure == 1)), na.rm = TRUE )


# Plots and tables for PRS, no pensioners and under AHC MIS
dt1315_prsnosharenopen_ahcMIS <- dt1315_noshare_1bu[is_HRP & (is.na(grossweight) == F) & grpd_tenure == 1 & 
                                                      head_pensioner == 0 & under_ahc_mis == 1, ]
des1315_prsnosharenopen_ahcMIS<- svydesign(ids = ~1, weights = ~grossweight, data = dt1315_prsnosharenopen_ahcMIS)


#Region
t_region_PRSnopen_ahcMIS <-as.data.frame(svymean(~GVTREGN, des1315_prsnosharenopen_ahcMIS, na.rm=TRUE))
setDT(t_region_PRSnopen_ahcMIS, keep.rownames = TRUE)[]

ggplot(na.omit(t_region_PRSnopen_ahcMIS), aes(x = mean, xmin = mean-SE, xmax = mean+SE, y =rn,
                                     colour = rn)) +
  geom_point() + geom_segment( aes(x = mean-SE, xend = mean+SE, y = rn,
                                   yend=rn)) +
  #scale_colour_manual(values=c("grey19","firebrick3", "cyan4", "darkorange1")) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  xlab("Prop of households under MIS in region") + guides(colour=FALSE, shape = FALSE) + ylab("Region")

t_regiontot_PRSnopen_ahcMIS <-as.data.frame(svytotal(~GVTREGN, des1315_prsnosharenopen_ahcMIS, na.rm=TRUE))

#Characteristics by grouped region
make.household.plots(des1315_prsnosharenopen_ahcMIS, dt1315_prsnosharenopen_ahcMIS, "PRS, no pensioner HRP, under MIS AHC", 
                     results_dir = "./prs_nopen_under_AHC", my_region = "grpd_region1" )


#Renting households who can't afford shared ownership ####

#Prop and total renting households who can't afford shared ownership (no sharers, 1 BU only)
svymean(~(aff_shared_noincben == 0 & grpd_tenure == 1), design = subset(des1315_noshare_1bu, head_pensioner == 0), na.rm=TRUE)
svytotal(~(aff_shared_noincben == 0 & grpd_tenure == 1), design = subset(des1315_noshare_1bu, head_pensioner == 0), na.rm=TRUE)

svymean(~aff_shared_noincben == 0, design = subset(des1315_noshare_1bu, grpd_tenure == 1 &  head_pensioner == 0), na.rm=TRUE)
svytotal(~aff_shared_noincben == 0, design = subset(des1315_noshare_1bu, grpd_tenure == 1 &  head_pensioner == 0), na.rm=TRUE)

#Plots and tables for non-pensioner renting households who can't afford shared ownership
dt1315_prsnosharenopen_sown <- dt1315_noshare_1bu[is_HRP & (is.na(grossweight) == F) & grpd_tenure == 1 & 
                                                      head_pensioner == 0 & aff_shared_noincben == 0, ]
des1315_prsnosharenopen_sown <- svydesign(ids = ~1, weights = ~grossweight, data = dt1315_prsnosharenopen_sown)

#Region
t_region_PRSnopen_sown <-as.data.frame(svymean(~GVTREGN, des1315_prsnosharenopen_sown, na.rm=TRUE))
setDT(t_region_PRSnopen_sown, keep.rownames = TRUE)[]

ggplot(na.omit(t_region_PRSnopen_sown), aes(x = mean, xmin = mean-SE, xmax = mean+SE, y =rn,
                                              colour = rn)) +
  geom_point() + geom_segment( aes(x = mean-SE, xend = mean+SE, y = rn,
                                   yend=rn)) +
  #scale_colour_manual(values=c("grey19","firebrick3", "cyan4", "darkorange1")) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  xlab("Prop of households who can't afford shared ownership in region") + guides(colour=FALSE, shape = FALSE) + ylab("Region")

t_regiontot_PRSnopen_sown <-as.data.frame(svytotal(~GVTREGN, des1315_prsnosharenopen_sown, na.rm=TRUE))

#Characteristics by grouped region
make.household.plots(des1315_prsnosharenopen_sown, dt1315_prsnosharenopen_sown, "PRS, no pensioner HRP, can't afford shared ownership", 
                     results_dir = "./prs_nopen_shared_ownership", my_region = "grpd_region1" )


#Overlap between shared ownership and MIS group ####
dt1315_nosharenopen_1bu <- dt1315_noshare_1bu[is_HRP & (is.na(grossweight) == F) & head_pensioner == 0, ]


dt1315_nosharenopen_1bu[ !is.na(aff_shared_noincben) & !is.na(under_ahc_mis), 
                        aff_shared_under_MIS := ifelse(aff_shared_noincben == 0 & under_ahc_mis == 1, 1, 0)]
dt1315_nosharenopen_1bu[ !is.na(aff_shared_noincben) & !is.na(under_ahc_mis), 
                        aff_shared_under_MIS := ifelse(aff_shared_noincben == 1 & under_ahc_mis == 1, 2, aff_shared_under_MIS)]
dt1315_nosharenopen_1bu[ !is.na(aff_shared_noincben) & !is.na(under_ahc_mis), 
                        aff_shared_under_MIS := ifelse(aff_shared_noincben == 0 & under_ahc_mis == 0, 3, aff_shared_under_MIS)]
dt1315_nosharenopen_1bu[ !is.na(aff_shared_noincben) & !is.na(under_ahc_mis), 
                        aff_shared_under_MIS := ifelse(aff_shared_noincben == 1 & under_ahc_mis == 0, 4, aff_shared_under_MIS)]


des1315_prsnosharenopen <- svydesign(ids = ~1, weights = ~grossweight, data = dt1315_nosharenopen_1bu[ grpd_tenure == 1 ,])
des1315_prsnosharenopen <- update(des1315_prsnosharenopen, aff_shared_under_MIS = factor(aff_shared_under_MIS, levels = c(1,2,3,4),
                                                                                         labels = c("under MIS and can't own", 
                                                                                                    "under MIS and can own", 
                                                                                                    "over MIS and can't own", 
                                                                                                    "over MIS and can own") ))

svymean(~aff_shared_under_MIS, des1315_prsnosharenopen, na.rm = TRUE)

#svymean(~GVTREGN, design = subset(des1315_prsnosharenopen, aff_shared_under_MIS == 2), na.rm = TRUE)
svyby(~GVTREGN, by = ~aff_shared_under_MIS, design = des1315_prsnosharenopen, FUN = svymean, na.rm = TRUE)

dt1315_nosharenopen_1bu[ , aff_shared_under_MIS := factor(aff_shared_under_MIS, levels = c(1,2,3,4),
                                                         labels = c("under MIS and can't own", 
                                                                    "under MIS and can own", 
                                                                    "over MIS and can't own", 
                                                                    "over MIS and can own")) ]

#Plots
ggplot(dt1315_nosharenopen_1bu[grpd_tenure == 1, ], aes(x = hh_grossinc_nohb_eq, weight = grossweight, fill = aff_shared_under_MIS)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))
  geom_histogram(binwidth = 1000) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  ggtitle("Gross equivalised household income - HB: PRS, no pensioner HRP") +
  xlim(-100, 75000) + facet_grid( grpd_region1 ~ .)
ggsave(filename = "./prs_nopen_MISownoverlap/objoverlap_hhgrossinc_nohb_eq.png")

ggplot(dt1315_nosharenopen_1bu[grpd_tenure == 1, ], aes(x = hh_grossinc_nohb, weight = grossweight, fill = aff_shared_under_MIS)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))
  geom_histogram(binwidth = 1000) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  ggtitle("Gross household income - HB: PRS, no pensioner HRP") +
  xlim(-100, 75000) + facet_grid( grpd_region1 ~ .)
ggsave(filename = "./prs_nopen_MISownoverlap/objoverlap_hhgrossinc_nohb.png")

ggplot(dt1315_nosharenopen_1bu[grpd_tenure == 1, ], aes(x = hh_grossinc_noincben_eq, weight = grossweight, fill = aff_shared_under_MIS)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))
  geom_histogram(binwidth = 1000) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  ggtitle("Gross equivalised household income - income related benefits: PRS, no pensioner HRP") +
  xlim(-100, 75000) + facet_grid( grpd_region1 ~ .)
ggsave(filename = "./prs_nopen_MISownoverlap/objoverlap_hhgrossinc_noincben_eq.png")

ggplot(dt1315_nosharenopen_1bu[grpd_tenure == 1, ], aes(x = hh_grossinc_noincben, weight = grossweight, fill = aff_shared_under_MIS)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))
  geom_histogram(binwidth = 1000) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  ggtitle("Gross household income - income related benefits: PRS, no pensioner HRP") +
  xlim(-100, 75000) + facet_grid( grpd_region1 ~ .)
ggsave(filename = "./prs_nopen_MISownoverlap/objoverlap_hhgrossinc_noincben.png")




## JAM group define: 2-5th percentile of different income distributions, for all non-pension HRP, non-shared households ####
dt1315_nosharenopen <- dt1315_noshare[head_pensioner == 0, ]

#rank households - 5% percentiles, gross equivalised household income ####
pc5val_hhgreqinc_nosharenopen <- calculate.percentile.thresholds(DT = dt1315_nosharenopen, distr_var = ~hh_grossinc_eq, 
                                                                 region_form = ~GVTREGN, perc_width = "5")
dt1315_nosharenopen <- assign.percentile(dt1315_nosharenopen, pc5val_hhgreqinc_nosharenopen, "hh_grossinc_eq", perc_width = "5")
colnames(dt1315_nosharenopen)[281] <- "pc5position_hhgreqinc_reg"
colnames(dt1315_nosharenopen)[282] <- "pc5position_hhgreqinc_nat"

#rank households - 10% percentiles, gross equivalised household income ####
pc10val_hhgreqinc_nosharenopen <- calculate.percentile.thresholds(DT = dt1315_nosharenopen, distr_var = ~hh_grossinc_eq, 
                                                                 region_form = ~GVTREGN, perc_width = "10")
dt1315_nosharenopen <- assign.percentile(dt1315_nosharenopen, pc10val_hhgreqinc_nosharenopen, "hh_grossinc_eq", perc_width = "10")
colnames(dt1315_nosharenopen)[283] <- "pc10position_hhgreqinc_reg"
colnames(dt1315_nosharenopen)[284] <- "pc10position_hhgreqinc_nat"

#rank households - 5% percentiles, gross household income -hb, equivalised ####
pc5val_hhgreqinc_nohb_nosharenopen <- calculate.percentile.thresholds(DT = dt1315_nosharenopen, distr_var = ~hh_grossinc_nohb_eq, 
                                                                 region_form = ~GVTREGN, perc_width = "5")
dt1315_nosharenopen <- assign.percentile(dt1315_nosharenopen, pc5val_hhgreqinc_nohb_nosharenopen, "hh_grossinc_nohb_eq", perc_width = "5")
colnames(dt1315_nosharenopen)[285] <- "pc5position_hhgreqincnohb_reg"
colnames(dt1315_nosharenopen)[286] <- "pc5position_hhgreqincnohb_nat"

#rank households - 10% percentiles, gross equivalised household income
pc10val_hhgreqinc_nohb_nosharenopen <- calculate.percentile.thresholds(DT = dt1315_nosharenopen, distr_var = ~hh_grossinc_nohb_eq, 
                                                                  region_form = ~GVTREGN, perc_width = "10")
dt1315_nosharenopen <- assign.percentile(dt1315_nosharenopen, pc10val_hhgreqinc_nohb_nosharenopen, "hh_grossinc_nohb_eq", perc_width = "10")
colnames(dt1315_nosharenopen)[287] <- "pc10position_hhgreqincnohb_reg"
colnames(dt1315_nosharenopen)[288] <- "pc10position_hhgreqincnohb_nat"

#rank households - 5% percentiles, gross equivalised AHC household income ####
pc5val_hhgreqinc_ahc_nosharenopen <- calculate.percentile.thresholds(DT = dt1315_nosharenopen, distr_var = ~hh_gross_ahc_eq, 
                                                                 region_form = ~GVTREGN, perc_width = "5")
dt1315_nosharenopen <- assign.percentile(dt1315_nosharenopen, pc5val_hhgreqinc_ahc_nosharenopen, "hh_gross_ahc_eq", perc_width = "5")
colnames(dt1315_nosharenopen)[289] <- "pc5position_hhgreqincahc_reg"
colnames(dt1315_nosharenopen)[290] <- "pc5position_hhgreqincahc_nat"

#rank households - 10% percentiles, gross equivalised household income
pc10val_hhgreqinc_ahc_nosharenopen <- calculate.percentile.thresholds(DT = dt1315_nosharenopen, distr_var = ~hh_gross_ahc_eq, 
                                                                  region_form = ~GVTREGN, perc_width = "10")
dt1315_nosharenopen <- assign.percentile(dt1315_nosharenopen, pc10val_hhgreqinc_ahc_nosharenopen, "hh_gross_ahc_eq", perc_width = "10")
colnames(dt1315_nosharenopen)[290] <- "pc10position_hhgreqincahc_reg"
colnames(dt1315_nosharenopen)[291] <- "pc10position_hhgreqincahc_nat"

#Create flags for JAM definition groups ####
dt1315_nosharenopen[ , JAMbase_greqinc := ifelse(pc10position_hhgreqinc_nat >= 2 & pc10position_hhgreqinc_nat <= 5, 1, 0 )]
dt1315_nosharenopen[ , JAMrf_greqinc := ifelse((pc10position_hhgreqinc_nat >= 2 & pc10position_hhgreqinc_nat <= 5) 
                                               & over20gross_allben == 0, 1, 0 )]
dt1315_nosharenopen[ , JAMnohb_greqinc := ifelse((pc10position_hhgreqincnohb_nat >= 2 & pc10position_hhgreqincnohb_nat <= 5), 1, 0 )]
dt1315_nosharenopen[ , JAMnohbrf_greqinc := ifelse((pc10position_hhgreqincnohb_nat >= 2 & pc10position_hhgreqincnohb_nat <= 5) 
                                                      & over20grossnohb_nohbben == 0, 1, 0 )]

dt1315_nosharenopen[ , JAMnohbwork_greqinc := ifelse((pc10position_hhgreqincnohb_nat >= 2 & pc10position_hhgreqincnohb_nat <= 5) 
                                                   & hrp_nowork == FALSE, 1, 0 )]

dt1315_nosharenopen[ , JAMbase_ahc := ifelse((pc10position_hhgreqincahc_nat >= 2 & pc10position_hhgreqincahc_nat <= 5), 1, 0 )]
dt1315_nosharenopen[ , JAMwork_ahc := ifelse((pc10position_hhgreqincahc_nat >= 2 & pc10position_hhgreqincahc_nat <= 5) 
                                                    & hrp_nowork == FALSE, 1, 0 )]


# Plot JAM groups in gross equivalised income distribution ####
dt1315_nosharenopen[ , JAMbase_greqinc := factor(JAMbase_greqinc)]
ggplot(dt1315_nosharenopen[!is.na(JAMbase_greqinc) & is_HRP==1,], aes(x = hh_grossinc_eq, fill = JAMbase_greqinc, weight = grossweight) ) +
  geom_histogram(binwidth = 1000) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  ggtitle("Gross equivalised household income: no pensioner HRP") +
  xlim(-100, 100000)
ggsave(filename = "./nopen_JAMdefs/JAMbase_hhgrossinceq.png")

dt1315_nosharenopen[ , JAMrf_greqinc := factor(JAMrf_greqinc)]
ggplot(dt1315_nosharenopen[!is.na(JAMrf_greqinc)& is_HRP==1,], aes(x = hh_grossinc_eq, fill = JAMrf_greqinc, weight = grossweight) ) +
  geom_histogram(binwidth = 1000) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  ggtitle("Gross equivalised household income: no pensioner HRP") +
  xlim(-100, 100000)
ggsave(filename = "./nopen_JAMdefs/JAMrf_hhgrossinceq.png")

dt1315_nosharenopen[ , JAMnohb_greqinc := factor(JAMnohb_greqinc)]
ggplot(dt1315_nosharenopen[!is.na(JAMnohb_greqinc)& is_HRP==1,], aes(x = hh_grossinc_eq, fill = JAMnohb_greqinc, weight = grossweight) ) +
  geom_histogram(binwidth = 1000) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  ggtitle("Gross equivalised household income: no pensioner HRP") +
  xlim(-100, 100000)
ggsave(filename = "./nopen_JAMdefs/JAMnohb_hhgrossinceq.png")

dt1315_nosharenopen[ , JAMnohbrf_greqinc := factor(JAMnohbrf_greqinc)]
ggplot(dt1315_nosharenopen[!is.na(JAMnohbrf_greqinc)& is_HRP==1,], aes(x = hh_grossinc_eq, fill = JAMnohbrf_greqinc, weight = grossweight) ) +
  geom_histogram(binwidth = 1000) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  ggtitle("Gross equivalised household income: no pensioner HRP") +
  xlim(-100, 100000)
ggsave(filename = "./nopen_JAMdefs/JAMnohbrf_hhgrossinceq.png")

dt1315_nosharenopen[ , JAMnohbrf_prs := ifelse(JAMnohbrf_greqinc ==1 & grpd_tenure == 1, 1, 0)]
dt1315_nosharenopen[ , JAMnohbrf_prs := factor(JAMnohbrf_prs)]
ggplot(dt1315_nosharenopen[!is.na(JAMnohbrf_prs)& is_HRP==1,], aes(x = hh_grossinc_eq, fill = JAMnohbrf_prs, weight = grossweight) ) +
  geom_histogram(binwidth = 1000) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  ggtitle("Gross equivalised household income: no pensioner HRP") +
  xlim(-100, 100000)
ggsave(filename = "./nopen_JAMdefs/JAMnohbrfprs_hhgrossinceq.png")


dt1315_nosharenopen[ , JAMnohbwork_greqinc := factor(JAMnohbwork_greqinc)]
ggplot(dt1315_nosharenopen[!is.na(JAMnohbwork_greqinc)& is_HRP==1,], aes(x = hh_grossinc_eq, fill = JAMnohbwork_greqinc, weight = grossweight) ) +
  geom_histogram(binwidth = 1000) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  ggtitle("Gross equivalised household income: no pensioner HRP") +
  xlim(-100, 100000)
ggsave(filename = "./nopen_JAMdefs/JAMnohbwork_hhgrossinceq.png")

dt1315_nosharenopen[ , JAMbase_ahc := factor(JAMbase_ahc)]
ggplot(dt1315_nosharenopen[!is.na(JAMbase_ahc)& is_HRP==1,], aes(x = hh_grossinc_eq, fill = JAMbase_ahc, weight = grossweight) ) +
  geom_histogram(binwidth = 1000) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  ggtitle("Gross equivalised household income: no pensioner HRP") +
  xlim(-100, 100000)
ggsave(filename = "./nopen_JAMdefs/JAMbaseahc_hhgrossinceq.png")

dt1315_nosharenopen[ , JAMwork_ahc := factor(JAMwork_ahc)]
ggplot(dt1315_nosharenopen[!is.na(JAMwork_ahc)& is_HRP==1,], aes(x = hh_grossinc_eq, fill = JAMwork_ahc, weight = grossweight) ) +
  geom_histogram(binwidth = 1000) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  ggtitle("Gross equivalised household income: no pensioner HRP") +
  xlim(-100, 100000)
ggsave(filename = "./nopen_JAMdefs/JAMworkahc_hhgrossinceq.png")


#Make tables of hhld characteristics for each definition ####
#merge in under_ahc_mis definition
keycols = c("SERNUM","BENUNIT","PERSON", "year")
setkeyv(dt1315_nosharenopen, keycols)
setkeyv(dt1315_noshare_1bu, keycols)
dt1315_nosharenopen <- merge(dt1315_nosharenopen, dt1315_noshare_1bu[, .SD, .SDcols = c("SERNUM","BENUNIT","PERSON", "year", 
                                                                                        "under_ahc_mis")], 
                             by = keycols, all.x = TRUE)

dt1315_nosharenopen[ , under_MIS_prs := ifelse(under_ahc_mis == 1 & grpd_tenure == 1, 1, 0)]

des1315_nosharenopen <- svydesign(ids = ~1, weights = ~grossweight, data = dt1315_nosharenopen[is_HRP & (is.na(grossweight) == F),])


#Proportion of non-pen households in each JAM group ####
des1315_nosharenopen <- update(des1315_nosharenopen, JAMbase_greqinc = factor(JAMbase_greqinc ==1))
t_prop_JAMbase_greq <- as.data.frame(svymean(~JAMbase_greqinc, des1315_nosharenopen, na.rm=TRUE))
setDT(t_prop_JAMbase_greq, keep.rownames = TRUE)[]
t_prop_JAMbase_greq<- t_prop_JAMbase_greq[2,] 
colnames(t_prop_JAMbase_greq)[1] <- "definition"
t_prop_JAMbase_greq$definition <- "JAM baseline"

des1315_nosharenopen <- update(des1315_nosharenopen, grpd_tenure = factor(JAMrf_greqinc ==1))
t_prop_JAMrf_greq <- as.data.frame(svymean(~JAMrf_greqinc, des1315_nosharenopen, na.rm=TRUE))
setDT(t_prop_JAMrf_greq, keep.rownames = TRUE)[]
t_prop_JAMrf_greq<- t_prop_JAMrf_greq[2,] 
colnames(t_prop_JAMrf_greq)[1] <- "definition"
t_prop_JAMrf_greq$definition <- "JAM RF"

des1315_nosharenopen <- update(des1315_nosharenopen, grpd_tenure = factor(JAMnohb_greqinc ==1))
t_prop_JAMnohb_greq <- as.data.frame(svymean(~JAMnohb_greqinc, des1315_nosharenopen, na.rm=TRUE))
setDT(t_prop_JAMnohb_greq, keep.rownames = TRUE)[]
t_prop_JAMnohb_greq<- t_prop_JAMnohb_greq[2,] 
colnames(t_prop_JAMnohb_greq)[1] <- "definition"
t_prop_JAMnohb_greq$definition <- "JAM no HB"

des1315_nosharenopen <- update(des1315_nosharenopen, grpd_tenure = factor(JAMnohbrf_greqinc ==1))
t_prop_JAMnohbrf_greq <- as.data.frame(svymean(~JAMnohbrf_greqinc, des1315_nosharenopen, na.rm=TRUE))
setDT(t_prop_JAMnohbrf_greq, keep.rownames = TRUE)[]
t_prop_JAMnohbrf_greq<- t_prop_JAMnohbrf_greq[2,] 
colnames(t_prop_JAMnohbrf_greq)[1] <- "definition"
t_prop_JAMnohbrf_greq$definition <- "JAM no HB RF"

des1315_nosharenopen <- update(des1315_nosharenopen, grpd_tenure = factor(JAMnohbwork_greqinc ==1))
t_prop_JAMnohbwork_greq <- as.data.frame(svymean(~JAMnohbwork_greqinc, des1315_nosharenopen, na.rm=TRUE))
setDT(t_prop_JAMnohbwork_greq, keep.rownames = TRUE)[]
t_prop_JAMnohbwork_greq<- t_prop_JAMnohbwork_greq[2,] 
colnames(t_prop_JAMnohbwork_greq)[1] <- "definition"
t_prop_JAMnohbwork_greq$definition <- "JAM no HB working"

prop_list <- list(t_prop_JAMbase_greq, t_prop_JAMrf_greq, t_prop_JAMnohbrf_greq, t_prop_JAMnohb_greq, t_prop_JAMnohbwork_greq )
t_prop_JAM<- rbindlist(prop_list, fill = TRUE) 
rm(prop_list)
rm(t_prop_JAMbase_greq, t_prop_JAMrf_greq, t_prop_JAMnohbrf_greq, t_prop_JAMnohb_greq, t_prop_JAMnohbwork_greq)

ggplot(na.omit(t_prop_JAM), aes(x = mean, xmin = mean-SE, xmax = mean+SE, y =definition,
                                     colour = definition)) +
  geom_point() + geom_segment( aes(x = mean-SE, xend = mean+SE, y = definition,
                                   yend=definition)) +
  #scale_colour_manual(values=c("grey19","firebrick3", "cyan4", "darkorange1")) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  xlab("Prop of non-pensioner HRP households") + guides(colour=FALSE, shape = FALSE) + ylab("JAM definition")+
  ggtitle("Prop all non-pensioner HRP households in different JAM definitions")
ggsave(filename = "./nopen_JAMdefs/JAMdefs_propcompare.png")

write.csv(t_prop_JAM, file = "./nopen_JAMdefs/JAMdefs_propcompare.csv")



#Prop of each group under AHC MIS private renters #### 
des1315_nosharenopen <- update(des1315_nosharenopen, under_MIS_prs = factor(under_MIS_prs))
compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "under_MIS_prs", 
                  p_title = "Prop of JAM who are private renters under MIS - no pensioner HRP", plot_mode = "proportion")

des1315_nosharenopen <- update(des1315_nosharenopen, no_shared_prs = ifelse(aff_shared_noincben == 0 & grpd_tenure == 1, 1, 0 ))
#Above should be done in the dataset, not in survey design
des1315_nosharenopen <- update(des1315_nosharenopen, no_shared_prs = factor(no_shared_prs))
compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "no_shared_prs", 
                  p_title = "Prop of JAM who are private renters and can't afford shared ownership - no pensioner HRP", plot_mode = "proportion")

compare.JAM.plots(my_design = subset(des1315_nosharenopen, grpd_tenure == 1), results_dir = "./nopen_JAMdefs/", variable = "under_MIS_prs", 
                  p_title = "Prop of private renting JAM who are private renters under MIS - no pensioner HRP", plot_mode = "proportion")


#Characteristics by JAM group ####
des1315_nosharenopen <- update(des1315_nosharenopen, grpd_tenure = factor(grpd_tenure ))
compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "grpd_tenure", 
                  var_levels = c("grpd_tenure1", "grpd_tenure2", "grpd_tenure3", "grpd_tenure4"),
                  var_labels = c("PRS", "Social", "Outright", "Mortgaged"),
                  p_title = "Tenure by JAM definition - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, no_shared_prs = ifelse(aff_shared_noincben == 0 & grpd_tenure == 1, 1, 0 ))
des1315_nosharenopen <- update(des1315_nosharenopen, no_shared_prs = factor(no_shared_prs))
compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "no_shared_prs", 
                  var_levels = c("no_shared_prs0", "no_shared_prs1"),
                  var_labels = c("Can afford", "Can't afford"),
                  p_title = "Can afford shared ownership and PRS by JAM definition - no pensioner HRP", plot_mode = "var_compare")

svytotal(~grpd_tenure, design = subset(des1315_nosharenopen, JAMbase_greqinc ==1), na.rm=TRUE)

des1315_nosharenopen <- update(des1315_nosharenopen, GVTREGN = factor(GVTREGN ))
compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "GVTREGN", 
                  var_levels = c("GVTREGNNorth East", "GVTREGNNorth West", "GVTREGNYorks & Humber", "GVTREGNEast Midlands", 
                                 "GVTREGNWest Midlands", "GVTREGNEast", "GVTREGNLondon", "GVTREGNSouth East", "GVTREGNSouth West"),
                  var_labels = c("North East", "North West", "Yorks & Humber", "East Midlands", 
                                 "West Midlands", "East", "London", "South East", "South West"),
                  p_title = "Region by JAM definition - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, HDAGE = factor(HDAGE))
compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "HDAGE", 
                  var_levels = c("HDAGE1", "HDAGE2", "HDAGE3", "HDAGE4", "HDAGE5", "HDAGE6"),
                  var_labels = c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65+"),
                  p_title = "Age by JAM definition - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, DVIL04A = factor(DVIL04A))
compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "DVIL04A", 
                  var_levels = c("DVIL04A1", "DVIL04A2", "DVIL04A3", "DVIL04A4"),
                  var_labels = c("Employed", "Family worker", "Unemployed", "Inactive"), 
                  p_title = "HRP employment (ILO) by JAM definition - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, ECOBU = factor(ECOBU))
compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "ECOBU", 
                  var_levels = c("ECOBU1", "ECOBU2", "ECOBU3", "ECOBU4", "ECOBU5", "ECOBU6", "ECOBU7", "ECOBU8"),
                  var_labels = c("1+ self employed", "Sing/couple all FT", 
                                 "Couple, 1 FT, 1 PT",  "Couple, 1 FT, 1 not working", 
                                 "No FT, 1+ PT", "Workless, head/spouse aged 60+", 
                                 "Workless, head/spouse unemployed", "Workless, inactive" ),
                  p_title = "Employment of household by JAM definition - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, SELFDEMP = factor(SELFDEMP))
compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "SELFDEMP", 
                  var_levels = c("SELFDEMP1", "SELFDEMP2", "SELFDEMP3", "SELFDEMP4", "SELFDEMP5","SELFDEMP6", "SELFDEMP7",
                                 "SELFDEMP8", "SELFDEMP9", "SELFDEMP10"),
                  var_labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
                                 "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other"),
                  p_title = "Self-reported HRP situation by JAM definition - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, has_kids16 = factor(has_kids16))
compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "has_kids16", 
                  var_levels = c("has_kids160", "has_kids161"),
                  var_labels = c("No school age kids", "Has school age kids"), 
                  p_title = "Has children by JAM definition - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, single_parent = factor(single_parent))
compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "single_parent", 
                  var_levels = c("single_parent0", "single_parent1"),
                  var_labels = c("Not Single parent head", "Single parent head "),
                  p_title = "Is single parent by JAM definition - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, has_disabled = ifelse((has_disabledad ==1 | has_disabledch == 1), 1, 0))
des1315_nosharenopen <- update(des1315_nosharenopen, has_disabled = factor(has_disabled))
compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "has_disabled", 
                  var_levels = c("has_disabled0", "has_disabled1"),
                  var_labels = c("No disabled", "Has disabled person"), 
                  p_title = "Has disabled adult or child by JAM definition - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, no_savings = ifelse(TOTCAPB3==0, 1, 0))
des1315_nosharenopen <- update(des1315_nosharenopen, no_savings = factor(no_savings))
compare.JAM.plots(my_design = des1315_nosharenopen, results_dir = "./nopen_JAMdefs/", variable = "no_savings", 
                  var_levels = c("no_savings0", "no_savings1"),
                  var_labels = c("No savings", "Has savings"), 
                  p_title = "Has savings by JAM definition - no pensioner HRP", plot_mode = "var_compare")


# Characteristics of renting households by LIPR definition####
des1315_prsnosharenopen <- subset(des1315_nosharenopen, grpd_tenure == 1)

des1315_prsnosharenopen <- update(des1315_prsnosharenopen, GVTREGN = factor(GVTREGN ))
compare.LIPR.plots(my_design = des1315_prsnosharenopen, results_dir = "./prs_nopen_comparedefs/", variable = "GVTREGN", 
                  var_levels = c("GVTREGNNorth East", "GVTREGNNorth West", "GVTREGNYorks & Humber", "GVTREGNEast Midlands", 
                                 "GVTREGNWest Midlands", "GVTREGNEast", "GVTREGNLondon", "GVTREGNSouth East", "GVTREGNSouth West"),
                  var_labels = c("North East", "North West", "Yorks & Humber", "East Midlands", 
                                 "West Midlands", "East", "London", "South East", "South West"),
                  p_title = "Region by LIPR definition - PRS no pensioner HRP", plot_mode = "var_compare")

des1315_prsnosharenopen <- update(des1315_prsnosharenopen, HDAGE = factor(HDAGE))
compare.LIPR.plots(my_design = des1315_prsnosharenopen, results_dir = "./prs_nopen_comparedefs/", variable = "HDAGE", 
                  var_levels = c("HDAGE1", "HDAGE2", "HDAGE3", "HDAGE4", "HDAGE5", "HDAGE6"),
                  var_labels = c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65+"),
                  p_title = "Age by JAM definition - PRS no pensioner HRP", plot_mode = "var_compare")

des1315_prsnosharenopen <- update(des1315_prsnosharenopen, DVIL04A = factor(DVIL04A))
compare.LIPR.plots(my_design = des1315_prsnosharenopen, results_dir = "./prs_nopen_comparedefs/", variable = "DVIL04A", 
                  var_levels = c("DVIL04A1", "DVIL04A2", "DVIL04A3", "DVIL04A4"),
                  var_labels = c("Employed", "Family worker", "Unemployed", "Inactive"), 
                  p_title = "HRP employment (ILO) by JAM definition - PRS no pensioner HRP", plot_mode = "var_compare")

des1315_prsnosharenopen <- update(des1315_prsnosharenopen, ECOBU = factor(ECOBU))
compare.LIPR.plots(my_design = des1315_prsnosharenopen, results_dir = "./prs_nopen_comparedefs/", variable = "ECOBU", 
                  var_levels = c("ECOBU1", "ECOBU2", "ECOBU3", "ECOBU4", "ECOBU5", "ECOBU6", "ECOBU7", "ECOBU8"),
                  var_labels = c("1+ self employed", "Sing/couple all FT", 
                                 "Couple, 1 FT, 1 PT",  "Couple, 1 FT, 1 not working", 
                                 "No FT, 1+ PT", "Workless, head/spouse aged 60+", 
                                 "Workless, head/spouse unemployed", "Workless, inactive" ),
                  p_title = "Employment of household by JAM definition - PRS no pensioner HRP", plot_mode = "var_compare")

des1315_prsnosharenopen <- update(des1315_prsnosharenopen, SELFDEMP = factor(SELFDEMP))
compare.LIPR.plots(my_design = des1315_prsnosharenopen, results_dir = "./prs_nopen_comparedefs/", variable = "SELFDEMP", 
                  var_levels = c("SELFDEMP1", "SELFDEMP2", "SELFDEMP3", "SELFDEMP4", "SELFDEMP5","SELFDEMP6", "SELFDEMP7",
                                 "SELFDEMP8", "SELFDEMP9", "SELFDEMP10"),
                  var_labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
                                 "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other"),
                  p_title = "Self-reported HRP situation by JAM definition - PRS no pensioner HRP", plot_mode = "var_compare")

des1315_prsnosharenopen <- update(des1315_prsnosharenopen, has_kids16 = factor(has_kids16))
compare.LIPR.plots(my_design = des1315_prsnosharenopen, results_dir = "./prs_nopen_comparedefs/", variable = "has_kids16", 
                  var_levels = c("has_kids160", "has_kids161"),
                  var_labels = c("No school age kids", "Has school age kids"), 
                  p_title = "Has children by JAM definition - PRS no pensioner HRP", plot_mode = "var_compare")

des1315_prsnosharenopen <- update(des1315_prsnosharenopen, single_parent = factor(single_parent))
compare.LIPR.plots(my_design = des1315_prsnosharenopen, results_dir = "./prs_nopen_comparedefs/", variable = "single_parent", 
                  var_levels = c("single_parent0", "single_parent1"),
                  var_labels = c("Not Single parent head", "Single parent head "),
                  p_title = "Is single parent by JAM definition - PRS no pensioner HRP", plot_mode = "var_compare")

des1315_prsnosharenopen <- update(des1315_prsnosharenopen, has_disabled = ifelse((has_disabledad ==1 | has_disabledch == 1), 1, 0))
des1315_prsnosharenopen <- update(des1315_prsnosharenopen, has_disabled = factor(has_disabled))
compare.LIPR.plots(my_design = des1315_prsnosharenopen, results_dir = "./prs_nopen_comparedefs/", variable = "has_disabled", 
                  var_levels = c("has_disabled0", "has_disabled1"),
                  var_labels = c("No disabled", "Has disabled person"), 
                  p_title = "Has disabled adult or child by JAM definition - PRS no pensioner HRP", plot_mode = "var_compare")

des1315_prsnosharenopen <- update(des1315_prsnosharenopen, no_savings = ifelse(TOTCAPB3==0, 1, 0))
des1315_prsnosharenopen <- update(des1315_prsnosharenopen, no_savings = factor(no_savings))
compare.LIPR.plots(my_design = des1315_prsnosharenopen, results_dir = "./prs_nopen_comparedefs/", variable = "no_savings", 
                  var_levels = c("no_savings0", "no_savings1"),
                  var_labels = c("No savings", "Has savings"), 
                  p_title = "Has savings by JAM definition - PRS no pensioner HRP", plot_mode = "var_compare")

dt1315_nosharenopen[ , JAMnohbrf_MIC_overlap := ifelse(JAMnohbrf_greqinc ==1 & grpd_tenure == 1 & under_ahc_mis ==1, 1, 0)]
dt1315_nosharenopen[ , JAMnohbrf_MIC_overlap := factor(JAMnohbrf_MIC_overlap)]
ggplot(dt1315_nosharenopen[!is.na(JAMnohbrf_MIC_overlap)& is_HRP==1,], aes(x = hh_grossinc_eq, fill = JAMnohbrf_MIC_overlap, weight = grossweight) ) +
  geom_histogram(binwidth = 1000) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  ggtitle("Gross equivalised household income: no pensioner HRP") +
  xlim(-100, 100000)
ggsave(filename = "./nopen_JAMdefs/JAMnohbrf_MIC_overlap_hhgrossinceq.png")


dt1315_nosharenopen[ , under_MIS_prs := factor(under_MIS_prs)]
ggplot(dt1315_nosharenopen[!is.na(under_MIS_prs)& is_HRP==1,], aes(x = hh_grossinc_eq, fill = under_MIS_prs, weight = grossweight) ) +
  geom_histogram(binwidth = 1000) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "grey85")) +
  ggtitle("Gross equivalised household income: no pensioner HRP") +
  xlim(-100, 100000)
ggsave(filename = "./nopen_JAMdefs/under_MIS_prs_hhgrossinceq.png")


#More LIPR compare plots, this time cf to all working age households ####
des1315_nosharenopen <- update(des1315_nosharenopen, GVTREGN = factor(GVTREGN ))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./nopen_comparedefs/", variable = "GVTREGN", 
                   var_levels = c("GVTREGNNorth East", "GVTREGNNorth West", "GVTREGNYorks & Humber", "GVTREGNEast Midlands", 
                                  "GVTREGNWest Midlands", "GVTREGNEast", "GVTREGNLondon", "GVTREGNSouth East", "GVTREGNSouth West"),
                   var_labels = c("North East", "North West", "Yorks & Humber", "East Midlands", 
                                  "West Midlands", "East", "London", "South East", "South West"),
                   p_title = "Region - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, HDAGE = factor(HDAGE))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./nopen_comparedefs/", variable = "HDAGE", 
                   var_levels = c("HDAGE1", "HDAGE2", "HDAGE3", "HDAGE4", "HDAGE5", "HDAGE6"),
                   var_labels = c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65+"),
                   p_title = "Age - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, DVIL04A = factor(DVIL04A))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./nopen_comparedefs/", variable = "DVIL04A", 
                   var_levels = c("DVIL04A1", "DVIL04A2", "DVIL04A3", "DVIL04A4"),
                   var_labels = c("Employed", "Family worker", "Unemployed", "Inactive"), 
                   p_title = "HRP employment (ILO) - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, ECOBU = factor(ECOBU))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./nopen_comparedefs/", variable = "ECOBU", 
                   var_levels = c("ECOBU1", "ECOBU2", "ECOBU3", "ECOBU4", "ECOBU5", "ECOBU6", "ECOBU7", "ECOBU8"),
                   var_labels = c("1+ self employed", "Sing/couple all FT", 
                                  "Couple, 1 FT, 1 PT",  "Couple, 1 FT, 1 not working", 
                                  "No FT, 1+ PT", "Workless, head/spouse aged 60+", 
                                  "Workless, head/spouse unemployed", "Workless, inactive" ),
                   p_title = "Employment of household - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, SELFDEMP = factor(SELFDEMP))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./nopen_comparedefs/", variable = "SELFDEMP", 
                   var_levels = c("SELFDEMP1", "SELFDEMP2", "SELFDEMP3", "SELFDEMP4", "SELFDEMP5","SELFDEMP6", "SELFDEMP7",
                                  "SELFDEMP8", "SELFDEMP9", "SELFDEMP10"),
                   var_labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
                                  "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other"),
                   p_title = "Self-reported HRP situation - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, has_kids16 = factor(has_kids16))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./nopen_comparedefs/", variable = "has_kids16", 
                   var_levels = c("has_kids160", "has_kids161"),
                   var_labels = c("No school age kids", "Has school age kids"), 
                   p_title = "Has children - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, single_parent = factor(single_parent))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./nopen_comparedefs/", variable = "single_parent", 
                   var_levels = c("single_parent0", "single_parent1"),
                   var_labels = c("Not Single parent head", "Single parent head "),
                   p_title = "Is single parent - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, has_disabled = ifelse((has_disabledad ==1 | has_disabledch == 1), 1, 0))
des1315_nosharenopen <- update(des1315_nosharenopen, has_disabled = factor(has_disabled))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./nopen_comparedefs/", variable = "has_disabled", 
                   var_levels = c("has_disabled0", "has_disabled1"),
                   var_labels = c("No disabled", "Has disabled person"), 
                   p_title = "Has disabled adult or child - no pensioner HRP", plot_mode = "var_compare")

des1315_nosharenopen <- update(des1315_nosharenopen, no_savings = ifelse(TOTCAPB3==0, 1, 0))
des1315_nosharenopen <- update(des1315_nosharenopen, no_savings = factor(no_savings))
compare.LIPR.plots.refine(my_design = des1315_nosharenopen, results_dir = "./nopen_comparedefs/", variable = "no_savings", 
                   var_levels = c("no_savings0", "no_savings1"),
                   var_labels = c("No savings", "Has savings"), 
                   p_title = "Has savings - no pensioner HRP", plot_mode = "var_compare")

### what % of obj group is in each def? ####
des1315_prsnosharenopen <- update(des1315_prsnosharenopen, JAMbase_greqinc = factor(JAMbase_greqinc))
svymean(~JAMbase_greqinc, design = subset(des1315_prsnosharenopen, under_ahc_mis == 1), na.rm = TRUE)


des1315_prsnosharenopen <- update(des1315_prsnosharenopen, JAMrf_greqinc = factor(JAMrf_greqinc))
svymean(~JAMrf_greqinc, design = subset(des1315_prsnosharenopen, under_ahc_mis == 1), na.rm = TRUE)

des1315_prsnosharenopen <- update(des1315_prsnosharenopen, JAMnohb_greqinc = factor(JAMnohb_greqinc))
svymean(~JAMnohb_greqinc, design = subset(des1315_prsnosharenopen, under_ahc_mis == 1), na.rm = TRUE)

des1315_prsnosharenopen <- update(des1315_prsnosharenopen, JAMnohbwork_greqinc = factor(JAMnohbwork_greqinc))
svymean(~JAMnohbwork_greqinc, design = subset(des1315_prsnosharenopen, under_ahc_mis == 1), na.rm = TRUE)

des1315_prsnosharenopen <- update(des1315_prsnosharenopen, JAMnohbrf_greqinc = factor(JAMnohbrf_greqinc))
svymean(~JAMnohbrf_greqinc, design = subset(des1315_prsnosharenopen, under_ahc_mis == 1), na.rm = TRUE)

des1315_prsnosharenopen <- update(des1315_prsnosharenopen, JAMbase_greqinc = factor(JAMbase_greqinc))
svymean(~JAMbase_greqinc, design = subset(des1315_prsnosharenopen, under_ahc_mis == 1), na.rm = TRUE)


svymean(~JAMbase_greqinc, design = subset(des1315_prsnosharenopen, aff_shared_noincben == 0), na.rm = TRUE)
svymean(~JAMrf_greqinc, design = subset(des1315_prsnosharenopen, aff_shared_noincben == 0), na.rm = TRUE)
svymean(~JAMnohb_greqinc, design = subset(des1315_prsnosharenopen, aff_shared_noincben == 0), na.rm = TRUE)
svymean(~JAMnohbwork_greqinc, design = subset(des1315_prsnosharenopen, aff_shared_noincben == 0), na.rm = TRUE)
svymean(~JAMnohbrf_greqinc, design = subset(des1315_prsnosharenopen, aff_shared_noincben == 0), na.rm = TRUE)



#% of working age households fitting JAM defs & PRS ####
svytotal(~JAMrf_greqinc, des1315_nosharenopen, na.rm = TRUE)

des1315_nosharenopen <- update(des1315_nosharenopen, JAMbase_prs = ifelse(JAMbase_greqinc == 1 & grpd_tenure ==1, 1, 0))
des1315_nosharenopen <- update(des1315_nosharenopen, JAMbase_prs = factor(JAMbase_prs))
svymean(~JAMbase_prs, des1315_nosharenopen, na.rm = TRUE)
svytotal(~JAMbase_prs, des1315_nosharenopen, na.rm = TRUE)

des1315_nosharenopen <- update(des1315_nosharenopen, JAMnohbrf_prs = ifelse(JAMnohbrf_greqinc == 1 & grpd_tenure ==1, 1, 0))
des1315_nosharenopen <- update(des1315_nosharenopen, JAMnohbrf_prs = factor(JAMnohbrf_prs))
svymean(~JAMnohbrf_prs, des1315_nosharenopen, na.rm = TRUE)
svytotal(~JAMnohbrf_prs, des1315_nosharenopen, na.rm = TRUE)

svyquantile(~hh_grossinc, design = subset(des1315_prsnosharenopen, aff_shared_noincben == 0), quantiles = 0.5, na.rm=TRUE)
svyquantile(~hh_grossinc, design = subset(des1315_prsnosharenopen, under_ahc_mis == 1 ), quantiles = 0.5, na.rm=TRUE)
svyquantile(~hh_grossinc, design = subset(des1315_prsnosharenopen, JAMbase_greqinc == 1 ), quantiles = 0.5, na.rm=TRUE)
svyquantile(~hh_grossinc, design = subset(des1315_prsnosharenopen, JAMnohbrf_greqinc == 1 ), quantiles = 0.5, na.rm=TRUE)

# Overlap group - JAM + objective def, proportion of working age
svymean(~(JAMbase_prs & under_MIS_prs), des1315_nosharenopen, na.rm = TRUE)
svytotal(~(JAMbase_prs & under_MIS_prs), des1315_nosharenopen, na.rm = TRUE)

svymean(~(JAMnohbrf_prs & under_MIS_prs), des1315_nosharenopen, na.rm = TRUE)
svytotal(~(JAMnohbrf_prs & under_MIS_prs), des1315_nosharenopen, na.rm = TRUE)

svyquantile(~hh_grossinc, design = subset(des1315_nosharenopen, JAMnohbrf_prs ==1 &under_ahc_mis == 1 ), quantiles = 0.5, na.rm=TRUE)

svymean(~(JAMbase_prs & no_shared_prs), des1315_nosharenopen, na.rm = TRUE)
svytotal(~(JAMbase_prs & no_shared_prs), des1315_nosharenopen, na.rm = TRUE)

svymean(~(JAMnohbrf_prs & no_shared_prs), des1315_nosharenopen, na.rm = TRUE)
svytotal(~(JAMnohbrf_prs & no_shared_prs), des1315_nosharenopen, na.rm = TRUE)

#overlap plots
des1315_nosharenopen <- update(des1315_nosharenopen, shared_JAM = ifelse(aff_shared_noincben == 0 & JAMnohbrf_greqinc == 1, 1, 0))


# proportion of objective definition captured by JAM

svymean(~(JAMbase_prs), design = subset(des1315_nosharenopen, under_MIS_prs == 1), na.rm = TRUE)
svymean(~(JAMrf_greqinc == 1 & grpd_tenure == 1), design = subset(des1315_nosharenopen, under_MIS_prs == 1), na.rm = TRUE)
svymean(~(JAMnohb_greqinc == 1 & grpd_tenure == 1), design = subset(des1315_nosharenopen, under_MIS_prs == 1), na.rm = TRUE)
svymean(~(JAMnohbwork_greqinc == 1 & grpd_tenure == 1), design = subset(des1315_nosharenopen, under_MIS_prs == 1), na.rm = TRUE)
svymean(~(JAMnohbrf_prs), design = subset(des1315_nosharenopen, under_MIS_prs == 1), na.rm = TRUE)

svymean(~(JAMbase_prs), design = subset(des1315_nosharenopen, no_shared_prs == 1), na.rm = TRUE)
svymean(~(JAMrf_greqinc == 1 & grpd_tenure == 1), design = subset(des1315_nosharenopen, no_shared_prs == 1), na.rm = TRUE)
svymean(~(JAMnohb_greqinc == 1 & grpd_tenure == 1), design = subset(des1315_nosharenopen, no_shared_prs == 1), na.rm = TRUE)
svymean(~(JAMnohbwork_greqinc == 1 & grpd_tenure == 1), design = subset(des1315_nosharenopen, no_shared_prs == 1), na.rm = TRUE)
svymean(~(JAMnohbrf_prs), design = subset(des1315_nosharenopen, no_shared_prs == 1), na.rm = TRUE)



#prop multiple benu
ftable(svyby(formula = ~factor(multiple_benu), by = ~GVTREGN, design = desall_1415, FUN = svymean, na.rm = TRUE))
desall_1415 <- update(desall_1415, BENUNITS = factor(BENUNITS))
ftable(svyby(formula = ~BENUNITS, by = ~GVTREGN, design = desall_1415, FUN = svymean, na.rm = TRUE))


