#Calculates diferent measures of household income, for either HBAI or FRS
#Also calculates housing/income ratios
calculate.household.incomes <- function(data_table, survey, equivalise){
  data_table[ , not_1stbenu := ifelse(BENUNIT >1 , 1, 0)]
  data_table[ , multiple_benu := ifelse( (sum(not_1stbenu) > 0), 1, 0 ), by = .(SERNUM, year) ]
  data_table[ , nohbai_inc := ifelse( (is.na(ESGINCHH) | is.na(BHCDEF) | is.na(HBENBU)) , 1, 0) ]
  
  if(survey == "HBAI") {
    # incomes based on HBAI variables ####
    ## simple gross household income
    data_table[ ,hh_grossinc := ESGINCHH * BHCDEF * 52]
    
    
    ## gross household income minus HB (following definiton only suitable for households with 1 ben unit)
    #deflate SPI'd gross hhold income to average of survey year
    data_table[  multiple_benu == 0, hh_grossinc_nohb := (ESGINCHH - HBENBU) * BHCDEF * 52]

    ## gross household income minus income related benefits (including HB) - most relevant when thinking about ownership products
    data_table[ , hh_grossinc_noincben := (HHINC - HHIRBEN) * BHCDEF * 52]

    ## gross employment income for the household
    data_table[ , hh_grossinc_earn := (ESGJOBHH) * BHCDEF * 52]

  }
  
  if(survey == "FRS"){
    # Incomes based on FRS variables - so can use GROSS4 and not have to worry about restricting to 1BU ####
    #disadvantage is can't use HBAI in-year deflators; mainly an issue for CPI drop in 14/15
    ## simple gross household income

    data_table[ ,hh_grossinc := HHINC * 52]
   
    # gross household income minus housing benefit
    #Calculate factor to convert HB in to weekly amount
    data_table[ HBENPD == 1, hb_conv := 1]
    data_table[ HBENPD == 2, hb_conv := (1/2)]
    data_table[ HBENPD == 3, hb_conv := (1/3)]
    data_table[ HBENPD == 4, hb_conv := (1/4)]
    data_table[ HBENPD == 5, hb_conv := (12/52)]
    data_table[ HBENPD == 52, hb_conv := (1/52)]
    data_table[ !is.na(HBENAMT) & is.na(HBENPD), hb_conv := (1/4)]
    data_table[ , hh_hbweekly := HBENAMT*hb_conv]
    
    data_table[ , hh_grossinc_nohb := ifelse(!is.na(hh_hbweekly), (HHINC - hh_hbweekly) * 52, HHINC*52)]
  
    ## gross household income minus income related benefits (including HB) - most relevant when thinking about ownership products
    data_table[ , hh_grossinc_noincben := (HHINC - HHIRBEN) * 52]
     
    ## gross employment income for the household
    data_table[ , hh_grossinc_earn := (HEARNS) * 52]
    
    ## After housing costs income for the household
    data_table[ , hh_gross_ahc := (HHINC - GBHSCOST) * 52]
   
    if(equivalise == TRUE){
      ##Equivalised
      data_table[ ,hh_grossinc_eq := HHINC * 52 * equiv_weight]
      
       ## gross household income minus HB 
      data_table[ HBENPD == 1, hb_conv := 1]
      data_table[ HBENPD == 2, hb_conv := (1/2)]
      data_table[ HBENPD == 3, hb_conv := (1/3)]
      data_table[ HBENPD == 4, hb_conv := (1/4)]
      data_table[ HBENPD == 5, hb_conv := (12/52)]
      data_table[ HBENPD == 52, hb_conv := (1/52)]
      data_table[ !is.na(HBENAMT) & is.na(HBENPD), hb_conv := (1/4)]
      data_table[ , hh_hbweekly := HBENAMT*hb_conv]
      
      data_table[ , hh_grossinc_nohb_eq := ifelse(!is.na(hh_hbweekly), (HHINC - hh_hbweekly) * 52 * equiv_weight, HHINC*52 * equiv_weight)]
     
      ## gross household income minus income related benefits (including HB) - most relevant when thinking about ownership products
      data_table[ , hh_grossinc_noincben_eq := (HHINC - HHIRBEN) * 52 * equiv_weight]
        
      ## gross employment income for the household
      data_table[ , hh_grossinc_earn_eq := (HEARNS) * 52 * equiv_weight]
         
      # After housing costs income for the household
      data_table[ , hh_gross_ahc_eq := (HHINC - GBHSCOST) * 52 * equiv_weight]
     
    }
    
    
    #Deflate housing costs to 14/15 prices
    data_table[ (year == '1415'), housing_cost_1415 := GBHSCOST]
    #inflate 1314 to 1415 prices
    data_table[ (year == '1314'), housing_cost_1415:= GBHSCOST * (99.98/98.94)]
    #inflate 1213 to 1415 prices
    data_table[ (year == '1213'), housing_cost_1415:= GBHSCOST * (99.98/96.70)]
    data_table[ (year == '1112'), housing_cost_1415:= GBHSCOST * (99.98/94.23)]
    data_table[ (year == '1011'), housing_cost_1415:= GBHSCOST * (99.98/90.33)]
    data_table[ (year == '0910'), housing_cost_1415:= GBHSCOST * (99.98/87.28)]
    data_table[ (year == '0809'), housing_cost_1415:= GBHSCOST * (99.98/85.35)]
    data_table[ (year == '0708'), housing_cost_1415:= GBHSCOST * (99.98/82.27)]
    
    #Deflate savings to 14/15 prices
    data_table[ (year == '1415'), bu_savings_1415 := TOTCAPB3]
    #inflate 1314 to 1415 prices
    data_table[ (year == '1314'), bu_savings_1415 := TOTCAPB3 * (99.98/98.94)]
    #inflate 1213 to 1415 prices
    data_table[ (year == '1213'), bu_savings_1415 := TOTCAPB3 * (99.98/96.70)]
    data_table[ (year == '1112'), bu_savings_1415 := TOTCAPB3 * (99.98/94.23)]
    data_table[ (year == '1011'), bu_savings_1415 := TOTCAPB3 * (99.98/90.33)]
    data_table[ (year == '0910'), bu_savings_1415 := TOTCAPB3 * (99.98/87.28)]
    data_table[ (year == '0809'), bu_savings_1415 := TOTCAPB3 * (99.98/85.35)]
    data_table[ (year == '0708'), bu_savings_1415 := TOTCAPB3 * (99.98/82.27)]
  }
  
  data_table[ , housing_cost_gross := (housing_cost*52)/hh_grossinc ]
  #without hb
  data_table[ , housing_cost_gnohb := (housing_cost*52)/hh_grossinc_nohb ]
  #earned income
  data_table[ , housing_cost_earn := (housing_cost*52)/hh_grossinc_earn ]
}
