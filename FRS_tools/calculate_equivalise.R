calculate.equivalise <- function(DT) {

    # weight for 1st adult
  DT[ , equiv_weight := 0.67]
  
  #add for other adults
  DT[ , equiv_weight := equiv_weight + (sum(IsChild == 'FALSE')-1)*0.33, by=.(SERNUM, year)]
  
  #add for children over 14
  DT[ , equiv_weight := equiv_weight + (sum(IsChild == 'TRUE' & AGEc >= 14 & AGEc != 999))*0.33, by=.(SERNUM, year)]
  
  #add for children under 14
  DT[ , equiv_weight := equiv_weight + (sum(IsChild == 'TRUE' & AGEc < 14))*0.2, by=.(SERNUM, year)]
  
  #Make it a factor that income can be multiplied by
  DT[ , equiv_weight := 1/equiv_weight]
}