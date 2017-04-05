#Function to plot points with error bars of a variable comparing each category by region
#To plot on subset, create subset of main survey design then call function
# NB: 'variable' must be factor; use update(my_design, factor) before using this function

plot.survey.grpdregion <- function(my_design, results_dir, variable, var_levels, var_labels, p_title = "title", svy_fun = svymean, region = "GVTREGN", p_type = "point"){
  
  ftable(svyby(formula = as.formula(paste("~", variable)), by = as.formula(paste("~", region)), 
               design = my_design, FUN = svy_fun, na.rm = TRUE))
  
  
  t_variable <- as.data.frame( ftable(svyby(formula = as.formula(paste("~", variable)), by = as.formula(paste("~", region)),
                                              design = my_design,
                                              FUN = svy_fun, na.rm = TRUE))
  )
  
  totform <- reformulate(termlabels = c(variable))
  t_total <- as.data.frame(ftable(svymean(totform, design = my_design, na.rm = TRUE))
  )
  
  t_total$region <- "England"
  t_total <- dcast(t_total, region + Var1 ~ Var2, value.var = "Freq")
  colnames(t_total)[2] <- "category"
  colnames(t_total)[3] <- "svy_fun"  
  colnames(t_total)[4] <- "SE"  


  t_variable <- dcast(t_variable, paste(paste(region, collapse = "+" ), "+ Var3 ~ Var2" ))
  colnames(t_variable)[1] <- "region"
  colnames(t_variable)[2] <- "category"
  colnames(t_variable)[3] <- "svy_fun"  
  colnames(t_variable)[4] <- "SE" 

  l <- list(t_variable, t_total)
  t_variable <- rbindlist(l, fill = TRUE)
  t_variable$category <- factor( t_variable$category, levels = var_levels,
                                 labels = var_labels)

  #write csv only if plot not needed
  if(p_type == "none"){
    write.csv( t_variable, file = paste0(results_dir, "/",p_title, ".csv"))
  }
  
  #Plot as points with error bars
  else if(p_type == "point"){
    p <- ggplot(na.omit(t_variable), aes(x = svy_fun, xmin = svy_fun-SE, xmax = svy_fun+SE, y =region,
                                         colour = region)) +
      geom_point() + geom_segment( aes(x = svy_fun-SE, xend = svy_fun+SE, y = region,
                                       yend=region)) +
      #scale_colour_manual(values=c("grey19","firebrick3", "cyan4", "darkorange1")) +
      theme( legend.position = "top", panel.background = element_rect(fill = "white"),
             panel.grid.major = element_line(colour = "grey85")) +
      xlab("Prop in category") + guides(colour=FALSE, shape = FALSE) + ylab("Region")+
      ggtitle(p_title) + facet_grid( category~ .)

    ggsave(filename = paste0(results_dir, "/", p_title, ".png"), plot = p)
    write.csv( t_variable, file = paste0(results_dir, "/",p_title, ".csv"))
  }

  else if(p_type == "bar" ){
     #Plot as bars
     b <- ggplot(na.omit(t_variable), aes(x = category, y = svy_fun, fill = region)) +
       #geom_freqpoly( aes(group = factor(aff_shared_noincben)))+
       geom_bar(stat="identity", position = position_dodge()) +  
       #scale_fill_manual(values=c("#A7A8AA","#FF0000")) +
       guides(fill=guide_legend(title=NULL)) +
       theme( legend.position = "top", panel.background = element_rect(fill = "white") ) +
       xlab(paste(variable)) + ylab("Proportion") 
     
     #print(b)
     ggsave(filename = paste0(results_dir, "/", p_title, ".png"), plot = b)
     write.csv( t_variable, file = paste0(results_dir, "/",p_title, ".csv"))
   }
  
  if(p_type == "hbar"){
    #Plot as bars
    hb <- ggplot(na.omit(t_variable), aes(x = category, y = svy_fun, fill = region)) +
      #geom_freqpoly( aes(group = factor(aff_shared_noincben)))+
      geom_bar(stat="identity", position = position_dodge()) + 
      coord_flip() +
      #scale_fill_manual(values=c("#A7A8AA","#FF0000")) +
      guides(fill=guide_legend(title=NULL)) +
      theme(axis.title.y = element_blank()) +
      theme( legend.position = "top", panel.background = element_rect(fill = "white") ) +
      ylab("Proportion")
    
    #print(hb)
    ggsave(filename = paste0(results_dir, "/", p_title, ".png"), plot = hb)
    write.csv( t_variable, file = paste0(results_dir, "/",p_title, ".csv"))
  }


  
}