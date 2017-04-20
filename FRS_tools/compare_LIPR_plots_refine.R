#Makes a table and plots comparing different variables for renters selected using different 'low income' income definitions
#This is essentially a hack to replicate an 'overloaded' function in R
#also would be better to pass a vector of definitions that is iterated over - this way sucks!
compare.LIPR.plots.refine <- function(my_design, results_dir, variable, var_levels = c("default"), var_labels=c("default"), 
                              p_title = "title", plot_mode = "var_compare"){
  
  if(plot_mode == "proportion"){


    # t_noshared <- as.data.frame(svymean(as.formula(paste("~", variable)), 
    #                                    design = subset(my_design, aff_shared_noincben == 0 & grpd_tenure == 1),na.rm = TRUE)) 
    # setDT(t_noshared, keep.rownames = TRUE)[]
    # t_noshared <- t_noshared[2,] 
    # colnames(t_noshared)[1] <- "definition"
    # t_noshared$definition <- "No shared ownership PRS"
    
    t_ahc_mis <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                       design = subset(my_design, under_ahc_mis == 1 & grpd_tenure == 1),na.rm = TRUE)) 
    setDT(t_ahc_mis, keep.rownames = TRUE)[]
    t_ahc_mis <- t_ahc_mis[2,] 
    colnames(t_ahc_mis)[1] <- "definition"
    t_ahc_mis$definition <- "Under AHC MIS PRS"
    
    t_nosharedbenlim <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                           design = subset(my_design, aff_shared_noincben == 0 & grpd_tenure == 1 
                                                              & over20grossnohb_nohbben == 0 ),na.rm = TRUE)) 
    setDT(t_nosharedbenlim, keep.rownames = TRUE)[]
    t_nosharedbenlim<- t_nosharedbenlim[2,] 
    colnames(t_nosharedbenlim)[1] <- "definition"
    t_nosharedbenlim$definition <- "No shared ownership PRS ben limit"
    
    t_nosharedhrpwork <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                              design = subset(my_design, aff_shared_noincben == 0 & grpd_tenure == 1 
                                                              & hrp_nowork == FALSE ),na.rm = TRUE)) 
    setDT(t_nosharedhrpwork, keep.rownames = TRUE)[]
    t_nosharedhrpwork<- t_nosharedhrpwork[2,] 
    colnames(t_nosharedhrpwork)[1] <- "definition"
    t_nosharedhrpwork$definition <- "No shared ownership PRS HRP work"
    
    t_nosharedwork <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                               design = subset(my_design, aff_shared_noincben == 0 & grpd_tenure == 1 
                                                               & ECOBU!=6 & ECOBU!=7 & ECOBU!=8 ),na.rm = TRUE)) 
    setDT(t_nosharedwork, keep.rownames = TRUE)[]
    t_nosharedwork<- t_nosharedwork[2,] 
    colnames(t_nosharedwork)[1] <- "definition"
    t_nosharedwork$definition <- "No shared ownership PRS not workless"
    
    t_allworkage <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                              design = my_design,na.rm = TRUE)) 
    setDT(t_allworkage, keep.rownames = TRUE)[]
    t_allworkage<- t_allworkage[2,] 
    colnames(t_allworkage)[1] <- "definition"
    t_allworkage$definition <- "All working age"

    
    table_list <- list(t_ahc_mis, t_nosharedbenlim, t_nosharedhrpwork, t_nosharedwork, t_allworkage)
    t_prop<- rbindlist(table_list, fill = TRUE) 
    rm(table_list)
    rm(t_ahc_mis, t_nosharedbenlim, t_nosharedhrpwork, t_nosharedwork, t_allworkage)
    
    
    plot_a <- ggplot(na.omit(t_prop), aes(x = mean, xmin = mean-SE, xmax = mean+SE, y =definition,
                                              colour = definition)) +
      geom_point() + geom_segment( aes(x = mean-SE, xend = mean+SE, y = definition,
                                       yend=definition)) +
      #scale_colour_manual(values=c("grey19","firebrick3", "cyan4", "darkorange1")) +
      theme( legend.position = "top", panel.background = element_rect(fill = "white"),
             panel.grid.major = element_line(colour = "grey85")) +
      xlab("Prop of households") + guides(colour=FALSE, shape = FALSE) + ylab("Group")+
      ggtitle(p_title)
    ggsave(filename = paste0(results_dir, "/", p_title, ".png"), plot = plot_a)
    
    write.csv(t_prop, file = paste0(results_dir, "/",p_title, ".csv"))
    return()
  }
  
  else if (plot_mode == "var_compare"){
    
   
    
    t_noshared <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                        design = subset(my_design, aff_shared_noincben == 0 & grpd_tenure == 1), na.rm=TRUE))
    setDT(t_noshared, keep.rownames = TRUE)[]
    colnames(t_noshared)[1] <- variable
    t_noshared$definition <- "No shared ownership PRS"
    
    t_ahc_mis <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                       design = subset(my_design, under_ahc_mis == 1 & grpd_tenure == 1), na.rm=TRUE))
    setDT(t_ahc_mis, keep.rownames = TRUE)[]
    colnames(t_ahc_mis)[1] <- variable
    t_ahc_mis$definition <- "Under AHC MIS PRS"
    

    t_nosharedbenlim <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                              design = subset(my_design, aff_shared_noincben == 0 & grpd_tenure == 1 
                                                              & over20grossnohb_nohbben == 0 ), na.rm=TRUE))
    setDT(t_nosharedbenlim, keep.rownames = TRUE)[]
    colnames(t_nosharedbenlim)[1] <- variable
    t_nosharedbenlim$definition <- "No shared ownership ben limit PRS"
    
    t_nosharedhrpwork <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                              design = subset(my_design, aff_shared_noincben == 0 & grpd_tenure == 1 
                                                              & hrp_nowork == FALSE ), na.rm=TRUE))
    setDT(t_nosharedhrpwork, keep.rownames = TRUE)[]
    colnames(t_nosharedhrpwork)[1] <- variable
    t_nosharedhrpwork$definition <- "No shared ownership HRP work PRS"
    
    t_nosharedwork <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                               design = subset(my_design, aff_shared_noincben == 0 & grpd_tenure == 1 
                                                               & ECOBU!=6 & ECOBU!=7 & ECOBU!=8 ), na.rm=TRUE))
    setDT(t_nosharedwork, keep.rownames = TRUE)[]
    colnames(t_nosharedwork)[1] <- variable
    t_nosharedwork$definition <- "No shared ownership not workless PRS"
    
    t_allworkage <- as.data.frame(svymean(as.formula(paste("~", variable)), design = my_design, na.rm=TRUE))
    setDT(t_allworkage, keep.rownames = TRUE)[]
    colnames(t_allworkage)[1] <- variable
    t_allworkage$definition <- "All working age"
    
    table_list <- list(t_ahc_mis, t_nosharedbenlim, t_nosharedhrpwork, t_nosharedwork, t_allworkage)
    t_comp<- rbindlist(table_list, fill = TRUE) 
    rm(table_list)
    rm(t_ahc_mis, t_nosharedbenlim, t_nosharedhrpwork, t_nosharedwork, t_allworkage)
    
    t_comp[, (variable) := factor( get(variable), levels = var_levels, labels = var_labels)]
    
    print(t_comp)
    plot_b <- ggplot(na.omit(t_comp), aes(x = mean, xmin = mean-SE, xmax = mean+SE, y =definition,
                                            colour = definition)) +
      geom_point() + geom_segment( aes(x = mean-SE, xend = mean+SE, y = definition,
                                       yend=definition)) +
      #scale_colour_manual(values=c("grey19","firebrick3", "cyan4", "darkorange1")) +
      theme( legend.position = "top", panel.background = element_rect(fill = "white"),
             panel.grid.major = element_line(colour = "grey85")) +
      xlab("Prop of households in group") + guides(colour=FALSE, shape = FALSE) + ylab("Group")+
      ggtitle(p_title) + facet_grid( get(variable) ~ .)
    print(plot_b)
    ggsave(filename = paste0(results_dir, "/", p_title, ".png"), plot = plot_b)
    
    write.csv(t_comp, file = paste0(results_dir, "/",p_title, ".csv"))
    return()
  }
  
}