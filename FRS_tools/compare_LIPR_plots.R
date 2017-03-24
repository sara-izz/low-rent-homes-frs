#Makes a table and plots comparing different variables for renters selected using different 'low income' income definitions
#This is essentially a hack to replicate an 'overloaded' function in R
#Apparently this is better done according to 
compare.LIPR.plots <- function(my_design, results_dir, variable, var_levels = c("default"), var_labels=c("default"), 
                              p_title = "title", plot_mode = "var_compare"){
  
  if(plot_mode == "proportion"){

    t_ahc_mis <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                              design = subset(my_design, under_ahc_mis == 1),na.rm = TRUE)) 
    setDT(t_ahc_mis, keep.rownames = TRUE)[]
    t_ahc_mis <- t_ahc_mis[2,] 
    colnames(t_ahc_mis)[1] <- "definition"
    t_ahc_mis$definition <- "Under AHC MIS"
    
    t_noshared <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                       design = subset(my_design, aff_shared_noincben == 0),na.rm = TRUE)) 
    setDT(t_noshared, keep.rownames = TRUE)[]
    t_noshared <- t_noshared[2,] 
    colnames(t_noshared)[1] <- "definition"
    t_noshared$definition <- "No shared ownership"
    
    t_JAMbase_greq <- as.data.frame(svymean(as.formula(paste("~", variable)), design = subset(my_design, JAMbase_greqinc ==1), na.rm=TRUE))
    setDT(t_JAMbase_greq, keep.rownames = TRUE)[]
    colnames(t_JAMbase_greq)[1] <- variable
    t_JAMbase_greq$definition <- "JAM baseline"
    
    t_JAMnohbrf_greq <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                              design = subset(my_design, JAMnohbrf_greqinc ==1),na.rm = TRUE)) 
    setDT(t_JAMnohbrf_greq, keep.rownames = TRUE)[]
    t_JAMnohbrf_greq<- t_JAMnohbrf_greq[2,] 
    colnames(t_JAMnohbrf_greq)[1] <- "definition"
    t_JAMnohbrf_greq$definition <- "JAM no HB RF"
    

    
    table_list <- list(t_ahc_mis, t_noshared, t_JAMbase_greq, t_JAMnohbrf_greq)
    t_prop<- rbindlist(table_list, fill = TRUE) 
    rm(table_list)
    rm(t_ahc_mis, t_noshared, t_JAMbase_greq, t_JAMnohbrf_greq)
    
    
    plot_a <- ggplot(na.omit(t_prop), aes(x = mean, xmin = mean-SE, xmax = mean+SE, y =definition,
                                              colour = definition)) +
      geom_point() + geom_segment( aes(x = mean-SE, xend = mean+SE, y = definition,
                                       yend=definition)) +
      #scale_colour_manual(values=c("grey19","firebrick3", "cyan4", "darkorange1")) +
      theme( legend.position = "top", panel.background = element_rect(fill = "white"),
             panel.grid.major = element_line(colour = "grey85")) +
      xlab("Prop of LIPR households") + guides(colour=FALSE, shape = FALSE) + ylab("LIPR definition")+
      ggtitle(p_title)
    ggsave(filename = paste0(results_dir, "/", p_title, ".png"), plot = plot_a)
    
    write.csv(t_prop, file = paste0(results_dir, "/",p_title, ".csv"))
    return()
  }
  
  else if (plot_mode == "var_compare"){
    
    t_ahc_mis <- as.data.frame(svymean(as.formula(paste("~", variable)), design = subset(my_design, under_ahc_mis == 1), na.rm=TRUE))
    setDT(t_ahc_mis, keep.rownames = TRUE)[]
    colnames(t_ahc_mis)[1] <- variable
    t_ahc_mis$definition <- "Under AHC MIS"
    
    t_noshared <- as.data.frame(svymean(as.formula(paste("~", variable)), design = subset(my_design, aff_shared_noincben == 0), na.rm=TRUE))
    setDT(t_noshared, keep.rownames = TRUE)[]
    colnames(t_noshared)[1] <- variable
    t_noshared$definition <- "No shared ownership"
    
    t_JAMbase_greq <- as.data.frame(svymean(as.formula(paste("~", variable)), design = subset(my_design, JAMbase_greqinc ==1), na.rm=TRUE))
    setDT(t_JAMbase_greq, keep.rownames = TRUE)[]
    colnames(t_JAMbase_greq)[1] <- variable
    t_JAMbase_greq$definition <- "JAM baseline"
    

    t_JAMnohbrf_greq <- as.data.frame(svymean(as.formula(paste("~", variable)), design = subset(my_design, JAMnohbrf_greqinc ==1), na.rm=TRUE))
    setDT(t_JAMnohbrf_greq, keep.rownames = TRUE)[]
    colnames(t_JAMnohbrf_greq)[1] <- variable
    t_JAMnohbrf_greq$definition <- "JAM no HB modified RF"
    
    
    
    list <- list(t_ahc_mis, t_noshared, t_JAMbase_greq, t_JAMnohbrf_greq)
    t_comp <- rbindlist(list, fill = TRUE) 
    rm(list)
    rm(t_ahc_mis, t_noshared, t_JAMbase_greq, t_JAMnohbrf_greq)
    
    t_comp[, (variable) := factor( get(variable), levels = var_levels, labels = var_labels)]
    
    print(t_comp)
    plot_b <- ggplot(na.omit(t_comp), aes(x = mean, xmin = mean-SE, xmax = mean+SE, y =definition,
                                            colour = definition)) +
      geom_point() + geom_segment( aes(x = mean-SE, xend = mean+SE, y = definition,
                                       yend=definition)) +
      #scale_colour_manual(values=c("grey19","firebrick3", "cyan4", "darkorange1")) +
      theme( legend.position = "top", panel.background = element_rect(fill = "white"),
             panel.grid.major = element_line(colour = "grey85")) +
      xlab("Prop of LIPR group") + guides(colour=FALSE, shape = FALSE) + ylab("LIPR definition")+
      ggtitle(p_title) + facet_grid( get(variable) ~ .)
    print(plot_b)
    ggsave(filename = paste0(results_dir, "/", p_title, ".png"), plot = plot_b)
    
    write.csv(t_comp, file = paste0(results_dir, "/",p_title, ".csv"))
    return()
  }
  
}