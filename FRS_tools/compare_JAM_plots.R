#Makes a table and plots comparing different variables for renters selected using different 'JAM' income definitions
#This is essentially a hack to replicate an 'overloaded' function in R

compare.JAM.plots <- function(my_design, results_dir, variable, var_levels = c("default"), var_labels=c("default"), 
                                     p_title = "title", plot_mode = "var_compare"){
  
  if(plot_mode == "proportion"){
    t_JAMbase_greq <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                            design = subset(my_design, JAMbase_greqinc ==1),na.rm = TRUE))
    
    setDT(t_JAMbase_greq, keep.rownames = TRUE)[]
    t_JAMbase_greq<- t_JAMbase_greq[2,] 
    colnames(t_JAMbase_greq)[1] <- "definition"
    t_JAMbase_greq$definition <- "JAM baseline"
    
    
    t_JAMrf_greq <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                          design = subset(my_design, JAMrf_greqinc ==1),na.rm = TRUE))
      
    setDT(t_JAMrf_greq, keep.rownames = TRUE)[]
    t_JAMrf_greq<- t_JAMrf_greq[2,] 
    colnames(t_JAMrf_greq)[1] <- "definition"
    t_JAMrf_greq$definition <- "JAM RF"
    
    t_JAMnohbrf_greq <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                              design = subset(my_design, JAMnohbrf_greqinc ==1),na.rm = TRUE)) 
    setDT(t_JAMnohbrf_greq, keep.rownames = TRUE)[]
    t_JAMnohbrf_greq<- t_JAMnohbrf_greq[2,] 
    colnames(t_JAMnohbrf_greq)[1] <- "definition"
    t_JAMnohbrf_greq$definition <- "JAM no HB RF"
    
    t_JAMnohb_greq <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                            design = subset(my_design, JAMrf_greqinc ==1),na.rm = TRUE))
      as.data.frame(svymean(~under_MIS_prs, design = subset(my_design, JAMnohb_greqinc ==1& grpd_tenure == 1), na.rm=TRUE))
    setDT(t_JAMnohb_greq, keep.rownames = TRUE)[]
    t_JAMnohb_greq<- t_JAMnohb_greq[2,] 
    colnames(t_JAMnohb_greq)[1] <- "definition"
    t_JAMnohb_greq$definition <- "JAM no HB"
    
    
    t_JAMnohbwork_greq <- as.data.frame(svymean(as.formula(paste("~", variable)), 
                                                design = subset(my_design, JAMnohbwork_greqinc ==1),na.rm = TRUE))
    setDT(t_JAMnohbwork_greq, keep.rownames = TRUE)[]
    t_JAMnohbwork_greq<- t_JAMnohbwork_greq[2,] 
    colnames(t_JAMnohbwork_greq)[1] <- "definition"
    t_JAMnohbwork_greq$definition <- "JAM no HB working"
    
    table_list <- list(t_JAMbase_greq, t_JAMrf_greq, t_JAMnohbrf_greq, t_JAMnohb_greq, t_JAMnohbwork_greq )
    t_prop_JAM<- rbindlist(table_list, fill = TRUE) 
    rm(table_list)
    rm(t_JAMbase_greq, t_JAMrf_greq, t_JAMnohbrf_greq, t_JAMnohb_greq, t_JAMnohbwork_greq)
    
    
    plot_a <- ggplot(na.omit(t_prop_JAM), aes(x = mean, xmin = mean-SE, xmax = mean+SE, y =definition,
                                         colour = definition)) +
      geom_point() + geom_segment( aes(x = mean-SE, xend = mean+SE, y = definition,
                                       yend=definition)) +
      #scale_colour_manual(values=c("grey19","firebrick3", "cyan4", "darkorange1")) +
      theme( legend.position = "top", panel.background = element_rect(fill = "white"),
             panel.grid.major = element_line(colour = "grey85")) +
      xlab("Prop of JAM households") + guides(colour=FALSE, shape = FALSE) + ylab("JAM definition")+
      ggtitle(p_title)
    ggsave(filename = paste0(results_dir, "/", p_title, ".png"), plot = plot_a)
    
    write.csv(t_prop_JAM, file = paste0(results_dir, "/",p_title, ".csv"))
    return()
  }
  
  else if (plot_mode == "var_compare"){
    t_JAMbase_greq <- as.data.frame(svymean(as.formula(paste("~", variable)), design = subset(my_design, JAMbase_greqinc ==1), na.rm=TRUE))
    setDT(t_JAMbase_greq, keep.rownames = TRUE)[]
    colnames(t_JAMbase_greq)[1] <- variable
    t_JAMbase_greq$definition <- "JAM baseline"
    
    t_JAMrf_greq <- as.data.frame(svymean(as.formula(paste("~", variable)), design = subset(my_design, JAMrf_greqinc ==1), na.rm=TRUE))
    setDT(t_JAMrf_greq, keep.rownames = TRUE)[]
    colnames(t_JAMrf_greq)[1] <- variable
    t_JAMrf_greq$definition <- "JAM RF"
    
    t_JAMnohbrf_greq <- as.data.frame(svymean(as.formula(paste("~", variable)), design = subset(my_design, JAMnohbrf_greqinc ==1), na.rm=TRUE))
    setDT(t_JAMnohbrf_greq, keep.rownames = TRUE)[]
    colnames(t_JAMnohbrf_greq)[1] <- variable
    t_JAMnohbrf_greq$definition <- "JAM no HB modified RF"
    
    t_JAMnohb_greq <- as.data.frame(svymean(as.formula(paste("~", variable)), design = subset(my_design, JAMnohb_greqinc ==1), na.rm=TRUE))
    setDT(t_JAMnohb_greq, keep.rownames = TRUE)[]
    colnames(t_JAMnohb_greq)[1] <- variable
    t_JAMnohb_greq$definition <- "JAM no HB"
    
    
    t_JAMnohbwork_greq <- as.data.frame(svymean(as.formula(paste("~", variable)), design = subset(my_design, JAMnohbwork_greqinc ==1), na.rm=TRUE))
    setDT(t_JAMnohbwork_greq, keep.rownames = TRUE)[]
    colnames(t_JAMnohbwork_greq)[1] <- variable
    t_JAMnohbwork_greq$definition <- "JAM no HB and working"
    
    
    list <- list(t_JAMbase_greq, t_JAMrf_greq, t_JAMnohb_greq, t_JAMnohbrf_greq, t_JAMnohbwork_greq)
    t_jamdef <- rbindlist(list, fill = TRUE) 
    rm(list)
    rm(t_JAMbase_greq, t_JAMrf_greq, t_JAMnohb_greq, t_JAMnohbrf_greq, t_JAMnohbwork_greq)
    
    t_jamdef[, (variable) := factor( get(variable), levels = var_levels, labels = var_labels)]

    print(t_jamdef)
    plot_b <- ggplot(na.omit(t_jamdef), aes(x = mean, xmin = mean-SE, xmax = mean+SE, y =definition,
                                         colour = definition)) +
      geom_point() + geom_segment( aes(x = mean-SE, xend = mean+SE, y = definition,
                                       yend=definition)) +
      #scale_colour_manual(values=c("grey19","firebrick3", "cyan4", "darkorange1")) +
      theme( legend.position = "top", panel.background = element_rect(fill = "white"),
             panel.grid.major = element_line(colour = "grey85")) +
      xlab("Prop of JAM group") + guides(colour=FALSE, shape = FALSE) + ylab("JAM definition")+
      ggtitle(p_title) + facet_grid( get(variable) ~ .)
    print(plot_b)
    ggsave(filename = paste0(results_dir, "/", p_title, ".png"), plot = plot_b)

    write.csv(t_jamdef, file = paste0(results_dir, "/",p_title, ".csv"))
    return()
  }
  
}