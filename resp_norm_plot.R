# normalized plots


resp_norm_plot <- function(df,col_param,resp_type,
                           fill_param,shape_param,res,
                           p_size){
  
  if(resp_type == "head"){
    plt =  ggplot(df, 
                  aes(bins, 
                      ec_norm, 
                      color = factor(cde))) +
      stat_summary(data = df,
                   fun.data = mean_se, 
                   geom = "pointrange",
                   aes(fill = factor(cde),
                       shape = factor(cde)),
                   size = p_size,
                   fatten = 5,
                   stroke = 0.3)
    
  } else {
    
    df_mean <- df %>% 
      group_by(cde,bins) %>%
      summarise(resp_norm = mean(resp_norm)) 
    
    # bin_max_rate <- df_mean[which.max(df_mean$resp_norm),][2] %>% as.numeric()
    
    df_max <- df_mean %>%
      filter(resp_norm == max(resp_norm)) %>%
      as.data.frame()
    
    # df_max[,2]
    # 
    # df_max_rate <- data.frame(x = df_max[1,2],
    #                           ymin = df_max[1,3],
    #                           ymax = df_max[2,3],
    #                           cde = 1)
    
    plt = ggplot(df, 
                 aes(bins, 
                     resp_norm, 
                     color = factor(cde))) +
      stat_summary(data = df,
                   fun.data = mean_se, 
                   geom = "pointrange",
                   aes(fill = factor(cde),
                       shape = factor(cde)),
                   size = p_size,
                   fatten = 10,
                   stroke = 0.5) + 
      geom_segment(data = df_max,aes(x = bins,
                                      xend = bins,
                                      y = resp_norm + 0.2,
                                      yend = resp_norm + 0.05),
                    color = "black",
                    arrow = arrow(length = unit(0.2,"cm")),size = 1)
  }
  plt +
    scale_shape_manual(name ="", values = shape_param,
                       labels=c("1" = "Tandem","2"="Yoked"))+
    scale_color_manual(name ="",values = col_param,
                       labels=c("1" = "Tandem","2"="Yoked"))+
    scale_fill_manual(name ="",values = fill_param,
                      labels=c("1" = "Tandem","2"="Yoked"))+
    scale_x_continuous(limits = c(0,180/res),
                       labels = every_nth(seq(0,180,20),3,inverse = TRUE),
                       breaks = seq(0,180/res,20/res), 
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0,1.125,0.125),
                       expand = c(0,0),
                       limits = c(0,1.125),
                       labels = every_nth(seq(0,1.125,0.125),2,inverse = TRUE))+
    labs(x = "Time in trial (s)", y = TeX("Normalized response rate ($\\pm SEM$)")) +
    geom_vline(xintercept = 60/res, linetype = "dashed")+
    expand_limits(y = c(0,1),x = c(0,182))+
    guides(shape = guide_legend(override.aes = list(size = 0.5)))+
    pub_theme
}
