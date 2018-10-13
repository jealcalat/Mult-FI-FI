
plt_metrics_dot_err <- function(df,metric,col_param,fill_param,theme){
  
  x <- "phase"
  y <- metric
  pd <- position_dodge(0.9)
  
  df_sum <- df %>% 
    `[`(c(y,x,"cde")) %>%
    group_by(phase,cde) %>%
    summarise(y_m = mean(!!sym(y)),
              y.max = upper(!!sym(y)))
  
  median.quartile <- function(x){
    out <- quantile(x, probs = c(0.25,0.5,0.75))
    names(out) <- c("ymin","y","ymax")
    return(out) 
  }
  
  if( y == "high_rate"){
    y.axis <- "High rate (r/s)"
    # 3 valores para breaks
    breaks <- df[[y]]
    breaks <- c(min(floor(breaks)),
                max(ceiling(breaks))/4,
                max(ceiling(breaks))/2,
                (max(ceiling(breaks))/2 + max(ceiling(breaks)))/2,
                max(ceiling(breaks)))
  } else if (y == "r2"){
    y.axis <- "High rate (r/s)"
    breaks <- df[[y]]
    breaks <- c(min(floor(breaks)),
                max(ceiling(breaks))/4,
                max(ceiling(breaks))/2,
                (max(ceiling(breaks))/2 + max(ceiling(breaks)))/2,
                max(ceiling(breaks)))
  } else {
    y.axis <- paste(tools::toTitleCase(y),"(s)")
    
    breaks <- df[[y]] %>% 
    {
      d <- .
      max_bin <- max(d) %>% ceiling()
      breaks <- d %>% cut((0:max_bin) * 10) %>% 
        ifelse(is.na(.),0,.) %>% sort()
      breaks <- breaks %>% 
        unique()
      # breaks <- seq(from = min(breaks),
      #               to   = max(breaks),
      #               by   = 20)
      boolean <- breaks %% 2
      breaks <- breaks[!boolean] * 10
    } 
  }
  
  plot <- 
    ggplot(df,aes(x = rev(phase), color = factor(cde),fill = factor(cde))) +
    geom_violin(aes_string(y = y),alpha = 0.5, lwd = 0.4,
                position = pd, draw_quantiles = c(0.25,0.5,0.75))+
    # stat_summary(aes_string(y = y),position = pd,
    #   fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    #   geom = "pointrange", color = "black"
    # ) +
    # stat_summary(aes_string(y = y),position = pd,
    #            fun.y=median.quartile,geom='point',shape=95,size = 8) +
    
    # stat_summary(aes_string(y = y),position = pd,
    #              fun.y=median.quartile,geom='line') +
    # geom_boxplot(aes_string(y = y),alpha = 0.5, lwd = 0.2,
    #              position = pd,width = 0.2)+
  # scale_fill_manual(name ="Component",values = fill_param,
  #                   labels=c("1" = "Tandem","2"="Yoked"))+
  scale_x_discrete(labels=c("Peak Training","Yoked"))+
    labs(y = y.axis,x = "Phase") +
    scale_color_manual(name ="Component",values = col_param,
                       labels=c("1" = "Tandem","2"="Yoked"))+
    scale_fill_manual(name ="",values = fill_param,
                      labels = c("1" = "Training","2" = "Yoked"))+
    scale_y_continuous(breaks = breaks,
                       labels = every_nth(breaks,2,inverse = TRUE)) +
    expand_limits(y=c(max(breaks)))+
    pub_theme
  plot
  ggsave(filename = paste0("~/Dropbox/mult_IF_aco_exp/Plots_&_dfs/Final/actualización/kmeans",y,".pdf"),
         plot = plot,dpi = 600,height = 1.77*1.5,width = 1.77*1.9)
  
}
