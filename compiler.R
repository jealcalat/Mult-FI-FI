# compiler.R
# Compila los programas para nacer los análisis necesarios.
# wrapper
# version 2
source("~/Dropbox/mult_IF_aco_exp/v2/wrapper.R")
# version 3
source("~/Dropbox/mult_IF_aco_exp/v3/wrapper.R")

## changepoints methods
# # 1. low_high_low ---- 
# 
df_p_lhl <- pooled_ind_ana_df(326:333,25,30) %>% as.data.frame()

tot_p <- nrow(df_p_lhl)
df_p_lhl<- df_p_lhl %>%
  filter(start < stop)
filt1_p <- nrow(df_p_lhl)
df_p_lhl <- df_p_lhl %>%
  filter(r1 < high_rate & high_rate > r3)
filt2_p <- nrow(df_p_lhl)
p_loss <- (tot_p - filt2_p)/tot_p

# metrics_vec <- c("stop","start","middle","high_rate","spread")

# # total length = 1673
# # after filtering for start < stop = 1613
# # after filtering for high_rate > r1 & r3 = 1419
# 
# # 2. Fit rectangular function
# 
# df_p <- pooled_ind_ana_pwise_df(326:333,18,30) %>% as.data.frame()
# 
# # write df
# 
# write.csv(df_p,"individual_trials_metrics_pwise.csv",row.names = F)

# filter for r2<r1>r3

# df_p <- df_p %>% filter(r1 < r2 & r2 > r1)

# 3. Clustering by k medoids, pam function that is robust to outliers ----

df_p <- pooled_ind_ana_km_df(326:333,30,30) #%>% as.data.frame()
# total = 960

write.csv(df_p,"ind_metrics_kmedoids.csv",row.names = F)

df_p$phase <- factor(df_p$phase)

levels(df_p$phase) <- c("Yoked","Training")

df_p$phase <- factor(df_p$phase,levels = c("Training","Yoked"))

ggplot(df_p %>%
         filter(sujeto != "M332"),aes(x = opt_k,
                fill = factor(cde),
                color = factor(cde))) + 
  geom_bar(position = "dodge") + 
  facet_wrap(~phase) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Tandem","2"="Yoked"))+
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked"))+
  labs(x = "Optimal N° of clusters",y = "Frecuency") +
  pub_theme + 
  theme(legend.position = c(0.15,0.85))



sum_loss <- df_p %>%
  group_by(sujeto,cde,phase) %>%
  summarise(ntrial = length(trial),
            loss_lhl  = sum(r2 < r1 | r2 < r3,na.rm = T),
            k_less3 = sum(opt_k < 3))

df_p <- df_p %>% filter(r1 < r2 & r2 > r3)

df_p <- df_p %>% filter(opt_k == 3)

metrics_vec <- c("stop","start","middle","r2","spread")

# # individual, boxplots
# for(x in metrics_vec) {
#   plt_metrics(df_p,x) 
#   }

# grouped metrics

# # cod for phase
# 
# df_p$phase <- ifelse(df_p$phase == "aco",3,1)
# 
# df_p <- df_p %>%
#   mutate(jitter_phase = jitter(phase,amount = 1.2))

# for(x in metrics_vec) {
#   plt_metrics_dot_err(df_p,x,col_param,fill_param,pub_theme)
#   }
# 
# 
# df_p %>% filter(cde == 2,phase == "aco") %>%
# {
#    d <- .
#    plot(d$start,d$stop,ylim = c(0,160))
# }

metrics_vec <- c("stop","start","mid","spread")

# convert continuous into bins
for(j in metrics_vec){
  df_p_lhl[j] <- df_p_lhl %>%
    `[[`(j) %>%
    {
      b <- .
      b <- get_bins(b,1,180,2)
      b*2
    }
}

start_f <- df_p_lhl %>%
  group_by(cde,phase,start) %>%
  summarise(f = n()) %>%
  group_by(cde,phase) %>%
  mutate(p = f/n())

start_f$phase <- factor(start_f$phase)

levels(start_f$phase) <- c("Yoked","Training")

start_f$phase <- factor(start_f$phase,levels = c("Training","Yoked"))

ggplot(start_f,aes(x = start,y = p,linetype = factor(cde),
                   color = factor(cde),fill = factor(cde))) +
  geom_point(color = "black",alpha = 1,size = 1,shape = 21) +
  geom_line(color = "black",alpha = 1,size = 0.5) +
  # geom_density(aes(x = start),color = "black",
  #              alpha = 0.4,size = 0.3) +
  scale_x_continuous(breaks = seq(0,180,30),limits = c(0,180),
                     labels = every_nth(seq(0,180,30),2, T,T)) +
  scale_y_continuous(breaks = seq(0,0.6,0.1),
                     labels = every_nth(seq(0,0.6,0.1),2, T,T)) +
  expand_limits(x = c(0,180))+
  labs(y = "Probability density", x = "Time (s)") +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked"))+
  facet_wrap(~phase) +
  pub_theme


df_p_lhl$phase <- factor(df_p_lhl$phase)

levels(df_p_lhl$phase) <- c("Yoked","Training")

df_p_lhl$phase <- factor(df_p_lhl$phase, levels = c("Training","Yoked"))

start_den <- 
  ggplot(df_p_lhl,aes(x = start,linetype = factor(cde),
                      color = factor(cde),fill = factor(cde))) +
  geom_density(color = "black",alpha = 0.4,size = 0.3) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.05),
                     breaks = seq(0,0.05,0.01),
                     labels = every_nth(seq(0,0.05,0.01),2)) +
  scale_x_continuous(breaks = seq(0,180,30),limits = c(0,180),
                     expand = c(0,0),
                     labels = every_nth(seq(0,180,30),2, T,T)) +
  labs(y = "Probability density", x = "Time (s)") +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked"))+
  facet_wrap(~phase) +
  scale_linetype_manual(name ="Component",values = c(1,2),
                        labels=c("1" = "Tandem","2"="Yoked")) +
  pub_theme + 
  theme(legend.position = c(0.85,0.45),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.4,"cm"))
start_den

stop_den <- 
  ggplot(df_p_lhl,aes(x = stop,linetype = factor(cde),
                      color = factor(cde),fill = factor(cde))) +
  geom_density(color = "black",alpha = 0.4,size = 0.3) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.025),
                     breaks = seq(0,0.025,0.005),
                     labels = every_nth(seq(0,0.025,0.005),2)) +
  scale_x_continuous(breaks = seq(0,180,30),limits = c(0,180),
                     expand = c(0,0),
                     labels = every_nth(seq(0,180,30),2, T,T)) +
  labs(y = "Probability density", x = "Time (s)") +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked"))+
  scale_linetype_manual(name ="Component",values = c(1,2),
                        labels=c("1" = "Tandem","2"="Yoked")) +
  facet_wrap(~phase) +
  pub_theme
stop_den

middle_den <- 
  ggplot(df_p_lhl,aes(x = mid,linetype = factor(cde),
                      color = factor(cde),fill = factor(cde))) +
  geom_density(color = "black",alpha = 0.4,size = 0.3) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.03),
                     breaks = seq(0,0.03,0.005),
                     labels = every_nth(seq(0,0.03,0.005),2)) +
  scale_x_continuous(breaks = seq(0,180,30),limits = c(0,180),
                     expand = c(0,0),
                     labels = every_nth(seq(0,180,30),2, T,T)) +
  labs(y = "Probability density", x = "Time (s)") +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked"))+
  scale_linetype_manual(name ="Component",values = c(1,2),
                    labels=c("1" = "Tandem","2"="Yoked")) +
  facet_wrap(~phase) +
  pub_theme 
middle_den

svg("start_stop_middle.svg",
    height = 7 * 0.6,
    width = 5.5 * 0.6,
    antialias = "subpixel",
    pointsize = 12)
grid.arrange(start_den + labs(x = "",y=""),
             stop_den + labs(x = "",y="") + theme(strip.text.x = element_blank()),
             middle_den + labs(y="") + theme(strip.text.x = element_blank()),
             left = textGrob("Probability density",
                             gp = gpar(fontface = "bold",fontsize=14), rot=90),
             nrow = 3)
dev.off()

ggplot(df_p_lhl,aes(x = spread,linetype = factor(cde),
                    color = factor(cde),fill = factor(cde))) +
  geom_density(color = "black",alpha = 0.4,size = 0.3) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.03),
                     breaks = seq(0,0.03,0.005),
                     labels = every_nth(seq(0,0.03,0.005),2)) +
  scale_x_continuous(breaks = seq(0,180,60),
                     limits = c(0,180),
                     expand = c(0,0)) +
  labs(y = "Probability density", x = "Time (s)") +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked"))+
  facet_wrap(~phase) +
  pub_theme

ggplot(df_p_lhl,aes(x = spread,linetype = factor(cde),
                    color = factor(cde),fill = factor(cde))) +
  geom_histogram(position = "dodge",color = "black",alpha = 0.6,size = 0.3) +
  # scale_y_continuous(expand = c(0,0),
  #                    limits = c(0,0.03),
  #                    breaks = seq(0,0.03,0.005),
  #                    labels = every_nth(seq(0,0.03,0.005),2)) +
  # scale_x_continuous(breaks = seq(0,180,60),
  #                    limits = c(0,180),
  #                    expand = c(0,0)) +
  labs(y = "Probability density", x = "Time (s)") +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked"))+
  facet_wrap(~phase) +
  pub_theme

# ecdf

df_aco <- bind_all_Ss(326:333,20,1,1) %>%
  filter(evento == 11 | evento == 1,cum_trial <=30)

n_t <- length(unique(df_aco$cum_trial))

ggplot(df_aco,aes(x = cum_dt,color = factor(cum_trial))) + 
  stat_ecdf(alpha = 0.2) + 
  scale_color_manual(values = rep("black",n_t)) +
  facet_wrap(~cde)

df_training <- bind_all_Ss(326:333,20,1,0) %>%
  filter(evento == 11 | evento == 1,cum_trial <=30)

n_t <- length(unique(df_training$cum_trial))

ggplot(df_training,aes(x = cum_dt,color = factor(cum_trial))) + 
  stat_ecdf(alpha = 0.2) + 
  scale_color_manual(values = rep("black",n_t)) +
  facet_wrap(~cde)

# density

ggplot(df_aco,aes(x = cum_dt,group = cum_trial)) + 
  stat_density(geom="line",alpha = 0.3,aes(color = factor(sujeto))) +
  # scale_color_manual(values = rep("black",n_t)) +
  facet_wrap(~cde)

ggplot(df_training,aes(x = cum_dt,color = factor(cum_trial))) + 
  stat_density(geom="line",alpha = 0.3) +
  scale_color_manual(values = rep("black",n_t)) +
  facet_wrap(~cde)

# response rate overall

df_rate_aco <- 
  read_sep_by_lever_phase(aco = 1) %>%
  filter(evento == 1 | evento == 11) %>%
  group_by(sujeto,sesion,cde) %>%
  summarise(rate = length(evento)/max(t))

df_rate_aco$phase <- "Yoked"

df_rate_training <- 
  read_sep_by_lever_phase(aco = 0) %>%
  filter(evento == 1 | evento == 11 | evento == 13 | evento == 14) %>%
  group_by(sujeto,sesion,cde) %>%
  summarise(rate = length(evento)/max(t))

df_rate_training$phase <- "Training"

df_rate_both_phases <- bind_rows(df_rate_training,df_rate_aco)

df_rate_both_phases$cde <- factor(df_rate_both_phases$cde)

# levels(df_rate_both_phases$cde) <- c("Tandem","Yoked")
# 
# df_rate_both_phases$phase <- factor(df_rate_both_phases$phase, levels = c("Training","Yoked"))

shapes <- c(1:8)

ggplot(df_rate_both_phases,aes(x = sesion,y = rate)) +
  geom_point(aes(shape = sujeto)) +
  scale_shape_manual(name = "Subject",values = shapes) +
  geom_path(aes(group = sujeto),size = 0.3) +
  facet_wrap(phase ~ cde,scales = "free_x") +
  labs(y = "Response rate (r/s)",x = "Sessions") +
  pub_theme +
  theme(legend.position = "right")

svg("overall_rate_mean_both.svg",
    height = 5.5 * 0.6,
    width = 7 * 0.75,
    antialias = "subpixel",
    pointsize = 12)

ggplot(df_rate_both_phases %>%
         # filter(sujeto != "M332") %>%
         group_by(phase,cde,sesion) %>%
         summarise(mean_r = mean(rate),
                   y.min = lower(rate),
                   y.max = upper(rate)),
       aes(x = sesion,y = mean_r)) +
  geom_point(aes(color = cde,
                 fill = cde),
             size = 1.5,
             shape = 21) +
  geom_errorbar(aes(ymin = y.min,
                    ymax = y.max),
                size = 0.3) +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked")) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Tandem","2"="Yoked")) +
  scale_y_continuous(breaks = seq(0,0.5,0.05),
                     labels = every_nth(seq(0,0.5,0.05),2,T,T)) +
  scale_x_continuous(breaks = seq(0,40,5),
                     labels = every_nth(seq(0,40,5),2,T,T)) +
  geom_path(aes(color = cde),size = 0.3) +
  facet_wrap(~phase,scales = "free_x") +
  labs(y = TeX('Mean rate (r/s) $\\pm$ SEM'),x = "Sessions") +
  pub_theme +
  theme(legend.position = c(1.08,0.5))
dev.off()

write.csv(df_rate_both_phases,"overall_rate_both_phases.csv",row.names = F)
# same with shaded lines + mean

linetypes <- c(1:8)

ggplot(df_rate_both_phases,aes(x = sesion,y = rate)) +
  geom_point(aes(shape = sujeto,color = sujeto),size = 1,alpha = 0.8) +
  geom_line(aes(color = sujeto),alpha = 0.6,size = 0.3) +
  scale_color_manual(name = "Subject",values = rep("#737373",8)) +
  # geom_path(aes(group = sujeto,color = ),size = 0.3) +
  geom_line(data = df_rate_both_phases %>%
              group_by(phase,cde,sesion) %>%
              summarise(mean_r = mean(rate),
                        y.min = lower(rate),
                        y.max = upper(rate)),
            aes(y = mean_r),size = 0.5) +
  scale_shape_manual(values = linetypes) +
  facet_wrap(phase ~ cde,scales = "free_x") +
  scale_x_continuous(breaks = c(1,10,20,30,40)) +
  labs(y = "Response rate (r/s)",x = "Sessions") +
  pub_theme  +
  theme(legend.position = "right")
