# Results
# Source wrapper
source("~/Dropbox/mult_IF_aco_exp/v3/wrapper.R")

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
    height = 5.5 * 0.5,
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
  geom_path(aes(color = cde),size = 0.3) +
  geom_errorbar(aes(ymin = y.min,
                    ymax = y.max),
                size = 0.3) +
  geom_point(aes(color = cde,
                 fill = cde),
             size = 1.5,
             shape = 21) +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked")) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Tandem","2"="Yoked")) +
  scale_y_continuous(breaks = seq(0,0.5,0.05),
                     labels = every_nth(seq(0,0.5,0.05),2,T,T)) +
  scale_x_continuous(breaks = seq(0,40,5),
                     labels = every_nth(seq(0,40,5),2,T,T)) +
  facet_wrap(~phase,scales = "free_x") +
  labs(y = TeX('Mean rate (r/s) $\\pm$ SEM'),x = "Sessions") +
  pub_theme +
  theme(legend.position = c(0.95,0.87))
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


# Get df with lhl algorithm

df_p_lhl <- pooled_ind_ana_df(326:333,25,30) %>% as.data.frame()

# filter and get lost data

tot_p <- nrow(df_p_lhl)
df_p_lhl<- df_p_lhl %>%
  filter(start < stop)
filt1_p <- nrow(df_p_lhl)
df_p_lhl <- df_p_lhl %>%
  filter(r1 < high_rate & high_rate > r3)
filt2_p <- nrow(df_p_lhl)
p_loss <- (tot_p - filt2_p)/tot_p

# Converte string phase to factor
df_p_lhl$phase <- factor(df_p_lhl$phase)
levels(df_p_lhl$phase) <- c("Yoked","Training")
df_p_lhl$phase <- factor(df_p_lhl$phase, levels = c("Training","Yoked"))

# data to get full width half maximum

resp_times_aco <- bind_all_Ss(326:333,25,1,1) %>% resp_times(.,30)
resp_times_aco$phase <- "yoked"

resp_times_training <- bind_all_Ss(326:333,25,1,0) %>% resp_times(.,30)
resp_times_training$phase <- "training"

resp_times <- bind_rows(resp_times_aco,resp_times_training)

fwhm_2phases <- resp_times %>%
  group_by(sujeto, cde, phase,sesion) %>%
  filter(length(bins) > 1) %>%
  group_by(sujeto, cde, phase,sesion) %>%
  summarise(
    fwhm = 
    {
      rt <- bins
      rt <- rt[rt < 180]
      d  <- data.frame(x = density(rt)$x,
                       y = density(rt)$y)
      fwhm <- fwhm(d$x,d$y) # custom function
      if(length(fwhm) == 0 ) {
        NA} else { fwhm }
    }, 
    peak = 
    {
      rt <- bins
      rt <- rt[rt < 180]
      d  <- data.frame(x = density(rt)$x,
                       y = density(rt)$y)
      mfw <- mfw(d$x,d$y) # custom function
      if(length(mfw) == 0 ) {
        NA} else { mfw }
    }) %>% na.omit()

fwhm_2phases$cde <- fwhm_2phases$cde %>% as.factor

# Plot and store in variables

start_den <- 
  ggplot(df_p_lhl,aes(x = start,..density..)) +
  # stat_bin(geom="step",position = 'dodge',size = 0.5) +
  geom_step_hist(size = 0.4,
                 position='identity',
                 aes(color = factor(cde))) + 
  geom_histogram(size = 0.5,position='identity',
                 alpha = 0.5,
                 aes(fill = factor(cde))) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.05),
                     breaks = seq(0,0.05,0.01),
                     labels = every_nth(seq(0,0.05,0.01),2,T,F)) +
  scale_x_continuous(breaks = seq(0,180,30),
                     limits = c(0,180),
                     expand = c(0,0),
                     labels = every_nth(seq(0,180,30),2, T,T)) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Tandem","2"="Yoked"))+
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked"))+
  facet_wrap(~phase) +
  labs(x = "",y = "")+
  # scale_linetype_manual(name ="Component",values = c(1,2),
  #                       labels=c("1" = "Tandem","2"="Yoked")) +
  pub_theme + 
  theme(legend.position = c(0.45,0.65),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.key.size = unit(0.5,"cm"))

stop_den <- 
  ggplot(df_p_lhl,aes(x = stop,..density..)) +
  # stat_bin(geom="step",position = 'dodge',size = 0.5,
  #          aes(fill = factor(cde))) +
  geom_step_hist(size = 0.4,
                 position='identity',
                 aes(color = factor(cde))) + 
  geom_histogram(size = 0.5,position='identity',
                 alpha = 0.5,
                 aes(fill = factor(cde))) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.05),
                     breaks = seq(0,0.05,0.01),
                     labels = every_nth(seq(0,0.05,0.01),2,T,F)) +
  scale_x_continuous(breaks = seq(0,180,30),
                     limits = c(0,180),
                     expand = c(0,0),
                     labels = every_nth(seq(0,180,30),2, T,T)) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Tandem","2"="Yoked"))+
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked"))+
  facet_wrap(~phase) +
  labs(x = "Time in trial (s)",y = "")+
  # scale_linetype_manual(name ="Component",values = c(1,2),
  #                       labels=c("1" = "Tandem","2"="Yoked")) +
  pub_theme 

p1 <- 
  ggplot(fwhm_2phases, 
         aes(x = phase, 
             y = fwhm)) +
  aes(color = cde) +
  geom_point(position = position_jitterdodge(),
             size = 1.4,
             alpha = 0.8,
             shape = 21,
             aes(fill = cde)) +
  # stat_boxplot(geom ='errorbar',
  #              position = position_dodge(0.9), 
  #              aes(color = cde),
  #              alpha = 0.9,
  #              width = 0.5,
  #              lwd = 0.4,
  #              show.legend = FALSE) + 
  # geom_boxplot(position = position_dodge(0.9), 
  #              aes(color = cde,
  #                  fill = cde),
  #              # alpha = 0.9,
#              lwd = 0.4,show.legend = FALSE) + 
stat_summary(fun.y="mean",geom="crossbar",
             mapping=aes(ymin=..y.., ymax=..y..), 
             width=0.5,size = 0.3,
             position=position_dodge(0.8),show.legend = FALSE) +
  scale_x_discrete(labels = c("Training","Yoked")) +
  scale_y_continuous(breaks = seq(0,180,30),
                     labels = every_nth(seq(0,180,30),2,inverse = TRUE)) +
  expand_limits(y = c(0,180)) +
  scale_color_manual(name ="Component:",values = col_param,
                     labels=c("1" = "Tandem","2" = "Yoked"))+
  scale_fill_manual(name = "Component:",values = fill_param,
                    labels = c("1" = "Tandem","2" = "Yoked"))+
  labs(x = "",y = "FWHM (s)") +
  pub_theme + 
  guides(color = guide_legend(override.aes = list(size=2))) +
  theme(legend.position = "top",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.margin=margin(t = 0, b = 0, unit='cm'),
        legend.key.size = unit(0.4,"cm"))


p2 <- ggplot(fwhm_2phases, 
             aes(x = phase, 
                 y = peak, 
                 color = cde,
                 group = cde)) +
  geom_point(position = position_jitterdodge(0.5),
             size = 1.4,
             shape = 21,
             alpha = 0.8,
             aes(fill = cde)) +
  stat_summary(fun.y="mean",geom="crossbar", 
               mapping=aes(ymin=..y.., ymax=..y..), 
               width=0.5,size = 0.3,
               position=position_dodge(0.8),show.legend = FALSE) +
  scale_x_discrete(labels = c("Training","Yoked")) +
  scale_y_continuous(breaks = seq(0,180,30),
                     labels = every_nth(seq(0,180,30),2,inverse = TRUE)) +
  expand_limits(y = c(0,180)) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Tandem","2" = "Yoked"))+
  scale_fill_manual(name = "Component",values = fill_param,
                    labels = c("1" = "Tandem","2" = "Yoked"))+
  labs(x = "Phase",y = "Peak time (s)") +
  pub_theme 

pdf("start_stop_fwhm_peak.pdf",
    height = 6.5,
    width = 6.5,
    pointsize = 8)
# antialias = "subpixel",

grid.arrange(start_den + theme(plot.margin=margin(0.4, 0.3, 0, 0.4, "cm")),
             p1 + theme(plot.margin=margin(0.4, 0.5, 0, 0.3, "cm")),
             stop_den + theme(plot.margin=margin(0.4, 0.3, 0, 0.4, "cm")),
             p2 + theme(plot.margin=margin(0.7, 0.5, 0, 0.3, "cm")),
             left = textGrob("Density",
                             gp = gpar(fontface = "bold",fontsize=12), rot=90),
             nrow = 2)

grid.text("A",x = 0.08,y=0.97,gp = gpar(fontface = "bold",fontsize=14))
grid.text("B",x = 0.08,y=0.47,gp = gpar(fontface = "bold",fontsize=14))
grid.text("C",x = 0.55,y=0.97,gp = gpar(fontface = "bold",fontsize=14))
grid.text("D",x = 0.55,y=0.47,gp = gpar(fontface = "bold",fontsize=14))

dev.off()

#### Correlation analysis ####
load("correlation_data.RData")

svg("corr_coefficient.svg",
    height = 4,
    width = 4,
    antialias = "subpixel",
    pointsize = 12)

ggplot(rho_both_ph,aes(x = phase, y = rho)) +
  geom_point(position = position_jitterdodge(),
             size = 1.8,shape = 21,
             aes(fill = factor(cde))) +
  stat_summary(fun.y="mean",geom="crossbar", 
               mapping=aes(ymin=..y.., 
                           ymax=..y..,
                           color = factor(cde)), 
               width=0.5,size = 0.3,
               position=position_dodge(0.8),show.legend = FALSE) +
  # geom_boxplot(position = position_dodge(0.9),
  #              aes(color = factor(cde)),
  #              lwd = 0.4,
  #              fill = NA, show.legend = FALSE) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.5),
                     breaks = seq(0,0.5,0.1),
                     labels = every_nth(seq(0,0.5,0.1),2,T,F)) +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked")) +
  guides(fill = guide_legend(override.aes = list(size=1.7))) +
  scale_color_manual(values = col_param) +
  labs(y = bquote(rho),x = "Phase") +
  pub_theme + 
  theme(legend.position = c(0.6,0.85))
dev.off()

### Correlation coefficient by session ### 

ggplot(rho_both_ph_xs,aes(x = sesion, y = rho)) +
  geom_line(aes(linetype = factor(cde))) +
  geom_point(size = 1.8,shape = 21,
             aes(fill = factor(cde))) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.5),
                     breaks = seq(0,0.5,0.1),
                     labels = every_nth(seq(0,0.5,0.1),2,T,F)) +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked")) + 
  scale_x_continuous(breaks = seq(0,40,5),
                     labels = every_nth(seq(0,40,5),2,T,T)) +
  scale_linetype_manual(name ="Component",values = c(1,2),
                        labels=c("1" = "Tandem","2"="Yoked")) + 
  facet_wrap(~phase,scale ="free_x") +
  # guides(color = guide_legend(override.aes = list(size=1.7))) +
  scale_color_manual(values = col_param) +
  labs(y = bquote(rho),x = "Phase") +
  pub_theme + 
  theme(legend.position = c(0.6,0.85))

## panel plot ####

# normalized plots

df_tr <- bind_all_Ss(326:333,25,1,0) %>% 
  group_by(sujeto,sesion,cde,trial) %>%
  filter(sum(evento == 1) > 9 |
           sum(evento == 11) > 9) %>%
  as.data.frame() %>%
  resp_rate_df(.,1,30) %>%
  group_by(sujeto,cde) %>%
  mutate(resp_norm = (resp - min(resp))/(max(resp) - min(resp)),
         ec_norm = (ec_resp - min(ec_resp))/(max(ec_resp) - min(ec_resp)))

df_aco <- bind_all_Ss(326:333,25,1,1) %>% 
  group_by(sujeto,sesion,cde,trial) %>%
  filter(sum(evento == 1) > 9 |
           sum(evento == 11) > 9) %>%
  as.data.frame() %>%
  resp_rate_df(.,1,30) %>%
  group_by(sujeto,cde) %>%
  mutate(resp_norm = (resp - min(resp))/(max(resp) - min(resp)),
         ec_norm = (ec_resp - min(ec_resp))/(max(ec_resp) - min(ec_resp)))


# plot parameters

shape_param <- c("1" = 21, "2" = 21)

# Fase de acoplado

yoked_norm_plot <- 
  resp_norm_plot(df_aco,col_param,"lever",
                 fill_param,shape_param,1,
                 p_size = 0.2) +
  theme(plot.margin = margin(0.2,0.5,0,0, "cm"))

head_plt_y <- 
  resp_norm_plot(df_aco,col_param,"head",
                 res=1,fill_param,shape_param,
                 p_size = 0.18) +
  theme(legend.position = 'none',
        # axis.line = element_line(size = 0.09),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0.2,0.5,0,0, "cm"))

# fase entrenamiento
tr_norm_plot <- 
  resp_norm_plot(df_tr,col_param,"lever",
                 fill_param,shape_param,1,
                 p_size = 0.2) +
  theme(plot.margin = margin(0.2,0.5,0,0, "cm"))

head_plt_t <- 
  resp_norm_plot(df_tr,col_param,"head",res = 1,
                 fill_param,shape_param,
                 p_size = 0.18) +
  theme(legend.position = 'none',
        # axis.line = element_line(size = 0.09),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0.2,0.5,0,0, "cm"))

# qqplot of IRI

df_IF_aco <- bind_all_Ss(326:333,16,0,1) %>% as.data.frame()
df_IF_aco$phase <- "yoked"

df_IRI_aco  <- df_IF_aco %>%
  group_by(sujeto,cde,cum_trial,phase) %>%
  summarise(iri = cum_dt[evento == 2 | evento == 21]) %>%
  as.data.frame() %>%
  filter(iri <= quantile(iri, 0.95),
         iri > quantile(iri, 0.05)) %>%
  spread(cde,iri) %>%
  na.omit() %>%
  filter(cum_trial <= 30) 

colnames(df_IRI_aco)[c(4,5)] <- c("Tandem","Yoked")

qq_aco <- df_IRI_aco %>% 
  na.omit() %>%
  `[`(c("Tandem","Yoked")) %>%
  {
    xy <- .
    qq <- qq_points(xy)
    qq$phase <- "Yoked"
    qq
  }

df_IF_tr <- bind_all_Ss(326:333,16,0,0) %>% as.data.frame()

df_IF_tr$phase <- "training"

df_IRI_tr   <- df_IF_tr %>%
  group_by(sujeto,cde,cum_trial,phase) %>%
  summarise(iri = cum_dt[evento == 2 | evento == 21]) %>%
  as.data.frame() %>%
  filter(iri <= quantile(iri, 0.95),
         iri > quantile(iri, 0.05)) %>%
  spread(cde,iri) %>%
  na.omit() %>%
  filter(cum_trial <= 30) 

colnames(df_IRI_tr)[c(4,5)] <- c("Tandem","Yoked")

qq_tr <- df_IRI_tr %>% 
  na.omit() %>%
  `[`(c("Tandem","Yoked")) %>%
  {
    xy <- .
    qq <- qq_points(xy)
    qq$phase <- "Training"
    qq
  }

qq_both <- bind_rows(qq_tr,qq_aco)

qqplot <- 
  ggplot(qq_both,
         aes(x = Tandem,y = Yoked)) +
  geom_point(size = 1.5,
             stroke = 0.2,
             shape = 21,
             color = "black",
             fill = "#bdbdbd") +
  facet_wrap(~phase) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(60,95),
                     breaks = seq(60,95,5),
                     labels = every_nth(seq(60,95,5),2,T,F)) +
  scale_x_continuous(breaks = seq(60,95,5),
                     limits = c(60,95),
                     expand = c(0.01,0),
                     labels = every_nth(seq(60,95,5),2, T,T)) +
  geom_abline(intercept = 0,slope = 1,linetype = 2,size = 0.5) +
  xlab("Tandem IRI (s)")+
  ylab("Yoked IRI (s)") +
  pub_theme

# get kld data and plot

df_peak_aco <- bind_all_Ss(326:333,16,1,1) 

df_peak_aco <- df_peak_aco %>%
  group_by(sujeto,sesion,cde,trial) %>%
  filter(sum(evento == 1) > 9 |
           sum(evento == 11) > 9,
         cum_dt <= 180)

df_peak_aco <- df_peak_aco %>%
  filter(cum_trial <= 30 )
res = 2
df_peak_aco$bins <- 
  df_peak_aco$cum_dt %>%
  get_bins(.,x.min = 1,x.max = max(.),resolution = res)

df_peak_aco$phase <- "yoked"

df_peak_tr <- bind_all_Ss(326:333,16,1,0) 

df_peak_tr <- 
  df_peak_tr %>%
  group_by(sujeto,sesion,cde,trial) %>%
  filter(sum(evento == 1) > 9 |
           sum(evento == 11) > 9,
         cum_dt <= 180)

df_peak_tr <- 
  df_peak_tr %>%
  filter(cum_trial <= 30 )

df_peak_tr$bins <- 
  df_peak_tr$cum_dt %>%
  get_bins(.,x.min = 1,x.max = max(.),resolution = res)

df_peak_tr$phase <- "training"

both_ph <- bind_rows(df_peak_aco,df_peak_tr)

s_vector <- unique(both_ph$sujeto)

d <- lapply(s_vector,function(k){
  df.s <- both_ph[both_ph$sujeto == k,]
  v_ph <- unique(df.s$phase)
  phse <- lapply(v_ph,function(f){
    df.s.f <- df.s[df.s$phase == f,]
    ses <- unique(df.s.f$sesion)
    sesion <- lapply(ses,function(s){
      df.s.f.s <- df.s.f[df.s.f$sesion == s,]
      tand_bins <- df.s.f.s$bins[df.s.f.s$evento == 1] %>% 
        f_table(.,x.min = 1,x.max = 180)
      yok_bins <- df.s.f.s$bins[df.s.f.s$evento == 11] %>% 
        f_table(.,x.min = 1,x.max = 180)
      P <- tand_bins/sum(tand_bins)
      Q <- yok_bins/sum(yok_bins)
      kld.s <- kld_d(P,Q)
      df_kld <- data.frame(s = k,kld = kld.s,sesion = s,phase = f)
      return(df_kld)
    }
    ) %>% bind_rows()
    return(sesion)
  }
  ) %>% bind_rows()
  return(phse)
}
) %>% bind_rows()

# reordenar por fase

d %<>% arrange(phase)

dsum <- d %>% group_by(phase) %>% summarise(kld = median(kld,na.rm = T))

save(df_aco,df_tr,qq_both,d,file = "rate_qq_dkldf.RData")

col_param2  <- c("training" = "black","yoked" = "black")
fill_param2 <- c("training" = "#808080","yoked" = "white")

dkl_plt <- 
  ggplot(d %>% na.omit(), aes(x = phase, y = kld,
                              color = factor(phase),
                              fill = factor(phase))) +  
  geom_jitter(width=0.2,
              shape = 21,
              size = 1.5,
              stroke = 0.4) +
  geom_crossbar(data = dsum, aes(ymin = kld, ymax = kld),
                size = 0.3,col = "black", width = .5) +
  scale_x_discrete(labels = c("Training","Yoked"))+
  scale_y_continuous(breaks = seq(-1,2.5,.5),
                     labels = every_nth(seq(-1,2.5,.5),2,T,F))+
  expand_limits(y = c(-0.5,2.5)) +
  scale_color_manual(name ="",values = col_param2,
                     labels=c("training" = "Training","yoked" = "Yoked"))+
  scale_fill_manual(name ="",values = fill_param2,
                    labels = c("training" = "Training","yoked" = "Yoked"))+
  labs(x = "Phase") +
  ylab(TeX('$ D_{KL}(\\textit{P_{Yoked} } || \\textit{ P_{Tandem}})$')) +
  pub_theme

# trbl
pdf("panel_rate_qqplot_dkl.pdf",
    height = 6.5,
    width = 6.5,
    pointsize = 12)

grid.arrange(tr_norm_plot + 
               xlab("") + 
               theme(plot.margin = margin(1,0.29,0,0.25, "cm")),
             yoked_norm_plot + 
               labs(x = "", y = "") +
               theme(plot.margin = margin(1.3,0.29,0,0.3, "cm")),
             qqplot + 
               theme(plot.margin = margin(0,0,0,0.5, "cm")),
             dkl_plt + 
               theme(plot.margin = margin(0.5,0.3,0,0.5, "cm")),
             ncol = 2,
             layout_matrix = rbind(c(1,1,1,2,2,2),
                                   c(3,3,3,3,4,4)))

grid.text("Time in trial (s)",
          x = 0.53,y = 0.51,
          gp = gpar(fontface = "bold"))


print(head_plt_t, 
      vp = grid::viewport(width = 0.36/2, 
                          height = 0.36/2,
                          x = unit(0.415, "npc"), 
                          y = unit(0.85, "npc")))
print(head_plt_y, 
      vp = grid::viewport(width = 0.36/2, 
                          height = 0.36/2,
                          x = unit(0.915, "npc"), 
                          y = unit(0.85, "npc")))

grid.text(c("A","C","B","D"),
          x = c(0.08,0.08,0.58,0.76),y = c(0.93,0.49,0.93,0.49),
          gp = gpar(fontface = "bold",
                    size = 14))
dev.off()
