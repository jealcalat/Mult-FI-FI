# Results
# Source wrapper
source("~/Dropbox/mult_IF_aco_exp/v3/wrapper.R")

# response rate overall
load("overall_rate_data.RData")
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

# Subject M332
# 
pdf("overall_rate_M332.pdf",
    height = 5.5 * 0.5,
    width = 7 * 0.75,
    pointsize = 12)

ggplot(df_rate_both_phases %>%
         filter(sujeto == "M332"),
       aes(x = sesion,y = rate)) +
  geom_path(aes(color = cde),size = 0.3) +
  # geom_errorbar(aes(ymin = y.min,
  #                   ymax = y.max),
  #               size = 0.3) +
  geom_point(aes(color = cde,
                 fill = cde),
             size = 1.5,
             shape = 21) +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked")) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Tandem","2"="Yoked")) +
  scale_y_continuous(breaks = seq(0,0.8,0.1),
                     labels = every_nth(seq(0,0.8,0.1),2,T,T)) +
  scale_x_continuous(breaks = seq(0,40,5),
                     labels = every_nth(seq(0,40,5),2,T,T)) +
  facet_wrap(~phase,scales = "free_x") +
  labs(y = "Response rate (r/s)",x = "Sessions") +
  pub_theme +
  theme(legend.position = c(0.49,0.87))

dev.off()
#------------- Individual trial analysis -------------------
# Load data

load(list.files(pattern = "^start(.*)RData"))

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

high_r <- 
  ggplot(df_p_lhl, 
         aes(x = phase, 
             y = high_rate)) +
  aes(color = factor(cde)) +
  stat_boxplot(geom ='errorbar',
               position = position_dodge(0.9),
               aes(color = factor(cde)),
               alpha = 0.9,
               width = 0.3,
               lwd = 0.3,
               show.legend = FALSE) +
  geom_boxplot(position = position_dodge(0.9),
               aes(color = factor(cde),
                   fill = factor(cde)),
               # alpha = 0.9,
               lwd = 0.4,show.legend = FALSE) +
  scale_x_discrete(labels = c("Training","Yoked")) +
  scale_y_continuous(breaks = seq(0,3,0.5),
                     expand = c(0,0),
                     labels = every_nth(seq(0,3,0.5),2,inverse = TRUE)) +
  expand_limits(y = c(0,3)) +
  scale_color_manual(name ="Component:",values = col_param,
                     labels=c("1" = "Tandem","2" = "Yoked"))+
  scale_fill_manual(name = "Component:",values = fill_param,
                    labels = c("1" = "Tandem","2" = "Yoked"))+
  labs(x = "",y = "High rate (r/s)") +
  pub_theme 


mid <- 
  ggplot(df_p_lhl, 
         aes(x = phase, 
             y = mid)) +
  aes(color = factor(cde)) +
  geom_point(position = position_jitterdodge(),
             size = 1.4,
             alpha = 0.8,
             shape = 21,
             aes(fill = factor(cde))) +
  stat_summary(fun.y="mean",
               geom="crossbar",
               mapping = aes(ymin=..y.., ymax=..y..), 
               width=0.5,size = 0.3,
               position=position_dodge(0.8),show.legend = FALSE) +
  scale_x_discrete(labels = c("Training","Yoked")) +
  scale_y_continuous(breaks = seq(20,170,15),
                     expand = c(0,0),
                     labels = every_nth(seq(20,170,15),2,inverse = TRUE)) +
  expand_limits(y = c(20,170)) +
  scale_color_manual(name ="Component:",values = col_param,
                     labels=c("1" = "Tandem","2" = "Yoked"))+
  scale_fill_manual(name = "Component:",values = fill_param,
                    labels = c("1" = "Tandem","2" = "Yoked"))+
  labs(x = "",y = "Middle of high rate state (s)") +
  pub_theme 

spread <- 
  ggplot(df_p_lhl, 
         aes(x = phase, 
             y = spread)) +
  aes(color = factor(cde)) +
  geom_point(position = position_jitterdodge(),
             size = 1.4,
             alpha = 0.8,
             shape = 21,
             aes(fill = factor(cde))) +
  stat_summary(fun.y="mean",
               geom="crossbar",
               mapping = aes(ymin=..y.., ymax=..y..), 
               width=0.5,size = 0.3,
               position=position_dodge(0.8),show.legend = FALSE) +
  scale_x_discrete(labels = c("Training","Yoked")) +
  scale_y_continuous(breaks = seq(0,180,30),
                     expand = c(0,0),
                     labels = every_nth(seq(0,180,30),2,inverse = TRUE)) +
  expand_limits(y = c(0,180)) +
  scale_color_manual(name ="Component:",values = col_param,
                     labels=c("1" = "Tandem","2" = "Yoked"))+
  scale_fill_manual(name = "Component:",values = fill_param,
                    labels = c("1" = "Tandem","2" = "Yoked"))+
  labs(x = "",y = "Spread (s)") +
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
  stat_summary(fun.y="median",geom="crossbar",
               mapping=aes(ymin=..y.., ymax=..y..), 
               width=0.5,size = 0.3,
               position=position_dodge(0.8),show.legend = FALSE) +
  scale_x_discrete(labels = c("Training","Yoked")) +
  scale_y_continuous(breaks = seq(0,180,30),
                     expand = c(0,0),
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
                     expand = c(0,0),
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

pdf("high_rate.pdf",
    height = 4,
    width = 4,
    pointsize = 10)
high_r
dev.off()

# grid.arrange(high_r,
#              spread,
#              mid)
# 

# ------------- Panel plot of good starts and stops ------------------------------

df_p_lhl_2 <- df_p_lhl %>% 
  filter(start < 60,stop > 60)

start_den_2 <- 
  ggplot(df_p_lhl_2,aes(x = start,..density..)) +
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

stop_den_2 <- 
  ggplot(df_p_lhl_2,aes(x = stop,..density..)) +
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

#### Correlation analysis ####
load("correlation_data.RData")

pdf("corr_coefficient.pdf",
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
  labs(y = "Correlation coefficient",x = "Phase") +
  pub_theme + 
  theme(legend.position = c(0.6,0.85))
dev.off()

### Correlation coefficient by session ### 

pdf("corr_coef_by_ses.pdf",
    height = 3,
    width = 6,
    pointsize = 10) 

ggplot(rho_both_ph_xs,aes(x = sesion, y = rho)) +
  geom_line(aes(linetype = factor(cde))) +
  geom_point(size = 1.8,shape = 21,
             aes(fill = factor(cde))) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.5),
                     breaks = seq(0,0.5,0.05),
                     labels = every_nth(seq(0,0.5,0.05),2,T,T)) +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2"="Yoked")) + 
  scale_x_continuous(breaks = seq(0,50,5),
                     expand = c(0,1),
                     labels = every_nth(seq(0,50,5),2,T,T)) +
  scale_linetype_manual(name ="Component",values = c(1,2),
                        labels=c("1" = "Tandem","2"="Yoked")) + 
  facet_wrap(~phase,scale ="free_x") +
  # guides(color = guide_legend(override.aes = list(size=1.7))) +
  scale_color_manual(values = col_param) +
  labs(y = "Correlation coefficient",x = "Phase") +
  pub_theme + 
  theme(legend.position = c(0.2,0.86))

dev.off()
# -----------------------------Panel plot -------------------------------

# Load data 
load(list.files(pattern = "^rate_qq"))
## Normalized plots 

shape_param <- c("1" = 21, "2" = 21)

# Fase de acoplado

yoked_norm_plot <- 
  resp_norm_plot(df_aco,col_param,"lever",
                 fill_param,shape_param,1,
                 p_size = 0.15) +
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
                 p_size = 0.15) +
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

qqplot <- 
  ggplot(qq_both,
         aes(x = Tandem,y = Yoked)) +
  geom_point(size = 1.5,
             stroke = 0.2,
             shape = 21,
             color = "black",
             fill = "#bdbdbd") +
  facet_wrap(~phase,scales = "free_y") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(60,95),
                     breaks = seq(60,95,5),
                     labels = every_nth(seq(60,95,5),2,T,T)) +
  scale_x_continuous(breaks = seq(60,95,5),
                     limits = c(60,95),
                     expand = c(0.01,0),
                     labels = every_nth(seq(60,95,5),2, T,T)) +
  geom_abline(intercept = 0,slope = 1,linetype = 2,size = 0.5) +
  xlab("Tandem IRI (s)")+
  ylab("Yoked IRI (s)") +
  pub_theme

# get kld data and plot

col_param2  <- c("training" = "black","yoked" = "black")
fill_param2 <- c("training" = "#808080","yoked" = "white")

dkl_plt <- 
  ggplot(d %>% na.omit(), aes(x = phase, y = kld,
                              color = factor(phase),
                              fill = factor(phase))) +  
  geom_jitter(width = 0.2,
              shape = 21,
              size = 1.5,
              stroke = 0.4) +
  geom_crossbar(data = dsum, aes(ymin = kld, ymax = kld),
                size = 0.3,col = "black", width = .5) +
  scale_x_discrete(labels = c("Training","Yoked"))+
  scale_y_continuous(breaks = seq(-1,2.5,.5),
                     expand = c(0,0),
                     labels = every_nth(seq(-1,2.5,.5),2,T,F))+
  expand_limits(y = c(-0.5,2.5)) +
  scale_color_manual(name ="",values = col_param2,
                     labels=c("training" = "Training","yoked" = "Yoked"))+
  scale_fill_manual(name ="",values = fill_param2,
                    labels = c("training" = "Training","yoked" = "Yoked"))+
  labs(x = "Phase") +
  ylab(TeX('$ D_{KL}(\\textit{P_{Yoked} } || \\textit{ P_{Tandem}})$')) +
  pub_theme

# - All together -

pdf("panel_rate_qqplot_dkl.pdf",
    height = 5.6,
    width = 6.5,
    pointsize = 12)

grid.arrange(tr_norm_plot + 
               xlab("") + 
               theme(plot.margin = margin(1,0.29,0,0.25, "cm")),
             yoked_norm_plot + 
               labs(x = "", y = "") +
               theme(plot.margin = margin(1,0.29,0,0.3, "cm")),
             qqplot + 
               theme(plot.margin = margin(0,0,0,0.5, "cm")),
             dkl_plt + 
               theme(plot.margin = margin(0.54,0.3,0,0.5, "cm")),
             ncol = 2,
             layout_matrix = rbind(c(1,1,1,2,2,2),
                                   c(1,1,1,2,2,2),
                                   c(1,1,1,2,2,2),
                                   c(3,3,3,3,4,4),
                                   c(3,3,3,3,4,4)))

grid.text("Time in trial (s)",
          x = 0.53,y = 0.41,
          gp = gpar(fontface = "bold"))

print(head_plt_t, 
      vp = grid::viewport(width = 0.36/1.8, 
                          height = 0.36/1.8,
                          x = unit(0.415, "npc"), 
                          y = unit(0.82, "npc")))
print(head_plt_y, 
      vp = grid::viewport(width = 0.36/1.8, 
                          height = 0.36/1.8,
                          x = unit(0.915, "npc"), 
                          y = unit(0.82, "npc")))

grid.text(c("A","C","B","D"),
          x = c(0.08,0.08,0.58,0.76),y = c(0.93,0.4,0.93,0.4),
          gp = gpar(fontface = "bold",
                    size = 14))
dev.off()

# ------ Optimal number of clusters based on kmedoids -------------

kmedoids <- read_csv("ind_metrics_kmedoids.csv",col_names = T)

kmedoids %<>% as.data.frame

kmedoids_sum <- kmedoids %>%
  group_by(phase,sujeto,cde,opt_k) %>%
  summarise(counts = n()) %>%
  as.data.frame()

kmedoids_sum$phase <- factor(kmedoids_sum$phase)

levels(kmedoids_sum$phase) <- c("Yoked","Training")

kmedoids_sum$phase <- factor(kmedoids_sum$phase,levels = c("Training","Yoked"))

pdf("optimal_kmedoids.pdf",
    width = 6,
    height = 3,
    pointsize = 10)

ggplot(kmedoids_sum,
       aes(x = opt_k,y = counts)) +
  geom_boxplot(aes(x = factor(opt_k),
                   color = factor(cde),
                   fill = factor(cde)),
               alpha = 0.7,
               lwd = 0.4) +
  facet_wrap(~phase) + 
  scale_y_continuous(breaks = seq(1,26,2),
                     expand = c(0,0),
                     labels = every_nth(seq(1,26,2),2,T,T)) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Tandem","2" = "Yoked")) +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Tandem","2" = "Yoked")) +
  labs(x = "Optimal number of clusters", y = "Frecuency") +
  pub_theme

dev.off()

# ------------------- Post reinforcement pause -----------------------
# 
load(list.files(pattern = "^all_data_IF(.*)RData"))

all_data <- bind_rows(all_data_mIF,all_data_peak) %>% as.data.frame()

prp <- all_data %>%
  group_by(sujeto,sesion,cde,phase,trial) %>%
  filter(evento == 1 | evento == 11) %>%
  summarise(prp = cum_dt[1]) %>%
  filter(prp <= 180)

write.csv(prp,"prp.csv",row.names = F)

pdf("mean_prp.pdf",
    width = 6,
    height = 3,
    pointsize = 10)

ggplot(prp,aes(x = sesion,
               y = prp,
               color = factor(cde),
               fill = factor(cde))) + 
  stat_summary(geom = "line", 
               size = 0.3,
               fun.y = mean) +
  stat_summary(geom = "pointrange", 
               shape = 21,
               fun.data = mean_se, 
               stroke = 0.2,
               fatten = 5,
               size = 0.3) +
  scale_y_continuous(breaks = seq(20,90,5),
                     expand = c(0,0),
                     labels = every_nth(seq(20,90,5),2,T,T)) +
  scale_x_continuous(breaks = seq(0,40,5),
                     labels = every_nth(seq(0,40,5),2,T,T)) +
  labs(x = "Session",y = "Postreinforcement pause (s)") +
  expand_limits(y = c(30,60)) + 
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Tandem","2" = "Yoked"))+
  scale_fill_manual(name = "Component",values = fill_param,
                    labels = c("1" = "Tandem","2" = "Yoked"))+
  # stat_summary(geom = "point", 
  #              fun.y = mean) +
  facet_wrap(~phase,scale = "free_x") +
  pub_theme 

dev.off()

# --------------------------- Contiguity -----------------------------
# 

contig_yoked <- bind_all_Ss(326:333,6,0,1) %>%
  as.data.frame() %>%
  group_by(sujeto,sesion,cde,trial) %>%
  mutate(t_rf = cum_dt[evento == 2 | evento == 21]) %>%
  filter(evento == 1 | evento == 11)

contig_yoked$abs_err <- abs(contig_yoked$t_rf - contig_yoked$cum_dt)

contig_yoked$phase <- "Yoked"

pdf("r-R_contiguity_phase_yoked.pdf",
    width = 6,
    height = 6,
    pointsize = 10)

ggplot(contig_yoked,
       aes(x = abs_err + 1,
           y = ..density..,
           fill = factor(cde))) +
  geom_step_hist(size = 0.4,
                 position='identity',
                 aes(color = factor(cde)),
                 alpha = 0.9) + 
  geom_histogram(size = 0.4,
                 position='identity',
                 alpha = 0.4) +
  scale_x_log10("Time to next reinforcer (s)") +
  scale_y_continuous("Density",expand = c(0,0),
                     breaks = seq(0,2.5,0.5),
                     labels = every_nth(seq(0,2.5,0.5),2,T,F)) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Tandem","2" = "Yoked"))+
  scale_fill_manual(name = "Component",values = fill_param,
                    labels = c("1" = "Tandem","2" = "Yoked"))+
  expand_limits(x = c(0,100),y = c(0,2.5)) +
  facet_wrap(~sujeto,scales = "free_x") + 
  annotation_logticks(sides = "b",
                      short = unit(0.5,"mm"),
                      mid = unit(0.7,"mm"),
                      long = unit(0.9,"mm"),
                      size = 0.3) + 
  pub_theme +
  theme(legend.position = c(0.9,0.15))

dev.off()

contig_training <- bind_all_Ss(326:333,6,0,0) %>%
  as.data.frame() %>%
  group_by(sujeto,sesion,cde,trial) %>%
  mutate(t_rf = cum_dt[evento == 2 | evento == 21]) %>%
  filter(evento == 1 | evento == 11) 

contig_training$abs_err <- abs(contig_training$t_rf - contig_training$cum_dt)

contig_training$phase <- "Training"

contiguity_both <- bind_rows(contig_training,contig_yoked)
contiguity_both$phase <- factor(contiguity_both$phase)


pdf("r-R_contiguity_phase_training.pdf",
    width = 6,
    height = 6,
    pointsize = 10)

ggplot(contig_training,
       aes(x = abs_err + 1,
           fill = factor(cde))) +
  geom_density(position='identity',
               size = 0.2,
               alpha = 0.5) + 
  scale_x_log10("Time to next reinforcer (s)") +
  scale_y_continuous("Density",expand = c(0,0),
                     breaks = seq(0,2.5,0.5),
                     labels = every_nth(seq(0,2.5,0.5),2,T,F)) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Tandem","2" = "Yoked"))+
  scale_fill_manual(name = "Component",values = fill_param,
                    labels = c("1" = "Tandem","2" = "Yoked"))+
  expand_limits(x = c(0,100),y = c(0,2.5)) +
  facet_wrap(~sujeto,scales = "free_x") + 
  annotation_logticks(sides = "b",
                      short = unit(0.5,"mm"),
                      mid = unit(0.7,"mm"),
                      long = unit(0.9,"mm"),
                      size = 0.3) + 
  pub_theme +
  theme(legend.position = c(0.9,0.15))

dev.off()

pdf("r-R_contiguity_grouped.pdf",
    width = 6,
    height = 4,
    pointsize = 10)

ggplot(contiguity_both,
       aes(x = abs_err + 1,
           color = factor(cde),
           fill = factor(cde))) +
  geom_density(position='identity',
               size = 0.2,
               alpha = 0.5) + 
  scale_x_log10("Time to next reinforcer (s)",
                expand = c(0,0)) +
  scale_y_continuous("Density",expand = c(0,0),
                     breaks = seq(0,1.5,0.25),
                     labels = every_nth(seq(0,1.5,0.25),2,T,F)) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Tandem","2" = "Yoked"))+
  scale_fill_manual(name = "Component",values = fill_param,
                    labels = c("1" = "Tandem","2" = "Yoked"))+
  expand_limits(x = c(0,100),y = c(0,1.5)) +
  facet_wrap(~phase,scales = "free_x") + 
  annotation_logticks(sides = "b",
                      short = unit(0.5,"mm"),
                      mid = unit(0.7,"mm"),
                      long = unit(0.9,"mm"),
                      size = 0.3) + 
  pub_theme +
  theme(legend.position = c(0.9,0.75))

dev.off()
