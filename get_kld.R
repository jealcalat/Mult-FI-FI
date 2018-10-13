# klds
source("~/Dropbox/mult_IF_aco_exp/v2/wrapper.R")

df_IF_aco <- bind_all_Ss(326:333,16,1,1) 

df_IF_aco <- df_IF_aco %>%
  group_by(sujeto,sesion,cde,trial) %>%
  filter(sum(evento == 1) > 9 |
         sum(evento == 11) > 9,
         cum_dt <= 180)

df_IF_aco <- df_IF_aco %>%
  filter(cum_trial <= 30 )
res = 2
df_IF_aco$bins <- df_IF_aco$cum_dt %>%
  get_bins(.,x.min = 1,x.max = max(.),resolution = res)

df_IF_aco$phase <- "yoked"

df_IF_tr <- bind_all_Ss(326:333,16,1,0) %>%
  filter(cum_trial <= 30 )

df_IF_tr <- df_IF_tr %>%
  group_by(sujeto,sesion,cde,trial) %>%
  filter(sum(evento == 1) > 9 |
           sum(evento == 11) > 9,
         cum_dt <= 180)


df_IF_tr$bins <- df_IF_tr$cum_dt %>%
  get_bins(.,x.min = 1,x.max = max(.),resolution = res)

df_IF_tr$phase <- "training"
  
both_ph <- bind_rows(df_IF_aco,df_IF_tr)

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
library(magrittr)
library(latex2exp)
d %<>% arrange(phase)

dsum <- d %>% group_by(phase) %>% summarise(kld = median(kld,na.rm = T))

write.csv(d,"~/Dropbox/mult_IF_aco_exp/Plots_&_dfs/Final/actualización/kld_sujeto_fase_sesion.csv",
          row.names = F)

col_param  <- c("training" = "black","yoked" = "black")
fill_param <- c("training" = "#808080","yoked" = "white")

svg("dkl.svg",
    height = 4,
    width = 3.7,
    antialias = "subpixel",
    pointsize = 12)

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
  scale_color_manual(name ="",values = col_param,
                     labels=c("training" = "Training","yoked" = "Yoked"))+
  scale_fill_manual(name ="",values = fill_param,
                    labels = c("training" = "Training","yoked" = "Yoked"))+
  labs(x = "Phase") +
  ylab(TeX('$ D_{KL}(\\textit{P_{Yoked} } || \\textit{ P_{Tandem}})\\, (bits)$')) +
  theme(
    axis.line = element_line(colour = "black",size = 0.3),
    axis.ticks = element_line(colour = 'black',size = 0.2),
    legend.key = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.key.size = unit(0.5, "cm"),
    legend.background = element_blank(),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
    axis.title.y = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 9, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
    axis.ticks.length = unit(-1, "mm"),
    plot.margin=margin(0.5, 2, 0.1, 0.1, "cm"),
    panel.background = element_blank(),
    axis.text = element_text(colour = "black"),
    legend.position = 'none'
  ) 
dev.off()

# Alternative: boxplot

ggplot(d, aes(x = phase, y = kld,
              color = factor(phase),
              fill = factor(phase))) +  
  stat_boxplot(geom ='errorbar', width = 0.1) + 
  geom_boxplot() + 
  scale_x_discrete(labels = c("Training","Yoked"))+
  scale_y_continuous(breaks = seq(-1,2.5,.5),
                     labels = every_nth(seq(-1,2.5,.5),2,T,F))+
  expand_limits(y = c(-0.5,2.5)) +
  scale_color_manual(name ="",values = col_param,
                     labels=c("training" = "Training","yoked" = "Yoked"))+
  scale_fill_manual(name ="",values = fill_param,
                    labels = c("training" = "Training","yoked" = "Yoked"))+
  labs(x = "Phase") +
  ylab(TeX('$ D_{KL}(\\textit{P_{Yoked} } || \\textit{ P_{Tandem}})\\, (bits)$')) +
  theme(
    axis.line = element_line(colour = "black",size = 0.3),
    axis.ticks = element_line(colour = 'black',size = 0.2),
    legend.key = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.key.size = unit(0.5, "cm"),
    legend.background = element_blank(),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
    axis.title.y = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 9, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
    axis.ticks.length = unit(-1, "mm"),
    plot.margin=margin(0.5, 2, 0.1, 0.1, "cm"),
    panel.background = element_blank(),
    axis.text = element_text(colour = "black"),
    legend.position = 'none'
  ) 


# *************************** Tests ******************************* ####

# tand_resp_bins <- df_IF_aco$bins[df_IF_aco$evento == 1] %>%
#   f_table(.,x.min = 1,x.max = 180)
# 
# yok_resp_bins <- df_IF_aco$bins[df_IF_aco$evento == 11] %>%
#   f_table(.,x.min = 1,x.max = 180)
# 
# 
# P = tand_resp_bins[,2]/sum(tand_resp_bins[,2])
# 
# Q = yok_resp_bins[,2]/sum(yok_resp_bins[,2])
# 
# kld_d(Q,P)
# 
# 
# plot(P,type = "l",col = "blue")
# lines(Q,type = "l",col = "red")
# 
# # training
# 
# df_IF_tr <- bind_all_Ss(326:333,16,1,0) %>%
#   filter(cum_trial <= 30 )
# res = 2
# df_IF_tr$bins <- df_IF_tr$cum_dt %>%
#   get_bins(.,x.min = 1,x.max = max(.),resolution = res)
# 
# tand_resp_bins <- df_IF_tr$bins[df_IF_tr$evento == 1] %>%
#   f_table(.,x.min = 1,x.max = 180/res)
# 
# yok_resp_bins <- df_IF_tr$bins[df_IF_tr$evento == 11] %>%
#   f_table(.,x.min = 1,x.max = 180/res)
# 
# 
# P = tand_resp_bins[,2]/sum(tand_resp_bins[,2])
# 
# Q = yok_resp_bins[,2]/sum(yok_resp_bins[,2])
# 
# kld_d(Q,P)
# 
# plot(P,type = "l",col = "blue")
# lines(Q,type = "l",col = "red")
