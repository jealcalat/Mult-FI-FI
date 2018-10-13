# wrapper
source("~/Dropbox/mult_IF_aco_exp/v3/wrapper.R")
# normalized plots

df_tr <- bind_all_Ss(326:333,25,1,0) %>% 
  resp_rate_df(.,2,30) %>%
  group_by(sujeto,cde) %>%
  mutate(resp_norm = (resp - min(resp))/(max(resp) - min(resp)),
         ec_norm = (ec_resp - min(ec_resp))/(max(ec_resp) - min(ec_resp)))

df_aco <- bind_all_Ss(326:333,25,1,1) %>% 
  resp_rate_df(.,2,30) %>%
  group_by(sujeto,cde) %>%
  mutate(resp_norm = (resp - min(resp))/(max(resp) - min(resp)),
         ec_norm = (ec_resp - min(ec_resp))/(max(ec_resp) - min(ec_resp)))


# plot parameters

# col_param <- c("1" = "black","2"="black")
# fill_param <- c("1" = "#808080","2" = "white")
shape_param <- c("1" = 21, "2" = 21)


# Fase de acoplado

yoked_norm_plot <- resp_norm_plot(df_aco,col_param,"lever",
                                  fill_param,shape_param,2,
                                  p_size = 0.2) +
  theme(plot.margin = margin(0.2,0.5,0,0, "cm"))

head_plt <- resp_norm_plot(df_aco,col_param,"head",
                           res=2,fill_param,shape_param,
                           p_size = 0.18) +
  theme(legend.position = 'none',
        # axis.line = element_line(size = 0.09),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0.2,0.5,0,0, "cm"))

svg("~/Dropbox/mult_IF_aco_exp/Plots_&_dfs/Final/actualización/r.norm_h.inset_yoked.svg",
    height = 4,width = 4)

yoked_norm_plot 

print(head_plt, 
      vp = grid::viewport(width = 0.35*0.9, 
                          height=0.35*0.9,
                          x = unit(0.8, "npc"), 
                          y = unit(0.81, "npc")))
dev.off()
# training

tr_norm_plot <- resp_norm_plot(df_tr,col_param,"lever",
                               fill_param,shape_param,2,
                               p_size = 0.2) +
  theme(plot.margin = margin(0.2,0.5,0,0, "cm"))

head_plt <- resp_norm_plot(df_tr,col_param,"head",res=2,
                           fill_param,shape_param,
                           p_size = 0.18) +
  theme(legend.position = 'none',
        # axis.line = element_line(size = 0.09),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0.2,0.5,0,0, "cm"))

svg("~/Dropbox/mult_IF_aco_exp/Plots_&_dfs/Final/actualización/r.norm_h.inset_training.svg",
    height = 4,width = 4)

tr_norm_plot 

print(head_plt, 
      vp = grid::viewport(width = 0.35*0.9, 
                          height=0.35*0.9,
                          x = unit(0.8, "npc"), 
                          y = unit(0.81, "npc")))
dev.off()
