# al hacer table(dfList$evento) salen valores atípicos
# como los de vec_compare. Los reemplazaremos con el 
# siguiente código:

dfList <- bind_all_Ss(326:333,40)

df_acq <- dfList %>%
  group_by(sujeto,sesion,cde) %>%
  summarise(resp = sum(evento == 1 | evento == 11)) %>%
  # group_by(sesion,cde) %>%
  # summarise(resp = mean(resp)) %>%
  as.data.frame()

color <- c("1" = "grey30", "2" = "black")
shapes <- c(4,15,16,21,22,23,24,25)

df_acq$cde <- ifelse(df_acq$cde == 1,"Tandem","Yoked")

pdf("acquisition.pdf",
    height = 4,
    width = 5.5,
    pointsize = 12)

ggplot(df_acq,
       aes(sesion,resp,
           group = sujeto)) +
  geom_line() + 
  geom_point(aes(shape = sujeto),
             size = 2,
             fill = "white") + 
  scale_y_continuous(breaks = seq(0,150,25),
                     expand = c(0,0),
                     limits = c(0,150),
                     labels = every_nth(seq(0,150,25),2,T,T)) +
  scale_x_continuous(expand = c(0.01,0),
                     limits = c(1,25)) +
  scale_shape_manual(name = "Subject",values = shapes) +
  labs(x = "Session",y = "Total responses") +
  facet_wrap(~cde) +
  guides(color = guide_legend(override.aes = list(size=0.5))) +
pub_theme + 
  theme(legend.position = "top",
        legend.box.margin = margin(0,0,0,0,"cm"),
        legend.margin = margin(0,0,0,0,"cm"))

dev.off()

