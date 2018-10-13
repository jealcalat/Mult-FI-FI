

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


ggplot(qq_both,
       aes(x = Tandem,y = Yoked)) +
  geom_point(size = 1,
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
                     expand = c(0,0),
                     labels = every_nth(seq(60,95,5),2, T,T)) +
  geom_abline(intercept = 0,slope = 1,linetype = 2,size = 0.5) +
  xlab("Tandem IRI (s)")+
  ylab("Yoked IRI (s)") +
  pub_theme


tapply(qq_both$Tandem, qq_both$phase, summary)
