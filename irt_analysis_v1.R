

IRT_df <- bind_all_Ss(326:333,18,1,1) %>%
  as.data.frame()

IRT_df$phase <- "yoked"

head(IRT_df)

IRT_df <- IRT_df %>% 
  filter(evento == 1 | evento == 11,
         cum_dt <= 180) %>%
  group_by(sujeto,sesion,cde,cum_trial) %>%
  mutate(dt = c(cum_dt[1],diff(cum_dt)))



IRT_df$quarters <- IRT_df$cum_dt %>% 
{
  q <- .
  q <- cut(q,(1:max(q))*20)
  q <- ifelse(is.na(q),0,q)
  as.integer(q)
}


ggplot(IRT_df,
       aes(y = dt,x = cum_dt,color = factor(quarters))) + 
  geom_point() +
  # scale_y_log10() +
  facet_wrap(~cde)


ggplot(IRT_df %>%
         filter(dt < 60),
       aes(x = dt, 
           fill = factor(cde))) +
  geom_histogram(alpha=0.5,binwidth = 0.1) + 
  facet_wrap(~quarters)


IRT_df_mean <- IRT_df %>%
  group_by(cde,quarters,sujeto,sesion) %>%
  summarise(mean_dt = mean(dt))

ggplot(IRT_df_mean,
       aes(y = mean_dt,
           x = factor(sesion),
           color = factor(cde))) + 
  geom_boxplot() +
  # geom_line() +
  facet_wrap(~sujeto,scales = "free_y")
# scale_y_log10() +

# contiguity
# 

cont_df <- bind_all_Ss(326:333,6,0,1) %>%
  as.data.frame()

cont_df <- cont_df  %>%
  group_by(sujeto,sesion,cde,trial) %>%
  mutate(t_rf = cum_dt[evento == 2 | evento == 21]) %>%
  filter(evento == 1 | evento == 11) 

cont_df$abs_err <- abs(cont_df$t_rf - cont_df$cum_dt)

ggplot(cont_df,
       aes(x = abs_err + 1,
           fill = factor(cde))) +
  geom_histogram(position='identity') + 
  scale_x_log10() +
  facet_wrap(~sujeto)

cont_df_mean <- cont_df %>%
  group_by(sujeto,sesion,cde,trial) %>%
  summarise(rate = n()/t_rf[1],
            t_rf = t_rf[1]) %>%
  group_by(cde,sujeto) %>%
  summarise(mean_r = mean(rate),
            mean_t_rf = mean(t_rf))

ggplot(cont_df_mean,
       aes(x = mean_t_rf,
           y = mean_r,
           fill = factor(cde))) +
  geom_point(shape = 21)


# As in figure 5 of yin et al

cont_df$iri_bin <- cont_df$abs_err  %>%
{
  iri <- .
  iri <- cut(iri,(1:max(iri)) * 10)
  iri <- ifelse(is.na(iri),0,iri)
  as.integer(iri)
}

cont_dr_prop <- cont_df %>%
  group_by(sujeto,cde) %>%
  mutate(resp_n = length(evento)) %>%
  group_by(sujeto,cde,iri_bin) %>%
  summarise(resp_p = length(evento)/unique(resp_n))

ggplot(cont_dr_prop,
       aes(x = iri_bin*10,
           y = resp_p,
           color = factor(cde))) + 
  geom_point() 
