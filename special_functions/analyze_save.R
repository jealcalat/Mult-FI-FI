# ----- ---------------------Data preparation ----------------------------

source("~/Dropbox/mult_IF_aco_exp/v3/wrapper.R")

# ------------------------------ All data -------------------------------
# Data of peak trials
 
peak_yoked <- bind_all_Ss(326:333,30,1,1) 
peak_yoked$phase <- "Yoked"

peak_training <- bind_all_Ss(326:333,40,1,0) 
peak_training$phase <- "Training"

all_data_peak <- bind_rows(peak_training,peak_yoked)

all_data_peak <- all_data_peak %>%
  select(cum_dt,evento,sujeto,sesion,cde,trial,phase)

# Phase and cde (the component) as factors

all_data_peak$phase <- factor(all_data_peak$phase)
all_data_peak$cde <- factor(all_data_peak$cde)

# Data of multiple fixed interval 

mIF_yoked <-  bind_all_Ss(326:333,30,0,1) 
mIF_yoked$phase <- "Yoked"

mIF_training <-  bind_all_Ss(326:333,40,0,0) 
mIF_training$phase <- "Training"

all_data_mIF <- bind_rows(mIF_training,mIF_yoked)

# Phase and cde (the component) as factors

all_data_mIF$phase <- factor(all_data_mIF$phase)
all_data_mIF$cde <- factor(all_data_mIF$cde)

save(all_data_mIF,all_data_peak,file = "all_data_IF_&_peak.RData")

# --------------------------- Overall rate ------------------------------

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

save(df_rate_both_phases,file = "overall_rate_data.RData")

# ------------------- Individual trial analysis ---------------------

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

resp_times_aco <- bind_all_Ss(326:333,18,1,1) %>% resp_times(.,30)
resp_times_aco$phase <- "yoked"

resp_times_training <- bind_all_Ss(326:333,25,1,0) %>% resp_times(.,30)
resp_times_training$phase <- "training"

resp_times_df <- bind_rows(resp_times_aco,resp_times_training)

fwhm_2phases <- resp_times_df %>%
  group_by(sujeto, cde, phase,sesion)%>%
  filter(length(bins) > 1) %>%
  group_by(sujeto,cde,phase,sesion) %>%
  summarise(
    fwhm = 
    {
      rt <- bins
      rt <- rt[rt < 180]
      d  <- data.frame(x = density(rt)$x,
                       y = density(rt)$y)
      fwhm <- fwhm(d$x,d$y)$fwhm # custom function
      if(length(fwhm) == 0 ) {
        NA} else { fwhm }
    }, 
    peak = 
    {
      rt <- bins
      rt <- rt[rt < 180]
      d  <- data.frame(x = density(rt)$x,
                       y = density(rt)$y)
      mfw <- fwhm(d$x,d$y)$peak # custom function
      if(length(mfw) == 0 ) {
        NA} else { mfw }
    }) %>% na.omit()

fwhm_2phases$cde <- fwhm_2phases$cde %>% as.factor

save(df_p_lhl,fwhm_2phases,file = "start_stop_fwhm_data.RData")

#------------ Correlation analysis ----------------------------

df_IF_tr <- bind_all_Ss(326:333,16,0,0) %>% as.data.frame()

df_IF_tr <- df_IF_tr %>%
  filter(cum_trial <= 69)

rho_tr_tand <- df_IF_tr %>%
  filter(cde == 1) %>%
  select(cum_dt,evento,cde,sesion) %>%
  group_by(cde,sesion) %>%
  summarise(rho = matrix(c(cum_dt,evento),ncol = 2) %>%
  {
    t_ev <- .
    rho <- rR_corr(t_ev,1,2,1000,200)
    rho
  })

rho_tr_yoked <- df_IF_tr %>%
  filter(cde == 2) %>%
  select(cum_dt,evento,cde,sesion) %>%
  group_by(sesion,cde) %>%
  summarise(rho = matrix(c(cum_dt,evento),ncol = 2) %>%
  {
    t_ev <- .
    rho <- rR_corr(t_ev,11,21,1000,200)
    rho
  })

rho_tr <- bind_rows(rho_tr_tand,rho_tr_yoked)

rho_tr$phase <- "training"

df_IF_aco <- bind_all_Ss(326:333,16,0,1) %>% as.data.frame()

df_IF_aco <- df_IF_aco %>%
  filter(cum_trial <= 69)

rho_aco_tand <- df_IF_aco %>%
  filter(cde == 1) %>%
  select(cum_dt,evento,cde,sesion) %>%
  group_by(cde,sesion) %>%
  summarise(rho = matrix(c(cum_dt,evento),ncol = 2) %>%
  {
    t_ev <- .
    rho <- rR_corr(t_ev,1,2,1000,200)
    rho
  })

rho_aco_yoked <- df_IF_aco %>%
  filter(cde == 2) %>%
  select(cum_dt,evento,cde,sesion) %>%
  group_by(cde,sesion) %>%
  summarise(rho = matrix(c(cum_dt,evento),ncol = 2) %>%
  {
    t_ev <- .
    rho <- rR_corr(t_ev,11,21,1000,200)
    rho
  })

rho_aco <- bind_rows(rho_aco_tand,rho_aco_yoked)

rho_aco$phase <- "aco"

rho_both_ph <- bind_rows(rho_tr,rho_aco)

rho_both_ph$phase <- factor(rho_both_ph$phase)
levels(rho_both_ph$phase ) <- c("Yoked","Training")

rho_both_ph$phase  <- factor(rho_both_ph$phase , levels = c("Training","Yoked"))

#--------------------- Correlation coefficient by session ---------------- 

df_IF_tr_xs <- bind_all_Ss(326:333,40,0,0) %>% as.data.frame()

rho_tr_tand_xs <- df_IF_tr_xs %>%
  filter(cde == 1) %>%
  select(cum_dt,evento,cde,sesion) %>%
  group_by(cde,sesion) %>%
  summarise(rho = matrix(c(cum_dt,evento),ncol = 2) %>%
  {
    t_ev <- .
    rho <- rR_corr(t_ev,1,2,1000,200)
    rho
  })


rho_tr_yoked_xs <- df_IF_tr_xs %>%
  filter(cde == 2) %>%
  select(cum_dt,evento,cde,sesion) %>%
  group_by(sesion,cde) %>%
  summarise(rho = matrix(c(cum_dt,evento),ncol = 2) %>%
  {
    t_ev <- .
    rho <- rR_corr(t_ev,11,21,1000,200)
    rho
  })

rho_tr_xs <- bind_rows(rho_tr_tand_xs,rho_tr_yoked_xs)

rho_tr_xs$phase <- "training"

df_IF_aco_xs <- bind_all_Ss(326:333,30,0,1) %>% as.data.frame()

rho_aco_tand_xs <- df_IF_aco_xs %>%
  filter(cde == 1) %>%
  select(cum_dt,evento,cde,sesion) %>%
  group_by(cde,sesion) %>%
  summarise(rho = matrix(c(cum_dt,evento),ncol = 2) %>%
  {
    t_ev <- .
    rho <- rR_corr(t_ev,1,2,1000,200)
    rho
  })

rho_aco_yoked_xs <- df_IF_aco_xs %>%
  filter(cde == 2) %>%
  select(cum_dt,evento,cde,sesion) %>%
  group_by(cde,sesion) %>%
  summarise(rho = matrix(c(cum_dt,evento),ncol = 2) %>%
  {
    t_ev <- .
    rho <- rR_corr(t_ev,11,21,1000,200)
    rho
  })

rho_aco_xs <- bind_rows(rho_aco_tand_xs,rho_aco_yoked_xs)

rho_aco_xs$phase <- "aco"

rho_both_ph_xs <- bind_rows(rho_tr_xs,rho_aco_xs)

rho_both_ph_xs$phase <- factor(rho_both_ph_xs$phase)

levels(rho_both_ph_xs$phase ) <- c("Yoked","Training")

rho_both_ph_xs$phase  <- factor(rho_both_ph_xs$phase, levels = c("Training","Yoked"))

save(rho_both_ph,rho_both_ph_xs,file = "correlation_data.RData")

# ----------------------- Normalized plots ---------------------------------

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

#  --------------------- Qplot of IRI ---------------------------------

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


# --------------------------------- Dkl -----------------------------------

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

# Reordenar por fase

d %<>% arrange(phase)

dsum <- d %>% group_by(phase) %>% summarise(kld = median(kld,na.rm = T))

save(df_aco,df_tr,qq_both,d,dsum,file = "rate_qq_dkl.RData")

#----- Clustering by k medoids, pam function that is robust to outliers -------

df_p <- pooled_ind_ana_km_df(326:333,30,30) %>% as.data.frame()
# total = 960

write.csv(df_p,"~/Dropbox/LAyCC/km_plots/ind_metrics_kmedoids.csv",row.names = F)
