


alch_ret_daily <- Alchian_Data %>% 
  filter(!is.na(`DJIA`)) %>% 
  # calculate price returns
  mutate(ret_bc = 100*(`Beryllium Corp`/dplyr::lag(`Beryllium Corp`) -1),
         ret_lith = 100*(`Lithium Corp of America`/dplyr::lag(`Lithium Corp of America`) -1),
         ret_ASRC = 100*(`American Smelting & Refining Co.`/dplyr::lag(`American Smelting & Refining Co.`) -1),
         ret_MHI = 100*(`Metal Hydrides Inc`/dplyr::lag(`Metal Hydrides Inc`) -1),
         ret_DJIA = 100*(`DJIA`/dplyr::lag(`DJIA`) -1),
         # de-annualize yield
         Rf = `Market yield on 3 month bills`/365) 


alch_ret_weekly <- Alchian_Data %>% 
  # linearly interpolate missing risk free rates; this covers some cases where bond markets closed but stock markets didn't
  mutate(Rf_interp = zoo::na.approx(`Market yield on 3 month bills`, na.rm = FALSE)) %>% 
  filter(!is.na(`Metal Hydrides Inc`)) %>% 
  mutate(ret_bc = 100*(`Beryllium Corp`/dplyr::lag(`Beryllium Corp`) -1),
         ret_lith = 100*(`Lithium Corp of America`/dplyr::lag(`Lithium Corp of America`) -1),
         ret_ASRC = 100*(`American Smelting & Refining Co.`/dplyr::lag(`American Smelting & Refining Co.`) -1),
         ret_MHI = 100*(`Metal Hydrides Inc`/dplyr::lag(`Metal Hydrides Inc`) -1),
         ret_DJIA = 100*(`DJIA`/dplyr::lag(`DJIA`) -1),
         Rf = Rf_interp/52)


# set a particular day for BC as missing because of a stock split
alch_ret_daily$ret_bc[alch_ret_daily$Date == mdy("4/28/1953")] <- NA
alch_ret_weekly$ret_bc[alch_ret_weekly$Date == mdy("5/1/1953")] <- NA

## windows
# T0 to T1: presample
# T1 to T2: event window
# T2 to T3: post-sample

T0 <- ymd("1953-01-01")
T1 <- ymd("1953-07-31")
T2 <- ymd("1953-12-31")
T3 <- ymd("1954-12-31")


# plot normalized price to illustrate what he was talking about

alch_ret_weekly_plot <- alch_ret_weekly %>% 
  select(Date:DJIA) %>% 
  filter(Date > T1 & Date <= T3) %>% 
  pivot_longer(-Date) %>% 
  group_by(name) %>% 
  mutate(value = value/value[1])


ggplot(data = filter(alch_ret_weekly_plot, (Date > T1 & Date <= T2)), aes(x = Date, y= value, color = name))+
  geom_hline(yintercept = 1)+
  geom_line(linewidth = 2)+
  ylab("Stock price (Aug 1, 1953 == 1)")+
  guides(color = guide_legend(title = element_blank()))+
  theme_carleton()+
  theme(legend.position = c(.2,.8))+
  scale_color_paletteer_d("ggthemes::calc")

ggsave("figures/naive_price_norm.png", device = "png", dpi = "retina", scale = 1, bg = "white",
       width=320*.9, height =180*.9, units = "mm")

ggplot(data = filter(alch_ret_weekly_plot, (Date > T1 & Date <= T3)), aes(x = Date, y= value, color = name))+
  geom_hline(yintercept = 1)+
  geom_line(linewidth = 2)+
  ylab("Stock price (Aug 1, 1953 == 1)")+
  guides(color = guide_legend(title = element_blank()))+
  theme_carleton()+
  scale_color_paletteer_d("ggthemes::calc")+
  theme(legend.position = c(.2,.8))
ggsave("figures/naive_price_ext.png", device = "png", dpi = "retina", scale = 1, bg = "white",
       width=320*.9, height =180*.9, units = "mm")




naive_daily <- list("Beryllium Corp" = lm(ret_bc ~ 1, data = alch_ret_daily, subset = (Date >= T0 & Date <= T1)),
                    "Lithium Corp.of Am."=lm(ret_lith ~ 1, data = alch_ret_daily, subset = (Date >= T0 & Date <= T1)),
                    "ASRC" = lm(ret_ASRC ~ 1, data = alch_ret_daily, subset = (Date >= T0 & Date <= T1)),
                    "DJIA" = lm(ret_DJIA ~ 1, data =alch_ret_daily, subset = (Date >= T0 & Date <= T1) ))

naive_weekly <- list("Beryllium Corp" = lm(ret_bc ~ 1, data = alch_ret_weekly, subset = (Date >= T0 & Date <= T1)),
                     "Lithium Corp.of Am."=lm(ret_lith ~ 1, data = alch_ret_weekly, subset = (Date >= T0 & Date <= T1)),
                     "ASRC" = lm(ret_ASRC ~ 1, data = alch_ret_weekly, subset = (Date >= T0 & Date <= T1)),
                     "MHI" =  lm(ret_MHI ~ 1, data = alch_ret_weekly, subset = (Date >= T0 & Date <= T1)),
                     "DJIA" = lm(ret_DJIA ~ 1, data =alch_ret_weekly, subset = (Date >= T0 & Date <= T1) ))


### Table formatting for modelsummary

starlist <- c('*' = .1, '**' = 0.05, '***' = 0.01)


gm <- gof_map
gm$omit <- FALSE
cm <- c("(Intercept)" = "Mean return")

modelsummary(naive_daily, coef_map = cm, stars = starlist, fmt = 2, gof_map = c("nobs", "r.squared","adj.r.squared", "F","rmse","vcov.type"), escape = FALSE, vcov ="NeweyWest")#, output = paste0(table_path, "/naive_weekly.docx"))
modelsummary(naive_weekly, coef_map = cm, stars = starlist, fmt = 2, gof_map = c("nobs", "r.squared","adj.r.squared", "F","rmse","vcov.type"), escape = FALSE, vcov ="NeweyWest")# , output = paste0(table_path, "/naive_weekly.docx"))

### Abnormal returns


alch_naive_daily_AR <- alch_ret_daily %>% 
  mutate(naive_bc =   predict(naive_daily[[1]],alch_ret_daily, interval = "none"),
         naive_lith=  predict(naive_daily[[2]],alch_ret_daily, interval = "none"),
         naive_ASRC = predict(naive_daily[[3]],alch_ret_daily, interval = "none"),
         naive_DJIA = predict(naive_daily[[4]],alch_ret_daily, interval = "none")) %>% 
  mutate(AR_bc_const_ret =   ret_bc - naive_bc,
         AR_lith_const_ret= ret_lith  - naive_lith,
         AR_ASRC_const_ret= ret_ASRC  - naive_ASRC,
         AR_DJIA_const_ret = ret_DJIA - naive_DJIA) %>% 
  # select(`Date`,ex_DJI, ex_bc:ex_ASRC, CAPM_bc:AR_ASRC_CAPM) %>% 
  mutate(inwindow = Date %within% interval(start = T1, end =T2)) 

### Weekly abnormal returns


alch_naive_weekly_AR <- alch_ret_weekly %>% 
  mutate(naive_bc = predict(naive_weekly[[1]],  alch_ret_weekly, interval = "none"),
         naive_lith= predict(naive_weekly[[2]], alch_ret_weekly, interval = "none"),
         naive_ASRC = predict(naive_weekly[[3]],alch_ret_weekly,interval = "none") ,
         naive_MHI = predict(naive_weekly[[4]], alch_ret_weekly,interval = "none") ,
         naive_DJIA = predict(naive_weekly[[5]], alch_ret_weekly,interval = "none")) %>% 
  
  mutate(AR_bc_const_ret = ret_bc  - naive_bc,
         AR_lith_const_ret = ret_lith - naive_lith,
         AR_ASRC_const_ret = ret_ASRC - naive_ASRC,
         AR_MHI_const_ret = ret_MHI   - naive_MHI,
         AR_DJIA_const_ret = ret_DJIA   - naive_DJIA) %>% 
  # select(`Date`,ex_DJI, ex_bc:ex_MHI, CAPM_bc:AR_MHI_CAPM) %>% 
  mutate(inwindow = Date %within% interval(start = T1, end = T2))



#### Variance of AR - MacKinlay 1997, eqn 8

# for all of this it would be nicer to write a function
# and there are probably prebuilt packages in R for this, like in Stata
# but it's also probably good to code it up by hand once

##
n_daily_bc <-   nrow(naive_daily$`Beryllium Corp`$model)
n_daily_lith <- nrow(naive_daily$`Lithium Corp.of Am.`$model)
n_daily_ASRC <- nrow(naive_daily$`ASRC`$model)
n_daily_DJIA <- nrow(naive_daily$`DJIA`$model)


bc_df  <- naive_daily$`Beryllium Corp`$df.residual
lith_df<- naive_daily$`Lithium Corp.of Am.`$df.residual
ASRC_df<- naive_daily$`ASRC`$df.residual
DJIA_df <- naive_daily$`DJIA`$df.residual


rse_bc  <-  sqrt(sum((naive_daily[[1]]$residuals)^2 )/(naive_daily[[1]]$df.residual))
rse_lith <- sqrt(sum((naive_daily[[2]]$residuals)^2 )/(naive_daily[[2]]$df.residual))
rse_ASRC <- sqrt(sum((naive_daily[[3]]$residuals)^2 )/(naive_daily[[3]]$df.residual))
rse_DJIA <- sqrt(sum((naive_daily[[4]]$residuals)^2 )/(naive_daily[[4]]$df.residual))


# weekly

n_weekly_bc <-   nrow(naive_weekly$`Beryllium Corp`$model)
n_weekly_lith <- nrow(naive_weekly$`Lithium Corp.of Am.`$model)
n_weekly_lith <- nrow(naive_weekly$`Lithium Corp.of Am.`$model)
n_weekly_MHI <- nrow(naive_weekly$`MHI`$model)
n_weekly_DJIA <- nrow(naive_weekly$`DJIA`$model)


bc_weekly_df  <-  naive_weekly$`Beryllium Corp`$df.residual
lith_weekly_df<-  naive_weekly$`Lithium Corp.of Am.`$df.residual
ASRC_weekly_df<-  naive_weekly$`ASRC`$df.residual
MHI_weekly_df<-  naive_weekly$`MHI`$df.residual
DJIA_weekly_df <- naive_weekly$`DJIA`$df.residual

rse_weekly_bc  <- sqrt(sum((naive_weekly[[1]]$residuals)^2 )/(naive_weekly[[1]]$df.residual))
rse_weekly_lith <- sqrt(sum((naive_weekly[[2]]$residuals)^2 )/(naive_weekly[[2]]$df.residual))
rse_weekly_ASRC <- sqrt(sum((naive_weekly[[3]]$residuals)^2 )/(naive_weekly[[3]]$df.residual))
rse_weekly_MHI <- sqrt(sum((naive_weekly[[4]]$residuals)^2 )/(naive_weekly[[4]]$df.residual))
rse_weekly_DJIA <- sqrt(sum((naive_weekly[[5]]$residuals)^2 )/(naive_weekly[[5]]$df.residual))

# plot the standardized AR, under a null of zero

alch_plot_std_AR <- alch_naive_daily_AR %>% 
  select(Date,AR_bc_const_ret:AR_DJIA_const_ret) %>% 
  mutate(`Beryllium Corp.` = AR_bc_const_ret/rse_bc,
         `Lithium Corp. of America`  = AR_lith_const_ret/rse_lith,
         `ASRC` = AR_ASRC_const_ret/rse_ASRC,
         `Dow Jones Industrial Average` = AR_DJIA_const_ret/rse_DJIA) %>% 
  select(Date, `Beryllium Corp.`:`Dow Jones Industrial Average`) %>% 
  pivot_longer(cols = -Date)

#

ggplot(data = filter(alch_plot_std_AR, Date >= T1 & Date <= T2), aes(x = Date, y = value, color = name))+
  geom_point(alpha = 0.25, size = 2, color = "black") +
  geom_point(data = filter(alch_plot_std_AR, (Date >= T1 & Date <= T2& abs(value)>=1.96)), aes(x = Date, y = value), color = "red", size = 2, shape = "square")+
  theme_carleton()+
  geom_hline(yintercept = 0, alpha = .5)+
  geom_hline(yintercept = 0, alpha = .5)+
  geom_hline(aes(yintercept = 1.96),  linetype = 2)+
  geom_hline(aes(yintercept =- 1.96),  linetype = 2)+
  guides(color = "none")+
  scale_color_paletteer_d("ggthemes::calc")+
  ylab('Standardized abnormal return')+  facet_wrap(facets = vars(as_factor(name)))

ggsave("figures/AR_naive_daily.png", device = "png", dpi = "retina", scale = 1, bg = "white",
       width=320*.9, height =180*.9, units = "mm")

alch_plot_std_w_AR <- alch_naive_weekly_AR %>% 
  select(Date,AR_bc_const_ret:AR_DJIA_const_ret) %>% 
  mutate(`Beryllium Corp.` = AR_bc_const_ret/rse_weekly_bc,
         `Lithium Corp. of America`  = AR_lith_const_ret/rse_weekly_lith,
         `ASRC` = AR_ASRC_const_ret/rse_weekly_ASRC,
         `Metal Hydrides, Inc` = AR_MHI_const_ret/rse_weekly_MHI,
         `Dow Jones Industrial Average` = AR_DJIA_const_ret/rse_weekly_DJIA) %>% 
  select(Date, `Beryllium Corp.`:`Dow Jones Industrial Average`) %>% 
  pivot_longer(cols = -Date)

#

ggplot(data = filter(alch_plot_std_w_AR, Date >= T1 & Date <= T2), aes(x = Date, y = value, color = name))+
  geom_point(alpha = 0.25, size = 2, color = "black") +
  geom_point(data = filter(alch_plot_std_w_AR, (Date >= T1 & Date <= T2& abs(value)>=1.96)), aes(x = Date, y = value), color = "red", size = 2, shape = "square")+
  theme_carleton()+
  geom_hline(yintercept = 0, alpha = .5)+
  geom_hline(yintercept = 0, alpha = .5)+
  geom_hline(aes(yintercept = 1.96),  linetype = 2)+
  geom_hline(aes(yintercept =- 1.96),  linetype = 2)+
  guides(color = "none")+
  ylab('Standardized abnormal return')+  facet_wrap(facets = vars(as_factor(name)))

ggsave("figures/AR_naive_weekly.png", device = "png", dpi = "retina", scale = 1, bg = "white",
       width=320*.9, height =180*.9, units = "mm")




tauwstart = 0
tauwend = 26

row_event <- which(alch_naive_weekly_AR$Date == T1)


alch_naive_weekly_CAR<-alch_naive_weekly_AR %>% 
  filter(Date >= T1 & Date <=T2) %>% 
  #filter(!is.na(ex_MHI)) %>% 
 # mutate(tauw = row_number() - row_test) %>% 
 # filter(tauw >= tauwstart & tauw <= tauwend) %>% 
  mutate(CAR_MHI  = cumsum(AR_MHI_const_ret),
         CAR_BC   = cumsum(AR_bc_const_ret),
         CAR_ASRC = cumsum(AR_ASRC_const_ret),
         CAR_LC   = cumsum(AR_lith_const_ret),
         CAR_DJIA = cumsum(AR_DJIA_const_ret)) %>% 
  mutate(CAR_var_MHI = row_number()*rse_weekly_MHI^2,
         CAR_var_BC  = row_number()*rse_weekly_bc^2,
         CAR_var_ASRC= row_number()*rse_weekly_ASRC^2,
         CAR_var_LC  = row_number()*rse_weekly_lith^2,
         CAR_var_DJIA= row_number()*rse_weekly_DJIA^2) %>% 
  mutate(CAR_MHI_std  = CAR_MHI /sqrt( CAR_var_MHI ),
         CAR_BC_std   = CAR_BC  /sqrt( CAR_var_BC  ),
         CAR_ASRC_std = CAR_ASRC/sqrt( CAR_var_ASRC),
         CAR_LC_std   = CAR_LC  /sqrt( CAR_var_LC  ),
         CAR_DJIA_std = CAR_DJIA/sqrt( CAR_var_DJIA))


ggplot(alch_naive_weekly_CAR, aes(x = as.Date(Date)))+
  geom_line(aes(y = CAR_BC_std, color = "Beryllium Corp")) + geom_point(aes(y = CAR_BC_std, color = "Beryllium Corp"))+
  geom_line(aes(y = CAR_LC_std, color = "Lithium Corp. of America"))+ geom_point(aes(y = CAR_LC_std, color = "Lithium Corp. of America"))+
  geom_line(aes(y = CAR_ASRC_std, color = "ASRC"))+ geom_point(aes(y = CAR_ASRC_std, color = "ASRC")) +
  geom_line(aes(y = CAR_MHI_std, color = "MHI"))+ geom_point(aes(y = CAR_MHI_std, color = "MHI")) +
 # geom_line(aes(y = CAR_DJIA_std, color = "DJIA"), color = "black")+ geom_point(aes(y = CAR_DJIA_std, color = "DJIA"), color = "black")+
  theme_carleton()+
  geom_hline(yintercept = 0, alpha = .5)+
  geom_hline(aes(yintercept = 1.96),  linetype = 2)+
  geom_hline(aes(yintercept =- 1.96),  linetype = 2)+
  geom_hline(aes(yintercept = 1.645),  linetype = 3)+
  geom_hline(aes(yintercept =- 1.645),  linetype = 3)+
  ylab('Standardized cumulative excess return')+
  scale_color_paletteer_d("ggthemes::calc")+
  labs(color = element_blank())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
  xlab("Date")

ggsave("figures/CAR_naive_weekly.png", device = "png", dpi = "retina", scale = 1, bg = "white",
       width=320*.9, height =180*.9, units = "mm")


alch_naive_daily_CAR<-alch_naive_daily_AR %>% 
  filter(Date >= T1 & Date <=T2) %>% 
  filter(!is.na(ret_bc)) %>% 
  # mutate(tauw = row_number() - row_test) %>% 
  # filter(tauw >= tauwstart & tauw <= tauwend) %>% 
  mutate(#CAR_MHI  = cumsum(AR_MHI_const_ret),
         CAR_BC   = cumsum(AR_bc_const_ret),
         CAR_ASRC = cumsum(AR_ASRC_const_ret),
         CAR_LC   = cumsum(AR_lith_const_ret),
         CAR_DJIA = cumsum(AR_DJIA_const_ret)) %>% 
  mutate(#CAR_var_MHI = row_number()*rse_MHI^2,
         CAR_var_BC  = row_number()*rse_bc^2,
         CAR_var_ASRC= row_number()*rse_ASRC^2,
         CAR_var_LC  = row_number()*rse_lith^2,
         CAR_var_DJIA= row_number()*rse_DJIA^2) %>% 
  mutate(#CAR_MHI_std  = CAR_MHI /sqrt( CAR_var_MHI ),
         CAR_BC_std   = CAR_BC  /sqrt( CAR_var_BC  ),
         CAR_ASRC_std = CAR_ASRC/sqrt( CAR_var_ASRC),
         CAR_LC_std   = CAR_LC  /sqrt( CAR_var_LC  ),
         CAR_DJIA_std = CAR_DJIA/sqrt( CAR_var_DJIA))


ggplot(alch_naive_daily_CAR, aes(x = as.Date(Date)))+
  geom_line(aes(y = CAR_BC_std, color = "Beryllium Corp")) + geom_point(aes(y = CAR_BC_std, color = "Beryllium Corp"))+
  geom_line(aes(y = CAR_LC_std, color = "Lithium Corp. of America"), size = 1)+ geom_point(aes(y = CAR_LC_std, color = "Lithium Corp. of America"))+
  geom_line(aes(y = CAR_ASRC_std, color = "ASRC"))+ geom_point(aes(y = CAR_ASRC_std, color = "ASRC")) +
  #geom_line(aes(y = CAR_MHI_std, color = "MHI"))+ geom_point(aes(y = CAR_MHI_std, color = "MHI")) +
#  geom_line(aes(y = CAR_DJIA_std, color = "DJIA"))+ geom_point(aes(y = CAR_DJIA_std, color = "DJIA"))+
  theme_carleton()+
  geom_hline(yintercept = 0, alpha = .5)+
  geom_hline(aes(yintercept = 1.96),  linetype = 2)+
  geom_hline(aes(yintercept =- 1.96),  linetype = 2)+
  ylab('Standardized cumulative excess return')+
  labs(color = element_blank())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
  scale_color_paletteer_d("ggthemes::calc")+
  xlab("Date")

ggsave("figures/CAR_naive_daily.png", device = "png", dpi = "retina", scale = 1, bg = "white",
       width=320*.9, height =180*.9, units = "mm")

ggplot(alch_naive_daily_CAR, aes(x = as.Date(Date)))+
  geom_line(aes(y = CAR_LC), size = 2)+
  geom_ribbon(aes(ymin = CAR_LC - 1.96*sqrt( CAR_var_LC  ), ymax = CAR_LC + 1.96*sqrt( CAR_var_LC  )), color = "lightgrey", alpha = 0.25)+
  theme_carleton()+
  geom_hline(yintercept = 0, alpha = .5)+
  ylab('Cumulative excess return\nand 95% CI')+
  labs(color = element_blank())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
  xlab("Date")
  ggsave("figures/CAR_LC_naive_daily.png", device = "png", dpi = "retina", scale = 1, bg = "white",
         width=320*.9, height =180*.9, units = "mm")



alch_naive_daily_CAR<-alch_naive_daily_AR %>% 
  filter(Date >= T1 & Date <=T2) %>% 
  filter(!is.na(ret_bc)) %>% 
  # mutate(tauw = row_number() - row_test) %>% 
  # filter(tauw >= tauwstart & tauw <= tauwend) %>% 
  mutate(#CAR_MHI  = cumsum(AR_MHI_const_ret),
    CAR_BC   = cumsum(AR_bc_const_ret),
    CAR_ASRC = cumsum(AR_ASRC_const_ret),
    CAR_LC   = cumsum(AR_lith_const_ret),
    CAR_DJIA = cumsum(AR_DJIA_const_ret)) %>% 
  mutate(#CAR_var_MHI = row_number()*rse_MHI^2,
    CAR_var_BC  = row_number()*rse_bc^2,
    CAR_var_ASRC= row_number()*rse_ASRC^2,
    CAR_var_LC  = row_number()*rse_lith^2,
    CAR_var_DJIA= row_number()*rse_DJIA^2) %>% 
  mutate(#CAR_MHI_std  = CAR_MHI /sqrt( CAR_var_MHI ),
    CAR_BC_std   = CAR_BC  /sqrt( CAR_var_BC  ),
    CAR_ASRC_std = CAR_ASRC/sqrt( CAR_var_ASRC),
    CAR_LC_std   = CAR_LC  /sqrt( CAR_var_LC  ),
    CAR_DJIA_std = CAR_DJIA/sqrt( CAR_var_DJIA))
  
  # %>% 
  # mutate(CAR_var_MHI = (row_number())*rse_MHI^2) %>% 
  #   
  #   
  #   
  #   
  # # this standardizes it.  asymptotically and under the null, CAR is normally distributed with a variance that depends on the windo w as in the previous mutate statement
  # # so 2 standarddeviations above normal rejects the null at approximately the 95% level for a two sided test, etc
  # mutate(CAR_MHI_std = CAR_MHI/sqrt(CAR_var_MHI)) %>% 
  # mutate(pval_MHI = 2*pt(abs(CAR_MHI_std),df = MHI_df, lower.tail =FALSE) ) %>% 
  # left_join(select(alch_capm_daily_CAR, Date, tau))
  # 


# 
# 
# taustart = -36
# tauend = 40
# alch_naivedaily_CAR <- alch_naive_weekly_AR %>% 
#   filter(!is.na(ex_lith)) # clean up before retiming
# 
# row_test <- which(alch_naive_weekly_CAR$Date == ymd("1954-03-01"))
# 
