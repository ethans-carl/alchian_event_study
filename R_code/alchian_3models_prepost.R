


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

## Fama-French

FF3fac <- read_csv("F-F_Research_Data_Factors_daily.CSV", 
                   skip = 3) %>% 
  mutate(Date = ymd(`...1`)) %>% 
  select(Date, `Mkt-RF`:`HML`)

FF_daily_dset <- left_join(alch_ret_daily, FF3fac)
FF_weekly_dset <- left_join(alch_ret_weekly, FF3fac)

cm2 <- c("(Intercept)" = "alpha", "ret_DJIA" = "Return on DJIA", "Mkt-RF" = "Broad stock return minus risk free rate (FF)", "SMB"  = "SMB", "HML" = "HML")



## windows
# T0 to T1: presample
# T1 to T2: event window
# T2 to T3: post-sample

T0 <- ymd("1953-01-01")
T1 <- ymd("1953-07-31")
T2 <- ymd("1954-03-31")
T3 <- ymd("1954-12-31")


### Same window as naive approach

mean_ret_daily <- list("Beryllium Corp" = lm(ret_bc ~ 1, data = FF_daily_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                     "Lithium Corp.of Am."=lm(ret_lith ~ 1, data = FF_daily_dset,  subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                     "ASRC" = lm(ret_ASRC ~ 1, data = FF_daily_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))))


mean_ret_weekly <- list("Beryllium Corp" = lm(ret_bc ~ 1, data = FF_weekly_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                      "Lithium Corp.of Am."=lm(ret_lith ~ 1, data = FF_weekly_dset,  subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                      "ASRC" = lm(ret_ASRC ~ 1, data = FF_weekly_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                      "MHI" =  lm(ret_MHI ~  1, data = FF_weekly_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))))




market_daily <- list("Beryllium Corp" = lm(ret_bc ~ ret_DJIA, data = FF_daily_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                     "Lithium Corp.of Am."=lm(ret_lith ~ ret_DJIA, data = FF_daily_dset,  subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                     "ASRC" = lm(ret_ASRC ~ ret_DJIA, data = FF_daily_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))))


market_weekly <- list("Beryllium Corp" = lm(ret_bc ~ ret_DJIA, data = FF_weekly_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                      "Lithium Corp.of Am."=lm(ret_lith ~ ret_DJIA, data = FF_weekly_dset,  subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                      "ASRC" = lm(ret_ASRC ~ ret_DJIA, data = FF_weekly_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                      "MHI" =  lm(ret_MHI ~  ret_DJIA, data = FF_weekly_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))))






FF_daily <- list("Beryllium Corp" = lm(ret_bc ~ `Mkt-RF` + SMB + HML, data = FF_daily_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                 "Lithium Corp.of Am."=lm(ret_lith ~ `Mkt-RF` + SMB + HML, data = FF_daily_dset, subset =  ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                 "ASRC" = lm(ret_ASRC ~ `Mkt-RF` + SMB + HML, data = FF_daily_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))))


FF_weekly <- list("Beryllium Corp" = lm(ret_bc ~ `Mkt-RF` + SMB + HML, data = FF_weekly_dset, subset =((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                  "Lithium Corp.of Am."=lm(ret_lith ~ `Mkt-RF` + SMB + HML, data = FF_weekly_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                  "ASRC" = lm(ret_ASRC ~ `Mkt-RF` + SMB + HML, data = FF_weekly_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))),
                  "MHI" =  lm(ret_MHI ~  `Mkt-RF` + SMB + HML, data = FF_weekly_dset, subset = ((Date >= T0 & Date <= T1)|(Date>T2 & Date <=T3))))

### Table formatting for modelsummary

starlist <- c('*' = .1, '**' = 0.05, '***' = 0.01)


gm <- gof_map
gm$omit <- FALSE


modelsummary(c(mean_ret_daily, market_daily, FF_daily), coef_map = cm2, stars = starlist, fmt = 2, gof_map = c("nobs", "r.squared","adj.r.squared", "F","rmse","vcov.type"), escape = FALSE, vcov ="NeweyWest", output = paste0(table_path, "/3models_prepost_daily.docx"))
modelsummary(c(mean_ret_weekly, market_weekly, FF_weekly), coef_map = cm2, stars = starlist, fmt = 2, gof_map = c("nobs", "r.squared","adj.r.squared", "F","rmse","vcov.type"), escape = FALSE, vcov ="NeweyWest" , output = paste0(table_path, "/3models_prepost_weekly.docx"))

### Abnormal returns


alch_3mod_daily_AR <- FF_daily_dset %>% 
  mutate(FF_bc =   predict(FF_daily[[1]],FF_daily_dset, interval = "none"),
         FF_lith=  predict(FF_daily[[2]],FF_daily_dset, interval = "none"),
         FF_ASRC = predict(FF_daily[[3]],FF_daily_dset, interval = "none"),
         market_bc =   predict(market_daily[[1]],FF_daily_dset, interval = "none"),
         market_lith=  predict(market_daily[[2]],FF_daily_dset, interval = "none"),
         market_ASRC = predict(market_daily[[3]],FF_daily_dset, interval = "none"),
        naive_bc = predict(mean_ret_daily[[1]],  FF_daily_dset, interval = "none"),
        naive_lith=predict(mean_ret_daily[[2]], FF_daily_dset, interval = "none"),
        naive_ASRC=predict(mean_ret_daily[[3]],FF_daily_dset,interval = "none")) %>% 
    mutate(AR_bc_FF_ret =   ret_bc - FF_bc,
         AR_lith_FF_ret= ret_lith  - FF_lith,
         AR_ASRC_FF_ret= ret_ASRC  - FF_ASRC,
         AR_bc_market_ret =   ret_bc - market_bc,
         AR_lith_market_ret= ret_lith  - market_lith,
         AR_ASRC_market_ret= ret_ASRC  - market_ASRC,
         AR_bc_const_ret = ret_bc  - naive_bc,
         AR_lith_const_ret = ret_lith - naive_lith,
         AR_ASRC_const_ret = ret_ASRC - naive_ASRC) %>% 
  # select(`Date`,ex_DJI, ex_bc:ex_ASRC, CAPM_bc:AR_ASRC_CAPM) %>% 
  mutate(inwindow = Date %within% interval(start = T1, end =T2)) 

### Weekly abnormal returns


alch_3mod_weekly_AR <- FF_weekly_dset %>% 
  mutate(FF_bc = predict(FF_weekly[[1]],  FF_weekly_dset, interval = "none"),
         FF_lith= predict(FF_weekly[[2]], FF_weekly_dset, interval = "none"),
         FF_ASRC = predict(FF_weekly[[3]],FF_weekly_dset,interval = "none") ,
         FF_MHI = predict(FF_weekly[[4]], FF_weekly_dset,interval = "none"),
         naive_bc =  predict(mean_ret_weekly[[1]],  alch_ret_weekly, interval = "none"),
         naive_lith= predict(mean_ret_weekly[[2]], alch_ret_weekly, interval = "none"),
         naive_ASRC= predict(mean_ret_weekly[[3]],alch_ret_weekly,interval = "none") ,
         naive_MHI = predict(mean_ret_weekly[[4]], alch_ret_weekly,interval = "none") ,
         market_bc = predict(market_weekly[[1]],  alch_ret_weekly, interval = "none"),
         market_lith= predict(market_weekly[[2]], alch_ret_weekly, interval = "none"),
         market_ASRC = predict(market_weekly[[3]],alch_ret_weekly,interval = "none") ,
         market_MHI = predict(market_weekly[[4]], alch_ret_weekly,interval = "none") ) %>% 
  
  mutate(AR_bc_FF_ret = ret_bc  - FF_bc,
         AR_lith_FF_ret = ret_lith - FF_lith,
         AR_ASRC_FF_ret = ret_ASRC - FF_ASRC,
         AR_MHI_FF_ret = ret_MHI   - FF_MHI,
         AR_bc_const_ret = ret_bc  - naive_bc,
         AR_lith_const_ret = ret_lith - naive_lith,
         AR_ASRC_const_ret = ret_ASRC - naive_ASRC,
         AR_MHI_const_ret = ret_MHI   - naive_MHI,
         AR_bc_market_ret = ret_bc  - market_bc,
         AR_lith_market_ret = ret_lith - market_lith,
         AR_ASRC_market_ret = ret_ASRC - market_ASRC,
         AR_MHI_market_ret = ret_MHI   - market_MHI) %>% 
  # select(`Date`,ex_DJI, ex_bc:ex_MHI, CAPM_bc:AR_MHI_CAPM) %>% 
  mutate(inwindow = Date %within% interval(start = T1, end = T2))


# n_daily_bc <-   nrow(FF_daily$`Beryllium Corp`$model)
# n_daily_lith <- nrow(FF_daily$`Lithium Corp.of Am.`$model)
# n_daily_ASRC <- nrow(FF_daily$`ASRC`$model)
# 
# 
# bc_df  <- FF_daily$`Beryllium Corp`$df.residual
# lith_df<- FF_daily$`Lithium Corp.of Am.`$df.residual
# ASRC_df<- FF_daily$`ASRC`$df.residual


rse_FF_daily_bc  <-  sqrt(sum((FF_daily[[1]]$residuals)^2 )/(FF_daily[[1]]$df.residual))
rse_FF_daily_lith <- sqrt(sum((FF_daily[[2]]$residuals)^2 )/(FF_daily[[2]]$df.residual))
rse_FF_daily_ASRC <- sqrt(sum((FF_daily[[3]]$residuals)^2 )/(FF_daily[[3]]$df.residual))

rse_const_daily_bc  <-  sqrt(sum((mean_ret_daily[[1]]$residuals)^2 )/(mean_ret_daily[[1]]$df.residual))
rse_const_daily_lith <- sqrt(sum((mean_ret_daily[[2]]$residuals)^2 )/(mean_ret_daily[[2]]$df.residual))
rse_const_daily_ASRC <- sqrt(sum((mean_ret_daily[[3]]$residuals)^2 )/(mean_ret_daily[[3]]$df.residual))

rse_market_daily_bc  <-  sqrt(sum((market_daily[[1]]$residuals)^2 )/(market_daily[[1]]$df.residual))
rse_market_daily_lith <- sqrt(sum((market_daily[[2]]$residuals)^2 )/(market_daily[[2]]$df.residual))
rse_market_daily_ASRC <- sqrt(sum((market_daily[[3]]$residuals)^2 )/(market_daily[[3]]$df.residual))

# weekly

# n_weekly_bc <-   nrow(FF_weekly$`Beryllium Corp`$model)
# n_weekly_lith <- nrow(FF_weekly$`Lithium Corp.of Am.`$model)
# n_weekly_lith <- nrow(FF_weekly$`Lithium Corp.of Am.`$model)
# n_weekly_MHI <- nrow(FF_weekly$`MHI`$model)
# 
# 
# bc_weekly_df  <-  FF_weekly$`Beryllium Corp`$df.residual
# lith_weekly_df<-  FF_weekly$`Lithium Corp.of Am.`$df.residual
# ASRC_weekly_df<-  FF_weekly$`ASRC`$df.residual
# MHI_weekly_df<-  FF_weekly$`MHI`$df.residual

rse_FF_weekly_bc  <- sqrt(sum((FF_weekly[[1]]$residuals)^2 )/(FF_weekly[[1]]$df.residual))
rse_FF_weekly_lith <- sqrt(sum((FF_weekly[[2]]$residuals)^2 )/(FF_weekly[[2]]$df.residual))
rse_FF_weekly_ASRC <- sqrt(sum((FF_weekly[[3]]$residuals)^2 )/(FF_weekly[[3]]$df.residual))
rse_FF_weekly_MHI <- sqrt(sum((FF_weekly[[4]]$residuals)^2 )/(FF_weekly[[4]]$df.residual))


rse_const_weekly_bc  <-  sqrt(sum((mean_ret_weekly[[1]]$residuals)^2 )/(mean_ret_weekly[[1]]$df.residual))
rse_const_weekly_lith <- sqrt(sum((mean_ret_weekly[[2]]$residuals)^2 )/(mean_ret_weekly[[2]]$df.residual))
rse_const_weekly_ASRC <- sqrt(sum((mean_ret_weekly[[3]]$residuals)^2 )/(mean_ret_weekly[[3]]$df.residual))
rse_const_weekly_MHI <-  sqrt(sum((mean_ret_weekly[[4]]$residuals)^2 )/(mean_ret_weekly[[4]]$df.residual))

rse_market_weekly_bc  <- sqrt(sum((market_weekly[[1]]$residuals)^2 )/(market_weekly[[1]]$df.residual))
rse_market_weekly_lith <- sqrt(sum((market_weekly[[2]]$residuals)^2 )/(market_weekly[[2]]$df.residual))
rse_market_weekly_ASRC <- sqrt(sum((market_weekly[[3]]$residuals)^2 )/(market_weekly[[3]]$df.residual))
rse_market_weekly_MHI <- sqrt(sum((market_weekly[[4]]$residuals)^2 )/(market_weekly[[4]]$df.residual))
# 
# 
# alch_plot_std_AR <- alch_FF_daily_AR %>% 
#   select(Date,AR_bc_FF_ret:AR_ASRC_FF_ret) %>% 
#   mutate(`Beryllium Corp.` = AR_bc_FF_ret/rse_bc,
#          `Lithium Corp. of America`  = AR_lith_FF_ret/rse_lith,
#          `ASRC` = AR_ASRC_FF_ret/rse_ASRC) %>% 
#   select(Date, `Beryllium Corp.`:`ASRC`) %>% 
#   pivot_longer(cols = -Date)
# 
# #
# 
# ggplot(data = filter(alch_plot_std_AR, Date >= T1 & Date <= T2), aes(x = Date, y = value, color = name))+
#   geom_point(alpha = 0.25, size = 2, color = "black") +
#   geom_point(data = filter(alch_plot_std_AR, (Date >= T1 & Date <= T2& abs(value)>=1.96)), aes(x = Date, y = value), color = "red", size = 2, shape = "square")+
#     theme_carleton() +   scale_color_paletteer_d("ggthemes::calc")+
#   geom_hline(yintercept = 0, alpha = .5)+
#   geom_hline(yintercept = 0, alpha = .5)+
#   geom_hline(aes(yintercept = 1.96),  linetype = 2)+
#   geom_hline(aes(yintercept =- 1.96),  linetype = 2)+
#   guides(color = "none")+
#   ylab('Standardized abnormal return')+  facet_wrap(facets = vars(as_factor(name)))
# 
# ggsave("figures/AR_FF_prepost_daily.png", device = "png", dpi = "retina", scale = 1, bg = "white",
#        width=320*.9, height =180*.9, units = "mm")
# 
# alch_plot_std_w_AR <- alch_FF_weekly_AR %>% 
#   select(Date,AR_bc_FF_ret:AR_MHI_FF_ret) %>% 
#   mutate(`Beryllium Corp.` = AR_bc_FF_ret/rse_weekly_bc,
#          `Lithium Corp. of America`  = AR_lith_FF_ret/rse_weekly_lith,
#          `ASRC` = AR_ASRC_FF_ret/rse_weekly_ASRC,
#          `Metal Hydrides, Inc` = AR_MHI_FF_ret/rse_weekly_MHI) %>% 
#   select(Date, `Beryllium Corp.`:`Metal Hydrides, Inc`) %>% 
#   pivot_longer(cols = -Date)
# 
# #
# 
# ggplot(data = filter(alch_plot_std_w_AR, Date >= T1 & Date <= T2), aes(x = Date, y = value, color = name))+
#   geom_point(alpha = 0.25, size = 2, color = "black") +
#   geom_point(data = filter(alch_plot_std_w_AR, (Date >= T1 & Date <= T2& abs(value)>=1.96)), aes(x = Date, y = value), color = "red", size = 2, shape = "square")+
#     theme_carleton() +   scale_color_paletteer_d("ggthemes::calc")+
#   geom_hline(yintercept = 0, alpha = .5)+
#   geom_hline(yintercept = 0, alpha = .5)+
#   geom_hline(aes(yintercept = 1.96),  linetype = 2)+
#   geom_hline(aes(yintercept =- 1.96),  linetype = 2)+
#   guides(color = "none")+
#   ylab('Standardized abnormal return')+  facet_wrap(facets = vars(as_factor(name)))
# 
# if(savefile == TRUE){
# ggsave("figures/AR_3model_prepost_weekly.png", device = "png", dpi = "retina", scale = 1, bg = "white",
#        width=320*.9, height =180*.9, units = "mm")
# }



# tauwstart = 0
# tauwend = 26
# 
# row_event <- which(alch_FF_weekly_AR$Date == T1)


alch_3mod_weekly_CAR<-alch_3mod_weekly_AR %>% 
  filter(Date >= T1 & Date <=T2) %>% 
  #filter(!is.na(ex_MHI)) %>% 
  # mutate(tauw = row_number() - row_test) %>% 
  # filter(tauw >= tauwstart & tauw <= tauwend) %>% 
  mutate(CAR_FF_MHI  = cumsum(AR_MHI_FF_ret),
         CAR_FF_BC   = cumsum(AR_bc_FF_ret),
         CAR_FF_ASRC = cumsum(AR_ASRC_FF_ret),
         CAR_FF_LC   = cumsum(AR_lith_FF_ret),
         CAR_market_MHI  = cumsum(AR_MHI_market_ret),
         CAR_market_BC   = cumsum( AR_bc_market_ret),
         CAR_market_ASRC =cumsum(AR_ASRC_market_ret),
         CAR_market_LC   =cumsum(AR_lith_market_ret),
         CAR_const_MHI  = cumsum( AR_MHI_const_ret),
         CAR_const_BC   = cumsum(  AR_bc_const_ret),
         CAR_const_ASRC = cumsum(AR_ASRC_const_ret),
         CAR_const_LC   = cumsum(AR_lith_const_ret),) %>% 
  mutate(CAR_var_FF_MHI = row_number()*rse_FF_weekly_MHI^2,
         CAR_var_FF_BC  = row_number()*rse_FF_weekly_bc^2,
         CAR_var_FF_ASRC= row_number()*rse_FF_weekly_ASRC^2,
         CAR_var_FF_LC  = row_number()*rse_FF_weekly_lith^2,
         CAR_var_market_MHI = row_number()*rse_market_weekly_MHI^2,
         CAR_var_market_BC  = row_number()*rse_market_weekly_bc^2,
         CAR_var_market_ASRC= row_number()*rse_market_weekly_ASRC^2,
         CAR_var_market_LC  = row_number()*rse_market_weekly_lith^2,
         CAR_var_const_MHI = row_number()*rse_const_weekly_MHI^2,
         CAR_var_const_BC  = row_number()*rse_const_weekly_bc^2,
         CAR_var_const_ASRC= row_number()*rse_const_weekly_ASRC^2,
         CAR_var_const_LC  = row_number()*rse_const_weekly_lith^2) %>% 
  mutate(CAR_MHI_FF_std  = CAR_FF_MHI /sqrt( CAR_var_FF_MHI ),
          CAR_BC_FF_std   = CAR_FF_BC  /sqrt( CAR_var_FF_BC  ),
        CAR_ASRC_FF_std = CAR_FF_ASRC/sqrt( CAR_var_FF_ASRC),
          CAR_LC_FF_std   = CAR_FF_LC  /sqrt( CAR_var_FF_LC  ),
        CAR_MHI_const_std  =  CAR_const_MHI /sqrt( CAR_var_const_MHI ),
         CAR_BC_const_std   = CAR_const_BC  /sqrt(CAR_var_const_BC  ),
       CAR_ASRC_const_std =   CAR_const_ASRC/sqrt(  CAR_var_const_ASRC),
         CAR_LC_const_std   = CAR_const_LC  /sqrt(CAR_var_const_LC  ),
        CAR_MHI_market_std  =  CAR_market_MHI /sqrt( CAR_var_FF_MHI ),
         CAR_BC_market_std   = CAR_market_BC  /sqrt( CAR_var_FF_BC  ),
       CAR_ASRC_market_std =   CAR_market_ASRC/sqrt( CAR_var_FF_ASRC),
         CAR_LC_market_std   = CAR_market_LC  /sqrt( CAR_var_FF_LC  )) %>% 
  select(Date, starts_with("CAR")) %>% 
  pivot_longer(cols = -Date) %>% 
  mutate(`Normal ret. model` = case_when(str_detect(name, "const") ~ "Mean return",
                                         str_detect(name, "FF") ~ "Fama-French",
                                         str_detect(name, "market") ~ "Market",
                                         TRUE ~ NA)) %>% 
  mutate(`Firm` =case_when(str_detect(name, "MHI") ~ "MHI",
                           str_detect(name, "BC") ~ "Beryllium Corp",
                           str_detect(name, "ASRC") ~ "ASRC",
                           str_detect(name, "LC") ~ "Lithium Corp. of America",
                           TRUE ~ NA))

ggplot(filter(alch_3mod_weekly_CAR, str_detect(name, "std")), aes(x = as.Date(Date), y = value))+
  geom_line(aes(y = value)) + geom_point(aes(y = value))+
    theme_carleton() +   scale_color_paletteer_d("ggthemes::calc")+
  geom_hline(yintercept = 0, alpha = .5)+
  geom_hline(aes(yintercept = 1.96),  linetype = 2)+
  geom_hline(aes(yintercept =- 1.96),  linetype = 2)+
  ylab('Standardized cumulative excess return\nExtended window')+
  labs(color = element_blank())+
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")+
  xlab("Date")+
  facet_grid(rows = vars(Firm), cols = vars(`Normal ret. model`))

if(savefile == TRUE){
ggsave("figures/CAR_3mod_prepost_weekly.png", device = "png", dpi = "retina", scale = 2, bg = "white",
       width=320*.9, height =180*.9, units = "mm")
}


alch_3mod_daily_CAR<-alch_3mod_daily_AR %>% 
  filter(Date >= T1 & Date <=T2) %>% 
 # filter(!is.na(market_lith)) %>% 
  # mutate(tauw = row_number() - row_test) %>% 
  # filter(tauw >= tauwstart & tauw <= tauwend) %>% 
  mutate(CAR_FF_BC   = cumsum(AR_bc_FF_ret),
         CAR_FF_ASRC = cumsum(AR_ASRC_FF_ret),
         CAR_FF_LC   = cumsum(AR_lith_FF_ret),
         CAR_market_BC   = cumsum( AR_bc_market_ret),
         CAR_market_ASRC =cumsum(AR_ASRC_market_ret),
         CAR_market_LC   =cumsum(AR_lith_market_ret),
         CAR_const_BC   = cumsum(  AR_bc_const_ret),
         CAR_const_ASRC = cumsum(AR_ASRC_const_ret),
         CAR_const_LC   = cumsum(AR_lith_const_ret),) %>% 
  mutate(CAR_var_FF_BC  = row_number()*rse_FF_daily_bc^2,
         CAR_var_FF_ASRC= row_number()*rse_FF_daily_ASRC^2,
         CAR_var_FF_LC  = row_number()*rse_FF_daily_lith^2,
         CAR_var_market_BC  = row_number()*rse_market_daily_bc^2,
         CAR_var_market_ASRC= row_number()*rse_market_daily_ASRC^2,
         CAR_var_market_LC  = row_number()*rse_market_daily_lith^2,
         CAR_var_const_BC  = row_number()*rse_const_daily_bc^2,
         CAR_var_const_ASRC= row_number()*rse_const_daily_ASRC^2,
         CAR_var_const_LC  = row_number()*rse_const_daily_lith^2) %>% 
  mutate(CAR_BC_FF_std   = CAR_FF_BC  /sqrt( CAR_var_FF_BC  ),
         CAR_ASRC_FF_std = CAR_FF_ASRC/sqrt( CAR_var_FF_ASRC),
         CAR_LC_FF_std   = CAR_FF_LC  /sqrt( CAR_var_FF_LC  ),
         CAR_BC_const_std   = CAR_const_BC  /sqrt(CAR_var_const_BC  ),
         CAR_ASRC_const_std =   CAR_const_ASRC/sqrt(  CAR_var_const_ASRC),
         CAR_LC_const_std   = CAR_const_LC  /sqrt(CAR_var_const_LC  ),
         CAR_BC_market_std   = CAR_market_BC  /sqrt( CAR_var_FF_BC  ),
         CAR_ASRC_market_std =   CAR_market_ASRC/sqrt( CAR_var_FF_ASRC),
         CAR_LC_market_std   = CAR_market_LC  /sqrt( CAR_var_FF_LC  )) %>% 
  select(Date, starts_with("CAR")) %>% 
  pivot_longer(cols = -Date) %>% 
  mutate(`Normal ret. model` = case_when(str_detect(name, "const") ~ "Mean return",
                                         str_detect(name, "FF") ~ "Fama-French",
                                         str_detect(name, "market") ~ "Market",
                                         TRUE ~ NA)) %>% 
  mutate(`Firm` =case_when(str_detect(name, "MHI") ~ "MHI",
                           str_detect(name, "BC") ~ "Beryllium Corp",
                           str_detect(name, "ASRC") ~ "ASRC",
                           str_detect(name, "LC") ~ "Lithium Corp. of America",
                           TRUE ~ NA))

ggplot(filter(alch_3mod_daily_CAR, str_detect(name, "std")), aes(x = as.Date(Date), y = value))+
  geom_line(aes(y = value)) + geom_point(aes(y = value))+
  theme_carleton() +   scale_color_paletteer_d("ggthemes::calc")+
  geom_hline(yintercept = 0, alpha = .5)+
  geom_hline(aes(yintercept = 1.96),  linetype = 2)+
  geom_hline(aes(yintercept =- 1.96),  linetype = 2)+
  ylab('Standardized cumulative excess return\nExtended window')+
  labs(color = element_blank())+
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")+
  xlab("Date")+
  facet_grid(rows = vars(Firm), cols = vars(`Normal ret. model`))

if(savefile == TRUE){
  ggsave("figures/CAR_3mod_prepost_daily.png", device = "png", dpi = "retina", scale = 2, bg = "white",
         width=320*.9, height =180*.9, units = "mm")
}


only_post_daily <- list("Mean Return"=lm(ret_lith ~ 1, data = FF_daily_dset,  subset = (Date>T2 & Date <=T3)),
                  "Market" =lm(ret_lith ~ ret_DJIA, data = FF_daily_dset,  subset = (Date>T2 & Date <=T3)),
                  "Fama-French"=lm(ret_lith ~ `Mkt-RF` + SMB + HML, data = FF_daily_dset, subset =  (Date>T2 & Date <=T3)))

only_post_weekly <- list("Mean Return"=lm(ret_lith ~ 1, data = FF_weekly_dset,  subset = (Date>T2 & Date <=T3)),
                        "Market" =lm(ret_lith ~ ret_DJIA, data = FF_weekly_dset,  subset = (Date>T2 & Date <=T3)),
                        "Fama-French"=lm(ret_lith ~ `Mkt-RF` + SMB + HML, data = FF_weekly_dset, subset =  (Date>T2 & Date <=T3)))

modelsummary(c(only_post_weekly, only_post_daily), coef_map = cm2, stars = starlist, fmt = 2, gof_map = c("nobs", "r.squared","adj.r.squared", "F","rmse","vcov.type"), escape = FALSE, vcov ="NeweyWest" , output = paste0(table_path, "/3models_post_both.docx"))


