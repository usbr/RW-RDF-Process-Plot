library(hydroGOF)
if(res_vec[k] == "Fontenelle"){
  #filter for only target months 
  df1 = filter(df_res1, format.Date(fcst_date, '%m') == '03' | format.Date(fcst_date, '%m') == '06' |format.Date(fcst_date, '%m') == '07')
  df2 = filter(df_res2, format.Date(fcst_date, '%m') == '03' | format.Date(fcst_date, '%m') == '06' |format.Date(fcst_date, '%m') == '07')
  dp1 = filter(df_perfect1, format.Date(fcst_date, '%m') == '03' | format.Date(fcst_date, '%m') == '06' |format.Date(fcst_date, '%m') == '07')
  dp2 = filter(df_perfect2, format.Date(fcst_date, '%m') == '03' | format.Date(fcst_date, '%m') == '06' |format.Date(fcst_date, '%m') == '07')
  print(paste(res_vec[k],"NSE for Target Months:",run_dir[1],"FcstvObs",NSE(df1$fcst,df1$obs),
              run_dir[2],"FcstvObs",NSE(df2$fcst,df2$obs),
              run_dir[1],"PrftvObs",NSE(dp1$fcst,dp1$obs),
              run_dir[2],"PrftvObs",NSE(dp2$fcst,dp2$obs)
  ))
  
  NSE(df_res2$fcst,df_res2$obs)
  NSE(df_perfect1$fcst,df_perfect1$obs)
  NSE(df_perfect2$fcst,df_perfect2$obs)
}

NSE(df_res1$fcst,df_res1$obs)
NSE(df_res2$fcst,df_res2$obs)
NSE(df_perfect1$fcst,df_perfect1$obs)
NSE(df_perfect2$fcst,df_perfect2$obs)
format(df_perfect1$fcst_dates,%m%) 


df_p1 = cbind.data.frame(Val = unique(df_esp$obs), Model = rep("OBS",times=length(unique(df_esp$obs))))
df_p2 = cbind.data.frame(Val = df_res1$Val, Model = rep(model_vec[1],times=length(df_esp$Val)))
df_p3 = cbind.data.frame(Val = df_esp$Val, Model = rep(model_vec[2],times=length(df_esp$Val)))
df_p = rbind.data.frame(df_p1,df_p2,df_p3) 