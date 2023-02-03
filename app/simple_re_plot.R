library(data.table)

ww_loads = fread(paste0("app/data/ww_loads", output_suffix, ".csv"))
ww_re = fread(paste0("app/data/ww_re_estimates", output_suffix, ".csv"))

ww_re %>% 
  ggplot() + 
  geom_ribbon(aes(x=date, ymin=CI_down_Re_estimate, ymax=CI_up_Re_estimate), alpha=0.4, fill='red')+
  geom_line(aes(x=date, y=Re_estimate), color='red') + 
  geom_hline(yintercept=1, linetype=2)+
  facet_grid(influenza_type~observation_type, scales = 'free_y')
  
ww_re$CI_down_Re_estimate

