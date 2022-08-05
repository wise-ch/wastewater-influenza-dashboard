# Re estimation helper functions

load_re_helpers <- function(path = 'helper_code') {
  daily_re_helpers1 = "https://raw.githubusercontent.com/covid-19-Re/shiny-dailyRe/master/app/otherScripts/2_utils_getInfectionIncidence.R"
  daily_re_helpers2 = "https://raw.githubusercontent.com/covid-19-Re/shiny-dailyRe/master/app/otherScripts/3_utils_doReEstimates.R"
  
  source(daily_re_helpers1)
  source(daily_re_helpers2)
  
  ww_fn <- paste0(path, '/wastewater_functions.R')
  source(ww_fn)
  
}

estimate_Re <- function(new_data, config_df, n_boot = 50, smooth_param = TRUE, max_it = 100) {
  deconv_ww_data <- data.frame()
  Re_ww <- data.frame()
  for(row_i in 1:nrow(config_df)){
    # for wastewater
    new_deconv_data = deconvolveIncidence(new_data %>%
                                            filter(region == config_df[row_i, 'region']) %>%
                                            filter(variant == config_df[row_i, 'variant']),
                                          incidence_var = config_df[row_i, 'incidence_var'],
                                          getCountParams(unlist(config_df[row_i, 'GammaParams'])[1]),
                                          getCountParams(unlist(config_df[row_i, 'GammaParams'])[2]),
                                          smooth_param = smooth_param, n_boot = n_boot)
    
    new_deconv_data <- new_deconv_data %>%
      mutate(incidence_var = config_df[row_i, 'incidence_var'],
             incubationParams = unlist(config_df[row_i, 'GammaParams'])[1],
             onsetToCountParams = unlist(config_df[row_i, 'GammaParams'])[2],
             GammaParams = paste0(incubationParams, '_', onsetToCountParams),
             source = GammaParams)
    
    ##### Get Re 
    # works up to here with n_boot = 0
    new_Re_ww = getReBootstrap(new_deconv_data)
    new_Re_ww <- new_Re_ww %>%
      mutate(variable = config_df[row_i, 'incidence_var'],
             incubationParams = unlist(config_df[row_i, 'GammaParams'])[1],
             onsetToCountParams = unlist(config_df[row_i, 'GammaParams'])[2],
             GammaParams = paste0(incubationParams, '_', onsetToCountParams),
             region = config_df[row_i, 'region'],
             variant = config_df[row_i, 'variant'])
    
    deconv_ww_data <- bind_rows(deconv_ww_data, new_deconv_data)
    Re_ww = bind_rows(Re_ww, new_Re_ww)
  }
  #if (n_boot == 0) {
  #  Re_ww_needed <- Re_ww %>% select(region, variant, data_type, date,  value) 
  #} else {
      Re_ww_needed <- Re_ww %>% select(region, variant, date, data_type,
                                     median_R_mean, median_R_highHPD, median_R_lowHPD) 
  #}
  
}


plot_Re <- function(data, location, var, data_source = 'source', lab = c('Wastewater (NGS)', 'Cases'), 
                    breaks = c('Wastewater (NGS)', 'Cases')) {
  
  #date_low <- min((data %>% filter(variant == var) %>% filter(source == 'Catchment cases'))$date)
  
  nsource <- length(lab)
  plotData <- data %>% filter(variant %in% var) %>% filter(region == location) #filter(date>=date_low) %>%
  
  # within the plot data, where is the beginning dates for each variant
  starting_dates <- plotData %>% group_by(variant, region) %>% 
        summarise(beg_date = min(date, na.rm = T))
    
  plotData %>% ggplot() +
    geom_line(aes(x = date, 
                  y = median_R_mean, 
                  group = interaction(variant, get(data_source)),
                  colour = get(data_source), 
                  linetype = variant),
              alpha = 0.7, lwd = 0.8) +
      geom_ribbon(aes(x = date, ymin = median_R_lowHPD,
                      ymax = median_R_highHPD, fill = get(data_source), 
                      group = interaction(variant, get(data_source))), 
                  alpha = 0.2, show.legend = F) +
      # geom_ribbon_pattern(aes(x = date, 
      #                         ymin = median_R_lowHPD,
      #                         ymax = median_R_highHPD,
      #                         pattern_angle = variant,
      #                         pattern_colour = get(data_source),
      #                         group = interaction(variant, get(data_source))), 
      #                     pattern = 'stripe', 
      #                     pattern_fill = 'grey', 
      #                     pattern_density = 0.05,
      #                     pattern_spacing = 0.1,
      #                     pattern_alpha = 0.4,
      #                     alpha = 0.001, 
      #                     show.legend = F) +
      # scale_pattern_angle_manual( values = c(30, -30, 30, -30, 30), 
      #                             labels = c("B.1.1.7","B.1.617.2","BA.1","BA.2","BA.5"),
      #                             breaks = c("B.1.1.7","B.1.617.2","BA.1","BA.2","BA.5")) +
      # scale_pattern_colour_manual(values = c(viridis(1), viridis(5)[4]), 
      #                             labels = lab,
      #                             breaks = breaks) +
    geom_hline(yintercept = 1) +
    # geom_vline(xintercept = as.Date('2021-11-20'), colour = 'grey', linetype = 'dashed') +
    # annotate('rect',xmin = as.Date('2021-11-10'), xmax = as.Date('2021-11-30'), ymin = -Inf, ymax = Inf ,
    #          fill = 'grey', alpha = 0.4) +
    geom_vline(xintercept = starting_dates$beg_date, colour = 'grey', linetype = 'dashed') +
    geom_text_repel(data = starting_dates,
                    aes(x = beg_date, y = 3.75, label = variant), colour = 'black')+ 
    coord_cartesian(ylim = c(0, 4), 
                    xlim = range(plotData$date)) +
    labs( x = ref[location], y = bquote("Estimated R"['e']~" (95% CI)"),
          colour = 'Source', fill = 'Source') +
    scale_colour_manual(values = c(viridis(1), viridis(5)[4]),
                        labels = lab,
                        breaks = breaks) +
    scale_fill_manual(values = c(viridis(1), viridis(5)[4]), 
                      labels = lab,
                      breaks = breaks) +
    guides(color = guide_legend(override.aes = list(size=5, shape = 0))) +
    theme_minimal() + 
    theme(strip.text = element_text(size=17),
          axis.text= element_text(size=14),
          axis.title =  element_text(size=16),
          legend.text= element_text(size=14),
          legend.title= element_text(size=17),
          plot.title = element_text(size = 18),
          panel.spacing.y = unit(2, "lines"),
          legend.position = 'bottom') 
    
    
    # would now also like to add vertical breaks where a new variant starts
    
}



plot_raw <- function(data,location, var,y, source, labs, breaks) {
  data %>% filter(region==location & variant == var)  %>% # for now do not need to use this mutate(n1 = n1/10^12) %>%
    ggplot() +
    geom_point(aes(x=date, y = get(y), colour = name_orig, shape = get(source))) +
    scale_x_date(date_breaks = "months", date_labels = "%b") +
    scale_y_continuous(labels = function(label) sprintf('%4.1f', label),
                       name = 'Wastewater values (red)', sec.axis = sec_axis(~. *0.2,name = "Variant abundance (grey)")) +
    scale_colour_manual(values = c(viridis(4)[1], 'darkgrey', 'firebrick', "#f4bc1c"), #'lightseagreen'
                        labels = labs,
                        breaks = breaks,
                        name = 'Variant specific',
                        guide = guide_legend(override.aes = list(size = 3) )) + # to increase size of point in legend
    geom_line(aes(x=date, y= get(y), linetype = get(source))) +
    geom_line(aes(x=date, y= norm_n1),colour = 'red', alpha = 0.3) +
    geom_line(aes(x=date, y= proportion*1000/2, colour = get(source)), alpha = 0.3) +
    labs(x = paste0(as.character(location),', ',as.character(var)) ) + 
    theme_minimal() +
    coord_cartesian(ylim=c(0, 500)) +
    scale_linetype_manual(values = c('solid', 'dotted'),labels = c('Smoothed', 'Unsmoothed'),
                          breaks = c('NGS smoothed', 'NGS unsmoothed'), name = 'Data type') +
    scale_shape_manual(values = c(16, 17),labels = c('Smoothed', 'Unsmoothed'),
                       breaks = c('NGS smoothed', 'NGS unsmoothed'), name = 'Data type') +
    theme(strip.text = element_text(size=17),
          axis.text= element_text(size=14),
          axis.title =  element_text(size=15),
          legend.text= element_text(size=14),
          legend.title= element_text(size=17),
          plot.title = element_text(size = 18),
          panel.spacing.y = unit(2, "lines"),
          legend.position = 'bottom')
}


find_variant_dates <- function(p, low_prop = 0.02) {
  # the longest stretch of values above 0.02
  runs <- rle(p>=low_prop)
  myruns = which(runs$values == TRUE & runs$lengths > 5)
  runs.lengths.cumsum = cumsum(runs$lengths)
  max_above_prop <- myruns[which.max(runs$lengths[myruns])]
  
  if (length(max_above_prop)==0) {
    return(c())
  }
  
  if (max_above_prop!=1) {
    start_ind <- runs.lengths.cumsum[max_above_prop-1]+1
    end_ind <- runs.lengths.cumsum[max_above_prop]
  }
  
  
  
  if (max_above_prop==1) {
    start_ind <- 1
    end_ind <- runs.lengths.cumsum[1]
  }
  
  return(c(start_ind,end_ind))
}

