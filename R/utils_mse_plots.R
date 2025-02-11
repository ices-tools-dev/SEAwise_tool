#' mse_plots 

#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

plot_mse_ssb <- function(df) {
  
  data <- df %>% filter(period != '2021', !(stock_name %in% constant_cpue))
  
  ggplot(data, aes(x = period, y = stock,  fill = ratio_ssb_trunc)) +
    geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
    scale_fill_gradient2(
      low = 'darkred',
      mid = "white",
      high = 'darkblue' , 
      midpoint = 0, name = "Change\n in SSB",
      limits = c(-100,100), breaks = seq(-100, 100, 50)
    ) +
    theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
    xlab('Period') + ylab('Stock')  + facet_wrap(fleet_dynamics~HCR, ncol = 4) +
    geom_hline(yintercept = c(5, 21,29,34,36)+0.5, col = 'red', linewidth = 0.8)
}

plot_mse_fRatio <- function(df) {
  
  data <- df %>% filter(period != '2021', !(stock_name %in% constant_cpue))
  
  ggplot(data, aes(x = period, y = stock,  fill = ratio_f_fmsy_trunc)) +
    geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
    scale_fill_gradient2(
      low = 'darkred',# muted("red"),
      mid = "white",
      high = 'darkblue' , #muted("blue"),
      midpoint = 0, name = "F/Fmsy",
      limits = c(-100,100), breaks = seq(-100, 100, 50),
    ) +
    theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
    xlab('Period') + ylab('Stock')  + facet_wrap(fleet_dynamics~HCR, ncol = 4) +
    geom_hline(yintercept = c(5, 21,29,34,36)+0.5, col = 'red', linewidth = 0.8)
  
}

plot_mse_mean_age <- function(df) {
  
  data <- df %>% filter(period != '2021', !(stock_name %in% constant_cpue))
  
  ggplot(data, aes(x = period, y = stock,  fill = ratio_average_age_trunc)) +
    geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
    scale_fill_gradient2(
      low = 'darkred',# muted("red"),
      mid = "white",
      high = 'darkblue' , #muted("blue"),
      midpoint = 0, name = "Change in\n average age",
      limits = c(-100,100), breaks = seq(-100, 100, 50),
      guide = guide_colourbar(nbin = 100, draw.ulim = FALSE, draw.llim = FALSE)
    ) +
    theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
    xlab('Period') + ylab('Stock')  + facet_wrap(fleet_dynamics~HCR, ncol = 4) +
    geom_hline(yintercept = c(5, 21,29,34,36)+0.5, col = 'red', linewidth = 0.8) 
  
}

plot_mse_ssb_vs_blim <- function(df) {
  
  data <- df %>% filter(period != '2021', !(stock_name %in% constant_cpue))
  
  ggplot(data, aes(x = period, y = stock,  fill = pBlim)) +
    geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
    scale_fill_gradient2(
      low = 'darkblue',# muted("red"),
      mid = 'white',
      high = 'darkred' , #muted("blue"),
      midpoint = 5, name = "p(SSB < Blim)",
      limits = c(0,100), breaks = seq(0, 100, 25),
    ) +
    theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
    xlab('Period') + ylab('Stock')  + facet_wrap(fleet_dynamics~HCR, ncol = 4) +
    geom_hline(yintercept = c(5, 21,29,34,36)+0.5, col = 'red', linewidth = 0.8)
}

plot_mse_fleet_landings <- function(df) {
  
  data <- df %>% filter(period != '2021', !(stock_name %in% constant_cpue))
  
  ggplot(data,  aes(x = period, y = fleet,  fill = ratio_landings_trunc)) +
    geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
    scale_fill_gradient2(
      low = 'white',# muted("red"),
      mid = "darkblue",
      high = 'darkgreen' , #muted("blue"),
      midpoint = 50, name = "Change\n in Landings",
      limits = c(0,100), breaks = seq(0, 100, 25)
    ) +
    theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
    xlab('Period') + ylab('Fleet')  + facet_wrap(fleet_dynamics~HCR, ncol = 4) +
    geom_hline(yintercept = c(3, 19,35,45,47)+0.5, col = 'red', linewidth = 0.8)
  
}

plot_mse_stock_landings <- function(df) {
  
  data <- df %>% filter(period != '2021', !(stock_name %in% constant_cpue))
  
  ggplot(data, aes(x = period, y = stock,  fill = ratio_landings_trunc)) +
    geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
    scale_fill_gradient2(
      low = 'white',# muted("red"),
      mid = "darkblue",
      high = 'darkgreen' , #muted("blue"),
      midpoint = 50, name = "Change\n in Landings",
      limits = c(0,100), breaks = seq(0, 100, 25)) +
    theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
    xlab('Period') + ylab('Stock')  + facet_wrap(fleet_dynamics~HCR, ncol = 4) +
    geom_hline(yintercept = c(5, 29,37,42,47)+0.5, col = 'red', linewidth = 0.8)
}


#' Generic function to plot changes in mse indicators
#'
#' @param df 
#' @param input 
#' @param list_params 
#' @param ecoregion 
#'
#' @return
#' @export
#' @importFrom magrittr  %>% 
#' @importFrom dplyr filter
#' @importFrom ggthemes theme_hc
#' @import ggplot2
#'
plot_mse_generic <- function(df, input, list_params, ecoregion) {
  #browser()
  plot_params <- list_params[[input]]
  
  data <- df %>% filter(period != '2021')#, !(stock_name %in% constant_cpue))
  
  if(ecoregion != "all_ecoregions") {
    data <- filter(data, region == ecoregion)
  }
  
  plot <- ggplot(data, aes(x = period, y = !!sym(plot_params$y),  fill = !!sym(plot_params$var))) +
    geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
    scale_fill_gradient2(
      low = plot_params$col[1],
      mid = plot_params$col[2],
      high = plot_params$col[3],
      midpoint = plot_params$midpoint, 
      name = plot_params$title,
      limits = plot_params$lims, 
      breaks = plot_params$breaks) +
    theme_hc() +  
    theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
    xlab('Period') + 
    ylab(plot_params$y_lab) + 
    facet_wrap(fleet_dynamics~HCR) 
  
  if(ecoregion == "all_ecoregions") {
    plot <- plot + geom_hline(yintercept = plot_params$hline + 0.5, col = 'red', linewidth = 0.8)
  }
  plot
}




#' Plot regional level changes in indicators
#'
#' @param df 
#'
#' @return
#' 
#' @export
#' @importFrom magrittr  %>% 
#' @importFrom dplyr filter
#' @importFrom ggthemes theme_hc
#' @import ggplot2
#'

plot_mse_indicators_regional <- function(df, ecoregion) {
  
  if(ecoregion != "all_ecoregions") {
    df <- filter(df, region == ecoregion)
  }
  ggplot(df, aes(x = period, y = substr(indicator,7, nchar(indicator)),  fill = value_trunc)) +
    geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
    scale_fill_gradient2(
      low = 'darkred',# muted("red"),
      mid = "white",
      high = 'darkblue' , #muted("blue"),
      midpoint = 0, name = "Change\n in indicator",
      limits = c(-100,100), breaks = seq(-100, 100, 50)
    ) +
    theme_hc() +  
    theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
    geom_text(aes(label = round(value_trunc), color = abs(value_trunc) > 60)) +
    scale_color_manual(guide = 'none', values = c("black", "white")) +
    xlab('Period') + ylab('Indicator')  + 
    facet_wrap(vars(fleet_dynamics,HCR))
}



