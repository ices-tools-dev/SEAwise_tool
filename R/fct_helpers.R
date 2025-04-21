#' helpers
#'
#' @description ´display_text´ subsets a list of dataframes and extracts the relevant section of text
#'
#' @param list_object a list of dataframes each containing 'ref' and 'text' columns
#' @param tab a character vector - the name of a list entry
#' @param section a character vector indicating which row to extract
#'
#' @return A character string
#'
#' @noRd

select_text <- function(list_object, tab, section){
  
  list_object[[tab]] %>% dplyr::filter(section == !!section) %>% dplyr::pull(text)
}

scenario_colours <- c(
  "Scenario A" = "#1b9e77",
  "Scenario B" = "#d95f02",
  "Scenario C" = "#7570b3"
)

seawise_var_labels <- function(){
  var_labels <- c(
    'avg.KW' = "Average kW",
    GVA   = "Gross Value Add",
    land_val = "Landings value",
    'landings.value' = "Landings value",
    landings = "Landings",
    land = "Landings",
    vessels = "Vessels",
    "Number.of.vessels" = "Vessels",
    'Employment(FTE)' = "Full Time Employment",
    'jobs' = "Full Time Employment",
    CO2_emission = "CO2 Emissions",
    'CO2 emissions' = "CO2 Emissions (kg)",
    BE = "Belgium",
    DK = "Denmark",
    DE = "Germany",
    EN = "England",
    ES = "Spain",
    FRA = "France",
    France = "France",
    FR = "France",
    GE = "Germany",
    IE = "Ireland",
    NL = "Netherlands",
    SC = "Scotland",
    Spain = "Spain",
    SW = "Sweden",
    UKE = "England",
    "Case-study" = "Case-study",
    "Case-study-mod" = "Case-study-mod",
    "Case-study_gear" = "Case-study_gear",
    "FMSY-Min" = "FMSY-Min",
    "FMSY-Min_gearmo" = "FMSY-Min_gearmo",
    "FMSY-Min_unifor" = "FMSY-Min_unifor",
    "FMSY-Min_weight" = "FMSY-Min_weight",
    "PGY-Min" = "PGY-Min",
    "PGY-Min_uniform" = "PGY-Min_uniform",
    "PGY-Min_weighte" = "PGY-Min_weighte",
    "Status quo" = "Status quo",
    "Status-quo_unif" = "Status-quo_unif",
    "Status-quo_weig" = "Status-quo_weig",
    "FLBEIA" = "FLBEIA"
  )
  return(var_labels)
}

plot_f <- function(data, refs, region) {
  
  if(region %in% c("greater_north_sea", "celtic_seas")){

    ggplot(data, aes(x=year,y=q50,ymin=q025, ymax=q975, colour = scenario, fill = scenario)) +
      geom_line(size = 1) +
      #scale_colour_manual(values = scenario_colours)
      labs(x='Years', y='Fishing pressure F') +
      geom_hline(yintercept=refs$f,linetype="dashed")+
      annotate("text",x=min(data$year+1),y=1.1*refs$f,label=refs$fref)+
      geom_ribbon(alpha=0.3, show.legend = F)+#,col="#00B292",fill="#00B292") +
      ylim(0, 1.1*max(data$q975))
  } else if (region == "bay_of_biscay"){
    plt <- ggplot(data, aes(x=year,y=q50,ymin=q05, ymax=q95, colour = scenario, fill = scenario)) +
      geom_line(size = 1) +
      labs(x='Years', y='Fishing pressure F') +
      geom_ribbon(alpha=0.3, show.legend = F) +
      ylim(0, 1.1*max(data$q95))
    
    if(!is.na(refs$f)) {
      plt <-   plt +
        geom_hline(yintercept=refs$f,linetype="dashed") +
      annotate("text",x=min(data$year+1),y=1.1*refs$f,label=refs$fref)
    }
    plt
    
  } else if (region %in% c("central_mediterranean", "eastern_mediterranean")) {
    
    ggplot(data, aes(x=Year,y=Median,ymin=CI05,ymax=CI95, colour = scenario, fill = scenario)) +
      geom_line(size = 1) +
      geom_ribbon(alpha=0.3) + 
      labs(x='Years', y='Fishing pressure F') + 
      geom_hline(yintercept=refs$f,linetype="dashed")+
      annotate("text",x=min(data$Year+1),y=1.1*refs$f,label="proxy Fmsy (f0.1)")+
      ylim(0, 1.1*max(data$CI95))
  }
  
}

plot_ssb <- function(data, refs, region) {
  if(region %in% c("greater_north_sea", "celtic_seas")){
    ggplot(data, aes(x=year,y=q50/1000,ymin=q025/1000, ymax=q975/1000, colour = scenario, fill = scenario)) +
      geom_line(size = 1) +
      geom_ribbon(alpha=0.3, show.legend = F) +
      labs(x='Years', y='SSB in 1000 t')+
      geom_hline(yintercept=refs$Blim/1000, linetype="dashed") +
      geom_hline(yintercept=refs$b/1000, linetype="dotted") +
      #annotate("text",x=min(data$year+1),y=52,label="ANK - BoB FLBEIA demersal", fontface = "bold") +
      annotate("text",x=min(data$year+1),y=1.1*refs$Blim/1000,label="Blim") +
      annotate("text",x=min(data$year+1),y=1.1*refs$b/1000,label=refs$bref) #+
    #ylim(0,60)
  } else if (region == "bay_of_biscay"){
    plt <- ggplot(data, aes(x=year,y=q50/1000,ymin=q05/1000, ymax=q95/1000, colour = scenario, fill = scenario)) +
      geom_line(size = 1) +
      geom_ribbon(alpha=0.3) +
      labs(x='Years', y='SSB in 1000 t')
    if(!is.na(refs$Blim) && !is.na(refs$b)){
    plt <-   plt + geom_hline(yintercept=refs$Blim/1000, linetype="dashed") +
        geom_hline(yintercept=refs$b/1000, linetype="dotted") +
        annotate("text",x=min(data$year+1),y=1.1*refs$Blim/1000,label="Blim") +
        annotate("text",x=min(data$year+1),y=1.1*refs$b/1000,label=refs$bref)
    }
    plt
  } else if (region %in% c("central_mediterranean", "eastern_mediterranean")) {
    ggplot(data, aes(x=Year,y=Median/1000,ymin=CI05/1000,ymax=CI95/1000, colour = scenario, fill = scenario)) +
      geom_line(size = 1) +
      geom_ribbon(alpha=0.3) + 
      labs(x='Years', y='SSB in 1000 t')+ 
      geom_hline(yintercept=refs$Blim/1000, linetype="dashed") +
      geom_hline(yintercept=refs$b/1000, linetype="dotted") +
      annotate("text",x=min(data$Year+1),y=1.1*refs$Blim/1000,label="Blim") +
      annotate("text",x=min(data$Year+1),y=1.1*refs$b/1000,label=refs$bref)+
      ylim(0, 1.1*max(max(data$Median), max(data$CI95))/1000)
  }
}

plot_recruitment <- function(data, refs, region) {

  if(region %in% c("greater_north_sea", "celtic_seas")){
    ggplot(data, aes(y=q50/1000000, x=year, colour = scenario, fill = scenario)) +
      geom_line(size = 1) +
      geom_ribbon(alpha=0.3, aes(ymin=q025/1000000, ymax=q975/1000000), show.legend = F) +
      # geom_bar(position="dodge", stat='identity') +
      # geom_errorbar(aes(ymin=q025/1000000, ymax=q975/1000000), width=.2, position=position_dodge(.9))+
      labs(x='Years', y='Recruitment in millions')
    
  } else if (region == "bay_of_biscay"){
    ggplot(data, aes(y=q50/1000000, x=year, colour = scenario, fill = scenario)) +
      geom_line(size = 1) +
      labs(x='Years', y='Recruitment in millions') +
      geom_ribbon(alpha=0.3, aes(ymin=q05/1000000, ymax=q95/1000000), show.legend = F) 
      #geom_bar(position="dodge", stat='identity') +
      #geom_errorbar(aes(ymin=q05/1000000, ymax=q95/1000000), width=.2, position=position_dodge(.9))
  } else if (region %in% c("central_mediterranean", "eastern_mediterranean")) {
  
    ggplot(data, aes(x=Year,y=Median/1000000, colour = scenario,fill = scenario)) +
      geom_bar(position="dodge", stat='identity') +
      labs(x='Years', y='Recruitment in millions') +
      geom_errorbar(aes(ymin=CI05/1000000, ymax=CI95/1000000), width=.2, position=position_dodge(.9))
  }
}

plot_catch <- function(data, refs, region) {

  if(region %in% c("greater_north_sea", "celtic_seas")){
    ggplot(data, aes(x=year,y=q50/1000, colour = scenario, fill = scenario)) +
      geom_line(size = 1) +
      geom_ribbon(alpha=0.3, aes(ymin=q025/1000, ymax=q975/1000), show.legend = F) +
      # geom_bar(position="dodge", stat='identity') +
      labs(x='Years', y='Catches in 1000 t') 
      # geom_errorbar(aes(ymin=q025/1000, ymax=q975/1000), width=.2, position=position_dodge(.9))
    
  } else if (region == "bay_of_biscay"){
    ggplot(data, aes(x=year,y=q50/1000, colour = scenario, fill = scenario)) +
      # geom_bar(position="dodge", stat='identity') +
      geom_line(size = 1) +
      labs(x='Years', y='Catches in 1000 t') +
      geom_ribbon(alpha=0.3, aes(ymin=q05/1000000, ymax=q95/1000000), show.legend = F) 
      # geom_errorbar(aes(ymin=q05/1000, ymax=q95/1000), width=.2, position=position_dodge(.9))
  } else if (region %in% c("central_mediterranean", "eastern_mediterranean")) {
    ggplot(data, aes(x=Year,y=Median/1000, colour = scenario, fill = scenario)) +
      geom_bar(position="dodge", stat='identity') +
      labs(x='Years', y='Catches in 1000 t') + 
      geom_errorbar(aes(ymin=CI05/1000, ymax=CI95/1000), width=.2, position=position_dodge(.9)) +
      ylim(0, 1.1*max(data$Median/1000))
  }
}