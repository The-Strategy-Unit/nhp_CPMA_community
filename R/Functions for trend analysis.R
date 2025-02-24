
Formatting_data_for_trends_analysis_denominator<-function(table){

denominator<-dplyr::tbl(
  sc,
  dbplyr::in_catalog("strategyunit","default", table)
)|>    
  filter(fyear>201314)|>
  group_by(age_range, 
           sex,
           fyear)|>
  summarise(total_episodes_emergency=sum(total_episodes_emergency),
            total_beddays_emergency=sum(total_beddays_emergency),
            total_episodes_elective=sum(total_episodes_elective),
            total_beddays_elective=sum(total_beddays_elective))|>
  collect()|>
  as.data.frame()

}



Formatting_data_for_trends_analysis<-function(table){
  
  numbers_over_time <- dplyr::tbl(
    sc,
    dbplyr::in_catalog("strategyunit","default", "sl_af_describing_mitigators_fyear")
  )|> collect()
  

  numbers_over_time<-as.data.frame(numbers_over_time)|>
    gather(key="cohorts", value="value", -fyear, -age_range, -sex, -episodes, -beddays)|>
    filter(value==1)|>
    filter(fyear>201314)|>
    group_by(age_range, 
             sex,
             fyear, 
             cohorts)|>
    summarise(episodes=sum(episodes),
              beddays=sum(beddays))|>
    ungroup()
  
  return(numbers_over_time)
  
}


plot_of_number_over_time<-function(data, mitigator, activity_type){
  
  data|>
    filter(cohorts==mitigator)|>
    group_by(fyear)|>
    summarise(activity=sum({{activity_type}}))|>
    mutate(year=paste0(stringr::str_sub(fyear, 1, 4), "/", stringr::str_sub(fyear, 5, 6)))|>
    ggplot()+
    geom_line(aes(y=activity, x=year, group=1), linewidth=1.4)+
    su_theme()+
    theme(axis.text=element_text(size=12),
          axis.title.y=element_text(size=14))+
    labs(y="Number",
         x=NULL,
         title=NULL)+ 
    scale_y_continuous(expand=c(0,0), limits=c(0,NA), labels = label_comma())
  
  
}
