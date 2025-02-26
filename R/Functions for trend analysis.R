
Formatting_data_for_trends_analysis_denominator<-function(table){

denominator<-dplyr::tbl(
  sc,
  dbplyr::in_catalog("strategyunit","default", table)
)|>    
  filter(fyear>201314)|>
  group_by(age_range, 
           sex,
           fyear, 
           icb)|>
  summarise(total_episodes_emergency=sum(total_episodes_emergency),
            total_beddays_emergency=sum(total_beddays_emergency),
            total_episodes_elective=sum(total_episodes_elective),
            total_beddays_elective=sum(total_beddays_elective))|>
  mutate(year=paste0(stringr::str_sub(fyear, 1, 4), "/", stringr::str_sub(fyear, 5, 6)))|>
  collect()|>
  as.data.frame()

}



Formatting_data_for_trends_analysis<-function(table){
  
  numbers_over_time <- dplyr::tbl(
    sc,
    dbplyr::in_catalog("strategyunit","default", "sl_af_describing_mitigators_fyear")
  )|> collect()
  

  numbers_over_time<-as.data.frame(numbers_over_time)|>
    gather(key="cohorts", value="value", -fyear, -age_range, -sex, -icb, -episodes, -beddays)|>
    filter(value==1)|>
    filter(fyear>201314)|>
    group_by(age_range, 
             sex,
             fyear,
             icb,
             cohorts)|>
    summarise(episodes=sum(episodes),
              beddays=sum(beddays))|>
    mutate(year=paste0(stringr::str_sub(fyear, 1, 4), "/", stringr::str_sub(fyear, 5, 6)))|>
    ungroup()
  
  return(numbers_over_time)
  
}


plot_of_number_over_time<-function(data, mitigator, activity_type){
  
  data|>
    filter(cohorts==mitigator)|>
    group_by(year)|>
    summarise(activity=sum({{activity_type}}))|>
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



plot_of_percentage_over_time<-function(data1, data2, mitigator, activity_type, denominator_type){
  
  
 denominator_data<-data1|>
    group_by(fyear)|>
    summarise(total_activity=sum({{denominator_type}}))
  
data2|>
    filter(cohorts==mitigator)|>
    group_by(fyear, year)|>
    summarise(activity=sum({{activity_type}}))|>
    left_join( denominator_data, by=c("fyear"))|>
    mutate(percentage=round((activity/total_activity)*100,1))|>
    ggplot()+
    geom_line(aes(y=percentage, x=year, group=1), linewidth=1.4)+
    su_theme()+
    theme(axis.text=element_text(size=12),
          axis.title.y=element_text(size=14))+
    labs(y="Percentage",
         x=NULL,
         title=NULL)+ 
    scale_y_continuous(expand=c(0,0), limits=c(0,NA), labels = label_comma())
  
  
  
}


plot_of_number_over_time_icb<-function(data, mitigator, activity_type){
  
  data|>
    filter(cohorts==mitigator)|>
    filter(fyear!=201415)|>
    group_by(year, icb)|>
    summarise(activity=sum({{activity_type}}))|>
  plot_ly(data, x = ~year, y = ~activity, type = 'scatter', mode = 'lines',
            line = list(color = 'rgb(205, 12, 24)', width = 4)) 
    
    ggplot()+
    geom_line(aes(y=activity, x=year, group=icb), linewidth=1.4)+
    su_theme()+
    theme(axis.text=element_text(size=12),
          axis.title.y=element_text(size=14))+
    labs(y="Number",
         x=NULL,
         title=NULL)+ 
    scale_y_continuous(expand=c(0,0), limits=c(0,NA), labels = label_comma())
  
  
}



plot_of_percentage_over_time_icb<-function(data1, data2, mitigator, activity_type, denominator_type){
  
  
  denominator_data<-data1|>
    group_by(fyear, icb)|>
    summarise(total_activity=sum({{denominator_type}}))
  
  data2|>
    filter(cohorts==mitigator)|>
    filter(fyear!=201415)|>
    group_by(fyear, year, icb)|>
    summarise(activity=sum({{activity_type}}))|>
    left_join( denominator_data, by=c("fyear", "icb"))|>
    mutate(percentage=round((activity/total_activity)*100,1))|>
    ggplot()+
    geom_line(aes(y=percentage, x=year, group=icb), linewidth=1.4)+
    su_theme()+
    theme(axis.text=element_text(size=12),
          axis.title.y=element_text(size=14))+
    labs(y="Percentage",
         x=NULL,
         title=NULL)+ 
    scale_y_continuous(expand=c(0,0), limits=c(0,NA), labels = label_comma())
  
  
  
}
