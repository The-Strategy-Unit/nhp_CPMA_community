
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
  
  data2<-data|>
    filter(cohorts==mitigator)|>
    group_by(year)|>
    summarise(activity=sum({{activity_type}}))
  
  data2|>
    ggplot()+
    geom_line(aes(y=activity, x=year, group=1), linewidth=1.4)+
    su_theme()+
    theme(axis.text=element_text(size=10),
          axis.title.y=element_text(size=12))+
    labs(y="Number",
         x=NULL,
         title=NULL)+ 
    scale_y_continuous(expand=c(0,0), limits=c(0,max(data2$activity)*1.05), labels = label_comma())
  
  
}



plot_of_percentage_over_time<-function(data1, data2, mitigator, activity_type, denominator_type){
  
  
 denominator_data<-data1|>
   filter(age_range!="NA")|>
    group_by(fyear)|>
    summarise(total_activity=sum({{denominator_type}}))
  
data3<-data2|>
    filter(cohorts==mitigator)|>
    group_by(fyear, year)|>
    summarise(activity=sum({{activity_type}}))|>
    left_join( denominator_data, by=c("fyear"))|>
    mutate(percentage=round((activity/total_activity)*100,1))

data3|>
    ggplot()+
    geom_line(aes(y=percentage, x=year, group=1), linewidth=1.4)+
    su_theme()+
    theme(axis.text=element_text(size=10),
          axis.title.y=element_text(size=12))+
    labs(y="Percentage",
         x=NULL,
         title=NULL)+ 
    scale_y_continuous(expand=c(0,0), limits=c(0,max(data3$percentage)*1.05), labels = label_comma())
  
  
  
}


plot_of_standardised_rates_over_time<-function(data, mitigator){
  
  data1<-data |>
    filter(cohorts==mitigator)
    
  data1|>
    ggplot()+
    geom_line(aes(y=value, x=year, group=1), linewidth=1.4)+
    su_theme()+
    theme(axis.text=element_text(size=10),
          axis.title.y=element_text(size=12))+
    labs(y="Standardised rate per 100,000",
         x=NULL,
         title=NULL)+ 
    scale_y_continuous(expand=c(0,0), limits=c(0,max(data1$value)*1.05), labels = label_comma())
  
  
  }
  

plot_of_number_over_time_icb<-function(data, mitigator, activity_type, pop_data){
  
  data|>
    filter(cohorts==mitigator)|>
    filter(fyear!=201415)|>
    group_by( icb, year)|>
    summarise(activity=sum(activity_type))|>
    left_join(pop_data[,c("icb24cdh", "icb_2024_name")], by=c("icb"="icb24cdh"))|>
    group_by(icb_2024_name)|>
    highlight_key(~icb_2024_name) |>
    plot_ly( x = ~year, y = ~activity, type = 'scatter',  mode = 'lines', text=~icb_2024_name,  line = list(color = "#686f73",  width = 1.5))|>
    highlight(~icb_2024_name, on = "plotly_click", off="plotly_doubleclick", dynamic=TRUE)|>
    layout(
      xaxis = list(title=list(text='Year', range=c(-10, 10), font = list(size = 20), standoff = 25), showticklabels = TRUE, showline = TRUE, showgrid = F , linewidth=2),
      yaxis = list(title = 'Number', showline = TRUE, showgrid = F , linewidth=2 ,zeroline = FALSE, tickformat = "digits")
    )
  
  
}



plot_of_percentage_over_time_icb<-function(data1, data2, pop_data, mitigator, activity_type, denominator_type){
  
  
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
    #left_join(pop_data[,c("icb24cdh", "icb_2024_name")], by=c("icb"="icb24cdh"))|>
    ggplot()+
    geom_line(aes(y=percentage, x=year, group=icb), linewidth=1.4)+
    su_theme()+
    theme(axis.text=element_text(size=10),
          axis.title.y=element_text(size=12))+
    labs(y="Percentage",
         x=NULL,
         title=NULL)+ 
    scale_y_continuous(expand=c(0,0), limits=c(0,NA), labels = label_comma())
  
  
  
}
