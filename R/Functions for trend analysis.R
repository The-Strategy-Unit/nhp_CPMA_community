
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
    ungroup()|>
    as.data.frame()
  
  return(numbers_over_time)
  
}

# Calculating numbers and percentages over time
data_number_percentage_over_time_icb<-function(data1, data2, pop_data, mitigator, denom1, denom2){
  
  denominator_data<-data1|>
    filter(fyear!=201415)|>
    filter(!is.na(icb))|>
    summarise(total_episodes=sum({{denom1}}),
              total_beddays=sum({{denom2}}),
              .by=c(fyear, icb))
  
  data2|>
    filter(cohorts==mitigator)|>
    filter(fyear!=201415)|>
    filter(!is.na(icb))|>
    summarise(episodes=sum(episodes),
              beddays=sum(beddays),
              .by=c(icb, year, fyear))|>
    left_join(denominator_data, by=c("fyear", "icb"))|>
    mutate(percentage_episodes=round((episodes/total_episodes)*100,1),
           percentage_beddays=round((beddays/total_beddays)*100,1))|>
    left_join(pop_data[,c("icb24cdh", "icb_2024_name")], by=c("icb"="icb24cdh"), relationship="many-to-many")|>
    distinct(year,episodes, beddays,percentage_episodes, percentage_beddays, total_episodes, total_beddays, icb_2024_name)|>
    filter(!is.nan(percentage_episodes)|!is.nan(percentage_beddays))|>
    as.data.frame()
  
}



plot_of_number_over_time<-function(data,  activity_type){
  
  data2<-data|>
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



plot_of_percentage_over_time<-function(data, activity_type, denominator){
  
data1<-data|>
  group_by(year)|>
  summarise(number=sum({{activity_type}}),
            total_number=sum({{denominator}}))|>
  mutate(percentage=round((number/total_number)*100,1))

data1|>
    ggplot()+
    geom_line(aes(y=percentage, x=year, group=1), linewidth=1.4)+
    su_theme()+
    theme(axis.text=element_text(size=10),
          axis.title.y=element_text(size=12))+
    labs(y="Percentage",
         x=NULL,
         title=NULL)+ 
    scale_y_continuous(expand=c(0,0), limits=c(0,max(data1$percentage)*1.05), labels = label_comma())
  
  
  
}


plot_of_standardised_rates_over_time<-function(data){
  
  data1<-data 
  
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
  



plotting_icb_over_time<-function(data, axis_title){
  
  data|>
    group_by(icb_2024_name)|>
    highlight_key(~icb_2024_name) |>
    plot_ly( x = ~year, y = ~activity, type = 'scatter',  mode = 'lines', text=~icb_2024_name,  line = list(color = "#686f73"), width=695, height=332)|>
    highlight(~icb_2024_name, on = "plotly_click", off="plotly_doubleclick", dynamic=FALSE)|>
    layout(
      xaxis = list(title="", showticklabels = TRUE, showline = TRUE, showgrid = F , linewidth=2),
      yaxis = list(title = axis_title, showline = TRUE, showgrid = F , linewidth=2 ,zeroline = FALSE, tickformat = "digits", anchor="free", shift=100)
    )
  
  
}

#Plotting change over time

plotting_percentage_change_over_time_by_icb<-function(data, values, eng_average, min_adjustment, max_adjustment ){
 

  data1<-data|>
    filter(year=="2019/20"| year=="2023/24")|>
    pivot_wider(names_from = c(year), values_from = {{values}})|>
    summarise(`2019/20`=max(`2019/20`, na.rm=TRUE),
              `2023/24`=max(`2023/24`, na.rm=TRUE),
              .by=c(icb_2024_name))|>
    mutate(change=round(((`2023/24`-`2019/20`)/`2019/20`)*100,1))|>
    filter(!is.nan(change))|>
    filter(change!="-Inf")|>
    arrange(change)|>
    mutate(icb_2024_name=factor(icb_2024_name))
  
  
  data1|>
    ggplot(aes(x=change, y=fct_reorder(icb_2024_name, change)))+
    geom_bar(stat="identity", fill=ifelse(data1$change<0,  "#70C19A", "#F19388")) +
    geom_vline(xintercept=eng_average, linetype="dashed", 
               color = "#686f73"   , size=0.4 )+
    geom_text(aes(x=eng_average, label="England\naverage", y=nrow(data1)+3), colour="#686f73", vjust=1, hjust =-0.09, size=2.7)+
    su_theme()+
    theme(axis.text.y=element_text(size=8),
          axis.title=element_text(size=10))+
    labs(y=NULL,
         x="Percentage change",
         title=NULL)+
    geom_text(aes(label=paste0(change, '%')), hjust=ifelse(data1$change<0, 1.1, -0.1), size=2.4)+
    scale_x_continuous(expand=c(0.01,0.01), limits=c((min(data1$change)*min_adjustment),(max(data1$change)*max_adjustment)))

  
}

# England average percentage of emergency admissions

calculating_england_average_percentage<-function(data, activity_type, denominator){
  
data|>
    filter(year=="2019/20"| year=="2023/24")|>
    group_by(year)|>
        summarise(number=sum({{activity_type}}),
              total_number=sum({{denominator}}))|>
    mutate(percentage=round((number/total_number)*100,1))|>
    pivot_wider(names_from = c(year), values_from = percentage)|>
    summarise(`2019/20`=max(`2019/20`, na.rm=TRUE),
              `2023/24`=max(`2023/24`, na.rm=TRUE))|>
    mutate(change=round(((`2023/24`-`2019/20`)/`2019/20`)*100,1))

}


# England average percentage of standardised rates

calculating_england_average_standardised<-function(data){
  
 data|>
    filter(year=="2019/20"| year=="2023/24")|>
    pivot_wider(names_from = c(year), values_from = value)|>
    summarise(`2019/20`=max(`2019/20`, na.rm=TRUE),
              `2023/24`=max(`2023/24`, na.rm=TRUE))|>
    mutate(change=round(((`2023/24`-`2019/20`)/`2019/20`)*100,1))
  
}

