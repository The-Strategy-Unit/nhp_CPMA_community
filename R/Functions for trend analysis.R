
Formatting_data_for_trends_analysis_denominator<-function(table, icb_pop){

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
              total_beddays_elective=sum(total_beddays_elective),
              total_episodes_all=sum(total_episodes_elective)+sum(total_episodes_emergency),
              total_beddays_all=sum(total_beddays_elective)+sum(total_beddays_emergency))|>
    add_year_column() |>
    collect()|>
    left_join(icb_pop[,c("icb24cdh", "icb_2024_name")]|>
                distinct(icb24cdh, icb_2024_name), by=c("icb"="icb24cdh"))|>
    as.data.frame()

}


Formatting_data_for_trends_analysis_cohorts <- function(table, icb_pop){
  
  numbers_over_time <- dplyr::tbl(
    sc,
    dbplyr::in_catalog("strategyunit","default", table)
  )|> 
    collect() |>
    as.data.frame(numbers_over_time) 
  
  numbers_over_time <- numbers_over_time |> 
    identify_whether_bedday_or_admissions_or_both(5:33) |>
    mutate(episodes=ifelse(activity_group=="beddays", 0, episodes))|>   #avoid counting admissions for efficiency only activity
    select(-activity_group, -number_of_cohorts)|>
    mutate_mechanism_columns() |> 
    gather(key="cohorts", value="value", -fyear, -age_range, -sex, -icb, -episodes, -beddays)|>
    filter(value==1)|>
    filter(fyear>201314)|>
    left_join(icb_pop[,c("icb24cdh", "icb_2024_name")]|>
                distinct(icb24cdh, icb_2024_name), by=c("icb"="icb24cdh"))|>
    group_by(age_range, 
             sex,
             fyear,
             icb,
             icb_2024_name,
             cohorts)|>
    summarise(episodes=sum(episodes),
              beddays=sum(beddays))|>
    add_year_column() |>
    ungroup()|>
    as.data.frame()
  
  return(numbers_over_time)
  
}

Formatting_data_for_trends_analysis_total_mitigation<-function(table, icb_pop){
  
  numbers_over_time <- dplyr::tbl(
    sc,
    dbplyr::in_catalog("strategyunit","default", table)
  )|> collect()|>
    filter(fyear>201314)|>
    as.data.frame()
  
  
numbers_over_time<- identify_whether_bedday_or_admissions_or_both(numbers_over_time, 5:33)|>
    mutate(episodes=ifelse(activity_group=="beddays", 0, episodes))|>   #avoid counting admissions for efficiency only activity
    select(-activity_group, -number_of_cohorts)|>
    left_join(icb_pop[,c("icb24cdh", "icb_2024_name")]|>
                distinct(icb24cdh, icb_2024_name), by=c("icb"="icb24cdh"))|>
    group_by(age_range, 
             sex,
             fyear,
             icb,
             icb_2024_name)|>
    summarise(episodes=sum(episodes),
              beddays=sum(beddays))|>
    add_year_column() |>
    ungroup()|>
    as.data.frame() 
  
  return(numbers_over_time)
  
}


# Number by LA over time

Formatting_la_data_for_trends <- function(table, sex_group) {
  
  la_numbers_over_time <- dplyr::tbl(
    sc,
    dbplyr::in_catalog("strategyunit","default", table)
  )|> 
    filter(sex==sex_group)|>
    collect()|>
    as.data.frame()
  
  la_numbers_over_time<- identify_whether_bedday_or_admissions_or_both(  la_numbers_over_time, 5:33)|>
    mutate(episodes=ifelse(activity_group=="beddays", 0, episodes))|>   #avoid counting admissions for efficiency only activity
    select(-activity_group, -number_of_cohorts)|>
    mutate_mechanism_columns() |> 
    gather(key="cohorts", value="value", -fyear, -age_range, -sex, -resladst_ons, -episodes, -beddays)|>
    filter(value==1)|>
    add_year_column()
 
  return(la_numbers_over_time)
}

Formatting_la_data_for_trends_total_mitigation<-function(table, sex_group, la_pop){
  
  la_numbers_over_time <- dplyr::tbl(
    sc,
    dbplyr::in_catalog("strategyunit","default", table)
  )|>     filter(sex==sex_group)|>
    collect()|>
    as.data.frame()
  
  
  la_numbers_over_time<- identify_whether_bedday_or_admissions_or_both(la_numbers_over_time, 5:33)|>
    mutate(episodes=ifelse(activity_group=="beddays", 0, episodes))|>   #avoid counting admissions for efficiency only activity
    select(-activity_group, -number_of_cohorts)|>
    left_join(la_pop[,c("ladcode23", "laname23")]|>
                distinct(ladcode23, laname23), by=c("resladst_ons"="ladcode23"))|>
    group_by(age_range, 
             sex,
             fyear,
             resladst_ons)|>
    summarise(episodes=sum(episodes),
              beddays=sum(beddays))|>
    add_year_column() |>
    ungroup()|>
    as.data.frame() 
  
  return(la_numbers_over_time)
  
}

Formatting_providers_data_for_trends <- function(sex_group) {
  data <- dplyr::tbl(
    sc,
    dbplyr::in_catalog(
      "strategyunit",
      "default",
      "sl_af_describing_mitigators_providers"
    )
  ) |>
    dplyr::filter(fyear >= "201819", sex == sex_group) |>
    sparklyr::collect()
  
  numbers_over_time <- data |>
    identify_whether_bedday_or_admissions_or_both(6:34) |>
    mutate(episodes = ifelse(activity_group == "beddays", 0, episodes)) |>   #avoid counting admissions for efficiency only activity
    select(-activity_group, -number_of_cohorts, -icb, -sex) |>
    mutate_mechanism_columns() |>
    gather(
      key = "cohorts",
      value = "value",
      -fyear,
      -age_range,
      -provider,
      -episodes,
      -beddays
    ) |>
    filter(value == 1) |>
    group_by(age_range, fyear, provider, cohorts) |>
    summarise(episodes = sum(episodes),
              beddays = sum(beddays)) |>
    add_year_column() |>
    mutate(sex = sex_group) |>
    ungroup()
  
  return(numbers_over_time)
}

Formatting_providers_data_for_trends_total_mitigation <- function(sex_group) {
  data <- dplyr::tbl(
    sc,
    dbplyr::in_catalog(
      "strategyunit",
      "default",
      "sl_af_describing_mitigators_providers"
    )
  ) |>
    dplyr::filter(fyear >= "201819", sex == sex_group) |>
    sparklyr::collect()
  
  numbers_over_time <- data |>
    identify_whether_bedday_or_admissions_or_both(6:34) |>
    mutate(episodes = ifelse(activity_group == "beddays", 0, episodes)) |>   #avoid counting admissions for efficiency only activity
    select(-activity_group, -number_of_cohorts, -icb, -sex) |>
    group_by(age_range, fyear, provider) |>
    summarise(episodes = sum(episodes),
              beddays = sum(beddays)) |>
    add_year_column() |>
    mutate(sex = sex_group) |>
    ungroup()
  
  return(numbers_over_time)
}

# Calculating numbers and percentages over time
data_number_percentage_over_time_icb<-function(data1, data2, mitigator, treatment_type){
  
  if(treatment_type=="emergency"){
    
    denominator_data<-data1|>
      filter(fyear!=201415)|>
      mutate(total_episodes=total_episodes_emergency,
                total_beddays= total_beddays_emergency)
    
  }
  else{
    denominator_data<-data1|>
      filter(fyear!=201415)|>
      mutate(total_episodes=total_episodes_all,
                total_beddays=total_beddays_all)
    
  }
  
 
  
  data2|>
    filter(cohorts==mitigator)|>
    filter(fyear!=201415)|>
    summarise(episodes=sum(episodes, na.rm=TRUE),
              beddays=sum(beddays, na.rm=TRUE),
              .by=c(icb_2024_name, fyear, year))|>
    left_join(denominator_data|>
                summarise(total_episodes=sum(total_episodes,na.rm=TRUE),
                          total_beddays=sum(total_beddays,na.rm=TRUE),
                          .by=c(icb_2024_name,fyear)
                ), by=c("fyear", "icb_2024_name"))|>
    mutate(percentage_episodes=(episodes/total_episodes)*100,
           percentage_beddays=(beddays/total_beddays)*100)|>
  mutate(percentage_episodes=ifelse(is.nan(percentage_episodes), NA, percentage_episodes))|>
    mutate(percentage_beddays=ifelse(is.nan(percentage_beddays), NA, percentage_beddays))|>
    as.data.frame() |>
    dplyr::mutate(icb_2024_name = simplify_icb_name(icb_2024_name))
  
}



plot_of_number_over_time<-function(data,  activity_type){
  
  data2<-data|>
    summarise(activity=sum({{activity_type}}), .by = year)
  
  max_number <- max(data2$activity)
  
  data2|>
    ggplot()+
    geom_line(aes(y=activity, x=year, group=1), linewidth=1.4)+
    geom_rect(aes(NULL,NULL,xmin="2019/20",xmax="2021/22"),
              ymin=0,ymax=max_number*1.1, fill="#686f73", size=0.5, alpha=0.01)+
    annotate("text", x ="2020/21", y = max_number*1.08, label = "COVID-19 pandemic", size=2.7)+
    su_theme()+
    theme(axis.text=element_text(size=10),
          axis.title.y=element_text(size=12))+
    labs(y="Number",
         x=NULL,
         title=NULL)+ 
    scale_y_continuous(expand=c(0,0), limits=c(0,max_number*1.1), labels = label_comma())
  
  
}



plot_of_percentage_over_time<-function(data, activity_type, denominator){
  
data1<-data|>
  summarise(number=sum({{activity_type}}),
            total_number=sum({{denominator}}),
            .by = year)|>
  mutate(percentage=janitor::round_half_up((number/total_number)*100,2))

max_percentage <- max(data1$percentage)

data1|>
    ggplot()+
    geom_line(aes(y=percentage, x=year, group=1), linewidth=1.4)+
  geom_rect(aes(NULL,NULL,xmin="2019/20",xmax="2021/22"),
            ymin=0,ymax=max(max_percentage)*1.1, fill="#686f73", size=0.5, alpha=0.01)+
  annotate("text", x ="2020/21", y = max(max_percentage)*1.08, label = "COVID-19 pandemic", size=2.7)+
    su_theme()+
    theme(axis.text=element_text(size=10),
          axis.title.y=element_text(size=12))+
    labs(y="Percentage",
         x=NULL,
         title=NULL)+ 
    scale_y_continuous(expand=c(0,0), limits=c(0,max(max_percentage)*1.1), labels = label_comma())
  
  
  
}


plot_of_standardised_rates_over_time<-function(data){
  
  max_value <- max(data$value)
  
  data|>    
    ggplot(aes(y=value, x=year, group=1))+
    geom_ribbon(aes(ymin = lowercl, ymax = uppercl), fill = "#5881c1" )+
    geom_line(linewidth=1.4)+
    geom_rect(aes(NULL,NULL,xmin="2019/20",xmax="2021/22"),
              ymin=0,ymax=max_value*1.1, fill="#686f73", size=0.5, alpha=0.01)+
    annotate("text", x ="2020/21", y = max_value*1.08, label = "COVID-19 pandemic", size=2.7)+
    su_theme()+
    theme(axis.text=element_text(size=10),
          axis.title.y=element_text(size=12))+
    labs(y="Standardised rate per 100,000",
         x=NULL,
         title=NULL,
         caption = "Blue ribbon indicates the 95% confidence intervals")+ 
    scale_y_continuous(expand=c(0,0), limits=c(0,max(data$uppercl)*1.1), labels = label_comma())
  
  
  }
  

plotting_icb_over_time<-function(data, axis_title){

  data1 <- data |> 
    dplyr::mutate(activity=janitor::round_half_up(activity,3),,
                  icb_2024_name = simplify_icb_name(icb_2024_name))
  
  if("lowercl" %in% names(data1)) {
    data2<-data1|>
      mutate(lower_ci=janitor::round_half_up(lowercl,2))|>
      mutate(upper_ci=janitor::round_half_up(uppercl,2))
    
    footnote<-"Blue ribbon indicates the 95% confidence intervals"
  } else {
    data2<-data1|>
      mutate(lower_ci=NA)|>
      mutate(upper_ci=NA)
    
    footnote<-""
  }
  
 fig<- data2|>
    group_by(icb_2024_name)|>
    highlight_key(~icb_2024_name) |>
    plot_ly( x = ~year, y = ~activity, type = 'scatter',  mode = 'lines', text=~icb_2024_name,  line = list(color = "#686f73"), width=660, height=300,
             hovertemplate = paste( "ICB: %{text}<br>",
                                    "Year: %{x}<br>",
                                    "Value: %{y}"))|>
    highlight(~icb_2024_name, on = "plotly_click", off="plotly_doubleclick", dynamic=FALSE)|>
    layout(
      shapes = list(
        list(type = "rect",
             fillcolor = "#686f73", line = list(color = "#686f73"), opacity = 0.1,
             x0 = "2019/20", x1 = "2021/22", xref = "x",
             y0 = 0, y1 =max(data2$activity)*1.1, yref = "y")),
      xaxis = list(title="", showticklabels = TRUE, showline = TRUE, showgrid = F , linewidth=1.6),
      yaxis = list(title = axis_title, rangemode="tozero",showline = TRUE, showgrid = F , linewidth=1.6 ,zeroline = FALSE, tickformat = "digits", anchor="free", shift=100),
      annotations = 
      list(x = "2020/21", y =max(data2$activity)*1.08, 
             text = "COVID-19 pandemic", 
             showarrow = F, 
            xref='x', 
            yref='y'))|>
    add_ribbons(ymin = ~lower_ci,
                ymax = ~upper_ci,
                line=list(color= "#5881c1"),
                fillcolor =  "#5881c1",
                opacity = 0.3, 
                name = '95% ribbon',
                showlegend = FALSE)
 
 fig2<-fig|>
   layout(annotations=list(x = 1, y = -0.16, 
              text = footnote, 
             showarrow = F, 
             xref='paper', 
             yref='paper'))

  fig2
}


# Calculating change over time

calculating_change_over_time<-function(data, values, geography ){
  
  change_over_time<-data|>
    filter(year=="2018/19"| year=="2023/24")|>
    pivot_wider(names_from = c(year), values_from = {{values}})|>
    summarise(`2018/19`=max(`2018/19`, na.rm=TRUE),
              `2023/24`=max(`2023/24`, na.rm=TRUE),
              .by=c({{geography}}))|>
    mutate(change=janitor::round_half_up(((`2023/24`-`2018/19`)/`2018/19`)*100,1))|>
    filter(!is.nan(change))|>
    filter(change!="-Inf")|>
    arrange(change)|>
    mutate(geography_level=factor({{geography}}))
  
  return(change_over_time)
}

#Plotting change over time

plotting_percentage_change_over_time_by_icb<-function(data, values, geography, eng_average ){
 
  data1 <- data |> 
    dplyr::mutate(dplyr::across(dplyr::any_of(c("icb_2024_name")),
                                ~simplify_icb_name(.)))
  
  change_over_time<-calculating_change_over_time(data1, {{values}}, {{geography}})
  
  change_over_time|>
    ggplot(aes(x=change, y=fct_reorder(geography_level, change)))+
    geom_bar(stat="identity", fill=ifelse(change_over_time$change<0,  "#70C19A", "#F19388")) +
    geom_vline(xintercept=eng_average, linetype="dashed", 
               color = "black"   , size=0.4 )+
    geom_text(aes(x=eng_average, label=paste0("England average\n (", eng_average, "%)"), y=nrow(change_over_time)+3), colour="black", vjust=1, hjust =-0.07, size=2.9)+
    su_theme()+
    theme(axis.text.y=element_text(size=8),
          axis.title=element_text(size=10))+
    labs(y=NULL,
         x="Percentage change",
         title=NULL)+
    geom_label(aes(label=paste0(change, '%')), hjust=ifelse(change_over_time$change<0, 1, 0), size=2.3, label.size = 0)+
    scale_x_continuous(expand=c(0.07,0.07) )

  
}


# England average numbers of emergency admissions

calculating_england_average_numbers<-function(data, activity_type){
  
  data|>
    filter(year=="2018/19"| year=="2023/24")|>
    summarise(number=sum({{activity_type}}), .by = year)|>
    pivot_wider(names_from = year, values_from = number)|>
    summarise(`2018/19`=max(`2018/19`, na.rm=TRUE),
              `2023/24`=max(`2023/24`, na.rm=TRUE))|>
    mutate(change=janitor::round_half_up(((`2023/24`-`2018/19`)/`2018/19`)*100,1))
  
}



# England average percentage of emergency admissions

calculating_england_average_percentage<-function(data, activity_type, denominator){
  
data|>
    filter(year=="2018/19"| year=="2023/24")|>
    summarise(number=sum({{activity_type}}),
              total_number=sum({{denominator}}), .by = year)|>
    mutate(percentage=(number/total_number)*100)|>
    pivot_wider(names_from = year, values_from = percentage)|>
    summarise(`2018/19`=max(`2018/19`, na.rm=TRUE),
              `2023/24`=max(`2023/24`, na.rm=TRUE))|>
    mutate(change=janitor::round_half_up(((`2023/24`-`2018/19`)/`2018/19`)*100,1))

}


# England average percentage of standardised rates

calculating_england_average_standardised<-function(data){
  
 data|>
    filter(year=="2018/19"| year=="2023/24")|>
    pivot_wider(names_from = c(year), values_from = value)|>
    summarise(`2018/19`=max(`2018/19`, na.rm=TRUE),
              `2023/24`=max(`2023/24`, na.rm=TRUE))|>
    mutate(change=janitor::round_half_up(((`2023/24`-`2018/19`)/`2018/19`)*100,1))
  
}

# Plot of total starting activity vs percentage change over the last 5 years

plotting_total_activity_vs_percentage_change_ggplot<-function(data, geography){
  
  data1 <- data |> 
    dplyr::mutate(dplyr::across(dplyr::any_of(c("icb_2024_name")),
                                ~simplify_icb_name(.)))
  
  geo_name<-deparse(substitute(geography))
  if(geo_name=="icb_2024_name"){
    area_label_text<-"ICB:"
  } else{
    area_label_text<-"Local Authority:"
  }
  
  plot_data<-  data1|>
    filter(year=="2018/19"| year=="2023/24")|>
    pivot_wider(names_from = c(year), values_from = value)|>
    summarise(`2018/19`=max(`2018/19`, na.rm=TRUE),
              `2023/24`=max(`2023/24`, na.rm=TRUE),
              .by=c({{geography}}))|>
    mutate(change=janitor::round_half_up(((`2023/24`-`2018/19`)/`2018/19`)*100,1))|>
    filter(!is.nan(change))|>
    filter(change!="-Inf")|>
    arrange(change)|>
    mutate(geo_name=factor({{geography}}))
  
  p<- plot_data|>
    ggplot(aes(x=`2018/19`, y=change, label=geo_name,  text = paste( area_label_text, geo_name, "<br>",
                       "2018/19 Rate:", `2018/19`, "<br>",
                       "Percentage change:", change)))+
    annotate("rect", xmin =(min(plot_data$`2018/19`)-(min(plot_data$`2018/19`)*0.2)) , xmax = mean(plot_data$`2018/19`), ymin =min(plot_data$change)*4 , ymax = 0 , fill= "#c9e7d9")+ 
    geom_point(colour="#686f73")+
    labs(x="Standardised rate for 2018/2019",
         y= "% change between 2018/19 and 2023/24")+
    su_theme()+
    theme(axis.title=element_text(size=12),
          legend.position="none")+
    geom_hline(yintercept=0, linetype="dashed", color = "#ec6555")+
    geom_vline(xintercept = mean(plot_data$`2018/19`), linetype="dashed", color = "#ec6555")+
   coord_cartesian(xlim =c(min(plot_data$`2018/19`)-1, max(plot_data$`2018/19`)), ylim = c((min(plot_data$change))*1.2 , (max(plot_data$change))*1.2))

  return(p)
  
}

plotting_total_activity_vs_percentage_change<-function(data){

  p <- plotting_total_activity_vs_percentage_change_ggplot(data, icb_2024_name)
  
  ggplotly(p, tooltip="text", width=660, height=450 )
}


plotting_total_activity_vs_percentage_change_ggplot_LA<-function(data, geography){
  
  data1 <- data |> 
    dplyr::mutate(dplyr::across(dplyr::any_of(c("icb_2024_name")),
                                ~simplify_icb_name(.)))
  
  geo_name<-deparse(substitute(geography))
  if(geo_name=="icb_2024_name"){
    area_label_text<-"ICB:"
  } else{
    area_label_text<-"Local Authority:"
  }
  
  plot_data<-  data1|>
    filter(year=="2018/19"| year=="2023/24")|>
    pivot_wider(names_from = c(year), values_from = value)|>
    summarise(`2018/19`=max(`2018/19`, na.rm=TRUE),
              `2023/24`=max(`2023/24`, na.rm=TRUE),
              .by=c({{geography}}, color_group))|>
    mutate(change=janitor::round_half_up(((`2023/24`-`2018/19`)/`2018/19`)*100,1))|>
    filter(!is.nan(change))|>
    filter(change!="-Inf")|>
    arrange(change)|>
    mutate(geo_name=factor({{geography}}))|>
    mutate(color_group=relevel(factor(color_group, ordered = FALSE ), ref = "1"))
  
  p<- plot_data|>
    ggplot(aes(x=`2018/19`, y=change, label=geo_name,  text = paste( area_label_text, geo_name, "<br>",
                                                                     "2018/19 Rate:", `2018/19`, "<br>",
                                                                     "Percentage change:", change)))+
    annotate("rect", xmin =(min(plot_data$`2018/19`)-(min(plot_data$`2018/19`)*0.2)) , xmax = mean(plot_data$`2018/19`), ymin =min(plot_data$change)*4 , ymax = 0 , fill= "#c9e7d9")+ 
    geom_point(colour="#f9bf07")+
    geom_point(aes(group=color_group), color="#686f73",  data = ~ subset(., color_group == "1")) +
    labs(x="Standardised rate for 2018/2019",
         y= "% change between 2018/19 and 2023/24")+
    su_theme()+
    scale_colour_manual(values=c("#686f73", "#f9bf07"))+
    theme(axis.title=element_text(size=12),
          legend.position="none")+
    geom_hline(yintercept=0, linetype="dashed", color = "#ec6555")+
    geom_vline(xintercept = mean(plot_data$`2018/19`), linetype="dashed", color = "#ec6555")+
    coord_cartesian(xlim =c(min(plot_data$`2018/19`)-1, max(plot_data$`2018/19`)), ylim = c((min(plot_data$change))*1.2 , (max(plot_data$change))*1.2))
  
  return(p)
  
}



plotting_total_activity_vs_percentage_change_LA<-function(data){
  
  p <- plotting_total_activity_vs_percentage_change_ggplot_LA(data, laname23)
  
  ggplotly(p, tooltip="text", width=660, height=450 )
}




generating_la_or_provider_table<-function(data, cohort){
  
  data<-data|>
    filter(cohorts==cohort) 
  
  min_value <- data |>
    dplyr::summarise(min = min(value, na.rm = TRUE)) |>
    dplyr::pull()
  
  max_value <- data |>
    dplyr::summarise(max = max(value, na.rm = TRUE)) |>
    dplyr::pull()
  
  table_data<-data |>
    select(any_of(c("Local Authority" = "laname23", 
                    "Provider" = "org_name", 
                    "ICB" = "icb_2024_name")),
           year, value) |>
    spread(key=year, value=value) |>
    mutate(`Percentage Change`=janitor::round_half_up(((`2023/24`-`2018/19`)/`2018/19`)*100,1))|>
    as.data.frame() |>
    mutate(across(dplyr::starts_with("20"), ~replace_na(as.character(.), "-")))|>
    mutate(across(dplyr::starts_with("20"), ~factor(., levels = c("-", min_value:max_value))))
  
  if("Provider" %in% names(table_data)) {
    table_data <- table_data |> 
      dplyr::rename("ICB (system)" = ICB)
  } 

  return(table_data)
  
}

