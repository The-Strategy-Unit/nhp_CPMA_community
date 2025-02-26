# Format cohort data
Formatting_data_for_cohort_overlap<-function(table){

  
  total_cohort_numbers <- dplyr::tbl(
    sc,
    dbplyr::in_catalog("strategyunit","default", table)
  )|>
    as.data.frame()|>
    group_by(alcohol_partially_attributable_acute,
             alcohol_partially_attributable_chronic,
             alcohol_wholly_attributable,
             ambulatory_care_conditions_acute,
             ambulatory_care_conditions_chronic,
             ambulatory_care_conditions_vaccine_preventable,
             eol_care_2_days,
             eol_care_3_to_14_days,
             falls_related_admissions,
             frail_elderly_high,
             frail_elderly_intermediate,
             intentional_self_harm,
             medically_unexplained_related_admissions,
             medicines_related_admissions_explicit,
             medicines_related_admissions_implicit_anti_diabetics,
             medicines_related_admissions_implicit_benzodiasepines,
             medicines_related_admissions_implicit_diurectics,
             medicines_related_admissions_implicit_nsaids,
             obesity_related_admissions,
             raid_ae,
             readmission_within_28_days,
             smoking,
             virtual_wards_activity_avoidance_ari,
             virtual_wards_activity_avoidance_heart_failure,
             zero_los_no_procedure_adult,
             zero_los_no_procedure_child,
             emergency_elderly,
             raid_ip,
             stroke_early_supported_discharge)|>
    summarise(episodes=sum(episodes),
              beddays=sum(beddays))|>
             ungroup()|>
             collect()
  
  total_cohort_numbers<-as.data.frame(total_cohort_numbers)
  
  return(total_cohort_numbers)
  
}

# Function to generate upset plot

plot_upset_plot<-function(dataset, activity_type){
  
  activity<-deparse(substitute(activity_type))
  
  # Filter down to highest combinations
  data_aggregated<-dataset|>
    slice_max({{activity_type}},n=15)
  
  max_value<-data_aggregated|>
    mutate(max=max({{activity_type}}))
  
  # Count of all mitigatable activity and disaggregate data
  if(activity=="episodes"){
    all_mitigatable_activity<-sum(dataset$episodes)
    
    data<-data_aggregated|>
      select(-beddays)|>
      uncount(episodes)
    
  } else {
    all_mitigatable_activity<-sum(dataset$beddays)
    
    data<-data_aggregated|>
      select(-episodes)|>
      uncount(beddays)
    
  }
  
  #Count of columns to feed into function
  number_columns<-ncol(data)-1
  
  #Generating the upset plot
  upset_plot_data<-data
  
  cohorts = colnames(data)[1:number_columns]
  
  upset_plot_data[cohorts] = upset_plot_data[cohorts] == 1
  
  size = get_size_mode('exclusive_intersection')
  
  
  plot<-ComplexUpset::upset(upset_plot_data, cohorts, name='Cohorts', 
                      width_ratio=0.1, n_intersections=15,
                      set_sizes=FALSE,
                      keep_empty_groups=FALSE,
                      themes=(upset_modify_themes(
                        list(
                          'intersections_matrix'=theme(text=element_text(size=13),
                                                       axis.title.x=element_blank())
                        ))),
                      base_annotations = list(
                        'Intersection size'=(
                          intersection_size(
                            text_mapping=aes(
                              label=paste0(round(!!get_size_mode('exclusive_intersection')/(all_mitigatable_activity) * 100, 1), '%', '\n ', scales::comma(!!size) )  ) , 
                            text=list(size=2.8),
                            bar_number_threshold = 1,
                            mapping=aes(fill='bars_color'))+
                            theme(axis.title.y = element_text(size=14, vjust=-24),
                                  axis.text.y=element_text(size=10),
                                  panel.grid.minor = element_blank(),
                                  panel.grid.major = element_blank())+
                            scale_fill_manual(values=c('bars_color'="#f9bf07"), guide='none')+
                            scale_y_continuous(limits=c(0,max_value$max*1.2), labels = label_comma()
                            ))))
  return(plot)
  
}

# Summary barchart of percentage of overlap with different groups

plotting_barchart_summary_of_overlaps<-function(data, cohort_name, activity_type){
  
  cohort_total<- data|>
    summarise(total=sum({{activity_type}}))
  
  data2<-data|>
    mutate(across(everything(), ~ .x* {{activity_type}}, .names = "{.col}"))|>
    select(-episodes, -beddays)|>
    gather(key="cohort", value="number")|>
    group_by(cohort)|>
    summarise(number=sum(number))|>
    mutate(percentage=round(((number/max(number))*100),1))|>
    arrange(desc(number))|>
    mutate(cohort=factor(cohort, unique(cohort)))
  
  data2|>
    ggplot(aes(x=fct_relevel(cohort, cohort_name), y=number))+
    geom_bar(stat="identity", fill=factor(ifelse(data2$cohort==cohort_name,"#686f73","#f9bf07")))+
    su_theme()+
    theme(axis.text=element_text(size=11),
          axis.title.y=element_text(size=16))+
    labs(y="Number",
         x=NULL,
         title=NULL)+ 
    geom_text(aes(label=paste0(scales::comma(number), ' (',percentage, '%)')), hjust=-0.05, size=3)+
    scale_y_continuous(limits=c(0, cohort_total$total*1.25), expand=c(0,0), labels = label_comma())+
    coord_flip()+
    scale_x_discrete(limits=rev)
  
}

# Number of cohorts of which the admissions are part
plotting_barchart_number_of_cohorts<-function(data, activity_type){
  
  data2<-data|>
    mutate(number_of_cohorts = rowSums(pick(1:29), na.rm = TRUE))|>
    mutate(number_of_cohorts=number_of_cohorts-1)|>
    mutate(number_of_cohorts=ifelse(number_of_cohorts>4, "5+", number_of_cohorts))|>
    summarise(activity=sum({{activity_type}}), .by=c(number_of_cohorts))
  
  data2<-  if(!0 %in% data2$number_of_cohorts){
    add_row(data2, number_of_cohorts="0", activity=0)
  }
  
  data2<- data2 |>
    mutate(percentage=round(activity/(sum(activity))*100,1))
  

  data2|>
    ggplot(aes(x=number_of_cohorts, y=activity))+
    geom_bar(stat="identity" , fill=factor(ifelse(data2$number_of_cohorts=="0","#686f73","#f9bf07")))+
    su_theme()+
    theme(axis.text=element_text(size=11),
          axis.title=element_text(size=12))+
    labs(y="Number",
         x="Number of other cohorts the activity is part of",
         title=NULL)+
    geom_text(aes(label=paste0(scales::comma(activity), ' \n(',percentage, '%)')), vjust=-0.2, size=2.7)+
    scale_y_continuous(limits=c(0, max(data2$activity)*1.2), expand=c(0,0), labels = label_comma())
  
  
  
}


