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
             stroke_early_supported_discharge,
             fyear)|>
    summarise(episodes=sum(episodes),
              beddays=sum(beddays))|>
             ungroup()|>
             collect()
  
  total_cohort_numbers<-as.data.frame(total_cohort_numbers)
  
  return(total_cohort_numbers)
  
}

# Function to generate upset plot

plot_upset_plot<-function(dataset, activity_type){
  
  data<-  dataset|>
    rename(activity={{activity_type}})|>
    mutate(total_activity=sum(activity))|>
    mutate(percentage=round((activity/total_activity)*100,1))|>
    slice_max(activity,n=15)|>
    arrange(desc(activity))|>
    mutate(id=row_number())
  
  numbers_in_each_cohort<-dataset|>
    gather(groups, values, -episodes, -beddays)|>
    filter(values!=0)|>
    group_by(groups)|>
    summarise(episodes=sum(episodes),
              beddays=sum(beddays))
  
  
  top_plot<-data|>
    ggplot(aes(x=id, y=activity))+
    geom_bar(stat="identity", fill="#f9bf07" )+
    su_theme()+
    theme(axis.text=element_text(size=11),
          axis.title=element_text(size=12),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_text(vjust=-6))+
    labs(y="Intersection size",
         x=NULL,
         title=NULL)+
    geom_text(aes(label=paste0(scales::comma(activity), ' \n(',percentage, '%)')), vjust=-0.2, size=2.9)+
    scale_y_continuous(limits=c(0,max(data$activity)*1.2), expand=c(0,0), labels = label_comma())+
    scale_x_continuous(expand=c(0.01,0.01))
  
  data2<- data|>
    dplyr::select(1:29, activity, id)|>
    dplyr::select(where(~ any(. != 0)))|>
    gather(groups, value, -id, -activity) |>
    mutate(value=as.character(value))|>
    left_join(numbers_in_each_cohort, by=c("groups"))|>
    arrange(id)|>
    mutate(groups=reorder(groups, {{activity_type}}))
  
  data3<-data2|>
    filter(value==1)
  
  bottom_plot<- data2|>
    ggplot(aes(x = id, y = groups,  group=value)) +
    geom_point(aes(color=value),size=4 , show.legend = FALSE)+ 
    geom_line(data=data3, aes(group=id), linewidth=1)+ 
    su_theme()+
    theme(axis.text.x=element_blank(),
          legend_position="none",
          axis.title=element_blank(),
          axis.ticks.x=element_blank(),
          axis.line=element_blank(),
          panel.background = element_rect(fill ='#EEEEEE', colour='#EEEEEE'))+
    labs(x=NULL, y=NULL)+
    scale_colour_manual(values=c("1"="black", "0"="white"))

  layout <- c(
    area(t = 0, l = 0, b = 24, r = 25),
    area(t = 25, l = 0, b = 38, r = 25))
  
  # final plot arrangement
  top_plot +  bottom_plot + plot_layout(design = layout)
  
}




# Summary barchart of percentage of overlap with different groups

plotting_barchart_summary_of_overlaps<-function(data, cohort_name, activity_type){
  
  cohort_total<- data|>
    summarise(total=sum({{activity_type}}))
  
  data2<-data|>
    select(-fyear)|>
    mutate(across(everything(), ~ .x* {{activity_type}}, .names = "{.col}"))|>
    select(-episodes, -beddays)|>
    gather(key="cohort", value="number")|>
    group_by(cohort)|>
    summarise(number=sum(number))|>
    mutate(percentage=round(((number/max(number))*100),1))|>
    arrange(desc(number))|>
    mutate(cohort=factor(cohort, unique(cohort)))|>
    mutate(cohort=fct_relevel(cohort,cohort_name ))|>
    arrange(cohort)|>
    mutate(colour=ifelse(cohort==cohort_name, "#000000", ifelse(number==0, "#686f73" , "#ec6555" )))
  
  data2|>
    ggplot(aes(x=cohort, y=percentage))+
    geom_bar(stat="identity", fill=factor(ifelse(data2$cohort==cohort_name,"#686f73","#f9bf07")))+
    scale_x_discrete(limits=rev)+
    su_theme()+
    theme(axis.text=element_text(size=11),
          axis.title=element_text(size=13),
          axis.text.y=element_text(colour=rev(data2$colour)),
          plot.caption=element_text(colour="#ec6555", size=11))+
    labs(y=paste0("Percentage of ", cohort_name, " cohort in each of the other cohorts"),
         caption = (paste0("Cohorts highlighted in red are those who overlap with the ",  "frail_elderly_high")),
         x=NULL,
         title=NULL)+ 
    geom_text(aes(label=paste0(percentage,  '% (',scales::comma(number), ')')), hjust=-0.05, size=3)+
    scale_y_continuous(limits=c(0, 124), expand=c(0,0), labels = label_comma())+
    coord_flip()
  
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
         x="No. of other cohorts the activity is part of",
         title=NULL)+
    geom_text(aes(label=paste0(scales::comma(activity), ' \n(',percentage, '%)')), vjust=-0.2, size=2.7)+
    scale_y_continuous(limits=c(0, max(data2$activity)*1.2), expand=c(0,0), labels = label_comma())
  
  
  
}

# Function to identify which cohorts are beddays only

identify_whether_bedday_or_admissions_or_both<-function(data){
  
 data|>
    mutate(number_of_cohorts = rowSums(pick(1:29), na.rm = TRUE))|>
    mutate(activity_group=ifelse(number_of_cohorts==1 &
                                 (emergency_elderly==1 |
                                  stroke_early_supported_discharge ==1|
                                  raid_ip==1
                                 ), "beddays",
                                 "admissions&beddays" ))
  
}

# Function to identify the mechanism group

identify_mechanism_group<-function(data){
  total_cohort_numbers_2324|>
    mutate(redirection=ifelse((ambulatory_care_conditions_acute==1|
                              ambulatory_care_conditions_chronic==1|
                              ambulatory_care_conditions_vaccine_preventable==1|
                              eol_care_2_days==1|
                              eol_care_3_to_14_days==1|
                              falls_related_admissions==1|
                              frail_elderly_high==1|
                              frail_elderly_intermediate==1|
                              medicines_related_admissions_explicit==1|
                              medicines_related_admissions_implicit_anti_diabetics==1|
                              medicines_related_admissions_implicit_benzodiasepines==1|
                              medicines_related_admissions_implicit_diurectics==1|
                              medicines_related_admissions_implicit_nsaids==1|
                              readmission_within_28_days==1|
                              zero_los_no_procedure_adult==1|
                              zero_los_no_procedure_child==1), 
                              "Redirection/Substitution",
                              NA ) )|>
    mutate(prevention=ifelse((alcohol_partially_attributable_acute==1|
                               alcohol_partially_attributable_chronic==1|
                               alcohol_wholly_attributable==1|
                               obesity_related_admissions==1|
                               smoking==1|
                               raid_ae==1|
                               intentional_self_harm==1|
                               medically_unexplained_related_admissions==1), 
                              "Prevention",
                              NA ) )|>
    mutate(relocation=ifelse((virtual_wards_activity_avoidance_ari==1|
                              virtual_wards_activity_avoidance_heart_failure ==1), 
                             "Relocation & Efficiencies",
                             NA ) )|>
    mutate(efficiencies=ifelse((emergency_elderly==1|
                                stroke_early_supported_discharge==1|
                                raid_ip==1), 
                             "Efficiencies",
                             NA ) )|>
    mutate(cohort=paste0(redirection,"-" ,prevention, "-", relocation, "-", efficiencies))|>
    group_by(cohort)|>
    summarise(episodes=sum(episodes),
              beddays=sum(beddays))
}
  
