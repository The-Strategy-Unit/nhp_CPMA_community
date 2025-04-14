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
             collect()|>
    as.data.frame(total_cohort_numbers)
  

  total_cohort_numbers<- identify_whether_bedday_or_admissions_or_both( total_cohort_numbers, 1:29)|>
    mutate(episodes=ifelse(activity_group=="beddays", 0, episodes))|>
    select(-activity_group, -number_of_cohorts)
  
  return(total_cohort_numbers)
  
}

# Function to generate upset plot

plot_upset_plot<-function(dataset, mitigator_table, activity_type){
  
  name<-deparse(substitute(activity_type))
  
  if(name=="episodes"){
  data<- dataset|>
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
           zero_los_no_procedure_child)|>
    summarise(activity=sum({{activity_type}}))|>
    ungroup()
  }
  else{
    data<-dataset|>
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
      summarise(activity=sum({{activity_type}}))|>
      ungroup()
  }
  
  
  plot_data<-data|>
    mutate(total_activity=sum(activity))|>
    mutate(percentage=janitor::round_half_up((activity/total_activity)*100,0))|>
    slice_max(activity,n=15)|>
    arrange(desc(activity))|>
    mutate(id=row_number())
  
  numbers_in_each_cohort<-data|>
    gather(groups, values, -activity)|>
    filter(values!=0)|>
    group_by(groups)|>
    summarise(total_activity=sum(activity))
  
  
  top_plot<-plot_data|>
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
    geom_text(aes(label=paste0(scales::comma(activity), ' \n(',percentage, '%)')), vjust=-0.2, size=2.6)+
    scale_y_continuous(limits=c(0,max(data$activity)*1.2), expand=c(0,0), labels = label_comma())+
    scale_x_continuous(expand=c(0.01,0.01))
  
  data2<- plot_data|>
    select(-percentage, -total_activity)|>
    dplyr::select(where(~ any(. != 0)))|>
    gather(groups, value, -id, -activity) |>
    mutate(value=as.character(value))|>
    left_join(numbers_in_each_cohort, by=c("groups"))|>
    left_join(mitigator_table[,c("mitigator_code", "mitigator_name")], by=c("groups"="mitigator_code"))|>
    arrange(id)|>
    mutate(mitigator_name=reorder(mitigator_name, total_activity))
  
  
  data3<-data2|>
    filter(value==1)
  
  bottom_plot<- data2|>
    ggplot(aes(x = id, y = mitigator_name,  group=value)) +
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

plot_upset_plot_mechanism_group<-function(dataset, mitigator_table, activity_type){
  
  data<-dataset|>
    mutate(activity={{activity_type}})
  
  plot_data<-data|>
    mutate(total_activity=sum(activity))|>
    mutate(percentage=janitor::round_half_up((activity/total_activity)*100,0))|>
    slice_max(activity,n=15)|>
    arrange(desc(activity))|>
    mutate(id=row_number())
  
  numbers_in_each_cohort<-data|>
    gather(groups, values, -activity)|>
    filter(values!=0)|>
    group_by(groups)|>
    summarise(total_activity=sum(activity))
  
  
  top_plot<-plot_data|>
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
    geom_text(aes(label=paste0(scales::comma(activity), ' \n(',percentage, '%)')), vjust=-0.2, size=2.4)+
    scale_y_continuous(limits=c(0,max(data$activity)*1.2), expand=c(0,0), labels = label_comma())+
    scale_x_continuous(expand=c(0.01,0.01))
  
  data2<- plot_data|>
    select(-percentage, -total_activity, -episodes, -beddays, -number_of_cohorts)|>
    dplyr::select(where(~ any(. != 0)))|>
    gather(groups, value, -id, -activity) |>
    mutate(value=as.character(value))|>
    left_join(numbers_in_each_cohort, by=c("groups"))|>
    left_join(mitigator_table[,c("mitigator_code", "mitigator_name")], by=c("groups"="mitigator_code"))|>
    arrange(id)|>
    mutate(mitigator_name=reorder(mitigator_name, total_activity))
  
  
  data3<-data2|>
    filter(value==1)
  
  bottom_plot<- data2|>
    ggplot(aes(x = id, y = mitigator_name,  group=value)) +
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

plotting_barchart_summary_of_overlaps<-function(data, cohort_name, activity_type, mitigator_table){
  
 cohort_name2<-mitigator_table$mitigator_name[mitigator_table$mitigator_code==cohort_name]
  
   cohort_total<- data|>
    summarise(total=sum({{activity_type}}))
  
  data2<-data|>
    select(-fyear)|>
    mutate(across(everything(), ~ .x* {{activity_type}}, .names = "{.col}"))|>
    select(-episodes, -beddays)|>
    gather(key="cohort", value="number")|>
    group_by(cohort)|>
    summarise(number=sum(number))|>
    mutate(percentage=janitor::round_half_up(((number/max(number))*100),0))|>
    left_join(mitigator_table[,c("mitigator_name", "mitigator_code", "mechanism")], by=c("cohort"="mitigator_code"))|>
    arrange(desc(number))|>
    mutate(mitigator_name=factor(mitigator_name, unique(mitigator_name)))|>
    mutate(mitigator_name=fct_relevel(mitigator_name, cohort_name2 ))|>
    arrange(mitigator_name)|>
    mutate(colour=ifelse(cohort==cohort_name, "#000000", ifelse(number==0, "#686f73" , "#ec6555" )))
  
  name<-deparse(substitute(activity_type))
  
  if(name=="episodes"){
     data2<-data2|>
       filter(mechanism!="Efficiencies")
  }
  
  else{
    data2<-data2
  }
  
  redirection<-data2|>
    filter(mechanism=="redirection_substitution")
  
  plot_redirection<-overlap_ggplot_facets(redirection, cohort_name , "Redirection/Substitution" , 5)
  
  prevention<-data2|>
    filter(mechanism=="prevention")
  
  plot_prevention<-overlap_ggplot_facets(prevention,cohort_name , "Prevention" ,4)
  
  relocation<-data2|>
    filter(mechanism=="efficiencies_relocation")
  
  plot_relocation<-overlap_ggplot_facets(relocation,cohort_name, "Relocation & Efficiencies",2 )
  
  efficiencies<-data2|>
    filter(mechanism=="efficiencies")
  
  plot_efficiencies<-overlap_ggplot_facets(efficiencies,cohort_name , "Efficiencies",1 )+
    labs(y="Percentage of cohort of interest present in other cohorts")
  
  
  layout <- c(area(
    t = 0,
    l = 0,
    b = 21,
    r = 6
  ),
  area(
    t = 22,
    l = 0,
    b = 31,
    r = 6
  ),
  area(
    t = 32,
    l = 0,
    b = 34,
    r = 6
  ),
  area(
    t = 35,
    l = 0,
    b = 38,
    r = 6
  ))
  # final plot arrangement
  
  if(name=="episodes"){
    plot_redirection + plot_prevention + plot_relocation + plot_layout(design=layout) +
      plot_annotation(caption="Cohorts highlighted in red are those who overlap with this cohort of interest",
                      theme = theme(
                        plot.caption = ggtext::element_textbox_simple(hjust = -33, vjust=10, size=13, colour =  "#ec6555")
                      ))
  }
  
  else{
    plot_redirection + plot_prevention + plot_relocation + plot_efficiencies + plot_layout(design=layout) +
      plot_annotation(caption="Cohorts highlighted in red are those who overlap with this cohort of interest",
                      theme = theme(
                        plot.caption = ggtext::element_textbox_simple(hjust = -33, vjust=10, size=13, colour =  "#ec6555")
                      ))
  } 
  
   
}

# Number of cohorts of which the admissions are part
plotting_barchart_number_of_cohorts<-function(data, activity_type){
  
  name<-deparse(substitute(activity_type))
  
  if(name=="episodes"){
  
  data2<-data|>
    select(-emergency_elderly, -stroke_early_supported_discharge, -raid_ip)|>
    mutate(number_of_cohorts = rowSums(pick(1:26), na.rm = TRUE))|>
    mutate(number_of_cohorts=number_of_cohorts-1)|>
    mutate(number_of_cohorts=ifelse(number_of_cohorts>4, "5+", number_of_cohorts))|>
    summarise(activity=sum({{activity_type}}), .by=c(number_of_cohorts))
  }
  else{
    data2<-data|>
      mutate(number_of_cohorts = rowSums(pick(1:29), na.rm = TRUE))|>
      mutate(number_of_cohorts=number_of_cohorts-1)|>
      mutate(number_of_cohorts=ifelse(number_of_cohorts>4, "5+", number_of_cohorts))|>
      summarise(activity=sum({{activity_type}}), .by=c(number_of_cohorts))
  }
  
  data2<-  if(!0 %in% data2$number_of_cohorts){
    add_row(data2, number_of_cohorts="0", activity=0)
  }
  else{
    data2<-data2
  }
  
  data2<- data2 |>
    mutate(percentage=janitor::round_half_up(activity/(sum(activity))*100,1))
  
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

plotting_barchart_number_of_cohorts_for_mechanism_group<-function(data, activity_type){
  
 data2<- data|>
    mutate(number_of_cohorts=ifelse(number_of_cohorts>4, "5+", number_of_cohorts))|>
    summarise(activity=sum({{activity_type}}), .by=c(number_of_cohorts))|>
    mutate(percentage=janitor::round_half_up(activity/(sum(activity))*100,1))
  
  
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

identify_whether_bedday_or_admissions_or_both<-function(data, columns){
  
 data|>
    mutate(number_of_cohorts = rowSums(pick(columns), na.rm = TRUE))|>
    mutate(activity_group=ifelse(alcohol_partially_attributable_acute==0 &
                                  alcohol_partially_attributable_chronic==0 &
                                  alcohol_wholly_attributable==0 &
                                  ambulatory_care_conditions_acute==0 &
                                  ambulatory_care_conditions_chronic==0 &
                                  ambulatory_care_conditions_vaccine_preventable==0 &
                                  eol_care_2_days==0 &
                                  eol_care_3_to_14_days==0 &
                                  falls_related_admissions==0 &
                                  frail_elderly_high==0 &
                                  frail_elderly_intermediate==0 &
                                  intentional_self_harm==0 &
                                  medically_unexplained_related_admissions==0 &
                                  medicines_related_admissions_explicit==0 &
                                  medicines_related_admissions_implicit_anti_diabetics==0 &
                                  medicines_related_admissions_implicit_benzodiasepines==0 &
                                  medicines_related_admissions_implicit_diurectics==0 &
                                  medicines_related_admissions_implicit_nsaids==0 &
                                  obesity_related_admissions==0 &
                                  raid_ae==0 &
                                  readmission_within_28_days==0 &
                                  smoking==0 &
                                  virtual_wards_activity_avoidance_ari==0 &
                                  virtual_wards_activity_avoidance_heart_failure==0 &
                                  zero_los_no_procedure_adult==0 &
                                  zero_los_no_procedure_child==0 ,
                                  "beddays",
                                 "admissions&beddays" ))
  
}

# Generate data for cohort mechanisms overlap

generate_data_for_mechanism_cohort_overlaps<-function(data){  
  
  data|>
    mutate_mechanism_columns() |>
    select(Redirection = redirection_substitution,
           Prevention = prevention,
           `Relocation&Efficiencies` = efficiencies_relocation,
           Efficiencies = efficiencies,
           episodes, 
           beddays)
  
}

#Plot a venn diagram total_cohort_numbers_2324
generate_venn_diagram<-function(data, activity_type){
  
  data1<-generate_data_for_mechanism_cohort_overlaps(data)|>
    summarise(beddays=sum(beddays),
              episodes=sum(episodes), .by=c(Redirection, Prevention, `Relocation&Efficiencies`, Efficiencies))|>
    mutate(episodes=janitor::round_half_up(episodes,0),
           beddays=janitor::round_half_up(beddays,0))|>
    uncount({{activity_type}})
  
  name<-deparse(substitute(activity_type))
  
  if(name=="episodes"){
    
    venn_data <- list(
      Redirection = which(data1$Redirection=="1"),
      Prevention = which(data1$Prevention=="1"),
      Relocation = which(data1$Relocation=="1")
    )
     
  }
  else{
    
    venn_data <- list(
      Redirection = which(data1$Redirection=="1"),
      Prevention = which(data1$Prevention=="1"),
      Relocation = which(data1$Relocation=="1"),
      Efficiencies= which(data1$Efficiencies=="1")
    )
    
  } 
  
  rm(data1)
  
  ggVennDiagram(venn_data, label_alpha = 0, label_size = 2.8) +
    scale_fill_distiller(palette = "Spectral") +
    scale_x_continuous(expand=c(0.1,0.1))+
    theme(legend.position="none",
          plot.title=element_text(face="bold", hjust = 0.5))
  
}
  
plotting_barchart_number_of_mechanism_groups<-function(data, activity_type){
  
  name<-deparse(substitute(activity_type))
  
  if(name=="episodes"){
    
    data2<-data|>
      generate_data_for_mechanism_cohort_overlaps()|>
      select(-Efficiencies)|>
      filter(episodes!=0)|>
      mutate(number_of_cohorts = rowSums(pick(1:3), na.rm = TRUE))|>
      summarise(activity=sum({{activity_type}}), .by=c(number_of_cohorts))
  }
  else{
    data2<-data|>
      generate_data_for_mechanism_cohort_overlaps()|>
      mutate(number_of_cohorts = rowSums(pick(1:4), na.rm = TRUE))|>
      summarise(activity=sum({{activity_type}}), .by=c(number_of_cohorts))
  }
  
  data2<- data2 |>
    mutate(percentage=janitor::round_half_up(activity/(sum(activity))*100,1))
  
  data2|>
    ggplot(aes(x=number_of_cohorts, y=activity))+
    geom_bar(stat="identity" , fill="#f9bf07")+
    su_theme()+
    theme(axis.text=element_text(size=9.5),
          axis.title=element_text(size=10.5))+
    labs(y="Number",
         x="No. of mechanism groups the\n activity is part of",
         title=NULL)+
    geom_text(aes(label=paste0(scales::comma(activity), ' \n(',percentage, '%)')), vjust=-0.2, size=2.7)+
    scale_y_continuous(limits=c(0, max(data2$activity)*1.2), expand=c(0,0), labels = label_comma())
  
  
  
}






  
# Function for ggplots for summary overlap chart

overlap_ggplot_facets<-function(dataset, cohort_name, title , ratio_number){ 
  

dataset|>
  ggplot(aes(x=factor(mitigator_name), y=percentage))+
    geom_segment(aes( y=0, yend=percentage,  x=mitigator_name), size=3.5, colour=(ifelse(dataset$cohort==cohort_name,"#686f73","#f9bf07"))) +
  scale_x_discrete(limits=rev)+
  su_theme()+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=14),
        axis.text.y=element_text(colour=rev(dataset$colour)),
        plot.caption=element_text(colour="#ec6555", size=11))+
  labs(y=NULL,
       x=NULL,
       title=title)+ 
  geom_text(aes(label=paste0(percentage,  '% (',scales::comma(number), ')')), hjust=-0.05, size=2.7)+
  scale_y_continuous(limits=c(0, 124), expand=c(0,0), labels = label_comma())+
  coord_fixed(ratio=ratio_number)+
  coord_flip() 

}

