
# Local Authority population data (https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales)

wrangling_la_population_data<-function(data){
  
  la_population_data<-read_excel(data,
                                 sheet="MYEB1",
                                 range = "A2:R57878")|>
    clean_names()|>
    filter(country=="E")
  
  la_population_data<-la_population_data|>
    mutate(age_range=case_when(age>=0 & age<=4 ~ "0-4",
                               age>=5 & age<=9 ~ "5-9",
                               age>=10 & age<=14 ~ "10-14",
                               age>=15 & age<=19 ~ "15-19",
                               age>=20 & age<=24 ~ "20-24",
                               age>=25 & age<=29 ~ "25-29",
                               age>=30 & age<=34 ~ "30-34",
                               age>=35 & age<=39 ~ "35-39",
                               age>=40 & age<=44 ~ "40-44",
                               age>=45 & age<=49 ~ "45-49",
                               age>=50 & age<=54 ~ "50-54",
                               age>=55 & age<=59 ~ "55-59",
                               age>=60 & age<=64 ~ "60-64",
                               age>=65 & age<=69 ~ "65-69",
                               age>=70 & age<=74 ~ "70-74",
                               age>=75 & age<=79 ~ "75-79",
                               age>=80 & age<=84 ~ "80-84",
                               age>=85 & age<=89 ~ "85-89",
                               age>=90 ~ "90+")
    )|>
    select(-age, -country, -population_2011, -population_2012, -population_2013, -population_2014)|>
    mutate(sex=case_when(sex=="M"~ 1,
                         sex=="F" ~ 2))|>
    group_by(ladcode23, laname23, sex,age_range )|>
    summarise(across(where(is.numeric), sum), .groups = 'drop')|>
    gather(key="fyear", value="la_population", -ladcode23, -laname23, -sex, -age_range )|>
    mutate(fyear=case_when(fyear=="population_2015" ~ "2015/16",
                          fyear=="population_2016" ~ "2016/17",
                          fyear=="population_2017" ~ "2017/18",
                          fyear=="population_2018" ~ "2018/19",
                          fyear=="population_2019" ~ "2019/20",
                          fyear=="population_2020" ~ "2020/21",
                          fyear=="population_2021" ~ "2021/22",
                          fyear=="population_2022" ~ "2022/23",
                          fyear=="population_2023" ~ "2023/24"))
  
  return(la_population_data)
  
}

# ICB population data https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/clinicalcommissioninggroupmidyearpopulationestimates

# Function to pull and format data for each year
extract_and_format_icb_population_data_by_yr<-function(data, sheet_name, year){
  
  data<-read_excel(data,
                   sheet=sheet_name,
                   range = "A4:GG110")|>
    clean_names()
  
  
  data<-data|>
    select(-`sicbl_2024_code`, -`sicbl_2024_name`, -nhser_2024_code, -nhser_2024_name, -total)|>
    group_by(icb_2024_code, icb_2024_name)|>
    summarise(across(where(is.numeric), sum), .groups = 'drop')|>
    gather(key="age_sex_group", value="population_number", -icb_2024_code, -icb_2024_name)|>
    mutate(age=substring(age_sex_group, 2))|>
    mutate(sex=substr(age_sex_group, start=1, stop=1))|>
    select(-age_sex_group)|>
    mutate(age=as.numeric(age))|>
    mutate(age_range=case_when(age>=0 & age<=4 ~ "0-4",
                               age>=5 & age<=9 ~ "5-9",
                               age>=10 & age<=14 ~ "10-14",
                               age>=15 & age<=19 ~ "15-19",
                               age>=20 & age<=24 ~ "20-24",
                               age>=25 & age<=29 ~ "25-29",
                               age>=30 & age<=34 ~ "30-34",
                               age>=35 & age<=39 ~ "35-39",
                               age>=40 & age<=44 ~ "40-44",
                               age>=45 & age<=49 ~ "45-49",
                               age>=50 & age<=54 ~ "50-54",
                               age>=55 & age<=59 ~ "55-59",
                               age>=60 & age<=64 ~ "60-64",
                               age>=65 & age<=69 ~ "65-69",
                               age>=70 & age<=74 ~ "70-74",
                               age>=75 & age<=79 ~ "75-79",
                               age>=80 & age<=84 ~ "80-84",
                               age>=85 & age<=89 ~ "85-89",
                               age>=90 ~ "90+")
    )|>
    summarise(icb_population=sum(population_number), .by=c(icb_2024_code, icb_2024_name, age_range, sex))|>
    mutate(fyear=year)|>
    mutate(sex=case_when(sex=="m"~ 1,
                         sex=="f" ~ 2))
  
  return(data)
  
}

#Function to merge data for all years and impute 2023/24 data
wrangling_icb_population_data<-function(data, data2){
  
  icb_population_2015<-extract_and_format_icb_population_data_by_yr(data,"Mid-2015 ICB 2024", "2015/16")
  icb_population_2016<-extract_and_format_icb_population_data_by_yr(data,"Mid-2016 ICB 2024", "2016/17")
  icb_population_2017<-extract_and_format_icb_population_data_by_yr(data,"Mid-2017 ICB 2024", "2017/18")
  icb_population_2018<-extract_and_format_icb_population_data_by_yr(data,"Mid-2018 ICB 2024", "2018/19")
  icb_population_2019<-extract_and_format_icb_population_data_by_yr(data,"Mid-2019 ICB 2024", "2019/20")
  icb_population_2020<-extract_and_format_icb_population_data_by_yr(data,"Mid-2020 ICB 2024", "2020/21")
  icb_population_2021<-extract_and_format_icb_population_data_by_yr(data,"Mid-2021 ICB 2024", "2021/22")
  icb_population_2022<-extract_and_format_icb_population_data_by_yr(data,"Mid-2022 ICB 2024", "2022/23")
  
  # Using 2022 data for 2023/24
  icb_population_2023<-icb_population_2022|>
    mutate(fyear="2023/24")
  
  # Combining populations for each year into on dataframe
  icb_population_data<-rbind(icb_population_2015,
                             icb_population_2016,
                             icb_population_2017,
                             icb_population_2018,
                             icb_population_2019,
                             icb_population_2020,
                             icb_population_2021,
                             icb_population_2022,
                             icb_population_2023)
  
  icb_codes_names<-read.csv(data2)|>
    clean_names()
  
  icb_population_data<-icb_population_data|>
    left_join(icb_codes_names[,c("icb24cd", "icb24cdh")], 
              by=c("icb_2024_code"="icb24cd") )|>
    select(icb24cdh, icb_2024_code, icb_2024_name, fyear, age_range, sex, icb_population)|>
    mutate(sex=as.character(sex))
  
}


# Standard pop for England 2021 census https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationandhouseholdestimatesenglandandwales/census2021

formatting_standard_england_population<-function(data){
  
  standard_england_pop_2021_census<-read.csv(data)|>
    mutate(sex=as.character(sex))
  
}  

# Age standardised dataset by ICB

generating_icb_age_sex_standardised_rates<-function(data, icb_pop, standard_pop, activity_type){
  
  data|>
    filter(fyear!=201415)|>
    left_join(icb_pop, by=c("icb_2024_name", "year"="fyear", "age_range", "sex"))|>
    left_join(standard_pop, by=c("age_range", "sex"))|>
    filter(!is.na(icb_2024_name))|>
    filter(age_range!="NA")|>
    group_by(icb_2024_name, icb, year, cohorts) |>
    rename(activity={{activity_type}})|>
    PHEindicatormethods::calculate_dsr(x =activity,    # observed number of events
                                       n = icb_population,  # non-standard pops for each stratum
                                       stdpop = pop)    # standard populations for England for each stratum
  

  
}

generating_england_age_sex_standardised_rates<-function(data, icb_pop, standard_pop, activity_type){
  
  data|>
    filter(fyear!=201415)|>
    left_join(icb_pop, by=c("icb_2024_name", "year"="fyear", "age_range", "sex"))|>
    left_join(standard_pop, by=c("age_range", "sex"))|>
    filter(!is.na(icb_2024_name))|>
    filter(age_range!="NA")|>
    rename(activity={{activity_type}})|>
    group_by(year, cohorts) |>
    PHEindicatormethods::calculate_dsr(x = activity,    # observed number of events
                                       n = icb_population,  # non-standard pops for each stratum
                                       stdpop = pop)    # standard populations for England for each stratum
  
  
  
}




