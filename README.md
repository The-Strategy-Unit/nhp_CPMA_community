# Quantifying mitigable activity: NHP Community Mitigators

In planning for the future, whether for sizing a new hospital or managing the pattern of service utilisation across a local health and care system, a judgement needs to be made about how demand and resultant activity will change over time. There are a number of components to this but one aspect is how changes to the way we provide services might change patterns of utilisation. In particular, how might different ways of working in 'the community' (including preventative approaches and greater use of technology) act to reduce demand for hospital treatment, i.e. mitigation of hospital activity.

This analysis focuses on potentially mitigable inpatient activity that relates to community alternatives - sometimes called the 'left shift'. This move 'from hospital to community' is one of the three strategic shifts set for the NHS by the current Government.

Analysis was conducted to understand the scale, trends, and regional variations of potentially mitigable hospital activity at national and sub-national levels. This repo contains the code required to run the analyses and generate the report in a quarto book format.

The report can be found at: https://connect.strategyunitwm.nhs.uk/nhp/community_mitigators/

An introductory video and user guide for the report can be viewed at: https://www.youtube.com/watch?v=egUhA9OtXyg

## Overview of repository contents

The following data was saved in the `population_data` folder:

- `2021 census data for England.csv` - Population and household estimates, England and Wales: Census 2021.	<https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationandhouseholdestimatesenglandandwales/census2021>
- `ethnicity_by_icb_2021.xlsx` - Ethnic group in England and Wales
Dataset, Released 29 November 2022 using data from Census 2021.	<https://www.ons.gov.uk/peoplepopulationandcommunity/culturalidentity/ethnicity/bulletins/ethnicgroupenglandandwales/census2021>
- `ICD10_diagnosis_code_descriptions.csv` -	NHS ICD-10 5th Edition data files: ICD10_Edition5_CodesAndTitlesAndMetadata_GB_20160401. <https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/28/items/258/releases>
- `Integrated_Care_Boards_(December_2024)_Names_and_Codes_in_EN.csv` - Integrated Care Boards (December 2024) Names and Codes in EN last updated on 12 Jan 2025 from gov.uk. <https://www.data.gov.uk/dataset/3891c3f4-337c-4981-99c7-268667e33692/integrated-care-boards-december-2024-names-and-codes-in-en>
- `LSOA_and_IMD.csv` - English indices of deprivation 2019. <https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019>
- `LSOA_pop.xlsx` - Lower layer Super Output Area population estimates 2022. <https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates>
- `LSOA11_to_LSOA21.csv` - LSOA (2011) to LSOA (2021) to Local Authority District (2022) Exact Fit Lookup for EW (V3). <https://geoportal.statistics.gov.uk/search?sort=Date%20Created%7Ccreated%7Cdesc&tags=LUP_EXACT_LSOA11_LSOA21>
- `LSOA21_to_ICB.csv` - LSOA (2021) to SICBL to ICB to Cancer Alliances to LAD (April 2024) look-up. <https://geoportal.statistics.gov.uk/search?sort=Date%20Created%7Ccreated%7Cdesc&tags=LUP_EXACT_LSOA21_SICBL_ICB_CAL>
- `myebtablesenglandwales20112023.xlsx` - Estimates of the population for England and Wales: Mid 2011 to mid-2023 detailed time series edition of this dataset edition of this dataset. <https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales>
- `sapeicb202420112022.xlsx` - Mid-2011 to mid-2022: Integrated Care Boards, 2024 geography edition of this dataset from ons.gov.uk. <https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/clinicalcommissioninggroupmidyearpopulationestimates>




Data was first processed in Databricks. Notebooks in the `databricks notebook` folder are numbered according to the order in which they were run.

The rest of the processing and analysis can be found in `_targets.R` with functions in the `R` folder. Functions have been split into files according to the analyses they were primarily related to and these files are numbered according to the layout of their corresponding sections in the final reports.

`create-mitigator-reports.R` is a script to create the 29 mitigator and 4 mechanism quarto files. Each of these quarto files uses child documents to create the various sections of the report. These child documents can be found in the `child-dir` folder and are numbered to match the function files for convenience.
