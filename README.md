# Quantifying mitigable activity: NHP Community Mitigators

In planning for the future, whether for sizing a new hospital or managing the pattern of service utilisation across a local health and care system, a judgement needs to be made about how demand and resultant activity will change over time. There are a number of components to this but one aspect is how changes to the way we provide services might change patterns of utilisation. In particular, how might different ways of working in 'the community' (including preventative approaches and greater use of technology) act to reduce demand for hospital treatment, i.e. mitigation of hospital activity.

This analysis focuses on potentially mitigable inpatient activity that relates to community alternatives - sometimes called the 'left shift'. This move 'from hospital to community' is one of the three strategic shifts set for the NHS by the current Government.

Analysis was conducted to understand the scale, trends, and regional variations of potentially mitigable hospital activity at national and sub-national levels. This repo contains the code required to run the analyses and generate the report in a quarto book format.

The report can be found at: https://connect.strategyunitwm.nhs.uk/nhp/community_mitigators/

An introductory video and user guide for the report can be viewed at:  https://www.youtube.com/watch?v=egUhA9OtXyg

## Overview of repo

Data was first processed in Databricks. Notebooks in the `databricks notebook` folder are numbered according to the order in which they were run.

The rest of the processing and analysis can be found in `_targets.R` with functions in the `R` folder. Functions have been split into files according to the analyses they were primarily related to and these files are numbered according to the layout of their corresponding sections in the final reports.

`create-mitigator-reports.R` is a script to create the 29 mitigator and 4 mechanism quarto files. Each of these quarto files uses child documents to create the various sections of the report. These child documents can be found in the `child-dir` folder and are numbered to match the function files for convenience.