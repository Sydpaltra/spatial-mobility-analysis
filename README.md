# Spatial-Mobility-Analysis

# About

The goal of this repository is to analyse mobility data collected between 2020 and 2022 in Germany. Data is available down to the post code level. Analysis is performed on the national, federal state, and post code level, and focuses on changes over time, differences between geographical regions and between urban and rural areas. This work is conducted as part of TU Berlin's contribution to the MODUS-Covid and infoXpand project.

## Files

- **CorrelationAnalysisNational.R** : Data is considered on the German national level, until 2023-01-01. We consider 7-day-incidence data (provided by RKI), hospitalization data (provided by RKI), covid-related deaths (provided by RKI), the out-of-home duration (in hrs/person) as a mobility indicator, maximum temperature data, precipitation data. Data is read in, cleaned, and aggregated on a weekly level. The following correlation plots are produced:
	1. Out-of-home duration vs. (approx) effective R-value
	2. Out-of-home duration vs. Incidence
	3. Out-of-home duration vs. log_10(Incidence)
	4. Out-of-home duration vs. Hositalization incidence
	5. Out-of-home duration vs. ICU Cases/100000
	6. Out-of-home duration vs. Deaths incidence
	7. Out-of-home duration vs. Maximum temperature (population-weighted average across fed. states)
	8. Out-of-home duration vs. Precipitation (in mm, population-weighted average across fed. states)
	9. Out-of-home duration vs. School holidays (population-weighted average across fed. states)
	10. Out-of-home duration vs. Public holidays (population-weighted average across fed. states)

- **FirstAnalysisFedStates.R** : Data is considered on a weekly/weekend only/weekday only level. Analysis is conducted until a) 2021-01-01 and b) 2023-01-01. Necessary mobility is read in,  4 plots are produced: 
	1. Absolute out-of-home duration over time, 
	2. Percentage change of the out-of-home duration compared to before the outbreak of COVID-19, 
	3. A time series boxplot depicting the variance across federal states, 
	4. A second time series boxplot depicting the variance across federal states, panels are created depending on reigning political parties

- **CorrelationAnalysisFedStates.R** : Data is considered on the German federal state and national level (for disease indicators), on the German federal state level (out-of-home duration, weather indicators) and until 2023-01-01. We consider 7-day-incidence data (provided by RKI), hospitalization data (provided by RKI), covid-related deaths (provided by RKI), the out-of-home duration (in hrs/person) as a mobility indicator, maximum temperature data, precipitation data. Data is read in, cleaned, and aggregated on a weekly level. The following correlation plots are produced:
	1. Out-of-home duration vs. (approx) federal effective R-value
	2. Out-of-home duration vs. (approx) national effective R-value
	3. Out-of-home duration vs. federal Incidence
	4. Out-of-home duration vs. national Incidence
	5. Out-of-home duration vs. federal log_10(Incidence)
	6. Out-of-home duration vs. national log_10(Incidence)
	7. Out-of-home duration vs. federal Hositalization incidence
	8. Out-of-home duration vs. national Hositalization incidence
	9. ToDo: Out-of-home duration vs. federal ICU cases/100,000
	10. ToDo: Out-of-home duration vs. national ICU cases/100,000
	11. Out-of-home duration vs. federal Deaths incidence
	12. Out-of-home duration vs. national Deaths incidence
	13. Out-of-home duration vs. federal Maximum temperature
	14. Out-of-home duration vs. federal Precipitation (in mm)
	15. Out-of-home duration vs. federal school vacations
	16. Out-of-home duration vs. federal public holidays
	
- **FirstAnalysisCounties.R**: Data is considered on a weekly/weekend only/weekday only level. Analysis is conducted until a) 2021-01-01 and b) 2023-01-01. Necessary mobility is read in,  3 plots are produced: 
	- 1) Absolute out-of-home duration over time, 
	- 2) Percentage change of the out-of-home duration compared to before the outbreak of COVID-19, 
	- 3) A time series boxplot depicting the variance across counties, for each federal state a panel is created

- **AnalysisByWave.R** : ToDo

## Directories
