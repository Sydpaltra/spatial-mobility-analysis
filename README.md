# Spatial-Mobility-Analysis

# About

The goal of this repository is to analyse mobility data collected between 2020 and 2022 in Germany. Data is available down to the post code level. Analysis is performed on the national, federal state, and post code level, and focuses on changes over time, differences between geographical regions and between urban and rural areas. This work is conducted as part of TU Berlin's contribution to the MODUS-Covid and infoXpand project.

## Files

- **FirstAnalysisFedStates.R** : Data is considered on a weekly/weekend only/weekday only level. Analysis is conducted until a) 2021-01-01 and b) 2023-01-01. Necessary mobility is read in,  4 plots are produced: 
	- 1) Absolute out-of-home duration over time, 
	- 2) Percentage change of the out-of-home duration compared to before the outbreak of COVID-19, 
	- 3) A time series boxplot depicting the variance across federal states, 
	- 4) A second time series boxplot depicting the variance across federal states, panels are created depending on reigning political parties
	
- **FirstAnalysisCounties.R**: Data is considered on a weekly/weekend only/weekday only level. Analysis is conducted until a) 2021-01-01 and b) 2023-01-01. Necessary mobility is read in,  3 plots are produced: 
	- 1) Absolute out-of-home duration over time, 
	- 2) Percentage change of the out-of-home duration compared to before the outbreak of COVID-19, 
	- 3) A time series boxplot depicting the variance across counties, for each federal state a panel is created


## Directories
