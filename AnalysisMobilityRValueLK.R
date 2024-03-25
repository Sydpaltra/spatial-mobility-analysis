library(tidyverse)

RKIIncidence <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")
RKIIncidence <- RKIIncidence %>% filter(Meldedatum < "2023-01-01") %>%
                mutate(week = isoweek(Meldedatum)) %>%
                mutate(year = year(Meldedatum)) %>%
                mutate(weekday = weekdays(Meldedatum))


LandkreisID <- read_csv("https://raw.githubusercontent.com/entorb/twitter-gov-accounts/main/data/DE-Landkreise-in.csv")
LandkreisID <- LandkreisID %>% select(c(LK_ID, LK_Name))

RKIIncidence <- left_join(RKIIncidence, LandkreisID, join_by(Landkreis_id == LK_ID))
RKIIncidence <- RKIIncidence %>% group_by(LK_Name) %>% mutate(approxR = lead(`Inzidenz_7-Tage`, 4)/`Inzidenz_7-Tage`)

RKIIncidence <- RKIIncidence %>% group_by(LK_Name, week, year) %>% summarise(`Inzidenz_7-Tage`=`Inzidenz_7-Tage`, approxR = mean(approxR, na.rm = TRUE), Meldedatum = max(Meldedatum))

colnames(dfLK)[1] <- "Meldedatum"
colnames(dfLK)[2] <- "LK_Name"
RKIIncidence <- left_join(RKIIncidence, dfLK, by = c("Meldedatum", "LK_Name")) %>% 
                filter(Meldedatum > "2020-03-01") %>%
                filter(Meldedatum < "2023-01-01")

RKIIncidence <- RKIIncidence %>% group_by(Meldedatum, Bundesland) %>% 
                                summarise(`Inzidenz_7-Tage`=`Inzidenz_7-Tage`, outOfHomeDuration = outOfHomeDuration, percentageChangeComparedToBeforeCorona = percentageChangeComparedToBeforeCorona, meanPercChange = mean(percentageChangeComparedToBeforeCorona, na.rm=TRUE), LK_Name = LK_Name, approxR = approxR, meanIncidence = mean(`Inzidenz_7-Tage`), na.rm=TRUE, meanapproxR = mean(approxR, na.rm=TRUE), meanoOH = mean(outOfHomeDuration, na.rm = TRUE)) %>%
                                mutate(AbweichungR = approxR - meanapproxR) %>%
                                mutate(AbweichungIncidence = `Inzidenz_7-Tage` - meanIncidence) %>%
                                mutate(AbweichungoOH = outOfHomeDuration - meanoOH) %>%
                                mutate(AbweichungPercChange = percentageChangeComparedToBeforeCorona - meanPercChange)

ggplot(RKIIncidence %>% filter(Bundesland == "Bayern") %>% filter(approxR < 1.5) %>% filter(Meldedatum > "2020-04-01")) +
geom_point(aes(y = AbweichungPercChange, x = AbweichungIncidence, color=LK_Name)) +
theme_minimal() +
theme(legend.position = "none") +
theme(text = element_text(size = 25)) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("PercChange - mean(percChange)") +
xlab("Incidence - mean(Incidence)")

ggplot(RKIIncidence %>% filter(Bundesland == "Bayern") %>% filter(approxR < 1.5) %>% filter(Meldedatum > "2020-04-01") %>% filter(Meldedatum<"2021-01-01")) +
geom_point(aes(x = AbweichungR, y = outOfHomeDuration), alpha = 0.3) +
theme_minimal() +
theme(text = element_text(size = 25)) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("OutOfHomeDuration") +
xlab("Approximation of R_eff - mean(approx of R_eff)")

ggplot(RKIIncidence %>% filter(Bundesland == "Hamburg") %>% filter(approxR < 1.5) %>% filter(Meldedatum > "2020-04-01") %>% filter(Meldedatum<"2021-01-01")) +
geom_point(aes(x = approxR, y = outOfHomeDuration), alpha = 0.3) +
theme_minimal() +
theme(text = element_text(size = 25)) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("OutOfHomeDuration") +
xlab("Approximation of R_eff")

