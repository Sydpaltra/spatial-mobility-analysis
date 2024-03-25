library(tidyverse)

RKIIncidence <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Bundeslaender.csv")
RKIIncidence <- RKIIncidence %>% filter(Meldedatum < "2023-01-01") %>%
                mutate(week = isoweek(Meldedatum)) %>%
                mutate(year = year(Meldedatum)) %>%
                mutate(weekday = weekdays(Meldedatum)) %>%
                mutate(Bundesland = case_when(Bundesland_id == "01" ~ "Schleswig-Holstein",
                                            Bundesland_id == "02" ~ "Hamburg",
                                            Bundesland_id == "03" ~ "Niedersachsen",
                                            Bundesland_id == "04" ~ "Bremen",
                                            Bundesland_id == "05" ~ "Nordrhein-Westfalen",
                                            Bundesland_id == "06" ~ "Hessen",
                                            Bundesland_id == "07" ~ "Rheinland-Pfalz",
                                            Bundesland_id == "08" ~ "Baden-Württemberg",
                                            Bundesland_id == "09" ~ "Bayern",
                                            Bundesland_id == "10" ~ "Saarland",
                                            Bundesland_id == "11" ~ "Berlin",
                                            Bundesland_id == "12" ~ "Brandenburg",
                                            Bundesland_id == "13" ~ "Mecklenburg-Vorpommern",
                                            Bundesland_id == "14" ~ "Sachsen",
                                            Bundesland_id == "15" ~ "Sachsen-Anhalt",
                                            Bundesland_id == "16" ~ "Thüringen"))

RKIIncidence <- RKIIncidence %>%
                filter(Altersgruppe == "00+") %>% 
                group_by(Bundesland) %>% 
                mutate(approxR = lead(`Inzidenz_7-Tage`, 4)/`Inzidenz_7-Tage`)

RKIIncidence <- RKIIncidence %>% group_by(Bundesland, week, year) %>% summarise(approxR = mean(approxR, na.rm = TRUE), Meldedatum = max(Meldedatum))

#RKIIncidence <- RKIIncidence %>% ungroup() %>%
#                filter(weekday == "Sunday")

RKIIncidence <- RKIIncidence %>% 
                filter(Meldedatum > "2020-03-01") %>%
                filter(Meldedatum < "2023-01-01")

FedStates <- read_delim("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekly.csv", delim = ";")
FedStates$date <- as.character(FedStates$date)
FedStates$date  <- paste0(str_sub(FedStates$date, 5, 6), "-", str_sub(FedStates$date, 7, 8), "-", str_sub(FedStates$date, 1, 4))
FedStates$date <- as.Date(FedStates$date, "%m-%d-%Y")

colnames(FedStates)[1] <- "Meldedatum"
colnames(FedStates)[2] <- "Bundesland"
RKIIncidence <- left_join(RKIIncidence, FedStates, by = c("Meldedatum", "Bundesland")) 

RKIIncidence <- RKIIncidence %>% group_by(Meldedatum) %>% 
                                summarise(outOfHomeDuration = outOfHomeDuration, Bundesland = Bundesland, approxR = approxR, meanapproxR = mean(approxR, na.rm=TRUE), meanoOH = mean(outOfHomeDuration, na.rm = TRUE)) %>%
                                mutate(AbweichungR = approxR - meanapproxR) %>%
                                mutate(AbweichungoOH = outOfHomeDuration - meanoOH)

ggplot(RKIIncidence %>% filter(approxR < 1.5) %>% filter(Meldedatum<"2021-01-01")) +
geom_point(aes(y = AbweichungoOH, x = approxR)) +
theme_minimal() +
theme(text = element_text(size = 25)) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("OutOfHomeDuration - mean(outOfHomeDuration)") +
xlab("Approximation of R_eff")

ggplot(RKIIncidence %>% filter(approxR < 1.5)) +
geom_point(aes(x = AbweichungR, y = outOfHomeDuration)) +
theme_minimal() +
theme(text = element_text(size = 25)) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("OutOfHomeDuration") +
xlab("Approximation of R_eff - mean(approx of R_eff)")

ggplot(RKIIncidence %>% filter(Meldedatum < "2021-01-01") %>% filter(approxR < 1.5)) +
geom_point(aes(x = approxR, y = outOfHomeDuration)) +
theme_minimal() +
theme(text = element_text(size = 25)) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("OutOfHomeDuration") +
xlab("Approximation of R_eff")

