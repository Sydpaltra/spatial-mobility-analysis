library(tidyverse)
library(gridExtra)
library(ggiraphExtra)

### NATIONAL LEVEL

# Reading in + preparing incidence data
RKIIncidence <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv")
RKIIncidence <- RKIIncidence %>% filter(Meldedatum < "2023-01-01") %>%
                mutate(week = isoweek(Meldedatum)) %>%
                mutate(year = year(Meldedatum)) %>%
                mutate(weekday = weekdays(Meldedatum)) %>%
                filter(Altersgruppe == "00+") %>%
                mutate(approxR = lead(`Inzidenz_7-Tage`, 4)/`Inzidenz_7-Tage`)

#RKIIncidence <- RKIIncidence %>% group_by(week, year) %>% summarise(Incidence = mean(`Inzidenz_7-Tage`, na.rm=TRUE), approxR = mean(approxR, na.rm = TRUE), Meldedatum = max(Meldedatum))
RKIIncidence <- RKIIncidence %>% filter(weekday == "Sunday")

RKIIncidence <- RKIIncidence %>% ungroup()
RKIIncidence <- RKIIncidence[order(RKIIncidence$Meldedatum), ]

RKIIncidence <- RKIIncidence %>% mutate(leadR = lead(RKIIncidence$approxR)) %>%
                                mutate(leadR2 = lead(RKIIncidence$approxR, 2)) %>%
                                mutate(leadR3 = lead(RKIIncidence$approxR, 3)) %>%
                                mutate(leadR4 = lead(RKIIncidence$approxR, 4))

RKIIncidence <- RKIIncidence %>% mutate(leadIncidence = lead(RKIIncidence$`Inzidenz_7-Tage`)) %>%
                                mutate(leadIncidence2 = lead(RKIIncidence$`Inzidenz_7-Tage`, 2)) %>%
                                mutate(leadIncidence3 = lead(RKIIncidence$`Inzidenz_7-Tage`, 3)) %>%
                                mutate(leadIncidence4 = lead(RKIIncidence$`Inzidenz_7-Tage`, 4))

RKIIncidence <- RKIIncidence %>% mutate(logInc = log10(`Inzidenz_7-Tage`)) %>%
                                mutate(logInclead = log10(leadIncidence)) %>%
                                mutate(logInclead2 = log10(leadIncidence2)) %>%
                                mutate(logInclead3 = log10(leadIncidence3)) %>%
                                mutate(logInclead4 = log10(leadIncidence4))

# Reading in + preparing hospitalization data
RKIHospitals <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/master/Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv")
RKIHospitals <- RKIHospitals %>% filter(Altersgruppe == "00+") %>%
                             filter(Bundesland == "Bundesgebiet") %>%
                             mutate(weekday = weekdays(Datum))
colnames(RKIHospitals)[1] <- "Meldedatum"
RKIHospitals <- RKIHospitals %>% filter(weekday == "Sunday") %>%
                select(c("Meldedatum", "7T_Hospitalisierung_Inzidenz"))

RKIIncidence <- left_join(RKIIncidence, RKIHospitals)

RKIIncidence <- RKIIncidence[order(RKIIncidence$Meldedatum), ]

RKIIncidence <- RKIIncidence %>% mutate(leadHospitals = lead(RKIIncidence$`7T_Hospitalisierung_Inzidenz`)) %>%
                                mutate(leadHospitals2 = lead(RKIIncidence$`7T_Hospitalisierung_Inzidenz`, 2)) %>%
                                mutate(leadHospitals3 = lead(RKIIncidence$`7T_Hospitalisierung_Inzidenz`, 3)) %>%
                                mutate(leadHospitals4 = lead(RKIIncidence$`7T_Hospitalisierung_Inzidenz`, 4))

# Reading in + prepating ICU data
RKIICU <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/Intensivkapazitaeten_und_COVID-19-Intensivbettenbelegung_in_Deutschland/main/Intensivregister_Deutschland_Kapazitaeten.csv")
RKIICU <- RKIICU %>% filter(behandlungsgruppe == "Erwachsene") %>% 
                    mutate(year = year(datum)) %>%
                    mutate(week = isoweek(datum)) %>% 
                    filter(datum < "2023-01-01")
RKIICU <- RKIICU %>% group_by(year, week) %>% summarise(datum = max(datum), ICUFaelle = sum(faelle_covid_aktuell), ICUIncidence = ICUFaelle/83200000*100000) %>% 
                    ungroup()

RKIIncidence <- left_join(RKIIncidence, RKIICU)

RKIIncidence <- RKIIncidence %>% mutate(leadICU= lead(RKIIncidence$ICUIncidence)) %>%
                                mutate(leadICU2 = lead(RKIIncidence$ICUIncidence, 2)) %>%
                                mutate(leadICU3 = lead(RKIIncidence$ICUIncidence, 3)) %>%
                                mutate(leadICU4 = lead(RKIIncidence$ICUIncidence, 4))

# Reading in + preparing data on deaths
RKIDeaths <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Todesfaelle_in_Deutschland/main/COVID-19-Todesfaelle_Deutschland.csv")
RKIDeaths <- RKIDeaths %>% filter(Berichtsdatum < "2023-01-01") %>%
                mutate(week = isoweek(Berichtsdatum)) %>%
                mutate(year = year(Berichtsdatum)) %>%
                mutate(weekday = weekdays(Berichtsdatum))

RKIDeaths <- RKIDeaths %>% group_by(week, year) %>% summarise(Meldedatum = max(Berichtsdatum), NeueTodesfaelle = sum(Todesfaelle_neu))

RKIDeaths <- RKIDeaths %>% ungroup() %>%
                            mutate(DeathsIncidence = NeueTodesfaelle/83200000*100000)

RKIIncidence <- left_join(RKIIncidence, RKIDeaths)
RKIIncidence <- RKIIncidence[order(RKIIncidence$Meldedatum), ]
RKIIncidence <- RKIIncidence %>% mutate(leadDeaths = lead(DeathsIncidence)) %>%
                                mutate(leadDeaths2 = lead(DeathsIncidence, 2)) %>%
                                mutate(leadDeaths3 = lead(DeathsIncidence, 3)) %>%
                                mutate(leadDeaths4 = lead(DeathsIncidence, 4))

# Reading in + preparing mobility data
MobilityGermany <- read_delim("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekly.csv", delim = ";")
MobilityGermany$date <- as.character(MobilityGermany$date)
MobilityGermany$date  <- paste0(str_sub(MobilityGermany$date, 5, 6), "-", str_sub(MobilityGermany$date, 7, 8), "-", str_sub(MobilityGermany$date, 1, 4))
MobilityGermany$date <- as.Date(MobilityGermany$date, "%m-%d-%Y")
MobilityGermany <- MobilityGermany %>% filter(BundeslandID == "Deutschland")

colnames(MobilityGermany)[1] <- "Meldedatum"
RKIIncidence <- left_join(RKIIncidence, MobilityGermany, by = "Meldedatum")

# Reading in + preparing weather data
Weather <- read_csv("/Users/sydney/git/spatial-mobility-analysis/Data/weatherDataGermany.csv")
colnames(Weather)[1] <- "Meldedatum"

RKIIncidence <- left_join(RKIIncidence, Weather)
RKIIncidence <- RKIIncidence %>% mutate(leadTmax = lead(tmax)) %>%
                                mutate(leadTmax2 = lead(tmax, 2)) %>%
                                mutate(leadTmax3 = lead(tmax, 3)) %>%
                                mutate(leadTmax4 = lead(tmax, 4))
RKIIncidence <- RKIIncidence %>% mutate(leadPrcp = lead(prcp)) %>%
                                mutate(leadPrcp2 = lead(prcp, 2)) %>%
                                mutate(leadPrcp3 = lead(prcp, 3)) %>%
                                mutate(leadPrcp4 = lead(prcp, 4))

RKIIncidence <- RKIIncidence %>% filter(Meldedatum > "2020-03-15") %>%
                                    filter(Meldedatum < "2021-01-01")

RKIIncidence <- RKIIncidence %>% mutate(wave = case_when(Meldedatum < "2020-05-15" ~ "1st wave (spring 20)",
                                                            Meldedatum < "2020-09-20" ~ "Summer break",
                                                            .default = "winter 20/21"))

# Reading in + prepating school vacation data
SchoolVac <- read_csv("/Users/sydney/git/spatial-mobility-analysis/data/school_vacations/school_vacations_weekly_nat.csv")
colnames(SchoolVac)[1] <- "Meldedatum"

RKIIncidence <- left_join(RKIIncidence, SchoolVac)
RKIIncidence <- RKIIncidence %>% mutate(leadSchoolVac = lead(schoolVacation)) %>%
                                mutate(leadSchoolVac2 = lead(schoolVacation, 2)) %>%
                                mutate(leadSchoolVac3 = lead(schoolVacation, 3)) %>%
                                mutate(leadSchoolVac4 = lead(schoolVacation, 4))

# Reading in + preparing public holiday
PublicHol <- read_csv("/Users/sydney/git/spatial-mobility-analysis/data/public_holiday/public_holidays_weekly_nat.csv")
colnames(PublicHol)[1] <- "Meldedatum"

RKIIncidence <- left_join(RKIIncidence, PublicHol)
RKIIncidence <- RKIIncidence %>% mutate(leadpubHoliday = lead(pubHoliday)) %>%
                                mutate(leadpubHoliday2 = lead(pubHoliday, 2)) %>%
                                mutate(leadpubHoliday3 = lead(pubHoliday, 3)) %>%
                                mutate(leadpubHoliday4 = lead(pubHoliday, 4))

# Out Of Home Duration vs approximation of R-value
pR <- ggplot(data = RKIIncidence) +
geom_point(aes(x = approxR, y = outOfHomeDuration, colour = wave), size = 2.5) +
theme_minimal() +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("outOfHomeDuration") +
xlab("Approximation of R_eff")

cor_df <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(cor_df) <- c("Variable", "Correlation", "Lead")
cor_df[nrow(cor_df) + 1, ] <- c("R", cor(RKIIncidence$approxR, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 0)
cor_df[nrow(cor_df) + 1, ] <- c("R", cor(RKIIncidence$leadR, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 1)
cor_df[nrow(cor_df) + 1, ] <- c("R", cor(RKIIncidence$leadR2, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 2)
cor_df[nrow(cor_df) + 1, ] <- c("R", cor(RKIIncidence$leadR3, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 3)
cor_df[nrow(cor_df) + 1, ] <- c("R", cor(RKIIncidence$leadR4, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 4)

# Out Of Home Duration vs Incidence
pInc <- ggplot(data = RKIIncidence) +
geom_point(aes(x = `Inzidenz_7-Tage`, y = outOfHomeDuration, colour = wave), size = 2.5) +
theme_minimal() +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("outOfHomeDuration") +
xlab("7 Day Incidence")
cor_df[nrow(cor_df) + 1, ] <- c("Incidence", cor(RKIIncidence$`Inzidenz_7-Tage`, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 0)
cor_df[nrow(cor_df) + 1, ] <- c("Incidence", cor(RKIIncidence$leadIncidence, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 1)
cor_df[nrow(cor_df) + 1, ] <- c("Incidence", cor(RKIIncidence$leadIncidence2, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 2)
cor_df[nrow(cor_df) + 1, ] <- c ("Incidence", cor(RKIIncidence$leadIncidence3, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 3)
cor_df[nrow(cor_df) + 1, ] <- c("Incidence", cor(RKIIncidence$leadIncidence4, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 4)

# Out Of Home Duration vs log(Incidence)
plogInc <- ggplot(data = RKIIncidence) +
geom_point(aes(x = logInc, y = outOfHomeDuration, colour = wave), size = 2.5) +
theme_minimal() +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("outOfHomeDuration") +
xlab("log(7 Day Incidence)")
cor_df[nrow(cor_df) + 1, ] <- c("logIncidence", cor(RKIIncidence$logInc, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 0)
cor_df[nrow(cor_df) + 1, ] <- c("logIncidence", cor(RKIIncidence$logInclead, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 1)
cor_df[nrow(cor_df) + 1, ] <- c("logIncidence", cor(RKIIncidence$logInclead2, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 2)
cor_df[nrow(cor_df) + 1, ] <- c("logIncidence", cor(RKIIncidence$logInclead3, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 3)
cor_df[nrow(cor_df) + 1, ] <- c("logIncidence", cor(RKIIncidence$logInclead4, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 4)


# Out Of Home Duration vs Hospital incidence
pHos <- ggplot(data = RKIIncidence) +
geom_point(aes(x = `7T_Hospitalisierung_Inzidenz`, y = outOfHomeDuration, colour = wave), size = 2.5) +
theme_minimal() +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("outOfHomeDuration") +
xlab("Hospitalization Incidence")
cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidence", cor(RKIIncidence$`7T_Hospitalisierung_Inzidenz`, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 0)
cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidence", cor(RKIIncidence$leadHospitals, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 1)
cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidence", cor(RKIIncidence$leadHospitals2, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 2)
cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidence", cor(RKIIncidence$leadHospitals3, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 3)
cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidence", cor(RKIIncidence$leadHospitals4, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 4)

# Out Of Home Duration vs Hospital incidence
pICU <- ggplot(data = RKIIncidence) +
geom_point(aes(x = ICUIncidence, y = outOfHomeDuration, colour = wave), size = 2.5) +
theme_minimal() +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
#labs(caption="Careful: ICU is given in cases/100,000 NOT in new(!) cases/100,000") +
ylab("outOfHomeDuration") +
xlab("ICU Belegung(!)/100.000")
cor_df[nrow(cor_df) + 1, ] <- c("ICUIncidence", cor(RKIIncidence$ICUIncidence, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 0)
cor_df[nrow(cor_df) + 1, ] <- c("ICUIncidence", cor(RKIIncidence$leadICU, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 1)
cor_df[nrow(cor_df) + 1, ] <- c("ICUIncidence", cor(RKIIncidence$leadICU2, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 2)
cor_df[nrow(cor_df) + 1, ] <- c("ICUIncidence", cor(RKIIncidence$leadICU3, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 3)
cor_df[nrow(cor_df) + 1, ] <- c("ICUIncidence", cor(RKIIncidence$leadICU4, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 4)


# Out Of Home Duration vs Death incidence
pD <- ggplot(data = RKIIncidence) +
geom_point(aes(x = DeathsIncidence, y = outOfHomeDuration, colour = wave), size = 2.5) +
theme_minimal() +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("outOfHomeDuration") +
xlab("Deaths Incidence")
cor_df[nrow(cor_df) + 1, ] <-c("DeathIncidence", cor(RKIIncidence$DeathsIncidence, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 0)
cor_df[nrow(cor_df) + 1, ] <- c("DeathIncidence", cor(RKIIncidence$leadDeaths, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 1)
cor_df[nrow(cor_df) + 1, ] <- c("DeathIncidence", cor(RKIIncidence$leadDeaths2, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 2)
cor_df[nrow(cor_df) + 1, ] <- c("DeathIncidence", cor(RKIIncidence$leadDeaths3, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 3)
cor_df[nrow(cor_df) + 1, ] <- c("DeathIncidence", cor(RKIIncidence$leadDeaths4, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 4)

# Out Of Home Duration vs maximum Temp
pTemp <- ggplot(data = RKIIncidence) +
geom_point(aes(x = tmax, y = outOfHomeDuration, color = wave), size = 2.5) +
theme_minimal() +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("outOfHomeDuration") +
xlab("Tmax [C°]")
cor_df[nrow(cor_df) + 1, ] <- c("Temp", cor(RKIIncidence$tmax, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 0)
cor_df[nrow(cor_df) + 1, ] <- c("Temp", cor(RKIIncidence$leadTmax, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 1)
cor_df[nrow(cor_df) + 1, ] <- c("Temp", cor(RKIIncidence$leadTmax2, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 2)
cor_df[nrow(cor_df) + 1, ] <- c("Temp", cor(RKIIncidence$leadTmax3, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 3)
cor_df[nrow(cor_df) + 1, ] <- c("Temp", cor(RKIIncidence$leadTmax4, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 4)

# Out of home duration vs. Precipitation
pPrcp <- ggplot(data = RKIIncidence) +
geom_point(aes(x = prcp, y = outOfHomeDuration, color = wave), size = 2.5) +
theme_minimal() +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("outOfHomeDuration") +
xlab("Prcp [mm]")
cor_df[nrow(cor_df) + 1, ] <- c("Prcp", cor(RKIIncidence$prcp, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 0)
cor_df[nrow(cor_df) + 1, ] <- c("Prcp", cor(RKIIncidence$leadPrcp, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 1)
cor_df[nrow(cor_df) + 1, ] <- c("Prcp", cor(RKIIncidence$leadPrcp2, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 2)
cor_df[nrow(cor_df) + 1, ] <- c("Prcp", cor(RKIIncidence$leadPrcp3, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 3)
cor_df[nrow(cor_df) + 1, ] <- c("Prcp", cor(RKIIncidence$leadPrcp4, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 4)

# Out-of-home duration vs. School vacation
pSchool <- ggplot(data = RKIIncidence) +
geom_point(aes(x = schoolVacation, y = outOfHomeDuration, color = wave), size = 2.5) +
theme_minimal() +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("outOfHomeDuration") +
xlab("School Vacation")
cor_df[nrow(cor_df) + 1, ] <- c("SchoolVac", cor(RKIIncidence$schoolVacation, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 0)
cor_df[nrow(cor_df) + 1, ] <- c("SchoolVac", cor(RKIIncidence$leadSchoolVac, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 1)
cor_df[nrow(cor_df) + 1, ] <- c("SchoolVac", cor(RKIIncidence$leadSchoolVac2, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 2)
cor_df[nrow(cor_df) + 1, ] <- c("SchoolVac", cor(RKIIncidence$leadSchoolVac3, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 3)
cor_df[nrow(cor_df) + 1, ] <- c("SchoolVac", cor(RKIIncidence$leadSchoolVac4, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 4)

# Out-of-home duration vs. public holidays
pPub <- ggplot(data = RKIIncidence) +
geom_point(aes(x = pubHoliday, y = outOfHomeDuration, color = wave), size = 2.5) +
theme_minimal() +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
ylab("outOfHomeDuration") +
xlab("Public Holiday")
cor_df[nrow(cor_df) + 1, ] <- c("PublicHol", cor(RKIIncidence$pubHoliday, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 0)
cor_df[nrow(cor_df) + 1, ] <- c("PublicHol", cor(RKIIncidence$leadpubHoliday, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 1)
cor_df[nrow(cor_df) + 1, ] <- c("PublicHol", cor(RKIIncidence$leadpubHoliday2, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 2)
cor_df[nrow(cor_df) + 1, ] <- c("PublicHol", cor(RKIIncidence$leadpubHoliday3, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 3)
cor_df[nrow(cor_df) + 1, ] <- c("PublicHol", cor(RKIIncidence$leadpubHoliday4, RKIIncidence$outOfHomeDuration, "pairwise.complete.obs"), 4)


PlotCorrelations <- arrangeGrob(pR, pInc, plogInc, pHos, pICU, pD, pTemp, pPrcp, pSchool, pPub, nrow = 5)
ggsave("CorrelationPlotNat.pdf", PlotCorrelations, dpi = 500, w = 17, h = 22)
ggsave("CorrelationPlotNat.png", PlotCorrelations, dpi = 500, w = 17, h = 22)


