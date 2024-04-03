library(tidyverse)
library(MMWRweek)
library(readxl)

### COUNTIES

# Reading in + preparing incidence data on national level
RKIIncidenceNational <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv")
RKIIncidenceNational <- RKIIncidenceNational %>% filter(Meldedatum < "2023-01-01") %>%
                mutate(week = isoweek(Meldedatum)) %>%
                mutate(year = year(Meldedatum)) %>%
                mutate(weekday = weekdays(Meldedatum)) %>%
                filter(Altersgruppe == "00+") %>%
                mutate(approxR = lead(`Inzidenz_7-Tage`, 4)/`Inzidenz_7-Tage`)

#RKIIncidenceNational <- RKIIncidenceNational %>% group_by(week, year) %>% summarise(Incidence = mean(`Inzidenz_7-Tage`, na.rm=TRUE), approxR = mean(approxR, na.rm = TRUE), Meldedatum = max(Meldedatum))
RKIIncidenceNational <- RKIIncidenceNational %>% filter(weekday == "Sunday")

RKIIncidenceNational <- RKIIncidenceNational %>% ungroup()
RKIIncidenceNational <- RKIIncidenceNational[order(RKIIncidenceNational$Meldedatum), ]

RKIIncidenceNational <- RKIIncidenceNational %>% mutate(leadR = lead(RKIIncidenceNational$approxR)) %>%
                                mutate(leadR2 = lead(RKIIncidenceNational$approxR, 2)) %>%
                                mutate(leadR3 = lead(RKIIncidenceNational$approxR, 3)) %>%
                                mutate(leadR4 = lead(RKIIncidenceNational$approxR, 4))
colnames(RKIIncidenceNational)[7] <- "Incidence"
RKIIncidenceNational <- RKIIncidenceNational %>% mutate(leadIncidence = lead(RKIIncidenceNational$Incidence)) %>%
                                mutate(leadIncidence2 = lead(RKIIncidenceNational$Incidence, 2)) %>%
                                mutate(leadIncidence3 = lead(RKIIncidenceNational$Incidence, 3)) %>%
                                mutate(leadIncidence4 = lead(RKIIncidenceNational$Incidence, 4))

RKIIncidenceNational <- RKIIncidenceNational %>% mutate(logIncidence = log10(Incidence)) %>%
                                mutate(leadlogIncidence = log10(leadIncidence)) %>%
                                mutate(leadlogIncidence2 = log10(leadIncidence2)) %>%
                                mutate(leadlogIncidence3 = log10(leadIncidence3)) %>%
                                mutate(leadlogIncidence4 = log10(leadIncidence4))


# Reading in + preparing hospitalization data on national level
RKIHospitals <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/master/Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv")
RKIHospitals <- RKIHospitals %>% filter(Altersgruppe == "00+") %>%
                             filter(Bundesland == "Bundesgebiet") %>%
                             mutate(weekday = weekdays(Datum))
colnames(RKIHospitals)[1] <- "Meldedatum"
RKIHospitals <- RKIHospitals %>% filter(weekday == "Sunday") %>%
                select(c("Meldedatum", "7T_Hospitalisierung_Inzidenz"))
colnames(RKIHospitals)[2] <- "Hospitals"
RKIIncidenceNational <- left_join(RKIIncidenceNational, RKIHospitals)

RKIIncidenceNational <- RKIIncidenceNational[order(RKIIncidenceNational$Meldedatum), ]

RKIIncidenceNational <- RKIIncidenceNational %>% mutate(leadHospitals = lead(RKIIncidenceNational$Hospitals)) %>%
                                mutate(leadHospitals2 = lead(RKIIncidenceNational$Hospitals, 2)) %>%
                                mutate(leadHospitals3 = lead(RKIIncidenceNational$Hospitals, 3)) %>%
                                mutate(leadHospitals4 = lead(RKIIncidenceNational$Hospitals, 4))

# Reading in + preparing data on deaths on national level
RKIDeaths <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Todesfaelle_in_Deutschland/main/COVID-19-Todesfaelle_Deutschland.csv")
RKIDeaths <- RKIDeaths %>% filter(Berichtsdatum < "2023-01-01") %>%
                mutate(week = isoweek(Berichtsdatum)) %>%
                mutate(year = year(Berichtsdatum)) %>%
                mutate(weekday = weekdays(Berichtsdatum))

RKIDeaths <- RKIDeaths %>% group_by(week, year) %>% summarise(Meldedatum = max(Berichtsdatum), NeueTodesfaelle = sum(Todesfaelle_neu))

RKIDeaths <- RKIDeaths %>% ungroup() %>%
                            mutate(DeathsIncidence = NeueTodesfaelle/83200000*100000)

RKIIncidenceNational <- left_join(RKIIncidenceNational, RKIDeaths)
RKIIncidenceNational <- RKIIncidenceNational[order(RKIIncidenceNational$Meldedatum), ]
RKIIncidenceNational <- RKIIncidenceNational %>% mutate(leadDeaths = lead(DeathsIncidence)) %>%
                                mutate(leadDeaths2 = lead(DeathsIncidence, 2)) %>%
                                mutate(leadDeaths3 = lead(DeathsIncidence, 3)) %>%
                                mutate(leadDeaths4 = lead(DeathsIncidence, 4))

#Reading in this data set which maps counties to federal states
MapLKBL <- read_csv2("/Users/sydney/Downloads/PLZ_Ort_Landkreis_Bundesland_Mapping_DE.csv", col_types = cols(.default = "c"))
for(row in 1:nrow(MapLKBL)){
  if(nchar(MapLKBL$PLZ[row]) == 4){
    MapLKBL[row, 1] = paste0(0, MapLKBL[row, 1], sep = "")
  }
}
MapLKBL <- MapLKBL %>% select(Landkreis, Bundesland) %>% distinct()

#Reading in data set that maps county id to county name
url <- "https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/04-kreise.xlsx?__blob=publicationFile"
destfile <- "X04_kreise.xlsx"
curl::curl_download(url, destfile)
MapLKIDName <- read_excel(destfile, col_names = FALSE, sheet = 2, skip = 8)
colnames(MapLKIDName) <- c("Landkreis_id", "RegionaleBezeichnung", "Landkreis", "NUTS3", "Flaeche", "Bevoelkerung", "BevoelkerungMaennlich", "BevoelkerungWeiblich", "BevoelkerungJeKm2")
MapLKIDName <- na.omit(MapLKIDName)
MapLKIDName <- MapLKIDName %>% select(Landkreis_id, Landkreis, Flaeche, Bevoelkerung, BevoelkerungJeKm2) %>% distinct()

MapLKIDName <- MapLKIDName %>% mutate(Landkreis = str_remove_all(Landkreis, ", Stadt|, Landeshauptstadt|, Hansestadt|, Freie und Hansestadt| a.d. Ilm| a.d.Waldnaab| i.d.OPf."))

MapLKIDName <- left_join(MapLKIDName, MapPLZBL)

cor_df <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(cor_df) <- c("Variable", "Correlation", "Lead", "FedState")

for(county in counties){
        #Reading in incidence data on county level
        RKIIncidenceCounty <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Landkreise.csv")
        RKIIncidenceCounty <- RKIIncidenceCounty %>% filter(Landkreis_id == county) %>% 
                        filter(Meldedatum < "2023-01-01") %>%
                        mutate(week = isoweek(Meldedatum)) %>%
                        mutate(year = year(Meldedatum)) %>%
                        mutate(weekday = weekdays(Meldedatum)) %>%
                        filter(weekday == "Sunday")
        colnames(RKIIncidenceCounty)[7] <- "IncidenceLK"

        RKIIncidenceCounty <- RKIIncidenceCounty[order(RKIIncidenceCounty$Meldedatum), ]
        RKIIncidenceCounty <- RKIIncidenceCounty %>% mutate(approxRLK = lead(IncidenceLK, 4)/IncidenceLK)
        RKIIncidenceCounty <- RKIIncidenceCounty %>% mutate(leadRLK = lead(RKIIncidenceCounty$approxRLK)) %>%
                                        mutate(leadRLK2 = lead(RKIIncidenceCounty$approxRLK, 2)) %>%
                                        mutate(leadRLK3 = lead(RKIIncidenceCounty$approxRLK, 3)) %>%
                                        mutate(leadRLK4 = lead(RKIIncidenceCounty$approxRLK, 4))

        RKIIncidenceCounty <- RKIIncidenceCounty %>% mutate(leadIncidenceLK = lead(RKIIncidenceCounty$IncidenceLK)) %>%
                                        mutate(leadIncidenceLK2 = lead(RKIIncidenceCounty$IncidenceLK, 2)) %>%
                                        mutate(leadIncidenceLK3 = lead(RKIIncidenceCounty$IncidenceLK, 3)) %>%
                                        mutate(leadIncidenceLK4 = lead(RKIIncidenceCounty$IncidenceLK, 4))

        RKIIncidenceCounty <- RKIIncidenceCounty %>% mutate(logIncidenceLK = log10(IncidenceLK)) %>%
                                        mutate(leadlogIncidenceLK = log10(leadIncidenceLK)) %>%
                                        mutate(leadlogIncidenceLK2 = log10(leadIncidenceLK2)) %>%
                                        mutate(leadlogIncidenceLK3 = log10(leadIncidenceLK3)) %>%
                                        mutate(leadlogIncidenceLK4 = log10(leadIncidenceLK4))

        #Reading in incidence data on federal state level
        RKIIncidenceFedState <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Bundeslaender.csv")
        RKIIncidenceFedState <- RKIIncidenceFedState %>% filter(Meldedatum < "2023-01-01") %>%
                        mutate(week = isoweek(Meldedatum)) %>%
                        mutate(year = year(Meldedatum)) %>%
                        mutate(weekday = weekdays(Meldedatum)) %>%
                        filter(Altersgruppe == "00+") %>%
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
                        
        RKIIncidenceFedState <- RKIIncidenceFedState %>% filter(Bundesland == as.character(MapLKIDName[which(MapLKIDName$Landkreis_id == county), 6]))             

        #RKIIncidenceFedState <- RKIFedStateNational %>% group_by(week, year) %>% summarise(Incidence = mean(`Inzidenz_7-Tage`, na.rm=TRUE), approxR = mean(approxR, na.rm = TRUE), Meldedatum = max(Meldedatum))
        RKIIncidenceFedState <- RKIIncidenceFedState %>% filter(weekday == "Sunday")

        RKIIncidenceFedState <- RKIIncidenceFedState %>% ungroup()
        RKIIncidenceFedState <- RKIIncidenceFedState[order(RKIIncidenceFedState$Meldedatum), ]
        
        colnames(RKIIncidenceFedState)[8] <- "IncidenceFed"
        RKIIncidenceFedState <- RKIIncidenceFedState %>% mutate(approxRFed = lead(IncidenceFed, 4)/IncidenceFed)
        RKIIncidenceFedState <- RKIIncidenceFedState %>% mutate(leadRFed = lead(RKIIncidenceFedState$approxRFed)) %>%
                                        mutate(leadRFed2 = lead(RKIIncidenceFedState$approxRFed, 2)) %>%
                                        mutate(leadRFed3 = lead(RKIIncidenceFedState$approxRFed, 3)) %>%
                                        mutate(leadRFed4 = lead(RKIIncidenceFedState$approxRFed, 4))

        RKIIncidenceFedState <- RKIIncidenceFedState %>% mutate(leadIncidenceFed = lead(RKIIncidenceFedState$IncidenceFed)) %>%
                                        mutate(leadIncidenceFed2 = lead(RKIIncidenceFedState$IncidenceFed, 2)) %>%
                                        mutate(leadIncidenceFed3 = lead(RKIIncidenceFedState$IncidenceFed, 3)) %>%
                                        mutate(leadIncidenceFed4 = lead(RKIIncidenceFedState$IncidenceFed, 4))

        RKIIncidenceFedState <- RKIIncidenceFedState %>% mutate(logIncidenceFed = log10(IncidenceFed)) %>%
                                        mutate(leadlogIncidenceFed = log10(leadIncidenceFed)) %>%
                                        mutate(leadlogIncidenceFed2 = log10(leadIncidenceFed2)) %>%
                                        mutate(leadlogIncidenceFed3 = log10(leadIncidenceFed3)) %>%
                                        mutate(leadlogIncidenceFed4 = log10(leadIncidenceFed4))

        RKIIncidenceCounty <- left_join(RKIIncidenceCounty, RKIIncidenceFedState, by = "Meldedatum")

        # Reading in + preparing hospitalization data on fed state level
        RKIHospitalsFedState <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Hospitalisierungen_in_Deutschland/master/Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv")
        RKIHospitalsFedState <- RKIHospitalsFedState %>% filter(Altersgruppe == "00+") %>%
                                filter(Bundesland == as.character(MapLKIDName[which(MapLKIDName$Landkreis_id == county), 6])) %>%
                                mutate(weekday = weekdays(Datum))
        colnames(RKIHospitalsFedState)[1] <- "Meldedatum"
        RKIHospitalsFedState <- RKIHospitalsFedState %>% filter(weekday == "Sunday") %>%
                        select(c("Meldedatum", "7T_Hospitalisierung_Inzidenz"))
        colnames(RKIHospitalsFedState)[2] <- "HospitalFed"

        RKIIncidenceCounty <- left_join(RKIIncidenceCounty, RKIHospitalsFedState, by = "Meldedatum")

        RKIIncidenceCounty <- RKIIncidenceCounty[order(RKIIncidenceCounty$Meldedatum), ]

        RKIIncidenceCounty <- RKIIncidenceCounty %>% mutate(leadHospitalFed = lead(RKIIncidenceCounty$HospitalFed)) %>%
                                        mutate(leadHospitalFed2 = lead(RKIIncidenceCounty$HospitalFed, 2)) %>%
                                        mutate(leadHospitalFed3 = lead(RKIIncidenceCounty$HospitalFed, 3)) %>%
                                        mutate(leadHospitalFed4 = lead(RKIIncidenceCounty$HospitalFed, 4))

        # Reading in + preparing data on deaths on fed state level
        RKIDeathsFed <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Todesfaelle_in_Deutschland/main/COVID-19-Todesfaelle_Bundeslaender.csv")
        RKIDeathsFed <- RKIDeathsFed %>%
                        filter(Region_Name == as.character(MapLKIDName[which(MapLKIDName$Landkreis_id == county), 6])) %>% 
                        mutate(year = str_sub(Datum, 1, 4)) %>%
                        mutate(year = as.numeric(year)) %>%
                        mutate(week = str_sub(Datum, -2, -1)) %>%
                        mutate(week = as.numeric(week)) %>%
                        mutate(Meldedatum = MMWRweek2Date(MMWRyear = year,
                                MMWRweek = week,
                                MMWRday = 1)) %>%
                        mutate(Meldedatum = Meldedatum + 7)

        #RKIDeaths <- RKIDeaths %>% group_by(week, year) %>% summarise(Meldedatum = max(Berichtsdatum), NeueTodesfaelle = sum(Todesfaelle_neu))

        RKIDeathsFed <- RKIDeathsFed %>% mutate(DeathsIncidenceFed = Todesfaelle/83200000*100000)

        RKIIncidenceCounty <- left_join(RKIIncidenceCounty, RKIDeathsFed, by = "Meldedatum")
        RKIIncidenceCounty <- RKIIncidenceCounty[order(RKIIncidenceCounty$Meldedatum), ]
        RKIIncidenceCounty <- RKIIncidenceCounty %>% mutate(leadDeathFed = lead(DeathsIncidenceFed)) %>%
                                        mutate(leadDeathFed2 = lead(DeathsIncidenceFed, 2)) %>%
                                        mutate(leadDeathFed3 = lead(DeathsIncidenceFed, 3)) %>%
                                        mutate(leadDeathFed4 = lead(DeathsIncidenceFed, 4))

        # Reading in + preparing mobility data
        MobilityGermany <- read_delim("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/landkreise/LK_mobilityData_weekly.csv", delim = ";")
        MobilityGermany$date <- as.character(MobilityGermany$date)
        MobilityGermany$date  <- paste0(str_sub(MobilityGermany$date, 5, 6), "-", str_sub(MobilityGermany$date, 7, 8), "-", str_sub(MobilityGermany$date, 1, 4))
        MobilityGermany$date <- as.Date(MobilityGermany$date, "%m-%d-%Y")
        MobilityGermany <- MobilityGermany %>% mutate(Landkreis = str_remove_all(Landkreis, "Landkreis |Kreis |Städteregion "))
        MobilityGermany <- MobilityGermany %>% filter(Landkreis == as.character(MapLKIDName[which(MapLKIDName$Landkreis_id == county), 2]))

        colnames(MobilityGermany)[1] <- "Meldedatum"
        RKIIncidenceCounty <- left_join(RKIIncidenceCounty, MobilityGermany, by = "Meldedatum")

        #RKIIncidenceCounty <- RKIIncidenceCounty %>% filter(BundeslandID == MapLKIDName[which(MapLKIDName$Landkreis_id == county), 6])
        RKIIncidenceCounty <- left_join(RKIIncidenceCounty, RKIIncidenceNational, join_by("Meldedatum"))

        # Reading in + preparing weather data
        # Weather <- read_csv("/Users/sydney/git/spatial-mobility-analysis/Data/weather/weather_weekly_fed.csv")
        # colnames(Weather)[1] <- "Meldedatum"

        # Weather <- Weather %>% filter(country == fedState)

        # RKIIncidenceFedState <- left_join(RKIIncidenceFedState, Weather)
        # RKIIncidenceFedState <- RKIIncidenceFedState[order(RKIIncidenceFedState$Meldedatum), ]

        # RKIIncidenceFedState <- RKIIncidenceFedState %>% mutate(leadTmax = lead(tmax)) %>%
        #                                 mutate(leadTmax2 = lead(tmax, 2)) %>%
        #                                 mutate(leadTmax3 = lead(tmax, 3)) %>%
        #                                 mutate(leadTmax4 = lead(tmax, 4))

        # RKIIncidenceFedState <- RKIIncidenceFedState %>% mutate(leadPrcp = lead(prcp)) %>%
        #                                 mutate(leadPrcp2 = lead(prcp, 2)) %>%
        #                                 mutate(leadPrcp3 = lead(prcp, 3)) %>%
        #                                 mutate(leadPrcp4 = lead(prcp, 4))

        SchoolVac <- read_csv("/Users/sydney/git/spatial-mobility-analysis/data/school_vacations/school_vacations_weekly_fed.csv")
        colnames(SchoolVac)[1] <- "Meldedatum"

        SchoolVac <- SchoolVac %>% filter(federalState == as.character(MapLKIDName[which(MapLKIDName$Landkreis_id == county), 6]))

        RKIIncidenceCounty <- left_join(RKIIncidenceCounty, SchoolVac)
        RKIIncidenceCounty <- RKIIncidenceCounty[order(RKIIncidenceCounty$Meldedatum), ]

        RKIIncidenceCounty <- RKIIncidenceCounty %>% mutate(leadSchoolVac = lead(schoolVacation)) %>%
                                        mutate(leadSchoolVac2 = lead(schoolVacation, 2)) %>%
                                        mutate(leadSchoolVac3 = lead(schoolVacation, 3)) %>%
                                        mutate(leadSchoolVac4 = lead(schoolVacation, 4))

        # Reading in + preparing public holiday
        PublicHol <- read_csv("/Users/sydney/git/spatial-mobility-analysis/data/public_holiday/public_holidays_weekly_fed.csv")
        
        colnames(PublicHol)[1] <- "Meldedatum"
        colnames(PublicHol)[2] <- "federalState"
        PublicHol <- PublicHol %>% filter(federalState == as.character(MapLKIDName[which(MapLKIDName$Landkreis_id == county), 6]))

        RKIIncidenceCounty<- left_join(RKIIncidenceCounty, PublicHol, by = "Meldedatum")
        RKIIncidenceCounty <- RKIIncidenceCounty[order(RKIIncidenceCounty$Meldedatum), ]

        RKIIncidenceCounty <- RKIIncidenceCounty %>% mutate(leadpubHoliday = lead(pubHoliday)) %>%
                                        mutate(leadpubHoliday2 = lead(pubHoliday, 2)) %>%
                                        mutate(leadpubHoliday3 = lead(pubHoliday, 3)) %>%
                                        mutate(leadpubHoliday4 = lead(pubHoliday, 4))

        RKIIncidenceCounty <- RKIIncidenceCounty %>%
                                filter(Meldedatum > "2020-03-15") %>%
                                filter(Meldedatum < "2021-01-01")

        RKIIncidenceCounty <- RKIIncidenceCounty %>% mutate(wave = case_when(Meldedatum < "2020-05-15" ~ "1st wave (spring 20)",
                                                                Meldedatum < "2020-09-20" ~ "Summer break",
                                                                .default = "winter 20/21"))

        # Out Of Home Duration vs approximation of R-value (on county level)
        pRLK <- ggplot(data = RKIIncidenceCounty) +
        geom_point(aes(x = approxRLK, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("Approximation of R_eff (county)")
        cor_df[nrow(cor_df) + 1, ] <- c("RLK", cor(RKIIncidenceFedState$approxRLK, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("RLK", cor(RKIIncidenceFedState$leadRLK, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("RLK", cor(RKIIncidenceFedState$leadRLK2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("RLK", cor(RKIIncidenceFedState$leadRLK3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("RLK", cor(RKIIncidenceFedState$leadRLK4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out Of Home Duration vs approximation of R-value (on fed level)
        pRFed <- ggplot(data = RKIIncidenceCounty) +
        geom_point(aes(x = approxRFed, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("Approximation of R_eff (Fed)")
        cor_df[nrow(cor_df) + 1, ] <- c("RFed", cor(RKIIncidenceFedState$approxRFed, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("RFed", cor(RKIIncidenceFedState$leadRFed, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("RFed", cor(RKIIncidenceFedState$leadRFed2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("RFed", cor(RKIIncidenceFedState$leadRFed3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("RFed", cor(RKIIncidenceFedState$leadRFed4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out Of Home Duration vs approximation of R-value (on nat level)
        pRNat <- ggplot(data = RKIIncidenceCounty) +
        geom_point(aes(x = approxR, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("Approximation of R_eff (Nat)")
        cor_df[nrow(cor_df) + 1, ] <- c("RNat", cor(RKIIncidenceFedState$approxR, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("RNat", cor(RKIIncidenceFedState$leadR, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("RNat", cor(RKIIncidenceFedState$leadR2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("RNat", cor(RKIIncidenceFedState$leadR3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("RNat", cor(RKIIncidenceFedState$leadR4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out Of Home Duration vs Incidence (LK)
        pIncLK <- ggplot(data = RKIIncidenceCounty %>% filter(Meldedatum > "2020-03-15")) +
        geom_point(aes(x = IncidenceLK, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("7 Day Incidence (county)")
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceLK", cor(RKIIncidenceFedState$IncidenceLK, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceLK", cor(RKIIncidenceFedState$leadIncidenceLK, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceLK", cor(RKIIncidenceFedState$leadIncidenceLK2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceLK", cor(RKIIncidenceFedState$leadIncidenceLK3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceLK", cor(RKIIncidenceFedState$leadIncidenceLK4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out Of Home Duration vs Incidence (fed)
        pIncFed <- ggplot(data = RKIIncidenceCounty %>% filter(Meldedatum > "2020-03-15")) +
        geom_point(aes(x = IncidenceFed, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("7 Day Incidence (fed)")
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceFed", cor(RKIIncidenceFedState$IncidenceFed, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceFed", cor(RKIIncidenceFedState$leadIncidenceFed, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceFed", cor(RKIIncidenceFedState$leadIncidenceFed2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceFed", cor(RKIIncidenceFedState$leadIncidenceFed3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceFed", cor(RKIIncidenceFedState$leadIncidenceFed4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out Of Home Duration vs Incidence (nat)
        pIncNat <- ggplot(data = RKIIncidenceCounty %>% filter(Meldedatum > "2020-03-15")) +
        geom_point(aes(x = Incidence, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("7 Day Incidence (nat)")
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceNat", cor(RKIIncidenceFedState$`Inzidenz_7-Tage`, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceNat", cor(RKIIncidenceFedState$leadIncidence, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceNat", cor(RKIIncidenceFedState$leadIncidence2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceNat", cor(RKIIncidenceFedState$leadIncidence3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("IncidenceNat", cor(RKIIncidenceFedState$leadIncidence4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out Of Home Duration vs log(Incidence) (fed)
        plogIncLK <- ggplot(data = RKIIncidenceCounty %>% filter(Meldedatum > "2020-03-15")) +
        geom_point(aes(x = logIncidenceLK, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("log(7 Day Incidence) (county)")
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceLK", cor(RKIIncidenceFedState$logIncidenceLK, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceLK", cor(RKIIncidenceFedState$leadlogIncidenceLK, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceLK", cor(RKIIncidenceFedState$leadlogIncidenceLK2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceLK", cor(RKIIncidenceFedState$leadlogIncidenceLK3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceLK", cor(RKIIncidenceFedState$leadlogIncidenceLK4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out Of Home Duration vs log(Incidence) (fed)
        plogIncFed <- ggplot(data = RKIIncidenceCounty %>% filter(Meldedatum > "2020-03-15")) +
        geom_point(aes(x = logIncidenceFed, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("log(7 Day Incidence) (fed)")
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceFed", cor(RKIIncidenceFedState$logIncidenceFed, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceFed", cor(RKIIncidenceFedState$leadlogIncidenceFed, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceFed", cor(RKIIncidenceFedState$leadlogIncidenceFed2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceFed", cor(RKIIncidenceFedState$leadlogIncidenceFed3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceFed", cor(RKIIncidenceFedState$leadlogIncidenceFed4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out Of Home Duration vs log(Incidence) (nat)
        plogIncNat <- ggplot(data = RKIIncidenceCounty %>% filter(Meldedatum > "2020-03-15")) +
        geom_point(aes(x = logIncidence, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("log(7 Day Incidence) (nat)")
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceNat", cor(RKIIncidenceFedState$logIncidence, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceNat", cor(RKIIncidenceFedState$leadlogIncidence, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceNat", cor(RKIIncidenceFedState$leadlogIncidence, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceNat", cor(RKIIncidenceFedState$leadlogIncidence, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("logIncidenceNat", cor(RKIIncidenceFedState$leadlogIncidence, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out Of Home Duration vs Hospital incidence (fed)
        pHospFed <- ggplot(data = RKIIncidenceCounty) +
        geom_point(aes(x = HospitalFed, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("Hospitalization Incidence (fed)")
        cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidenceFed", cor(RKIIncidenceFedState$HospitalFed, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidenceFed", cor(RKIIncidenceFedState$leadHospitalFed, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidenceFed", cor(RKIIncidenceFedState$leadHospitalFed2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidenceFed", cor(RKIIncidenceFedState$leadHospitalFed3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidenceFed", cor(RKIIncidenceFedState$leadHospitalFed4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out Of Home Duration vs Hospital incidence (nat)
        pHospNat <- ggplot(data = RKIIncidenceCounty) +
        geom_point(aes(x = `7T_Hospitalisierung_Inzidenz`, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("Hospitalization Incidence (nat)")
        cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidenceNat", cor(RKIIncidenceFedState$`7T_Hospitalisierung_Inzidenz`, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidenceNat", cor(RKIIncidenceFedState$leadHospitals, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidenceNat", cor(RKIIncidenceFedState$leadHospitals2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidenceNat", cor(RKIIncidenceFedState$leadHospitals3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("HospitalIncidenceNat", cor(RKIIncidenceFedState$leadHospitals4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out Of Home Duration vs Death incidence (fed)
        pDFed <- ggplot(data = RKIIncidenceCounty) +
        geom_point(aes(x = DeathsIncidenceFed, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("Deaths Incidence (fed)")
        cor_df[nrow(cor_df) + 1, ] <- c("DeathIncidenceFed", cor(RKIIncidenceFedState$DeathsIncidenceFed, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("DeathIncidenceFed", cor(RKIIncidenceFedState$leadDeathFed, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("DeathIncidenceFed", cor(RKIIncidenceFedState$leadDeathFed2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("DeathIncidenceFed", cor(RKIIncidenceFedState$leadDeathFed3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("DeathIncidenceFed", cor(RKIIncidenceFedState$leadDeathFed4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out Of Home Duration vs Death incidence (nat)
        pDNat <- ggplot(data = RKIIncidenceCounty) +
        geom_point(aes(x = DeathsIncidence, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("Deaths Incidence (nat)")
        cor_df[nrow(cor_df) + 1, ] <- c("DeathIncidenceNat", cor(RKIIncidenceFedState$DeathsIncidence, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("DeathIncidenceNat", cor(RKIIncidenceFedState$leadDeaths, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("DeathIncidenceNat", cor(RKIIncidenceFedState$leadDeaths2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("DeathIncidenceNat", cor(RKIIncidenceFedState$leadDeaths3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("DeathIncidenceNat", cor(RKIIncidenceFedState$leadDeaths4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out Of Home Duration vs maximum Temp
        # pTemp <- ggplot(data = RKIIncidenceFedState) +
        # geom_point(aes(x = leadTmax4, y = outOfHomeDuration, color = wave), size = 2.5) +
        # theme_minimal() +
        # theme(text = element_text(size = 25), legend.position = "bottom") +
        # theme(axis.ticks.x = element_line(),
        #                 axis.ticks.y = element_line(),
        #                 axis.ticks.length = unit(5, "pt")) +
        # ylab("outOfHomeDuration") +
        # xlab("Tmax [C°]")
        # cor_df[nrow(cor_df) + 1, ] <- c("Temp", cor(RKIIncidenceFedState$tmax, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        # cor_df[nrow(cor_df) + 1, ] <- c("Temp", cor(RKIIncidenceFedState$leadTmax, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        # cor_df[nrow(cor_df) + 1, ] <- c("Temp", cor(RKIIncidenceFedState$leadTmax2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        # cor_df[nrow(cor_df) + 1, ] <- c("Temp", cor(RKIIncidenceFedState$leadTmax3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        # cor_df[nrow(cor_df) + 1, ] <- c("Temp", cor(RKIIncidenceFedState$leadTmax4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out Of Home Duration vs prcp
        # pPrcp <- ggplot(data = RKIIncidenceFedState) +
        # geom_point(aes(x = leadPrcp4, y = outOfHomeDuration, color = wave), size = 2.5) +
        # theme_minimal() +
        # theme(text = element_text(size = 25), legend.position = "bottom") +
        # theme(axis.ticks.x = element_line(),
        #                 axis.ticks.y = element_line(),
        #                 axis.ticks.length = unit(5, "pt")) +
        # ylab("outOfHomeDuration") +
        # xlab("Precipitation [mm]")
        # cor_df[nrow(cor_df) + 1, ] <- c("Prcp", cor(RKIIncidenceFedState$prcp, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        # cor_df[nrow(cor_df) + 1, ] <- c("Prcp",cor(RKIIncidenceFedState$leadPrcp, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        # cor_df[nrow(cor_df) + 1, ] <- c("Prcp", cor(RKIIncidenceFedState$leadPrcp2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        # cor_df[nrow(cor_df) + 1, ] <- c("Prcp", cor(RKIIncidenceFedState$leadPrcp3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        # cor_df[nrow(cor_df) + 1, ] <- c("Prcp", cor(RKIIncidenceFedState$leadPrcp4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)


        # Out-of-home duration vs. School vacation
        pSchool <- ggplot(data = RKIIncidenceCounty) +
        geom_point(aes(x = schoolVacation, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("School Vacation")
        cor_df[nrow(cor_df) + 1, ] <- c("SchoolVac", cor(RKIIncidenceFedState$schoolVacation, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("SchoolVac", cor(RKIIncidenceFedState$leadSchoolVac, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("SchoolVac", cor(RKIIncidenceFedState$leadSchoolVac2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("SchoolVac", cor(RKIIncidenceFedState$leadSchoolVac3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("SchoolVac", cor(RKIIncidenceFedState$leadSchoolVac4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)

        # Out-of-home duration vs. public holidays
        pPub <- ggplot(data = RKIIncidenceCounty) +
        geom_point(aes(x = pubHoliday, y = outOfHomeDuration, color = wave), size = 2.5) +
        theme_minimal() +
        theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
        theme(axis.ticks.x = element_line(),
                        axis.ticks.y = element_line(),
                        axis.ticks.length = unit(5, "pt")) +
        ylab("outOfHomeDuration") +
        xlab("Public Holiday")
        cor_df[nrow(cor_df) + 1, ] <- c("PublicHol", cor(RKIIncidenceFedState$pubHoliday, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 0, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("PublicHol", cor(RKIIncidenceFedState$leadpubHoliday, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 1, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("PublicHol", cor(RKIIncidenceFedState$leadpubHoliday2, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 2, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("PublicHol", cor(RKIIncidenceFedState$leadpubHoliday3, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 3, fedState)
        cor_df[nrow(cor_df) + 1, ] <- c("PublicHol", cor(RKIIncidenceFedState$leadpubHoliday4, RKIIncidenceFedState$outOfHomeDuration, "pairwise.complete.obs"), 4, fedState)


        PlotCorrelations <- arrangeGrob(pRLK, pRFed, pRNat, pIncLK, pIncFed, pIncNat, plogIncLK, plogIncFed, plogIncNat, pHospFed, pHospNat, pDFed, pDNat, pSchool, pPub, nrow = 5)
    ggsave(paste0("CorrelationPlot", as.character(county), ".pdf"), PlotCorrelations, dpi = 500, w = 27, h = 37)
    ggsave(paste0("CorrelationPlot" , as.character(county), ".png"), PlotCorrelations, dpi = 500, w = 18, h = 31)
}

cor_df$Correlation <- as.double(cor_df$Correlation)
LargestCorrelation <- cor_df %>% group_by(FedState) %>% filter(Correlation == min(Correlation, na.rm=TRUE))
LargestCorrelation <- LargestCorrelation %>% distinct()
