library(tidyverse)

timeScales <- c("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/landkreise/LK_mobilityData_weekdays.csv",
"https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/landkreise/LK_mobilityData_weekends.csv",
"https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/landkreise/LK_mobilityData_weekly.csv")

dateRanges <- c("2021-01-01", "2023-01-01")

dfLK <- read_delim("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/landkreise/LK_mobilityData_weekly.csv", delim = ";")
dfLK <- dfLK %>% mutate(Landkreis = str_remove_all(Landkreis, "Kreis |Landkreis | am Rhein| an der Havel"))
dfLK$Landkreis <-replace(dfLK$Landkreis, dfLK$Landkreis == "Pfaffenhofen an der Ilm", "Pfaffenhofen a.d. Ilm") 
dfLK$Landkreis <-replace(dfLK$Landkreis, dfLK$Landkreis == "Neumarkt in der Oberpfalz", "Neumarkt i.d. Oberpfalz") 
dfLK$Landkreis <-replace(dfLK$Landkreis, dfLK$Landkreis == "Weiden in der Oberpfalz", "Weiden/Oberpfalz") 
dfLK$Landkreis <-replace(dfLK$Landkreis, dfLK$Landkreis == "Neustadt an der Weinstraße", "Neustadt/Weinstraße") 
dfLK$Landkreis <-replace(dfLK$Landkreis, dfLK$Landkreis == "Neustadt an der Waldnaab", "Neustadt a.d. Waldnaab") 
dfLK$Landkreis <-replace(dfLK$Landkreis, dfLK$Landkreis == "Landsberg am Lech", "Landsberg a. Lech") 
dfLK$Landkreis <-replace(dfLK$Landkreis, dfLK$Landkreis == "Dillingen an der Donau", "Dillingen a.d. Donau") 
dfLK$Landkreis <-replace(dfLK$Landkreis, dfLK$Landkreis == "Rotenburg (Wümme)", "Rotenburg/Wümme") 
dfLK$Landkreis <-replace(dfLK$Landkreis, dfLK$Landkreis == "Essen", "Essen/Ruhr") 

dfLK$date <- as.character(dfLK$date)
dfLK$date  <- paste0(str_sub(dfLK$date, 5, 6), "-", str_sub(dfLK$date, 7, 8), "-", str_sub(dfLK$date, 1, 4))
dfLK$date <- as.Date(dfLK$date, "%m-%d-%Y")

#Reading in this data set which maps counties to federal states
MapPLZBL <- read_csv2("/Users/sydney/Downloads/PLZ_Ort_Landkreis_Bundesland_Mapping_DE.csv", col_types = cols(.default = "c"))
for(row in 1:nrow(MapPLZBL)){
  if(nchar(MapPLZBL$PLZ[row]) == 4){
    MapPLZBL[row, 1] = paste0(0, MapPLZBL[row, 1], sep = "")
  }
}
colnames(MapPLZBL)[1] <- "zipCode"
#Reading in the data set that contains the no. of people/county
personStats <- read_csv("/Users/sydney/root/svn/shared-svn/projects/episim/data/Bewegungsdaten//20200302/20200302_domesticResidentActChains.avro_personStats.csv.gz")
personStats <- personStats %>% select(c("zipCode", "nPersons"))
personStats <- left_join(personStats, MapPLZBL)

personStats  <- personStats  %>% group_by(Landkreis, Bundesland) %>% summarise(nPersons = sum(nPersons))
personStats <- personStats %>% ungroup()

#Joining the different data frames
dfLK <- left_join(dfLK, personStats, join_by(Landkreis == Landkreis))

for(fedState in unique(dfLK$Bundesland)){
        if(!is.na(fedState)){
        for(dateRange in dateRanges){
        #Plotting absolute out of home duration
        ggplot(dfLK %>% filter(Bundesland != "Deutschland") %>% filter(Bundesland == fedState) %>% filter(date < dateRange)) +
        geom_point(aes(x = date, y = outOfHomeDuration, color = factor(Landkreis)), size = 3) +
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(text = element_text(size = 19)) +
        theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
        xlab("Date") +
        ylab("Average Time/Person [hrs]") +
        scale_x_date(date_breaks = "3 month", date_labels = "%m/%y")

        nameTimeScale <- str_split(timeScale[1], "_")[[1]][3]
        nameTimeScale <- str_sub(nameTimeScale, end = -5)
        ggsave(paste0("LK", as.character(fedState), nameTimeScale, "until", dateRange, "-outOfHome.png"), dpi = 500, w = 24, h = 12)
        ggsave(paste0("LK", as.character(fedState), nameTimeScale, "until", dateRange, "-outOfHome.pdf"), dpi = 500, w = 24, h = 12)

        #Plotting percentage change compared to before COVID (early March 2020)
        ggplot(dfLK %>% filter(Bundesland != "Deutschland") %>% filter(Bundesland == fedState) %>% filter(date < dateRange)) +
        geom_point(aes(x = date, y = percentageChangeComparedToBeforeCorona, color = factor(Landkreis)), size = 3) +
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(text = element_text(size = 19)) +
        theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
        xlab("Date") +
        ylab("Perc. Change") +
        scale_x_date(date_breaks = "3 month", date_labels = "%m/%y")
        ggsave(paste0("LK", as.character(fedState), nameTimeScale, "until", dateRange, "-PercChange.png"), dpi = 500, w = 24, h = 12)
        ggsave(paste0("LK", as.character(fedState), nameTimeScale, "until", dateRange, "-PercChange.pdf"), dpi = 500, w = 24, h = 12)

        ggplot(dfLK %>% filter(Bundesland != "Deutschland") %>% filter(Bundesland == fedState) %>% filter(date < dateRange)) +
        geom_boxplot(aes(x = date, y = outOfHomeDuration, group = date), size = 3) +
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(text = element_text(size = 30)) +
        theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
        xlab("Date") +
        ylab("Average Time/Person [hrs]") +
        scale_x_date(date_breaks = "3 month", date_labels = "%m/%y")
        ggsave(paste0("LK", as.character(fedState), nameTimeScale, "until", dateRange, "-BoxPlots.pdf"), dpi = 500, w = 36, h = 12)
        ggsave(paste0("LK", as.character(fedState), nameTimeScale, "until", dateRange, "-BoxPlots.png"), dpi = 500, w = 36, h = 12)

        ggplot(dfLK %>% filter(Bundesland != "Deutschland") %>% filter(Bundesland == fedState) %>% filter(date < dateRange)) +
        geom_boxplot(aes(x = date, y = percentageChangeComparedToBeforeCorona, group = date), size = 3) +
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(text = element_text(size = 30)) +
        theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
        xlab("Date") +
        ylab("Perc. Change") +
        scale_x_date(date_breaks = "3 month", date_labels = "%m/%y")
        ggsave(paste0("LK", as.character(fedState), nameTimeScale, "until", dateRange, "-BoxPlotsPercChange.pdf"), dpi = 500, w = 36, h = 12)
        ggsave(paste0("LK", as.character(fedState), nameTimeScale, "until", dateRange, "-BoxPlotsPercChange.png"), dpi = 500, w = 36, h = 12)
        }
        }
}

dfLK <- dfLK %>% mutate(typeOfArea = case_when(nPersons < 5000 ~ "Village",
                                                        nPersons < 20000 ~"Small Town",
                                                        nPersons < 100000 ~ "Medium Town",
                                                        nPersons < 300000 ~ "Small City",
                                                        nPersons < 1000000 ~ "Large City",
                                                        nPersons > 1000000 ~ "Metropolis"))

for(dateRange in dateRanges){
ggplot(dfLK %>% filter(Bundesland != "Deutschland") %>% filter(date < dateRange)) +
    geom_point(aes(x = date, y = percentageChangeComparedToBeforeCorona, color = Landkreis), size = 3) +
    theme_minimal() +
    facet_wrap(~typeOfArea, nrow = 2) +
    theme(legend.position = "none", legend.title = element_blank()) +
    theme(text = element_text(size = 19)) +
    theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
    xlab("Date") +
    ylab("Perc. Change") +
    scale_x_date(date_breaks = "3 month", date_labels = "%m/%y")

nameTimeScale <- str_split(timeScale[1], "_")[[1]][3]
nameTimeScale <- str_sub(nameTimeScale, end = -5)
    ggsave(paste0("LkByInhabitants", nameTimeScale, "until", dateRange, "-percChange.png"), dpi = 500, w = 24, h = 12)
    ggsave(paste0("LKByInhabitants", nameTimeScale, "until", dateRange, "-percChange.pdf"), dpi = 500, w = 24, h = 12)


ggplot(dfLK %>% filter(Bundesland != "Deutschland") %>% filter(date < dateRange)) +
    geom_point(aes(x = date, y = outOfHomeDuration, color = Landkreis), size = 3) +
    theme_minimal() +
    facet_wrap(~typeOfArea, nrow = 2) +
    theme(legend.position = "none", legend.title = element_blank()) +
    theme(text = element_text(size = 19)) +
    theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
    xlab("Date") +
    ylab("Perc. Change") +
    scale_x_date(date_breaks = "3 month", date_labels = "%m/%y")

    ggsave(paste0("LkByInhabitants", nameTimeScale, "until", dateRange, "-outOfHome.png"), dpi = 500, w = 24, h = 12)
    ggsave(paste0("LKByInhabitants", nameTimeScale, "until", dateRange, "-outOfHome.pdf"), dpi = 500, w = 24, h = 12)
}

dfLK <- dfLK %>% mutate(wave = case_when(date < "2020-05-01" ~ "2020-05-01",
                                                date < "2021-03-01" ~ "2021-03-01",
                                                date < "2021-08-01" ~ "2021-08-01",
                                                date < "2022-03-01" ~ "2022-03-01", 
                                                date < "2022-06-01" ~ "2022-06-01",
                                                date < "2022-09-01" ~ "2022-09-01",
                                                date < "2023-01-01" ~ "2022-12-31"))

dfLK$typeOfArea <- factor(dfLK$typeOfArea, levels = c("Village", "Small Town", "Medium Town", "Small City", "Large City", "Metropolis"))

ggplot(dfLK %>% filter(date < "2023-01-01") %>% filter(Bundesland != "Deutschland")) +
        geom_boxplot(aes(x = typeOfArea, y = outOfHomeDuration), size = 3) +
        theme_minimal() +
        facet_wrap(~wave, nrow = 2) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(text = element_text(size = 30)) +
        theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
        scale_color_manual(values = getPalette(colourCount)) +
        xlab("Town Size") +
        ylab("Out Of Home Duration/Person [Hrs]") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    ggsave(paste0("LkByInhabitants", nameTimeScale, "-Boxplots.png"), dpi = 500, w = 12, h = 12)
    ggsave(paste0("LkByInhabitants", nameTimeScale, "-Boxplots.pdf"), dpi = 500, w = 12, h = 12)

ggplot(dfLK %>% filter(date < "2023-01-01") %>% filter(Bundesland != "Deutschland")) +
        geom_boxplot(aes(x = typeOfArea, y = percentageChangeComparedToBeforeCorona), size = 3) +
        theme_minimal() +
        facet_wrap(~wave, nrow = 2) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(text = element_text(size = 30)) +
        theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
        scale_color_manual(values = getPalette(colourCount)) +
        xlab("Town Size") +
        ylab("Perc. Change") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    ggsave(paste0("LkByInhabitants", nameTimeScale, "-BoxplotsPercChange.png"), dpi = 500, w = 12, h = 12)
    ggsave(paste0("LkByInhabitants", nameTimeScale, "-BoxplotsPercChange.pdf"), dpi = 500, w = 12, h = 12)
