library(tidyverse)

timeScales <- c("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekly.csv",
"https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekends.csv", 
"https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekdays.csv")

#For each wave: Compute distribution of reduction
FedStates <- read_delim(timeScales[1], delim = ";")
FedStates$date <- as.character(FedStates$date)
FedStates$date  <- paste0(str_sub(FedStates$date, 5, 6), "-", str_sub(FedStates$date, 7, 8), "-", str_sub(FedStates$date, 1, 4))
FedStates$date <- as.Date(FedStates$date, "%m-%d-%Y")

Waves <- list(c("2020-03-01", "2020-05-01"),
            c("2020-08-01", "2021-03-01"),
            c("2021-06-01", "2021-08-01"),
            c("2021-11-01", "2022-03-01"),
            c("2023-03-01", "2022-06-01"),
            c("2023-06-01", "2022-09-01"),
            c("2023-09-01", "2022-12-31"))

#Group fed. states according to governing party in 2020
#First entry: SPD, second entry: CDU, third entry: Grüne, fourth entry: Linke
Laender <- list(c("Rheinland-Pfalz", "Niedersachsen", "Bremen", "Berlin", "Brandenburg", "Hamburg", "Mecklenburg-Vorpommern"),
                c("Bayern", "Hessen", "Nordrhein-Westfalen", "Schleswig-Holstein", "Sachsen-Anhalt", "Sachsen", "Saarland"),
                c("Baden-Württemberg"),
                c("Thüringen"))

FedStates <- FedStates %>% mutate(government = case_when(BundeslandID %in% Laender[[1]] ~ "SPD",
                                    BundeslandID %in% Laender[[2]] ~ "CDU",
                                    BundeslandID %in% Laender[[3]] ~ "Grün",
                                    BundeslandID %in% Laender[[4]] ~ "Linke"))

#Creating the data Frames in which results will be stores
dfResultsWaves <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(dfResultsWaves) <- c("Wave", "Min", "Max", "Mean", "Median", "Quantile25", "Quantile75")
fedStatesMin <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(fedStatesMin) <- c("BundeslandID", "Minimum", "date", "government", "wave", "Stadtstaat", "WestEast")

for(wave in Waves){
    FedStatesReduced <- FedStates %>% filter(date > wave[1]) %>%
                                    filter(date < wave[2]) %>%
                                    group_by(BundeslandID) %>%
                                    summarise(Minimum = min(percentageChangeComparedToBeforeCorona), date = date[which.min(percentageChangeComparedToBeforeCorona)], government = government) %>%
                                    filter(BundeslandID != "Deutschland")
    FedStatesReduced <- FedStatesReduced %>% mutate(wave = wave[2]) %>%
                                                mutate(Stadtstaat = case_when(BundeslandID %in% c("Berlin", "Hamburg", "Bremen") ~ "Stadtstaat", 
                                                                                                    .default = "KeinStadtstaat")) %>%
                                                mutate(WestEast = case_when(BundeslandID %in% c("Schleswig-Holstein", "Hamburg", "Bremen", "Niedersachsen", "Nordrhein-Westfalen", "Hessen", "Rheinland-Pfalz", "Saarland", "Baden-Württemberg", "Bayern") ~ "West",
                                                                                                    .default = "East")) 

    dfResultsWaves[nrow(dfResultsWaves) + 1, 1] <- wave[2]
    dfResultsWaves[nrow(dfResultsWaves), 2] <- min(FedStatesReduced$Minimum)
    dfResultsWaves[nrow(dfResultsWaves), 3] <- max(FedStatesReduced$Minimum)
    dfResultsWaves[nrow(dfResultsWaves), 4] <- mean(FedStatesReduced$Minimum)
    dfResultsWaves[nrow(dfResultsWaves), 5] <- median(FedStatesReduced$Minimum)
    dfResultsWaves[nrow(dfResultsWaves), 6] <- quantile(FedStatesReduced$Minimum, names = FALSE)[2]
    dfResultsWaves[nrow(dfResultsWaves), 7] <- quantile(FedStatesReduced$Minimum, names = FALSE)[4]

    fedStatesMin <- rbind(fedStatesMin, FedStatesReduced)
}


ggplot(fedStatesMin) +
geom_boxplot(aes(x = wave, y = Minimum), color = "#1b9e77", size = 2) +
theme_minimal() +
ylab("Maximum Reduction") +
theme(text = element_text(size = 23)) +
theme(axis.ticks.x = element_line(),
          axis.ticks.y = element_line(),
          axis.ticks.length = unit(5, "pt"))
ggsave("BoxPlotMaxReductionWaves.pdf", dpi = 500, w = 5, h = 9)
ggsave("BoxPlotMaxReductionWaves.png", dpi = 500, w = 5, h = 9)

#Panels for eastern/western federal states
ggplot(fedStatesMin) +
geom_boxplot(aes(x = wave, y = Minimum), color = "#1b9e77", size = 2) +
theme_minimal() +
ylab("Maximum Reduction") +
facet_wrap(~WestEast, nrow = 2) +
theme(text = element_text(size = 23)) +
theme(axis.ticks.x = element_line(),
          axis.ticks.y = element_line(),
          axis.ticks.length = unit(5, "pt"))

#Panels for "Stadtstaaten"
ggplot(fedStatesMin) +
geom_boxplot(aes(x = wave, y = Minimum), color = "#1b9e77", size = 2) +
theme_minimal() +
ylab("Maximum Reduction") +
facet_wrap(~Stadtstaat, nrow = 2) +
theme(text = element_text(size = 23)) +
theme(axis.ticks.x = element_line(),
          axis.ticks.y = element_line(),
          axis.ticks.length = unit(5, "pt"))

#Panels for governing party in 2020
ggplot(fedStatesMin) +
geom_boxplot(aes(x = wave, y = Minimum), color = "#1b9e77", size = 2) +
theme_minimal() +
ylab("Maximum Reduction") +
facet_wrap(~government, nrow = 2) +
theme(text = element_text(size = 23)) +
theme(axis.ticks.x = element_line(),
          axis.ticks.y = element_line(),
          axis.ticks.length = unit(5, "pt"))
