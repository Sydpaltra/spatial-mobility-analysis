library(tidyverse)
library(RColorBrewer)

#Different time scales: weekly, weekend average, business day average
timeScales <- c("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekly.csv",
"https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekends.csv", 
"https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekdays.csv")

dateRanges <- c("2021-01-01", "2023-01-01")

#Iterating over said time scales
for(timeScale in timeScales){
    for(dateRange in dateRanges){
        FedStates <- read_delim(timeScale, delim = ";")
        FedStates$date <- as.character(FedStates$date)
        FedStates$date  <- paste0(str_sub(FedStates$date, 5, 6), "-", str_sub(FedStates$date, 7, 8), "-", str_sub(FedStates$date, 1, 4))
        FedStates$date <- as.Date(FedStates$date, "%m-%d-%Y")

        colourCount <- length(unique(FedStates$BundeslandID))
        getPalette <- colorRampPalette(brewer.pal(16, "Dark2"))

        #Plotting absolute out of home duration
        ggplot(FedStates %>% filter(BundeslandID != "Deutschland")  %>% filter(date < dateRange)) +
        geom_line(FedStates %>% filter(BundeslandID == "Deutschland") %>% filter(date < dateRange), mapping = aes(x=date, y = outOfHomeDuration), size = 3) +
        geom_point(aes(x = date, y = outOfHomeDuration, color = factor(BundeslandID)), size = 3) +
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(text = element_text(size = 19)) +
        theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
        scale_color_manual(values = getPalette(colourCount)) +
        xlab("Date") +
        ylab("Average Time/Person [hrs]") +
        scale_x_date(date_breaks = "3 month", date_labels = "%m/%y")

        nameTimeScale <- str_split(timeScale[1], "_")[[1]][3]
        nameTimeScale <- str_sub(nameTimeScale, end = -5)

        ggsave(paste0("FedStates", nameTimeScale, "until", dateRange, "-outOfHome.png"), dpi = 500, w = 12, h = 12)
        ggsave(paste0("FedStates", nameTimeScale, "until", dateRange, "-outOfHome.pdf"), dpi = 500, w = 12, h = 12)

        #Plotting percentage change compared to before COVID (early March 2020)
        ggplot(FedStates %>% filter(BundeslandID != "Deutschland") %>% filter(date < dateRange)) +
        geom_line(FedStates %>% filter(BundeslandID == "Deutschland")  %>% filter(date < dateRange), mapping = aes(x=date, y = percentageChangeComparedToBeforeCorona), size = 3) +
        geom_point(aes(x = date, y = percentageChangeComparedToBeforeCorona, color = factor(BundeslandID)), size = 3) +
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(text = element_text(size = 19)) +
        theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
        scale_color_manual(values = getPalette(colourCount)) +
        xlab("Date") +
        ylab("Average Time/Person [hrs]") +
        scale_x_date(date_breaks = "3 month", date_labels = "%m/%y")
        ggsave(paste0("FedStates", nameTimeScale, "until", dateRange, "-PercChange.png"), dpi = 500, w = 12, h = 12)
        ggsave(paste0("FedStates", nameTimeScale, "until", dateRange, "-PercChange.pdf"), dpi = 500, w = 12, h = 12)

        ggplot(FedStates %>% filter(BundeslandID != "Deutschland") %>% filter(date < dateRange)) +
        geom_boxplot(aes(x = date, y = outOfHomeDuration, group = date), size = 3) +
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(text = element_text(size = 30)) +
        theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
        scale_color_manual(values = getPalette(colourCount)) +
        xlab("Date") +
        ylab("Average Time/Person [hrs]") +
        scale_x_date(date_breaks = "3 month", date_labels = "%m/%y")
        ggsave(paste0("FedStates", nameTimeScale, "until", dateRange, "-BoxPlots.pdf"), dpi = 500, w = 36, h = 12)
        ggsave(paste0("FedStates", nameTimeScale, "until", dateRange, "-BoxPlots.png"), dpi = 500, w = 36, h = 12)

        aLaender <- c("Rheinland-Pfalz", "Niedersachsen", "Bremen", "Berlin", "Brandenburg", "Hamburg", "Mecklenburg-Vorpommern")
        bLaender <- c("Bayern", "Hessen", "Nordrhein-Westfalen", "Schleswig-Holstein", "Sachsen-Anhalt", "Sachsen", "Saarland")
        FedStates <- FedStates %>% mutate(SPDCDU = case_when(BundeslandID %in% aLaender ~ "SPD", 
                                                             BundeslandID %in% bLaender ~ "CDU",
                                                             .default = "neither"))
        ggplot(FedStates %>% filter(BundeslandID != "Deutschland") %>% filter(date < dateRange)) +
        geom_boxplot(aes(x = date, y = outOfHomeDuration, group = date), size = 3) +
        theme_minimal() +
        facet_wrap(~SPDCDU, nrow = 3) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(text = element_text(size = 30)) +
        theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
        scale_color_manual(values = getPalette(colourCount)) +
        xlab("Date") +
        ylab("Average Time/Person [hrs]") +
        scale_x_date(date_breaks = "3 month", date_labels = "%m/%y")
        ggsave(paste0("FedStates", nameTimeScale, "until", dateRange, "-BoxPlotsSPDCDU.pdf"), dpi = 700, w = 36, h = 20)
        ggsave(paste0("FedStates", nameTimeScale, "until", dateRange, "-BoxPlotsSPDCDU.png"), dpi = 700, w = 36, h = 20)

    }
}
