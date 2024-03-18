library(tidyverse)
library(RColorBrewer)

#Data is currently stores locally
counties <- read_csv("/Users/sydney/root/svn/shared-svn/projects/episim/data/Bewegungsdaten/validierungsdaten_2023/koeln/20231231/WeeklyOutOfHomezipCodes.csv")

#Reading in this data set which maps counties to federal states
MapPLZBL <- read_csv2("/Users/sydney/Downloads/PLZ_Ort_Landkreis_Bundesland_Mapping_DE.csv", col_types = cols(.default = "c"))
for(row in 1:nrow(MapPLZBL)){
  if(nchar(MapPLZBL$PLZ[row]) == 4){
    MapPLZBL[row, 1] = paste0(0, MapPLZBL[row, 1], sep = "")
  }
}
MapPLZBL <- MapPLZBL %>% select(c("PLZ", "Bundesland"))
colnames(MapPLZBL)[1] <- "zipCode"

#Reading in the data set that contains the no. of people/county
personStats <- read_csv("/Users/sydney/root/svn/shared-svn/projects/episim/data/Bewegungsdaten//20200302/20200302_domesticResidentActChains.avro_personStats.csv.gz")
personStats <- personStats %>% select(c("zipCode", "nPersons"))
personStats <- left_join(personStats, MapPLZBL)

#Merging the no. of people/county, the federal state/county with the counties data set
counties <- left_join(counties, personStats, join_by(zipCode))

#Dividing the time frame into the different pandemic waves. Analysing each wave separetely
counties <- counties %>% mutate(wave = case_when(date < "2020-05-01" ~ "2020-05-01",
                                                date < "2021-03-01" ~ "2021-03-01",
                                                date < "2021-08-01" ~ "2021-08-01",
                                                date < "2022-03-01" ~ "2022-03-01", 
                                                date < "2022-06-01" ~ "2022-06-01",
                                                date < "2022-09-01" ~ "2022-09-01",
                                                date < "2023-01-01" ~ "2022-12-31"))


#ToDo: For now this does not work --> Large cities consist of several post codes
counties <- counties %>% mutate(typeOfArea = case_when(nPersons < 5000 ~ "Village",
                                                        nPersons < 20000 ~"Small Town",
                                                        nPersons < 100000 ~ "Medium Town",
                                                        nPersons < 300000 ~ "Small City",
                                                        nPersons < 1000000 ~ "Large City",
                                                        nPersons > 1000000 ~ "Metropolis"))
    

#ToDo: We are filtering for MeanHours < 24 --> Sth here went wrong, for some counties no. of people == too small --> Investigate
ggplot(counties %>% filter(act == "notHome") %>% filter(MeanHours < 24)  %>% filter(date < "2023-01-01") %>% filter(Bundesland != "Deutschland")) +
        geom_boxplot(aes(x = wave, y = MeanHours), size = 3) +
        theme_minimal() +
        facet_wrap(~Bundesland, nrow = 4) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(text = element_text(size = 30)) +
        theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
        scale_color_manual(values = getPalette(colourCount)) +
        xlab("Wave End") +
        ylab("Perc. Change Compared To Before Covid") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("BoxPlotsFedStates.pdf", dpi = 700, w = 36, h = 20)
ggsave("BoxPlotsFedStates.png", dpi = 700, w = 36, h = 20)

#Plot panel for villages/small towns/metdium towns etc --> This is under construction
ggplot(counties %>% filter(act == "notHome") %>% filter(MeanHours < 24)  %>% filter(date < "2023-01-01") %>% filter(Bundesland != "Deutschland")) +
        geom_boxplot(aes(x = wave, y = MeanHours), size = 3) +
        theme_minimal() +
        facet_wrap(~typeOfArea, nrow = 3) +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        theme(text = element_text(size = 30)) +
        theme(axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                axis.ticks.length = unit(5, "pt")) +
        scale_color_manual(values = getPalette(colourCount)) +
        xlab("Wave End") +
        ylab("Perc. Change Compared To Before Covid") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("BoxPlotsFedStatesTownSize.pdf", dpi = 700, w = 36, h = 20)
ggsave("BoxPlotsFedStatesTownSize.png", dpi = 700, w = 36, h = 20)