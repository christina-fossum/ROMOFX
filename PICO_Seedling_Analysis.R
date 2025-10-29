#Load Packages
library(tidyverse)
library(dplyr)
library(stringr)


SeedlingData <- read.csv("PICO_Data/ROMO_PICO_SeedlingData.csv")

# Remove unnecessary columns: (use 'select' to only keep columns of interest). *Tip: enter 'print(colnames(TreeData))' into console to view column names without having to open the dataframe*
SeedlingData <- SeedlingData %>% select(MacroPlot.Name, Sample.Event.Date, Quadrat, Item.Code, Status, Height, Count, 
                                        Num..Quad..Tran., Quad..Length, Quad..Width, Quad..Area)
#Rename
SeedlingData <- SeedlingData %>% rename(PlotID = MacroPlot.Name) %>%  mutate(PlotID = str_replace(PlotID, "^F", ""),  
                                                                     PlotID = str_replace(PlotID, "1T\\d{2}:(\\d{2})", "_\\1"))
#Add monitor status
StatusTable2 <- SeedlingData %>% select(PlotID, Sample.Event.Date) %>% distinct() %>% arrange(PlotID)
StatusTable2 <- StatusTable2 %>% mutate(MonStatus = c("02Year05", "00Pre", "01Year01", "01Year02", "01Year05", 
                                                    "02Year05", "00Pre", "01Year01", "01Year02", "01Year05", 
                                                    "02Year05", "00Pre", "01Year01", "01Year02", "01Year05", 
                                                    "01Year05", "00Pre", "02Year05", "00Pre", "01Year01", "01Year02", "01Year05", 
                                                    "01Year05", "00Pre", "01Year05", "00Pre", "01Year05", "00Pre", "01Year05", "00Pre", 
                                                    "01Year05", "00Pre", "01Year05", "00Pre", "01Year05", "00Pre", "01Year05", "00Pre",
                                                    "01Year05", "00Pre"))
SeedlingData <- left_join(StatusTable2, SeedlingData)


# Add whether it burned in 2020 or not
SeedlingData <- SeedlingData %>% mutate(Burned2020 = case_when(PlotID == "PICO_08" & MonStatus == "02Year05" ~ "no",
                                                       PlotID == "PICO_09" & MonStatus == "02Year05" ~ "no",
                                                       PlotID == "PICO_10" & MonStatus == "02Year05" ~ "yes",
                                                       PlotID == "PICO_11" & MonStatus == "01Year05" ~ "yes",
                                                       PlotID == "PICO_12" & MonStatus == "02Year05" ~ "yes",
                                                       PlotID == "PICO_13" & MonStatus == "01Year05" ~ "yes",
                                                       PlotID == "PICO_14" & MonStatus == "01Year05" ~ "yes",
                                                       PlotID == "PICO_15" & MonStatus == "01Year05" ~ "no",
                                                       PlotID == "PICO_16" & MonStatus == "01Year05" ~ "yes",
                                                       PlotID == "PICO_19" & MonStatus == "01Year05" ~ "no",
                                                       PlotID == "PICO_20" & MonStatus == "01Year05" ~ "yes",
                                                       PlotID == "PICO_21" & MonStatus == "01Year05" ~ "no",
                                                       PlotID == "PICO_24" & MonStatus == "01Year05" ~ "no",
                                                       PlotID == "PICO_24" & MonStatus == "01Year05" ~ "no", .default = NA))

# Plot

seedlings25 <- SeedlingData %>% filter(Burned2020 == "no" | Burned2020 == "yes")

#1. Starting with comparing number of seedlings in 2025 in burned vs. not burned plots

# summarize 
# group seedlings by number of seedlings per plot 
seedlings25 <- seedlings25 %>% group_by(PlotID, MonStatus, Burned2020) %>% summarise(Count = sum(Count))


#find average number of seedlings in each plot 

seedling_means <- seedlings25 %>%
  +     group_by(Burned2020) %>%
  +     summarise(mean_count = mean(Count, na.rm = TRUE))


#Plot difference in mean number of seedlings between the plots that burned in 2020 and those that did not

#pS <- ggplot(seedlings25, aes(x = Burned2020, y = Count, fill = Burned2020)) +
#  geom_boxplot(stat = "identity", color = "black", width = 0.6) +
#  labs(
#    title = "Mean Seedling Count",
#   x = "Burned in East Troublesome 2020",
#    y = "Mean Number of Seedlings"
#  ) +
#  theme_minimal() +
#  theme(legend.position = "none")

pS <- ggplot(seedlings25, aes(x = Burned2020, y = Count, fill = Burned2020)) +
  geom_boxplot() +
  labs(
    title = "Seedling Count",
    x = "Burned in East Troublesome 2020",
    y = "Number of Seedlings"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Run T-tests on data
seedlings25_burned <- seedlings25 %>% filter(Burned2020 == "yes")
seedlings25_unburned <- seedlings25 %>% filter(Burned2020 == "no") 

t.test(seedlings25_burned$Count, seedlings25_unburned$Count)


#Welch Two Sample t-test

#data:  seedlings25_burned$Count and seedlings25_unburned$Count
#t = -0.37027, df = 9.3173, p-value = 0.7195
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -70.77888  50.77888
#sample estimates:
#  mean of x mean of y 
#58        68 

# difference between the number of seedlings in a burned vs unburned plot is NOT significant


#2.

##Tidying Seedlings Report

SeedlingReport <- read.csv("PICO_Data/ROMO_PICO_SeedlingReport.csv")

# Change 'MacroPlot.Name' to 'PlotID' and re-format 'FPICO1T08:09' naming convention to be more streamlined
SeedlingReport <- SeedlingReport %>% rename(PlotID = Macroplot) %>%  mutate(PlotID = str_replace(PlotID, "^F", ""),  

#Change other names                                                                                                                                                                                                                        PlotID = str_replace(PlotID, "1T\\d{2}:(\\d{2})", "_\\1"))
SeedlingReport <- SeedlingReport %>% rename(AvgCount = Quad)
SeedlingReport <- SeedlingReport %>% rename(SeedlingsPerSqFt = SqFtm)
SeedlingReport <- SeedlingReport %>% rename(SeedlingsPerAcre = AcreHa)

# Add a column for whether plot burned in East Troublesome
SeedlingReport <- SeedlingReport %>% mutate(Burned2020 = case_when(PlotID == "PICO_08" & MonStatus == "02PostBurnYear05" ~ "no",
                                                           PlotID == "PICO_09" & MonStatus == "02PostBurnYear05" ~ "no",
                                                           PlotID == "PICO_10" & MonStatus == "02PostBurnYear05" ~ "yes",
                                                           PlotID == "PICO_11" & MonStatus == "01PostBurnYear05" ~ "yes",
                                                           PlotID == "PICO_12" & MonStatus == "02PostBurnYear05" ~ "yes",
                                                           PlotID == "PICO_13" & MonStatus == "01PostBurnYear05" ~ "yes",
                                                           PlotID == "PICO_14" & MonStatus == "01PostBurnYear05" ~ "yes",
                                                           PlotID == "PICO_15" & MonStatus == "01PostBurnYear05" ~ "no",
                                                           PlotID == "PICO_16" & MonStatus == "01PostBurnYear05" ~ "yes",
                                                           PlotID == "PICO_19" & MonStatus == "01PostBurnYear05" ~ "no",
                                                           PlotID == "PICO_20" & MonStatus == "01PostBurnYear05" ~ "yes",
                                                           PlotID == "PICO_21" & MonStatus == "01PostBurnYear05" ~ "no",
                                                           PlotID == "PICO_24" & MonStatus == "01PostBurnYear05" ~ "no",
                                                           PlotID == "PICO_24" & MonStatus == "01PostBurnYear05" ~ "no", .default = NA))

# Delete unnecessary Columns / rename/reformat
SeedlingReport <- SeedlingReport %>% select(PlotID, MonStatus, Item, Status, AvgCount, SeedlingsPerAcre, Height, Burned2020)


#3. Number of trees per species in burned vs. unburned


sr25 <- SeedlingReport %>% filter(Burned2020 == "no" | Burned2020 == "yes")

##plot the differences

sp1 <- ggplot(sr25, aes(x = Item, y = SeedlingsPerAcre, fill = Burned2020)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
  labs(
    title = "Distribution of Seedlings per Acre by Species and Burn Status",
    x = "Species (Item)",
    y = "Seedlings per Acre",
    fill = "Burned in 2020"
  ) +
  theme_minimal(base_size = 14)


#analysis
##Seedlings Per Acre

#ABLA
srABLA <- sr25 %>% filter(Item == "ABLA")
srABLA_burned <- srABLA %>% filter(Burned2020 == "yes")
srABLA_unburned <- srABLA %>% filter(Burned2020 == "no")

t_results <- sr25 %>%
  group_by(Item) %>%
  filter(length(unique(Burned2020)) == 2) %>%
  summarise(tidy_t = list(broom::tidy(wilcox.test(SeedlingsPerAcre ~ Burned2020)))) %>%
  unnest(tidy_t)

t.test(srABLA_burned$SeedlingsPerAcre, srABLA_unburned$SeedlingsPerAcre)

#PIEN
srPIEN <- sr25 %>% filter(Item == "PIEN")
srPIEN_burned <- srPIEN %>% filter(Burned2020 == "yes")
srPIEN_unburned <- srPIEN %>% filter(Burned2020 == "no")

t.test(srPIEN_burned$SeedlingsPerAcre, srPIEN_unburned$SeedlingsPerAcre)

#PICO
srPICO <- sr25 %>% filter(Item == "PICO")
srPICO_burned <- srPICO %>% filter(Burned2020 == "yes")
srPICO_unburned <- srPICO %>% filter(Burned2020 == "no")

t.test(srPICO_burned$SeedlingsPerAcre, srPICO_unburned$SeedlingsPerAcre)
#p-value = 0.5136

#We can't run a t.test on seedlings per acre in burned vs. unburned plots in ABLA, PIEN, or POTR because there is
#not enough data in the burned plots. By looking at the graph, we can see that there are none to few ABLA and PIEN 
#seedlings in the burned plots, and no POTR in the unburned plots. We can see that for PICO, there is not a significant
#difference in the number of seedlings per acre in either type of plot. 

#4. Height of seedlings in burned vs. unburned plots



sp3 <- ggplot(seedlings25, aes(x = Burned2020, y = Height, fill = Burned2020)) +
  geom_boxplot() +
  labs(
    title = "Distibution of Seedling Height by Burn Status",
    x = "Burned in East Troublesome 2020",
    y = "Seedling Height"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#difference in height between species
sp2 <- ggplot(sr25, aes(x = Item, y = Height, fill = Burned2020)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
  labs(
    title = "Distribution of Seedling Height by Species and Burn Status",
    x = "Species (Item)",
    y = "Height",
    fill = "Burned in 2020"
  ) +
  theme_minimal(base_size = 14)


#analysis 

t.test(sr25_burned$Height, sr25_unburned$Height)
#p-value = 0.0007766
#There a significant difference between height of seedlings in burned vs. unburned plots.

##PICO
t.test(srPICO_burned$Height, srPICO_unburned$Height)
#p-value = 0.03998

#There is a significant difference between seedling height in PICO between burned and unburned plots. The unburned plots
#tend to have taller seedlings than burned in PICO. 


