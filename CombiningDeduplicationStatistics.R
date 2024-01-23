rm(list = ls())
setwd("C:/Users/raoda/Desktop/R Stuff/CUT&Tag Processing")
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

samples <- c("1_78-1_K27me3_Accutase", "2_78-1_K27me3_Trypsin",
             "3_78-1_K27me3_FF", "4_78-1_K27me3_Scraped",
             "5_H21792_K27ac_Accutase", "6_H21792_K27ac_Trypsin",
             "7_H21792_K9me3_Trypsin", "8_H21792_K9me3_Accutase", 
             "9_H21792_K9me3_Scraped", "10_78-1_K27ac_Accutase", 
             "11_78-1_K27ac_Scraped")

order <- c("First Align","After Filter","After DeDupl")

file <- data.frame()  # Create an empty data frame to store results
for(sample in samples){
  for(event in order){
    file <- as.data.frame(t(read_delim(choose.files(), delim = ":", escape_double = FALSE, col_names = FALSE,
                                       trim_ws = TRUE, skip = 5))) %>% 
      mutate(Sample = sample, Event = event) %>%
      select(Sample, Event, V1, V5,V6,V7,V8,V9,V12,V14) %>% 
      rbind(file,.)
  }
}
write.csv(file, file = "1-9-24 Sequencing Mapping Statistics Pooled.csv")

# plotting stats -----------------------------------------------------------------------
# Example data
mappingStats <- read.csv(choose.files())

# Reorder the levels of the Sample variablema
mappingStats$Sample <- factor(mappingStats$Sample, levels = samples)
# Create a triple bar graph facet-wrapped by sample type
ggplot(mappingStats, aes(x = Sample, y = Read.Count, fill = Event)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Collection, ncol=1) +
  labs(title = "Mapping Statistics Per Sample Grouped By Collection",
       x = "Sample", y = "Read Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
