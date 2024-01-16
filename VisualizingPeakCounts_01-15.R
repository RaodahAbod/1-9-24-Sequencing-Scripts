rm(list = ls())
setwd("C:/Users/raoda/OneDrive/Desktop/R Stuff/CUT&Tag Processing")
library(tidyverse)
library(dplyr)
library(RColorBrewer)

dataNames <- c("1_78-1_K27me3_Accutase", "2_78-1_K27me3_Trypsin", "3_78-1_K27me3_FF",
               "4_78-1_K27me3_Scraped", "5_H21792_K27ac_Accutase", "6_H21792_K27ac_Trypsin",
               "7_H21792_K9me3_Trypsin", "8_H21792_K9me3_Accutase",
               "9_H21792_K9me3_Scraped", "10_78-1_K27ac_Accutase", "11_78-1_K27ac_Scraped")

file <- c()
for (dataset in dataNames){
 file = read.table(choose.files(), header = FALSE) %>% 
  mutate(`Data Name` = dataset) %>%
  rbind(file, .)
}

x <- ggplot(file, aes(x = `Data Name`, fill = `Data Name`)) + geom_bar() +
 labs(title = "Number of Peaks Across Samples") +
 geom_text(stat = "count", aes(label = after_stat(count)),
           position = position_dodge(width = 0.9), vjust = -0.5) + xlab("Data Sample")
x + theme(axis.text.x = element_text(angle = 45, hjust = 1))


ObtainPeakLength <- function(histonemod){
 histonemod <-  histonemod %>%
  mutate(histonemod,peakLength = histonemod[,3] - histonemod[,2]) %>%
  return()
}

length <- ObtainPeakLength(file)
colors <- brewer.pal(n = length(unique(length$`Data Name`)), name = "Set3")


p <- ggplot(length, aes(x = `Data Name`, y = peakLength)) + geom_boxplot(outlier.shape = NA) +
 labs(title = "Distribution of Peak Lengths") +
 ylim(0,2000) + ylab("Peak Length") + xlab("Data Sample") 

p + theme(axis.text.x = element_text(angle = 45, hjust = 1))

