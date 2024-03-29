rm(list = ls())
setwd("C:/Users/raoda/OneDrive/Desktop/R Stuff/CUT&Tag Processing")
library(tidyverse)
library(dplyr)
library(RColorBrewer)

accutase <- c("1_78-1_K27me3_Accutase", "5_H21792_K27ac_Accutase",
               "8_H21792_K9me3_Accutase", "10_78-1_K27ac_Accutase")
trypsin <- c("2_78-1_K27me3_Trypsin", "6_H21792_K27ac_Trypsin", "7_H21792_K9me3_Trypsin")
scraped <- c("4_78-1_K27me3_Scraped", "9_H21792_K9me3_Scraped", "11_78-1_K27ac_Scraped")
flashfroz <- c("3_78-1_K27me3_FF")

batchMySamples <- function(mySamples, collectionMethod){
  file <- c()
  for(samples in mySamples){
    file = read.table(choose.files(), header = FALSE) %>% 
      mutate(`Data Name` = samples,`Collection Method` = collectionMethod) %>%
      rbind(file, .)
    }
  return(file)
}

accutaseSamples <- batchMySamples(accutase,"Accutase")
trypsinSamples <- batchMySamples(trypsin,"Trypsin")
scrapedSamples <- batchMySamples(scraped,"Scraped")
ffSamples <- batchMySamples(flashfroz,"Flash Frozen")

bigBatched <- rbind(accutaseSamples,trypsinSamples,scrapedSamples,ffSamples)

# Plotting the bar plot -------------------------------------------------------------

barPlot <- function(input){
  x <- ggplot(input, aes(x = `Data Name`, fill = `Data Name`)) + geom_bar() +
    labs(title = "Number of SEACR Peaks Across Samples") +
    geom_text(stat = "count", aes(label = after_stat(count)),
              position = position_dodge(width = 0.9), vjust = -0.5) + xlab("Data Sample")
  x + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

barPlot(accutaseSamples)
barPlot(trypsinSamples)
barPlot(scrapedSamples)
barPlot(ffSamples)

names <- as.data.frame(unique(bigBatched$`Data Name`))

z <- ggplot(bigBatched, aes(x = `Data Name`, fill = `Data Name`)) + 
  geom_bar() + labs(title = "Number of SEACR Peaks Across Samples") + 
  facet_wrap(~`Collection Method`, ncol = 1) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            position = position_dodge(width = 0.9), vjust = -0.5) + 
  xlab("Data Sample") + coord_cartesian(ylim = c(0,40000))
z + theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

