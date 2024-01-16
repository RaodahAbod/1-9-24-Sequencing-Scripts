rm(list = ls())
setwd("C:/Users/raoda/Desktop/R Stuff/CUT&Tag Processing")
library(tidyverse)
library(dplyr)
library(viridis)

sampleList_k27me3 <- c("1_78-1_K27me3_Accutase", "2_78-1_K27me3_Trypsin", "3_78-1_K27me3_FF",
                "4_78-1_K27me3_Scraped", "7_H21792_K27me3_Trypsin", "8_H21792_K27me3_Accutase",
                "9_H21792_K27me3_Scraped")

sampleList_k27ac <- c("5_H21792_K27ac_Accutase", "6_H21792_K27ac_Trypsin",
                      "10_78-1_K27ac_Accutase", "11_78-1_K27ac_Scraped")

sampleList <- c(sampleList_k27ac,sampleList_k27me3)

fragLen_1 <- c()
for(sample in sampleList_k27me3){
        histInfo = "H3K27me3"
        fragLen_1 = read.table(choose.files(), header = FALSE) %>% 
                mutate(fragLen = V1 %>% as.numeric, 
                       fragCount = V2 %>% as.numeric, 
                       Weight = as.numeric(V2)/sum(as.numeric(V2)), 
                       Histone = histInfo[1], sampleInfo = sample) %>% rbind(fragLen_1, .) 
}

fragLen_2 <- c()
for(sample in sampleList_k27ac){
        histInfo = "H3K27ac"
        fragLen_2 = read.table(choose.files(), header = FALSE) %>% 
                mutate(fragLen = V1 %>% as.numeric, 
                       fragCount = V2 %>% as.numeric, 
                       Weight = as.numeric(V2)/sum(as.numeric(V2)), 
                       Histone = histInfo[1], sampleInfo = sample) %>% rbind(fragLen_2, .) 
}

fragLen <- rbind(fragLen_1,fragLen_2)

#fragLen$sampleInfo = factor(fragLen$sampleInfo, levels = sampleList)
#fragLen$Histone = factor(fragLen$Histone, levels = histList)

fragViolinPlot <- fragLen %>%
        ggplot(aes(x = sampleInfo, y = fragLen, weight = Weight, fill = sampleInfo)) +
        geom_violin(bw = 5, alpha = 0.8) +
        scale_y_continuous(breaks = seq(0, 800, 50)) +
        theme_bw(base_size = 20) +
        ylim(0, 1000) +
        ylab("Fragment Length") +
        xlab("")

fragViolinPlot + theme(axis.text.x = element_text(angle = 45, hjust = 1))



fragHistDist = fragLen %>% ggplot(aes(x = fragLen, y = fragCount, color = sampleInfo, 
                               group = sampleInfo, linetype = Histone)) +
        geom_line(size = 1) +
        scale_fill_manual(values = CUTTag_Colors) +  # Use scale_fill_manual with your custom colors        theme_bw(base_size = 20) +
        xlab("Fragment Length") +
        ylab("Count") +
        ylim(0,45000) +
        coord_cartesian(xlim = c(0, 500))
fragHistDist

fragHistDist <- fragLen %>% 
        ggplot(aes(x = fragLen, y = fragCount, color = sampleInfo, group = sampleInfo, linetype = Histone)) +
        geom_line(size = 1) +
        scale_color_manual(values = CUTTag_Colors) +  # Use scale_color_manual for line colors
        scale_linetype_manual(values = c("solid", "dashed")) +  # Use scale_linetype_manual for line types
        theme_bw(base_size = 20) +
        xlab("Fragment Length") +
        ylab("Count") +
        ylim(0, 20000) +
        coord_cartesian(xlim = c(0, 500))

fragHistDist



