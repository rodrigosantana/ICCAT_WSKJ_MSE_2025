########################################################################
## Description: Evaluating the input data (Reconditioning Stock
## Assessment, Catches and Combined index) for the MSE process...
##
## Maintainer: Datenkraft - ICCAT (Tropical Tunas Species Group)
## Author: Rodrigo Sant'Ana
## Created: qua jun 18 18:33:23 2025 (-0300)
## Version: 0.0.1
##
## URL:
## Doc URL:
##
## Database info:
##
### Commentary:
##
### Code:
########################################################################

########################################################################
######@> Setup R...

######@> Loading R packages...
library(dplyr)
library(reshape2)
library(readr)
library(ggplot2)
library(tidyverse)
library(viridis)
library(paletteer)
library(janitor)
library(readxl)
library(r4ss)
library(patchwork)
library(cowplot)
library(openMSE)

######@> Loading R functions...
source("00_script_functions_ver00.R")

######@> Path to the SS Reconditioned Models...
path01 <- "00_Reconditioning_SS/"

######@> Path to the SS SA Models 2022...
path02 <- "01_Data/SS3_SA_2022/"

######@> Setting up ggplot theme...

#####@> Importing fonts...
extrafont::loadfonts(device = "postscript")

#####@> ggplot theme...
rgb01 <- "black"
rgb02 <- "black"
seta <- grid::arrow(length = grid::unit(0.2, "cm"), type = "open")
seta2 <- grid::arrow(length = grid::unit(0.2, "cm"), type = "open",
                     ends = "both")
my_theme <- function(base_size = 18, base_family = "Helvetica") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(axis.ticks = element_line(colour = rgb01),
              axis.line = element_line(colour = rgb01, linewidth = 0.2),
              axis.text = element_text(colour = rgb02, size = 18),
              axis.title = element_text(size = 18),
              strip.text = element_text(size = 16,
                                        margin = ggplot2::margin(0.3,
                                                                 0.3,
                                                                 0.3,
                                                                 0.3,
                                                                 "cm"),
                                        face = "bold"),
              legend.background = element_blank(),
              legend.key = element_blank(),
              legend.text = element_text(size = 18),
              legend.title = element_text(size = 20),
              panel.background = element_blank(),
              panel.grid = element_line(linetype = "solid",
                                        linewidth = 0.2,
                                        colour = "gray90"),
              plot.background = element_blank(),
              complete = TRUE)
}

#####@> Testing theme...
df <- data.frame(x = rnorm(10), y = rnorm(10),
                 z = rep(c("A", "B"), each = 5))
ggplot(data = df, aes(x = x, y = y, fill = z)) +
    geom_point(pch = 21, size = 5) +
    facet_grid(~z) +
    my_theme()

########################################################################
######@> Loading Datasets...

######@> Loading SS3 SA scenarios from 2022...
old_SS <- ler_modelos_ss(path02)

######@> Loading SS3 Reconditioned scenarios from 2025...
new_SS <- ler_modelos_ss(path01)

######@> Loading combined index (index)...
load("01_Data/Input_Combined_Index_ver00.RData")

######@> Loading T1NC from ICCAT...

#####@> Download file (Necessary update link for future downloads)...
link <- "https://iccat.int/Data/t1nc_20250131.7z"
download.file(link, destfile = "01_Data/T1NC.7z", method = "wget")

#####@> Decompress file...
zip_file <- "01_Data/T1NC.7z"
output_dir <- "01_Data/"
command <- sprintf("7zz x '%s' -o'%s'", zip_file, output_dir)
system(command)

#####@> Loading...
t1nc <- read_excel("01_Data/t1nc-20250131_ALL.xlsx",
                   sheet = "Data") %>%
    data.frame

########################################################################
######@> Evaluating Input Data...

######@>----------------------------------------------------------------
######@> Extracting data...

######@> Extracting data from the Stock Synthesis scenarios
######@> (Trajectories and other quantities)...
summ_OSS <- SSsummarize(old_SS, SpawnOutputUnits = "biomass")$quants
summ_NSS <- SSsummarize(new_SS, SpawnOutputUnits = "biomass")$quants

######@> Extracting catch data from the Stock Synthesis scenarios...
catch_OSS <- old_SS$SS01$catch
catch_NSS <- new_SS$SS01$catch

######@> Cleaning T1NC for looking only to W-SKJ data...
t1nc.wskj <- t1nc %>%
    filter(Species == "SKJ",
           Stock == "ATW")

######@>----------------------------------------------------------------
######@> Evaluating catch data...

######@> Preparing datasets...

#####@> T1NC data updated...
tab01 <- t1nc.wskj %>%
    group_by(YearC) %>%
    summarise(Catch_t = sum(Qty_t, na.rm = TRUE)) %>%
    rename("Year" = YearC) %>%
    mutate(Type = "Task 1 Nominal Catch") %>%
    as.data.frame()

#####@> Old SS data...
tab02 <- catch_OSS %>%
    group_by(Yr) %>%
    summarise(Catch_t = sum(Obs, na.rm = TRUE)) %>%
    rename("Year" = Yr) %>%
    mutate(Type = "Stock Assessment 2022") %>%
    as.data.frame()

#####@> New SS data...
tab03 <- catch_NSS %>%
    group_by(Yr) %>%
    summarise(Catch_t = sum(Obs, na.rm = TRUE)) %>%
    rename("Year" = Yr) %>%
    mutate(Type = "Reconditioning 2025") %>%
    as.data.frame()

#####@> Combining datasets...
catches <- gtools::smartbind(tab01, tab02, tab03) %>%
    mutate(Type = factor(Type,
                         levels = c("Task 1 Nominal Catch",
                                    "Stock Assessment 2022",
                                    "Reconditioning 2025")))

######@> Evaluating the distinct datasets...
p00 <- ggplot(data = catches,
              aes(x = Year, y = Catch_t, colour = Type)) +
    geom_line(aes(linewidth = Type, linetype = Type)) +
    labs(x = "Year", y = "Catches (metric tons)", colour = "",
         linewidth = "", linetype = "") +
    scale_y_continuous(limits = c(0, 50000), expand = c(0, 0)) +
    scale_x_continuous(limits = c(1950, 2025),
                       breaks = seq(1950, 2025, 5)) +
    scale_linewidth_manual(values = c("Task 1 Nominal Catch" = 1.2,
                                      "Stock Assessment 2022" = 1.1,
                                      "Reconditioning 2025" = 1.0)) +
    scale_colour_paletteer_d("nationalparkcolors::Acadia",
                             direction = -1) +
    my_theme() +
    theme(legend.position = "inside",
          legend.position.inside = c(0.15, 0.9))
p00

p01 <- ggplot(data = catches,
              aes(x = Year, y = Catch_t, fill = Type, colour = Type,
                  linetype = Type)) +
    geom_area(alpha = 0.5) +
    labs(x = "Year", y = "Catches (metric tons)", colour = "",
         fill = "") +
    scale_y_continuous(limits = c(0, 50000), expand = c(0, 0)) +
    scale_x_continuous(limits = c(1950, 2025),
                       breaks = seq(1950, 2025, 15)) +
    scale_colour_paletteer_d("nationalparkcolors::Acadia",
                             direction = -1) +
    scale_fill_paletteer_d("nationalparkcolors::Acadia",
                             direction = -1) +
    facet_wrap(~Type, ncol = 3) +
    my_theme() +
    theme(legend.position = "none")
p01

#####@> Calculating differences...
tmp00 <- tab01 %>%
    full_join(tab02, by = "Year") %>%
    full_join(tab03, by = "Year") %>%
    select(Year, "T1NC" = Catch_t.x,
           "SA2022" = Catch_t.y,
           "RC2025" = Catch_t) %>%
    mutate(diff01 = T1NC - SA2022,
           diff02 = T1NC - RC2025,
           diff03 = SA2022 - RC2025) %>%
    select(Year, diff01, diff02, diff03) %>%
    pivot_longer(names_to = "Type", values_to = "Diff", 2:4) %>%
    mutate(Type = ifelse(Type == "diff01", "T1NC - SA2022",
                  ifelse(Type == "diff02", "T1NC - RC2025",
                         "SA2022 - RC2025")))

p02 <- ggplot(data = tmp00,
              aes(x = Year, y = Diff, fill = Type, colour = Type,
                  linetype = Type)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) + 
    geom_area(position = "identity", alpha = 0.5) +
    labs(x = "Year", y = "Catches (metric tons)", colour = "",
         fill = "", linetype = "") +
    scale_y_continuous(limits = c(-2000, 8000), expand = c(0, 0)) +
    scale_x_continuous(limits = c(1950, 2025),
                       breaks = seq(1950, 2025, 5)) +
    scale_colour_paletteer_d("nationalparkcolors::Acadia",
                             direction = -1) +
    scale_fill_paletteer_d("nationalparkcolors::Acadia",
                           direction = -1) +
    my_theme() +
    theme(legend.position = "inside",
          legend.position.inside = c(0.15, 0.9))
p02

#####@> Combining plots...
plot00 <- p00 / p01 / p02

ggsave("07_Results/02_Input_Data/Fig_03_Comp_Catches_ver00.tiff",
       plot = plot00, device = "tiff", units = "cm", w = 35, h = 40,
       dpi = 600, bg = "white")

######@>----------------------------------------------------------------
######@> Evaluating SSB, SSBmsy, SSB/SSBmsy, MSY, Fmsy, F_Fmsy
######@> trajectories...

######@> Extracting SSB, SSBmsy, SSB/SSBmsy, MSY, Fmsy and F_Fmsy
######@> trajectories...

#####@> SSB...
ssb_OSS <- summ_OSS %>%
    filter(grepl("SSB_[0-9]", Label)) %>%
    pivot_longer(names_to = "Scenarios", values_to = "SSB", 1:9) %>%
    select("Year" = Yr, Scenarios, SSB) %>%
    mutate(Type = "Stock Assessment 2022") %>%
    arrange(Scenarios, Year) %>%
    as.data.frame()

ssb_NSS <- summ_NSS %>%
    filter(grepl("SSB_[0-9]", Label)) %>%
    pivot_longer(names_to = "Scenarios", values_to = "SSB", 1:9) %>%
    select("Year" = Yr, Scenarios, SSB) %>%
    mutate(Type = "Reconditioning 2025") %>%
    arrange(Scenarios, Year) %>%
    as.data.frame()

#####@> SSBmsy...
ssbmsy_OSS <- summ_OSS %>%
    filter(grepl("SSB_MSY", Label)) %>%
    pivot_longer(names_to = "Scenarios", values_to = "SSBmsy", 1:9) %>%
    select(Scenarios, SSBmsy) %>%
    mutate(Type = "Stock Assessment 2022") %>%
    arrange(Scenarios) %>%
    as.data.frame()

ssbmsy_NSS <- summ_NSS %>%
    filter(grepl("SSB_MSY", Label)) %>%
    pivot_longer(names_to = "Scenarios", values_to = "SSBmsy", 1:9) %>%
    select(Scenarios, SSBmsy) %>%
    mutate(Type = "Reconditioning 2025") %>%
    arrange(Scenarios) %>%
    as.data.frame()

#####@> SSB/SSBmsy...
ssb_ssbmsy_OSS <- ssb_OSS %>%
    left_join(select(ssbmsy_OSS, -Type), by = "Scenarios") %>%
    mutate(SSB_SSBmsy = SSB/SSBmsy) %>%
    mutate(Year = Year - 1) %>%
    ## mutate(Type = "Stock Assessment 2022") %>%
    as.data.frame()

ssb_ssbmsy_NSS <- ssb_NSS %>%
    left_join(select(ssbmsy_NSS, -Type), by = "Scenarios") %>%
    mutate(SSB_SSBmsy = SSB/SSBmsy) %>%
    mutate(Year = Year - 1) %>%
    ## mutate(Type = "Reconditioning 2025") %>%
    as.data.frame()

#####@> MSY...
msy_OSS <- summ_OSS %>%
    filter(grepl("Dead_Catch_MSY", Label)) %>%
    pivot_longer(names_to = "Scenarios", values_to = "MSY", 1:9) %>%
    select(Scenarios, MSY) %>%
    mutate(Type = "Stock Assessment 2022") %>%
    arrange(Scenarios) %>%
    as.data.frame()

msy_NSS <- summ_NSS %>%
    filter(grepl("Dead_Catch_MSY", Label)) %>%
    pivot_longer(names_to = "Scenarios", values_to = "MSY", 1:9) %>%
    select(Scenarios, MSY) %>%
    mutate(Type = "Reconditioning 2025") %>%
    arrange(Scenarios) %>%
    as.data.frame()

#####@> F/Fmsy...
f_fmsy_OSS <- summ_OSS %>%
    filter(grepl("F_[0-9]", Label)) %>%
    pivot_longer(names_to = "Scenarios", values_to = "F_Fmsy", 1:9) %>%
    select("Year" = Yr, Scenarios, F_Fmsy) %>%
    mutate(Type = "Stock Assessment 2022") %>%
    arrange(Scenarios, Year) %>%
    filter(Year != 2021) %>%
    as.data.frame()

f_fmsy_NSS <- summ_NSS %>%
    filter(grepl("F_[0-9]", Label)) %>%
    pivot_longer(names_to = "Scenarios", values_to = "F_Fmsy", 1:9) %>%
    select("Year" = Yr, Scenarios, F_Fmsy) %>%
    mutate(Type = "Reconditioning 2025") %>%
    arrange(Scenarios, Year) %>%
    filter(Year != 2025) %>%
    as.data.frame()

#####@> FMSY...
fmsy_OSS <- summ_OSS %>%
    filter(grepl("annF_MSY", Label)) %>%
    pivot_longer(names_to = "Scenarios", values_to = "Fmsy", 1:9) %>%
    select(Scenarios, Fmsy) %>%
    mutate(Type = "Stock Assessment 2022") %>%
    arrange(Scenarios) %>%
    as.data.frame()

fmsy_NSS <- summ_NSS %>%
    filter(grepl("annF_MSY", Label)) %>%
    pivot_longer(names_to = "Scenarios", values_to = "Fmsy", 1:9) %>%
    select(Scenarios, Fmsy) %>%
    mutate(Type = "Reconditioning 2025") %>%
    arrange(Scenarios) %>%
    as.data.frame()

######@> Evaluating the trajectories between SA 2022 and RC 2025...

#####@> SSB...
SSB <- gtools::smartbind(ssb_OSS, ssb_NSS)

####@> Visualizing...
p05 <- ggplot(data = SSB,
              aes(x = Year, y = SSB, fill = Type, colour = Type,
                  linetype = Type)) +
    geom_line(linewidth = 0.9) +
    labs(x = "Year", y = "Spawning Stock Biomass", colour = "",
         fill = "", linetype = "") +
    ## scale_y_continuous(limits = c(0, 50000), expand = c(0, 0)) +
    scale_x_continuous(limits = c(1950, 2025),
                       breaks = seq(1950, 2025, 15)) +
    scale_colour_paletteer_d("nationalparkcolors::Acadia",
                             direction = -1) +
    scale_fill_paletteer_d("nationalparkcolors::Acadia",
                           direction = -1) +
    facet_wrap(~Scenarios, ncol = 3) +
    my_theme() +
    theme(legend.position = "top")
p05

ggsave("07_Results/02_Input_Data/Fig_04_Comp_SSB_ver00.tiff",
       plot = p05, device = "tiff", units = "cm", w = 35, h = 35,
       dpi = 600, bg = "white")

#####@> SSB/SSBmsy...
SSB_SSBmsy <- gtools::smartbind(ssb_ssbmsy_OSS, ssb_ssbmsy_NSS)

####@> Visualizing...
p06 <- ggplot(data = SSB_SSBmsy,
              aes(x = Year, y = SSB_SSBmsy, fill = Type, colour = Type,
                  linetype = Type)) +
    geom_line(linewidth = 0.9) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
    labs(x = "Year", y = expression(SSB/SSB[MSY]), colour = "",
         fill = "", linetype = "") +
    ## scale_y_continuous(limits = c(0, 50000), expand = c(0, 0)) +
    scale_x_continuous(limits = c(1950, 2025),
                       breaks = seq(1950, 2025, 15)) +
    scale_colour_paletteer_d("nationalparkcolors::Acadia",
                             direction = -1) +
    scale_fill_paletteer_d("nationalparkcolors::Acadia",
                           direction = -1) +
    facet_wrap(~Scenarios, ncol = 3) +
    my_theme() +
    theme(legend.position = "top")
p06

ggsave("07_Results/02_Input_Data/Fig_05_Comp_SSB_SSBmsy_ver00.tiff",
       plot = p06, device = "tiff", units = "cm", w = 35, h = 35,
       dpi = 600, bg = "white")

#####@> MSY...
MSY <- gtools::smartbind(msy_OSS, msy_NSS)

####@> Comparing slopes...
MSY <- MSY %>%
    pivot_wider(names_from = Type, values_from = MSY) %>%
    mutate(Diff = `Reconditioning 2025` - `Stock Assessment 2022`,
           Test = case_when(
               Diff > 0 ~ "Up",
               Diff < 0 ~ "Down")) %>%
    select(-Diff) %>%
    pivot_longer(names_to = "Type", values_to = "MSY",
                 cols = c(`Stock Assessment 2022`,
                          `Reconditioning 2025`)) %>%
    arrange(Type, Scenarios) %>%
    mutate(Type = factor(Type,
                         levels = c("Stock Assessment 2022",
                                    "Reconditioning 2025")))

####@> Visualizing...
p07 <- ggplot(data = MSY,
              aes(x = Type, y = MSY, fill = Test, colour = Test)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "gray50") +
    geom_vline(xintercept = 2, linetype = "dashed", colour = "gray50") +
    geom_line(aes(group = Scenarios), linewidth = 1) +
    geom_point(size = 3) +
    geom_text(data = filter(MSY, Type == "Stock Assessment 2022"),
              aes(x = Type, y = MSY, label = Scenarios),
              hjust = 1.2, colour = "black") +
    geom_text(data = filter(MSY, Type == "Reconditioning 2025"),
              aes(x = Type, y = MSY, label = Scenarios),
              hjust = -0.2, colour = "black") +
    labs(x = "", y = "Maximum Sustainable Yield", colour = "",
         fill = "", linetype = "") +
    scale_y_continuous(limits = c(25000, 50000), expand = c(0, 0)) +
    scale_colour_manual(values = c("Up" = "#00ba38",
                                   "Down" = "#f8766d")) +
    my_theme() +
    theme(legend.position = "none")
p07

ggsave("07_Results/02_Input_Data/Fig_06_Comp_MSY_ver00.tiff",
       plot = p07, device = "tiff", units = "cm", w = 25, h = 30,
       dpi = 600, bg = "white")

#####@> F/Fmsy...
F_Fmsy <- gtools::smartbind(f_fmsy_OSS, f_fmsy_NSS)

####@> Visualizing...
p08 <- ggplot(data = F_Fmsy,
              aes(x = Year, y = F_Fmsy, fill = Type, colour = Type,
                  linetype = Type)) +
    geom_line(linewidth = 0.9) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
    labs(x = "Year", y = expression(F/F[MSY]), colour = "",
         fill = "", linetype = "") +
    scale_y_continuous(limits = c(0, 1.2), expand = c(0, 0),
                       breaks = seq(0, 1.2, 0.2)) +
    scale_x_continuous(limits = c(1950, 2025),
                       breaks = seq(1950, 2025, 15)) +
    scale_colour_paletteer_d("nationalparkcolors::Acadia",
                             direction = -1) +
    scale_fill_paletteer_d("nationalparkcolors::Acadia",
                           direction = -1) +
    facet_wrap(~Scenarios, ncol = 3) +
    my_theme() +
    theme(legend.position = "top")
p08

ggsave("07_Results/02_Input_Data/Fig_07_Comp_F_Fmsy_ver00.tiff",
       plot = p08, device = "tiff", units = "cm", w = 35, h = 35,
       dpi = 600, bg = "white")

######@>----------------------------------------------------------------
######@> Comparing SSB/SSBmsy against Combined Index...

######@> Preparing data...

#####@> Preparing deterministic data...
temp <- SSB_SSBmsy %>%
    group_by(Year, Type) %>%
    summarise(SSB_SSBmsy = median(SSB_SSBmsy)) %>%
    arrange(Type, Year) %>%
    data.frame

####@> Average overlaped timeseries...
AVSSB_SSBmsy <- temp %>%
    filter(Year %in% index$Year) %>%
    group_by(Type) %>%
    summarise(AVSSB_SSBmsy = mean(SSB_SSBmsy)) %>%
    as.data.frame()

####@> Restimating the Scaled SSB_SSBmsy...
temp <- temp %>%
    left_join(AVSSB_SSBmsy, by = "Type") %>%
    mutate(SSBSSBmsy = SSB_SSBmsy / AVSSB_SSBmsy) %>%
    select(Year, Type, "Value" = SSBSSBmsy)

#####@> Smoothing the combined index...
index$Smooth <- as.numeric(stats::smooth(index$Obs, kind = "3RS3R",
                                         twiceit = TRUE,
                                         endrule = "copy"))

####@> Formating index data.frame...
index <- index %>%
    rename("Scaled" = Obs) %>%
    pivot_longer(names_to = "Type", values_to = "Value", 2:3) %>%
    mutate(Type = ifelse(Type == "Scaled", "Scaled abundance index",
                         "Smooth abundance index")) %>%
    as.data.frame()

#####@> Combining datasets...
temp02 <- gtools::smartbind(temp, index) %>%
    mutate(Test = "Scaled abundance index") %>%
    filter(Type %in% c("Stock Assessment 2022",
                       "Reconditioning 2025",
                       "Scaled abundance index")) %>%
    mutate(Type = factor(Type,
                         levels = c("Stock Assessment 2022",
                                    "Reconditioning 2025",
                                    "Scaled abundance index")))

temp03 <- gtools::smartbind(temp, index) %>%
    mutate(Test = "Smooth abundance index") %>%
    filter(Type %in% c("Stock Assessment 2022",
                       "Reconditioning 2025",
                       "Smooth abundance index")) %>%
    mutate(Type = factor(Type,
                         levels = c("Stock Assessment 2022",
                                    "Reconditioning 2025",
                                    "Smooth abundance index")))

######@> Visualizing tranjectories...
p09 <- ggplot(data = temp02,
              aes(x = Year, y = Value, colour = Type,
                  linetype = Type)) +
    geom_line(linewidth = 1.2) +
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
    scale_x_continuous(limits = c(1950, 2025),
                       breaks = seq(1950, 2025, 5)) +
    scale_colour_manual(values = c("#023743", "#72874E", "#476F84")) +
    ## scale_linetype_manual(
    ##     values = c("Stock Assessment 2022" = "dashed",
    ##                "Reconditioning 2025" = "solid",
    ##                "Scaled abundance index" = "longdash")) +
    labs(x = "Year",
         y = bquote("Rescaled values" ~ "(" * SSB/SSB[MSY] *
                        " | Index)"),
         colour = "", linetype = "") +
    facet_wrap(~Test) +
    my_theme() +
    theme(legend.position = "top")
p09

p10 <- ggplot(data = temp03,
              aes(x = Year, y = Value, colour = Type,
                  linetype = Type)) +
    geom_line(linewidth = 1.2) +
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
    scale_x_continuous(limits = c(1950, 2025),
                       breaks = seq(1950, 2025, 5)) +
    scale_colour_manual(values = c("#023743", "#72874E", "#476F84")) +
    ## scale_linetype_manual(
    ##     values = c("Stock Assessment 2022" = "dashed",
    ##                "Reconditioning 2025" = "solid",
    ##                "Scaled abundance index" = "longdash")) +
    labs(x = "Year",
         y = bquote("Rescaled values" ~ "(" * SSB/SSB[MSY] *
                        " | Index)"),
         colour = "", linetype = "") +
    facet_wrap(~Test) +
    my_theme() +
    theme(legend.position = "top")
p10

####@> Combining plots...
plot01 <- p09 / p10

ggsave("07_Results/02_Input_Data/Fig_08_Comp_Index_SSB_SSBmsy_ver00.tiff",
       plot = plot01, device = "tiff", units = "cm", w = 35, h = 35,
       dpi = 600, bg = "white")


########################################################################
##
##                  Creative Commons License 4.0
##                       (CC BY-NC-SA 4.0)
##
##  This is a humam-readable summary of (and not a substitute for) the
##  license (https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode)
##
##  You are free to:
##
##  Share - copy and redistribute the material in any medium or format.
##
##  The licensor cannot revoke these freedoms as long as you follow the
##  license terms.
##
##  Under the following terms:
##
##  Attribution - You must give appropriate credit, provide a link to
##  license, and indicate if changes were made. You may do so in any
##  reasonable manner, but not in any way that suggests the licensor
##  endorses you or your use.
##
##  NonCommercial - You may not use the material for commercial
##  purposes.
##
##  ShareAlike - If you remix, transform, or build upon the material,
##  you must distributive your contributions under the same license
##  as the  original.
##
##  No additional restrictions — You may not apply legal terms or
##  technological measures that legally restrict others from doing
##  anything the license permits.
##
########################################################################
