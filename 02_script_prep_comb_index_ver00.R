########################################################################
## Description: Preparing the updated abundance indices available for
## the reconditioning Operating Models...
##
## Maintainer: Datenkraft - ICCAT (Tropical Tunas Species Group)
## Author: Rodrigo Sant'Ana
## Created: qua jun 18 11:12:08 2025 (-0300)
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

######@> Loading updated indices..

#####@> Original SA Indices 2022...
tsCPUE2022 <- read_excel("01_Data/SKJEW_CPUE2022_06May2025.xlsx",
                         sheet = 2, range = "A7:M47") %>%
    select(Year,
           "BB_West_Obs" = "Index...2", "BB_West_SE" = "SE...3",
           "HL_RR_Obs" = "Index...4", "HL_RR_SE" = "SE...5",
           "US_GOM_Obs" = "Index...6", "US_GOM_SE" = "CV...7",
           "LL_USMX_Obs" = "Index...8", "LL_USMX_SE" = "CV...9",
           "PS_West_Obs" = "Index...10", "PS_West_SE" = "CV...11",
           "BRA_BB_hist_Obs" = "Index...12", "BRA_BB_hist_SE" =
                                                 "CV...13") %>%
    mutate(Text = "Original SA Indices 2022") %>%
    data.frame

####@> averaging to the mean...
tsCPUE2022$BRA_BB_hist_Obs <- tsCPUE2022$BRA_BB_hist_Obs /
    mean(tsCPUE2022$BRA_BB_hist_Obs, na.rm = TRUE)
tsCPUE2022$PS_West_Obs <- tsCPUE2022$PS_West_Obs /
    mean(tsCPUE2022$PS_West_Obs, na.rm = TRUE)

#####@> Updated Indices 2024...
tsCPUE2024 <- read_excel("01_Data/SKJEW_CPUE2022_06May2025.xlsx",
                        sheet = 5, range = "A6:M49") %>%
    select(Year,
           "BB_West_Obs" = "Index...2", "BB_West_SE" = "SD",
           "HL_RR_Obs" = "Index...4", "HL_RR_SE" = "SE",
           "US_GOM_Obs" = "Index...6", "US_GOM_SE" = "CV...7",
           "LL_USMX_Obs" = "Index...8", "LL_USMX_SE" = "CV...9",
           "PS_West_Obs" = "Index...10", "PS_West_SE" = "CV...11",
           "BRA_BB_hist_Obs" = "Index...12", "BRA_BB_hist_SE" =
                                                 "CV...13") %>%
    mutate(Text = "Updated Indices 2024") %>%
    data.frame

####@> averaging to the mean...
tsCPUE2024$BRA_BB_hist_Obs <- tsCPUE2024$BRA_BB_hist_Obs /
    mean(tsCPUE2024$BRA_BB_hist_Obs, na.rm = TRUE)
tsCPUE2024$PS_West_Obs <- tsCPUE2024$PS_West_Obs /
    mean(tsCPUE2024$PS_West_Obs, na.rm = TRUE)

#####@> Updated Indices 2025...
tsCPUE2025 <- read_excel("01_Data/SKJEW_CPUE2022_06May2025.xlsx",
                         sheet = "W-SKJ_MSE2025") %>%
    mutate(Text = "Updated Indices 2025") %>%
    data.frame

####@> averaging to the mean...
tsCPUE2025$BRA_BB_hist_Obs <- tsCPUE2025$BRA_BB_hist_Obs /
    mean(tsCPUE2025$BRA_BB_hist_Obs, na.rm = TRUE)
tsCPUE2025$PS_West_Obs <- tsCPUE2025$PS_West_Obs /
    mean(tsCPUE2025$PS_West_Obs, na.rm = TRUE)

########################################################################
######@> Evaluation abundance indices available...

######@> Merging all bases and selecting those indices that will be
######@> included on the combine index...
tsCPUE <- gtools::smartbind(tsCPUE2022, tsCPUE2024, tsCPUE2025) %>%
    select(Year:BB_West_SE, LL_USMX_Obs:Text) %>%
    pivot_longer(names_to = "Fleet_name", values_to = "Values", 2:9) %>%
    mutate(Type = extract_obs_se(Fleet_name),
           Fleet_name = remove_obs_se(Fleet_name)) %>%
    pivot_wider(names_from = "Type", values_from = "Values") %>%
    mutate(Fleet =
               ifelse(Fleet_name %in% c("BRA_BB_hist", "BB_West"),
                      "BB_West",
               ifelse(Fleet_name == "LL_USMX", "LL_USA",
                      "PS_West"))) %>%
    group_by(Year, Text, Fleet) %>%
    summarise(Obs = mean(Obs, na.rm = TRUE),
              SE = mean(SE, na.rm = TRUE))

####@> Visualizing...
p00 <- ggplot() +
    geom_line(data = tsCPUE,
              aes(x = Year, y = Obs, colour = Text), linewidth = 0.9) +
    facet_wrap(~Fleet, ncol = 1) +
    labs(x = "Year", y = "Scaled abundance index", colour = "") +
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
    scale_x_continuous(limits = c(1980, 2025),
                       breaks = seq(1980, 2025, 5)) + 
    my_theme() +
    theme(legend.position = c(0.2, 0.25))
p00

######@> Exportando as figuras anuais...
ggsave("06_Results/01_Indices/Fig_01_Comp_Indices_ver00.tiff",
       plot = p00, device = "tiff", units = "cm", w = 25, h = 35,
       dpi = 600, bg = "white")

########################################################################
######@> Preparing combined index...

#####@> Inverse variance weighted mean for the Brazilian, Venezuelan and
######> USA indices...
temp00 <- tsCPUE %>%
    mutate(w = 1/SE) %>%
    mutate(mu_w = Obs * w)

####@> Dump method 01...
tmp05A <- temp00 %>%
    group_by(Text, Year) %>%
    summarise(A = sum(mu_w, na.rm = TRUE),
              B = sum(w, na.rm = TRUE)) %>%
    mutate(Obs = A/B) %>%
    mutate(Fleet = "Inverse variance weighted",
           Method = "Method 01")

####@> Method 02...
tmp05B <- temp00 %>%
    group_by(Text, Year) %>%
    summarise(Obs = weighted.mean(Obs, 1/SE, na.rm = TRUE)) %>%
    mutate(Fleet = "Inverse variance weighted",
           Method = "Method 02")

####@> Combine...
tmp06 <- gtools::smartbind(as.data.frame(tmp05A),
                           as.data.frame(tmp05B))

####@> Visualizing - checking plot...
p01 <- ggplot() +
    geom_line(data = tmp06,
              aes(x = Year, y = Obs, colour = Method,
                  linewidth = Method)) +
    facet_wrap(~Text, ncol = 1) +
    labs(x = "Year", y = "Scaled abundance index", colour = "",
         linewidth = "") +
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
    scale_x_continuous(limits = c(1980, 2025),
                       breaks = seq(1980, 2025, 5)) +
    scale_linewidth_manual(values = c("Method 01" = 1.7,
                                      "Method 02" = 0.9)) +
    my_theme() +
    theme(legend.position = c(0.2, 0.25))
p01

####@> Visualizing - real plot...
p01 <- ggplot() +
    geom_line(data = tmp05B,
              aes(x = Year, y = Obs, colour = Text),
              linewidth = 0.9) +
    ## facet_wrap(~Text, ncol = 1) +
    labs(x = "Year", y = "Scaled abundance index", colour = "") +
    scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
    scale_x_continuous(limits = c(1980, 2025),
                       breaks = seq(1980, 2025, 5)) +
    my_theme() +
    theme(legend.position = c(0.2, 0.95))
p01


######@> Exportando as figuras anuais...
ggsave("06_Results/01_Indices/Fig_02_Comp_Inverse_Variance_ver00.tiff",
       plot = p01, device = "tiff", units = "cm", w = 35, h = 20,
       dpi = 600, bg = "white")

########################################################################
######@> Extracting final series to be included in MSE...

######@> Extraction...
index <- tmp05B %>%
    filter(Text == "Updated Indices 2025") %>%
    ungroup() %>%
    select(Year, Obs) %>%
    as.data.frame()

######@> Exporting...
save(index, file = "01_Data/Input_Combined_Index_ver00.RData")

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
