########################################################################
## Description: Simulating Historical Data based on the Operating Models
## imported from the SS3 Reconditioning Models...
##
## Maintainer: Datenkraft - ICCAT (Tropical Tuna Species Group)
## Author: Rodrigo Sant'Ana
## Powered by: Adrian Hordyk
## Created: sáb jun 21 20:01:23 2025 (-0300)
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

######@> Update openMSE R packages (This will update openMSE packages if
######@> there are updates on GitHub)...
source("01_script_openMSE_packages_ver00.R")

######@> Load some custom functions...
source("00_script_functions_ver00.R")

######@> Loading R packages...
library(openMSE)
library(dplyr)
library(reshape2)
library(readr)
library(ggplot2)
library(tidyverse)
library(viridis)
library(janitor)
library(readxl)
library(patchwork)
library(paletteer)
library(ggrepel)
library(r4ss)

######@> ggplot theme...
extrafont::loadfonts(device = "postscript")
rgb01 <- "black"
rgb02 <- "black"
seta <- grid::arrow(length = grid::unit(0.2, "cm"), type = "open")
seta2 <- grid::arrow(length = grid::unit(0.2, "cm"), type = "open",
                     ends = "both")
my_theme <- function(base_size = 18, base_family = "Helvetica") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(axis.ticks = element_line(colour = rgb01),
              axis.line = element_line(colour = rgb01, linewidth = 0.2),
              axis.text = element_text(colour = rgb02, size = 14),
              axis.title = element_text(size = 18),
              legend.background = element_blank(),
              legend.key = element_blank(),
              panel.background = element_blank(),
              panel.grid = element_line(linetype = "solid",
                                        linewidth = 0.1,
                                        colour = "gray90"),
              plot.background = element_blank(),
              complete = TRUE)
}

######@> Creating a folder to receive the hist objects...
if(dir.exists("03_Hists")) {
    print("OK, 03_Hists directory was already created!");
    output.dir <- "03_Hists/"
} else {
    dir.create("03_Hists");
    output.dir <- "03_Hists/"
}

######@> OM folder...
path01 <- "02_OMs/"

########################################################################
######@> Loading data...

######@> Operating models...
OMs <- readRDS(paste0(path01, "SS_Operating_models_IVInds_ver03.rds"),
               refhook = NULL)

#####@> Setting names for OMs...
names(OMs) <- paste0("OM", sprintf("%03d", 1:9))

######@> Loading SS3 Reconditioned scenarios from 2025...
new_SS <- ler_modelos_ss("00_Reconditioning_SS/")

########################################################################
######@> Simulating Historical Data...

######@> List of objects...
OM_Objects <- names(OMs)

######@> Looping to create historical data...
HistList <- list()
for(i in seq_along(OM_Objects)) {
  OM <- OMs[[i]]
  Hist <- Simulate(OM, parallel = FALSE, silent = FALSE)
  HistList[[i]] <- Hist
  nm <- paste0(OM_Objects[i], "_IVInds_ver03", ".hist")
  saveRDS(Hist, file.path("03_Hists", nm))
}
saveRDS(HistList, "03_Hists/HistList.rda")

########################################################################
######@> Examining historical data simulated...

######@> Examine Recruitment Deviations...
## Maxage - 1:6
## Historical period - 7:79 (73 years)
## Historical period estimated in SS - 48:79 (32 years)
## Projection period - 80:109 (30 years)

#####@> Fast look...
sims <- sample(1:100, 3) # random 3 simulations
par(mfrow = c(3, 3), mar = c(1, 1, 1, 1), oma = c(1, 1, 1, 1))
for (i in 1:9) {
    devs <- t(HistList[[i]]@SampPars$Stock$Perr_y[sims,])
    devs <- devs[45:105,]
    matplot(log(devs), type = "l", xlab = "", ylab = "",
            lwd = 1.3, ylim = c(-1, 1))
}

#####@> Deep look..
RecDevs <- extrair_perr_hist(HistList)

####@> Extracting a full summary from the RecDevs dataframe...
summRecDevs <- excedencias_por_OM_detalhada(RecDevs)

####@> Merging data...
RecDevs <- RecDevs %>%
    left_join(summRecDevs$detalhes)

####@> looking to the RecDevs structure...
p00 <- ggplot() +
    geom_line(data = filter(RecDevs, Year %in% 1987:2024),
              aes(x = Year, y = Devs, group = Sim),
              colour = "black") +
    geom_line(data = filter(RecDevs, Year > 2023),
              aes(x = Year, y = Devs, group = Sim,
                  colour = passou_qualquer),
              alpha = 0.1) +
    geom_segment(data = summRecDevs$resumo,
                 aes(x = 2012, xend = 2054,
                     y = limite_min, yend = limite_min),
                 linetype = "dashed", colour = "blue", alpha = 0.3) +
    geom_segment(data = summRecDevs$resumo,
                 aes(x = 2012, xend = 2054,
                     y = limite_max, yend = limite_max),
                 linetype = "dashed", colour = "blue", alpha = 0.3) +
    geom_label_repel(data = summRecDevs$resumo,
                     aes(x = 2051, y = 1.25,
                         label = paste0(proporcao_excedencias * 100,
                                        "%")),
                     colour = "red", size = 4, alpha = 0.5, force = 0) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
    scale_y_continuous(limits = c(-1.5, 1.5),
                       breaks = seq(-1.5, 1.5, 0.5)) +
    scale_x_continuous(breaks = seq(1987, 2057, 10)) +
    scale_colour_manual(values = c("black", "red")) +
    labs(x = "Year", y = "log(Recruitment Deviations)") +
    facet_wrap(~Name, ncol = 3) +
    my_theme() +
    theme(legend.position = "none",
          strip.placement = "outside",
          panel.spacing = unit(1, "lines"),
          plot.margin = margin(10, 20, 10, 10),
          plot.title.position = "plot")
p00

ggsave("07_Results/03_Historical_Simulations/Fig_09_Comp_RecDevs_ver00.tiff",
       plot = p00, device = "tiff", units = "cm", w = 40, h = 25,
       dpi = 600, bg = "white")

######@> Examine other quantities...

#####@> Extracting values from SS3 Reconditioning Models and Historical
#####@> Simulation Data...
CompareRefDF <- compareRefs("03_Hists/HistList.rda")

#####@> all equal except FMSY consistently higher in SS3..
CompareRefDF |>
    dplyr::group_by(Label) |>
    dplyr::summarise(Mean = round(mean(ratio), 3))

######@> Extracting data from the Stock Synthesis scenarios
######@> (Trajectories and other quantities)...

#####@> Base data from SS...
summ_NSS <- SSsummarize(new_SS, SpawnOutputUnits = "biomass")$quants

####@> SS3 - SSB...
ssb_NSS <- summ_NSS %>%
    filter(grepl("SSB_[0-9]", Label)) %>%
    pivot_longer(names_to = "Scenarios", values_to = "SSB", 1:9) %>%
    select("Year" = Yr, Scenarios, SSB) %>%
    mutate(Type = "Reconditioning 2025") %>%
    arrange(Scenarios, Year) %>%
    filter(Year < 2025) %>%
    as.data.frame()

####@> SS3 - SSBmsy...
ssbmsy_NSS <- summ_NSS %>%
    filter(grepl("SSB_MSY", Label)) %>%
    pivot_longer(names_to = "Scenarios", values_to = "SSBmsy", 1:9) %>%
    select(Scenarios, SSBmsy) %>%
    mutate(Type = "Reconditioning 2025") %>%
    arrange(Scenarios) %>%
    as.data.frame()

####@> SS3 - SSB/SSBmsy...
ssb_ssbmsy_NSS <- ssb_NSS %>%
    left_join(select(ssbmsy_NSS, -Type), by = "Scenarios") %>%
    mutate(SSB_SSBmsy = SSB/SSBmsy) %>%
    filter(Year < 2025) %>%
    ## mutate(Year = Year - 1) %>%
    ## mutate(Type = "Reconditioning 2025") %>%
    as.data.frame()

####@> SS3 - F/Fmsy...
f_fmsy_NSS <- summ_NSS %>%
    filter(grepl("F_[0-9]", Label)) %>%
    pivot_longer(names_to = "Scenarios", values_to = "F_Fmsy", 1:9) %>%
    select("Year" = Yr, Scenarios, F_Fmsy) %>%
    mutate(Type = "Reconditioning 2025") %>%
    arrange(Scenarios, Year) %>%
    filter(Year != 2025) %>%
    as.data.frame()

#####@> SS3 - FMSY...
fmsy_NSS <- summ_NSS %>%
    filter(grepl("annF_MSY", Label)) %>%
    pivot_longer(names_to = "Scenarios", values_to = "Fmsy", 1:9) %>%
    select(Scenarios, Fmsy) %>%
    ## mutate(Type = "Reconditioning 2025") %>%
    arrange(Scenarios) %>%
    as.data.frame()

#####@> SS3 - F...
f_NSS <- f_fmsy_NSS %>%
    left_join(fmsy_NSS, by = "Scenarios") %>%
    mutate(F = F_Fmsy / Fmsy,
           Type = "Reconditioning 2025")

####@> Change names...
names(ssb_NSS)[2] <- "scenario"
names(ssb_ssbmsy_NSS)[2] <- "scenario"
names(f_fmsy_NSS)[2] <- "scenario"
names(fmsy_NSS)[1] <- "scenario"
names(f_NSS)[2] <- "scenario"

#####@> Base data from MSE...

####@> MSE - Reference points...
refpoints <- get_Quantities(HistList)

####@> MSE - SSB...
ssb_Hist <- get_Quantities02(HistList) %>%
    mutate(Type = "Operating Model")

####@> MSE - F...
f_Hist <- get_Quantities02(HistList, variable = "F") %>%
    mutate(Type = "Operating Model")

####@> MSE - SSB/SSBmsy...
ssb_ssbmsy_Hist <- ssb_Hist %>%
    left_join(refpoints, by = "scenario") %>%
    mutate(ssb_ssbmsy = value/ssbmsy)

####@> MSE - F/Fmsy???...
f_fmsy_Hist <- f_Hist %>%
    left_join(refpoints, by = "scenario") %>%
    mutate(F_Fmsy = value/fmsy,
           Type = "Operating Model")

#####@> Comparing outputs...

#####@> SSB...
p01 <- ggplot() +
    geom_line(data = ssb_NSS,
              aes(x = Year, y = SSB, colour = Type),
              linewidth = 1) +
    geom_line(data = ssb_Hist,
              aes(x = year, y = value, colour = Type),
              linewidth = 1, linetype = "dashed") +
    labs(x = "Year", y = "Spawning Biomass (t)", colour = "") +
    facet_wrap(~scenario) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 4E5)) +
    scale_x_continuous(breaks = seq(1950, 2025, 15)) +
    scale_colour_manual(values = c("Reconditioning 2025" = "#023743",
                                   "Operating Model" = "#A4BED5")) +
    my_theme() +
    theme(strip.placement = "outside",
          panel.spacing = unit(1, "lines"),
          plot.margin = margin(10, 20, 10, 10),
          plot.title.position = "plot",
          legend.position = "inside",
          legend.position.inside = c(0.1, 0.97))
p01

ggsave("07_Results/03_Historical_Simulations/Fig_10_Comp_SSB_ver00.tiff",
       plot = p01, device = "tiff", units = "cm", w = 40, h = 25,
       dpi = 600, bg = "white")

#####@> SSB/SSBmsy...
p02 <- ggplot() +
    geom_line(data = ssb_ssbmsy_NSS,
              aes(x = Year, y = SSB_SSBmsy, colour = Type),
              linewidth = 1) +
    geom_line(data = ssb_ssbmsy_Hist,
              aes(x = year, y = ssb_ssbmsy, colour = Type),
              linewidth = 1, linetype = "dashed") +
    labs(x = "Year", y = expression(SSB/SSB[MSY]), colour = "") +
    facet_wrap(~scenario) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 6)) +
    scale_x_continuous(breaks = seq(1950, 2025, 15)) +
    scale_colour_manual(values = c("Reconditioning 2025" = "#023743",
                                   "Operating Model" = "#A4BED5")) +
    my_theme() +
    theme(strip.placement = "outside",
          panel.spacing = unit(1, "lines"),
          plot.margin = margin(10, 20, 10, 10),
          plot.title.position = "plot",
          legend.position = "inside",
          legend.position.inside = c(0.1, 0.97))
p02

ggsave("07_Results/03_Historical_Simulations/Fig_11_Comp_SSB_SSBmsy_ver00.tiff",
       plot = p02, device = "tiff", units = "cm", w = 40, h = 25,
       dpi = 600, bg = "white")

#####@> F/Fmsy...
p03 <- ggplot() +
    geom_line(data = f_fmsy_NSS,
              aes(x = Year, y = F_Fmsy, colour = Type),
              linewidth = 1) +
    geom_line(data = f_fmsy_Hist,
              aes(x = year, y = F_Fmsy, colour = Type),
              linewidth = 1, linetype = "dashed") +
    labs(x = "Year", y = expression(F/F[MSY]), colour = "") +
    facet_wrap(~scenario) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 2),
                       breaks = seq(0, 2, 0.5)) +
    scale_x_continuous(breaks = seq(1950, 2025, 15)) +
    scale_colour_manual(values = c("Reconditioning 2025" = "#023743",
                                   "Operating Model" = "#A4BED5")) +
    my_theme() +
    theme(strip.placement = "outside",
          panel.spacing = unit(1, "lines"),
          plot.margin = margin(10, 20, 10, 10),
          plot.title.position = "plot",
          legend.position = "inside",
          legend.position.inside = c(0.1, 0.97))
p03

ggsave("07_Results/03_Historical_Simulations/Fig_12_Comp_F_Fmsy_ver00.tiff",
       plot = p03, device = "tiff", units = "cm", w = 40, h = 25,
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
