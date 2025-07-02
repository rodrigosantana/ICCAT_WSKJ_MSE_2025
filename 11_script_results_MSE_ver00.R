########################################################################
## Description: Consolidating results from the MSE process implemented
## for the Western Atlantic Skipjack tuna...
##
## Maintainer: Datenkraft - ICCAT (Tropical Tunas Species Group)
## Author: Rodrigo Sant'Ana
## Created: qua jul  2 09:03:22 2025 (-0300)
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

## ######@> Load some MP custom functions...
## source("06_script_MP_Internal_Functions_ver00.R")

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

######@> Path to save Results...

#####@> Tune profiles...
path01 <- "07_Results/04_Tuned_MPs/"

#####@> MSE projections...
path02 <- "07_Results/05_MSE_Projections/"

######@> Functions to prepare results...

#####@> Function to estimate PGK...
CalcPGK <- function(DF, Years = 2025:2054) {
    yrs <- paste(range(Years), collapse = "-")
    fDF <- DF |> dplyr::filter(Year %in% Years) |>
        dplyr::group_by(MP) |>
        dplyr::mutate(InGreen = SB_SBMSY > 1 & F_FMSY < 1) |>
        dplyr::summarise(PGK = mean(InGreen))
    names(fDF)[2] <- paste(names(fDF)[2], yrs)
    fDF
}

#####@> Function to extract data from MSE objects...
GetInfo <- function(MPpath) {
    msefiles <- list.files(MPpath, full.names = TRUE)
    if (length(msefiles) < 1)
        stop("No files found for: ", basename(MPpath))
    OMList <- list()
    for (j in seq_along(msefiles)) {
        mse <- readRDS(msefiles[j])
        t <- strsplit(mse@Name, "_")[[1]][3:4]
        ProjectYears <- seq(mse@OM$CurrentYr[1] + 1,
                            by = 1,
                            length.out = mse@proyears)
        OMList[[j]] <- data.frame(Sim = 1:mse@nsim,
                                  MP = basename(MPpath),
                                  Growth = t[1],
                                  Steepness = t[2],
                                  Year = rep(ProjectYears,
                                             each = mse@nsim),
                                  SB_SBMSY = as.vector(mse@SB_SBMSY),
                                  F_FMSY = as.vector(mse@F_FMSY),
                                  Removals = as.vector(mse@Removals),
                                  TAC = as.vector(mse@TAC),
                                  Landings = as.vector(mse@Catch))
    }
    do.call("rbind", OMList)
}

########################################################################
######@> Loading and preparing data for projections...

######@> List of management options...
ManagementOptions <- list.dirs("06_MSEs",
                               recursive = FALSE,
                               full.names = TRUE)

######@> Select the DataLag and Management Interval options...
MngOption <- ManagementOptions[1] ## DataLag_1_Interval_3

######@> Defining the path for the selected Management options...
MPpaths <- list.dirs(MngOption, recursive = FALSE, full.names = TRUE)

#####@> Filtering cases - MPs tuned to 1-30...
MPpaths <- MPpaths[grepl("1_30", MPpaths)]

########################################################################
######@> Consolidating results...

######@> Extracting data from MSEs...
MPList <- lapply(MPpaths, GetInfo)

######@> Transform to data.frame...
DF <- do.call("rbind", MPList)

######@> Checking PGK60%...
CalcPGK(DF) ## For all projection period...
CalcPGK(DF, Years = 2028:2035) ## For the middle term...

######@> Time Series Plot...
plotVars <- c("F_FMSY", "SB_SBMSY", "TAC", "Removals", "Landings")
ribbonCols <- c("darkgray", "black")

######@> All OMs Combined...
DFplot <- DF |> tidyr::pivot_longer(dplyr::all_of(plotVars)) |>
  dplyr::group_by(Year, MP, name) |>
  dplyr::summarise(Median = median(value),
                   Lower1 = quantile(value, 0.025),
                   Upper1 = quantile(value, 0.975),
                   Lower2 = quantile(value, 0.15),
                   Upper2 = quantile(value, 0.85))
DFplot$name <- factor(DFplot$name, levels = plotVars, ordered = TRUE)
DFplot$MP <- gsub("_1_30", "", DFplot$MP)

ggplot(DFplot, aes(x = Year)) +
    facet_grid(name ~ MP, scales = "free") +
    expand_limits(y = 0) +
    geom_ribbon(aes(ymin = Lower1,
                    ymax = Upper1),
                alpha = 0.5, fill = ribbonCols[1]) +
    geom_ribbon(aes(ymin = Lower2,
                    ymax = Upper2),
                alpha = 0.5, fill = ribbonCols[2]) +
    geom_line(aes(y = Median)) +
    my_theme()


######@> SB/SBMSY By OM...
DFplotbyOM <- DF |>
  dplyr::group_by(Year, MP, Growth, Steepness) |>
  dplyr::summarise(Median = median(SB_SBMSY),
                   Lower1 = quantile(SB_SBMSY, 0.025),
                   Upper1 = quantile(SB_SBMSY, 0.975),
                   Lower2 = quantile(SB_SBMSY, 0.15),
                   Upper2 = quantile(SB_SBMSY, 0.85))
DFplotbyOM$MP <- gsub("_1_30", "", DFplotbyOM$MP)

## DFplotbyOM <- DFplotbyOM |>
##     dplyr::filter(MP %in% c("SP_1", "SP_2", "SP_3" ,"SP_4", "SP_5"))

ggplot(DFplotbyOM, aes(x = Year, color = MP)) +
  facet_grid(Growth ~ Steepness) +
  geom_hline(yintercept = 1, linetype = 2) +
  expand_limits(y = 0) +
  # geom_ribbon(aes(ymin = Lower1,
  #                 ymax = Upper1),
  #             alpha = 0.5, fill = ribbonCols[1]) +
  # geom_ribbon(aes(ymin = Lower2,
  #                 ymax = Upper2),
  #             alpha = 0.5, fill = ribbonCols[2]) +
  geom_line(aes(y = Median)) +
  my_theme()

# ---- Kobe Time ----


# ---- Violin ----



## CalcPGK(DF)

## PGKm <- sapply(MSE_list, function(X) {
##   ## Years 4 - 10 - index 5:11 becuase first projection year is 2025
##   ## before MP is used
##   mean(X@SB_SBMSY[ , , 5:11] > 1 & X@F_FMSY[ , , 5:11] < 1)
## })
## PGKw <- mean(PGKm)



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
