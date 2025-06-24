########################################################################
## Description: Preparing and Testing the Management Procedures
## developed for WSKJ MSE...
##
## Maintainer: Datenkraft - ICCAT (Tropical Tunas Species Group)
## Author: Rodrigo Sant'Ana
## Created: seg jun 23 14:47:05 2025 (-0300)
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

######@> Load some MP custom functions...
source("06_script_MP_Internal_Functions_ver00.R")

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

######@> Path to Historical Simulated Data folder...
path01 <- "03_Hists/"

########################################################################
######@> Loading data...

######@> Historical Simulated Data...
HistList <- readRDS(paste0(path01, "HistList.rda"))

######@> Management Procedure...

#####@>  Source pre-tuned MPs...

####@> Remove MPs from global enviroment...
suppressWarnings(rm(list = avail("MP")))

####@> Load MPs into global environment...
for (fl in list.files("04_MPs", full.names = TRUE)) {
    source(fl)
}

####@> Function to get MP names...
getMPNames <- function() {
    ls(envir = .GlobalEnv)[vapply(ls(envir = .GlobalEnv),
                                  MSEtool:::getclass, logical(1),
                                  classy = 'MP')]
}
getMPNames()

########################################################################
######@> Testing Management Procedures...

######@>----------------------------------------------------------------
######@> Constant Catch MPs...

#####@> Selecting one Hist...
Hist <- HistList[[3]]

#####@> Projecting MPs...
MSE <- Project(Hist, MPs = c("CC_20kt", "CC_30kt", "CC_40kt"))

#####@> Looking to the results...
Converge(MSE)              ## Convergence check...
Pplot(MSE)                 ## Trajectories for ratio quantities...
Kplot(MSE)                 ## Kobe plot...
Tplot(MSE)                 ## Tradoff plot...
wormplot(MSE, Bref = 0.4)  ## 
PWhisker(MSE)              ## MSE performance whisker plot...
NOAA_plot(MSE)             ## Trade-offs plots and performance table...

######@>----------------------------------------------------------------
######@> Constant Exploitation MPs...

#####@> Selecting one Hist...
Hist <- HistList[[3]]

#####@> Projecting MPs...
MSE <- Project(Hist, MPs = c("CE1", "CE2", "CE3"))

#####@> Looking to the results...
Converge(MSE)              ## Convergence check...
Pplot(MSE)                 ## Trajectories for ratio quantities...
Kplot(MSE)                 ## Kobe plot...
Tplot(MSE)                 ## Tradoff plot...
wormplot(MSE, Bref = 0.4)  ## 
PWhisker(MSE)              ## MSE performance whisker plot...
NOAA_plot(MSE)             ## Trade-offs plots and performance table...

######@>----------------------------------------------------------------
######@> Index Ratio MPs...

#####@> Selecting one Hist...
Hist <- HistList[[3]]

#####@> Projecting MPs...
MSE <- Project(Hist, MPs = c("IR1", "IR2", "IR3"))

#####@> Looking to the results...
Converge(MSE)              ## Convergence check...
Pplot(MSE)                 ## Trajectories for ratio quantities...
Kplot(MSE)                 ## Kobe plot...
Tplot(MSE)                 ## Tradoff plot...
wormplot(MSE, Bref = 0.4)  ## 
PWhisker(MSE)              ## MSE performance whisker plot...
NOAA_plot(MSE)             ## Trade-offs plots and performance table...

######@>----------------------------------------------------------------
######@> Surplus Production Models MPs...

#####@> Selecting one Hist...
Hist <- HistList[[3]]

#####@> Projecting MPs...
MSE <- Project(Hist, MPs = c("SP_01", "SP_02", "SP_03", "SP_04"))

#####@> Looking to the results...
Converge(MSE)              ## Convergence check...
Pplot(MSE)                 ## Trajectories for ratio quantities...
Kplot(MSE)                 ## Kobe plot...
Tplot(MSE)                 ## Tradoff plot...
wormplot(MSE, Bref = 0.4)  ## 
PWhisker(MSE)              ## MSE performance whisker plot...
NOAA_plot(MSE)             ## Trade-offs plots and performance table...

######@>----------------------------------------------------------------
######@> Surplus Production Models with Adrian Hordyk HCR MPs...

#####@> Selecting one Hist...
Hist <- HistList[[3]]

#####@> Projecting MPs...
MSE <- Project(Hist, MPs = c("SPAH_01", "SPAH_02", "SPAH_03", "SPAH_04",
                             "SPAH_05"))

#####@> Looking to the results...
Converge(MSE)              ## Convergence check...
Pplot(MSE)                 ## Trajectories for ratio quantities...
Kplot(MSE)                 ## Kobe plot...
Tplot(MSE)                 ## Tradoff plot...
wormplot(MSE, Bref = 0.4)  ## 
PWhisker(MSE)              ## MSE performance whisker plot...
NOAA_plot(MSE)             ## Trade-offs plots and performance table...

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
