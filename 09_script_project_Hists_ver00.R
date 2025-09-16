########################################################################
## Description: Projecting Operating Models based on the new Historical
## Simulated data...
##
## Maintainer: Datenkraft - ICCAT (Tropical Tunas Species Group)
## Author: Rodrigo Sant'Ana
## Created: qua jun 25 10:44:09 2025 (-0300)
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
my_theme <- function(base_size = 20, base_family = "Helvetica") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(axis.ticks = element_line(colour = rgb01),
              axis.line = element_line(colour = rgb01, linewidth = 0.2),
              axis.text = element_text(colour = rgb02, size = 20),
              axis.title = element_text(size = 20),
              strip.text = element_text(size = 20,
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

######@> Creating a folder to receive the hist objects...
if(dir.exists("06_MSEs")) {
    print("OK, 06_MSEs directory was already created!");
    output.dir <- "06_MSEs/"
} else {
    dir.create("06_MSEs");
    output.dir <- "06_MSEs/"
}

########################################################################
######@> Loading and preparing data for projections...

######@> Loading Historical data...
Hists <- readRDS("03_Hists/HistList.rda")

######@> List management options...
ManagementOptions <- list.dirs("05_TunedMPs", recursive = FALSE,
                               full.names = TRUE)

######@> Select the DataLag and Management Interval options (change the
######@> index of the ManagementOptions to implement the projection based
######@> on the respective MP)...
MngOption <- ManagementOptions[5] ## LRP 0.2

######@> Defining and creating the directory to acomodate the
######@> projections...
MSEDir <- file.path("06_MSEs", basename(MngOption))
if (!dir.exists(MSEDir)) {
    dir.create(MSEDir)
}

######@> Listing tuned MPs...
tunedMPs <- list.files(MngOption, pattern = ".mp", full.names = TRUE,
                       recursive = FALSE)
MPnames <- gsub('.mp', '', basename(tunedMPs))

######@> Select MPs to Project...
ProjectMPs <- MPnames

########################################################################
######@> Projecting Tuned Management Procedures...

######@> Reading and preparing the Tuned MPs for projections...
for (i in seq_along(ProjectMPs)) {
    ind <- match(ProjectMPs[i], MPnames)
    mp <- readRDS(tunedMPs[ind])$MPout
    assign(ProjectMPs[i], mp, envir = .GlobalEnv)
}

######@> Running projections...

#####@> Defining parallel...
parallel <- FALSE ## getting errors (saveRDS are overwriting the same
## folder)

#####@> Exporting variables and functions to parallel process...
if (parallel) {
  setup(cpus = 9)
  sfExport(list = list("Hists",
                       "Catchdf",
                       "FixedTAC",
                       "SameTAC",
                       "adjust_TAC",
                       "adjust_TAC2",
                       "ProjectMPs",
                       "mp",
                       "CE_2_31",
                       "IR_2_31",
                       "SP_2_31",
                       "SPAH_2_31",
                       "MSEDir"))
}

#####@> Looping process over MPs and then over OMs...
for (mp in ProjectMPs) {
    message("\nProjecting MP: ", mp)
    if (!dir.exists(file.path(MSEDir, mp)))
        dir.create(file.path(MSEDir, mp))
    if (parallel) {
        sfLapply(seq_along(Hists), function(i) {
            Hist <- Hists[[i]]
            nm <- gsub("OM", "", Hist@OM@Name) |> trimws()
            nm <- paste(sprintf("%03d", i), nm, sep = "_")
            nm <- paste0(nm, ".mse")
            MSE <- Project(Hist, MPs = mp, parallel = FALSE,
                           silent = TRUE)
            saveRDS(MSE, file.path(MSEDir, mp, nm))
        })
    } else {
        for (i in seq_along(Hists)) {
            message("OM: ", i, "/", length(Hists))
            Hist <- Hists[[i]]
            nm <- gsub("OM", "", Hist@OM@Name) |> trimws()
            nm <- paste(sprintf("%03d", i), nm, sep="_")
            nm <- paste0(nm, ".mse")
            MSE <- Project(Hist, MPs = mp, parallel = FALSE,
                           silent = TRUE)
            saveRDS(MSE, file.path(MSEDir, mp, nm))
        }
    }
}

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
