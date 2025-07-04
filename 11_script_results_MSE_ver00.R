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

######@> Load some MP custom functions...
source("06_script_MP_Internal_Functions_ver00.R")

######@> Load PM functions...
source("10_script_prepare_PMs_ver00.R")

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
library(gt)

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

#####@> Function to estimate PGK - Performance Metric...
CalcPGK <- function(DF, Years = 2025:2054) {
    yrs <- paste(range(Years), collapse = "-")
    fDF <- DF |> dplyr::filter(Year %in% Years) |>
        dplyr::group_by(MP) |>
        dplyr::mutate(InGreen = SB_SBMSY > 1 & F_FMSY < 1) |>
        dplyr::summarise(PGK = mean(InGreen))
    names(fDF)[2] <- paste(names(fDF)[2], yrs)
    fDF
}

#####@> Function to estimate LRP - Performance Metric...
CalcLRP <- function(DF, Years = 2025:2054, Ref = 0.4) {
    yrs <- paste(range(Years), collapse = "-")
    fDF <- DF |> dplyr::filter(Year %in% Years) |>
        dplyr::group_by(MP) |>
        dplyr::mutate(Stat = SB_SBMSY < Ref) |>
        dplyr::summarise(LRP = mean(Stat))
    names(fDF)[2] <- paste(names(fDF)[2], yrs)
    fDF
}

#####@> Function to estimate NLRP - Performance Metric...
CalcNLRP <- function(DF, Years = 2025:2054, Ref = 0.4) {
    yrs <- paste(range(Years), collapse = "-")
    fDF <- DF |> dplyr::filter(Year %in% Years) |>
        dplyr::group_by(MP) |>
        dplyr::mutate(Stat = SB_SBMSY > Ref) |>
        dplyr::summarise(NLRP = mean(Stat))
    names(fDF)[2] <- paste(names(fDF)[2], yrs)
    fDF
}

#####@> Function to estimate POF - Performance Metric...
CalcPOF <- function(DF, Years = 2025:2054, Ref = 1) {
    yrs <- paste(range(Years), collapse = "-")
    fDF <- DF |> dplyr::filter(Year %in% Years) |>
        dplyr::group_by(MP) |>
        dplyr::mutate(Stat = F_FMSY > Ref) |>
        dplyr::summarise(POF = mean(Stat))
    names(fDF)[2] <- paste(names(fDF)[2], yrs)
    fDF
}

#####@> Function to estimate PNOF - Performance Metric...
CalcPNOF <- function(DF, Years = 2025:2054, Ref = 1) {
    yrs <- paste(range(Years), collapse = "-")
    fDF <- DF |> dplyr::filter(Year %in% Years) |>
        dplyr::group_by(MP) |>
        dplyr::mutate(Stat = F_FMSY < Ref) |>
        dplyr::summarise(PNOF = mean(Stat))
    names(fDF)[2] <- paste(names(fDF)[2], yrs)
    fDF
}

#####@> Function to estimate AvC - Performance Metric...
CalcAvC <- function(DF, Stat = Removals, Years = 2025:2054) {
    yrs <- paste(range(Years), collapse = "-")
    fDF <- DF |> dplyr::filter(Year %in% Years) |>
        dplyr::group_by(MP) |>
        dplyr::summarise(AvC = median({{ Stat }}))
    names(fDF)[2] <- paste(names(fDF)[2], yrs)
    fDF
}

#####@> Function to estimate VarC - Performance Metric...
CalcVarC <- function(DF, Years = 2025:2054) {
    if (length(Years) < 6) {
        stop("For meaningful triad comparisons, you must supply at least 6 years.")
    }
    yrs <- paste(range(Years), collapse = "-")
    df_filtered <- DF |>
        dplyr::filter(Year %in% Years) |>
        dplyr::mutate(
                   period = floor((Year - min(Years)) / 3) + 1)
    tac_means <- df_filtered |>
        dplyr::group_by(MP, Sim, period) |>
        dplyr::summarise(mean_tac = mean(TAC, na.rm = TRUE),
                         .groups = "drop")
    tac_changes <- tac_means |>
        dplyr::group_by(MP, Sim) |>
        dplyr::arrange(period) |>
        dplyr::mutate(
                   tac_change = ((((mean_tac - dplyr::lag(mean_tac)) /
                       dplyr::lag(mean_tac))^2)^0.5)) |>
        dplyr::filter(!is.na(tac_change)) |>
        dplyr::ungroup()
    summary <- tac_changes |>
        dplyr::group_by(MP) |>
        dplyr::summarise(
                   VarC = mean(tac_change, na.rm = TRUE),
                   ## sd_tac_change = sd(tac_change, na.rm = TRUE),
                   .groups = "drop")
    names(summary)[2] <- paste(names(summary)[2], yrs)
    summary
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

######@> Management Procedure...

#####@> List tuned management options...
ManagementOptionsTune <- list.dirs("05_TunedMPs", recursive = FALSE,
                                   full.names = TRUE)

#####@> Select the DataLag and Management Interval options - For tuned
#####@> MPs...
MngOptionTune <- ManagementOptionsTune[1] ## DataLag_1_Interval_3

#####@> Listing tuned MPs...
tunedMPs <- list.files(MngOptionTune, full.names = TRUE, recursive = FALSE)
MPnames <- gsub('.mp', '', basename(tunedMPs))

#####@> Select MPs to Project...
ProjectMPs <- MPnames

#####@> Reading and preparing the Tuned MPs for projections...
for (i in seq_along(ProjectMPs)) {
    ind <- match(ProjectMPs[i], MPnames)
    mp <- readRDS(tunedMPs[ind])$MPout
    assign(ProjectMPs[i], mp, envir = .GlobalEnv)
}

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

######@> Loading MSEs - This process can take more than a minute...
MSEs <- lapply(MPpaths, GetMSEs)

#####@> Extracting MSEs based on a specific OM and combine them...
OMsQnt25_h6 <- addMPs(extrair_por_substring(MSEs, "Qnt25_h6"))
OMsQnt50_h6 <- addMPs(extrair_por_substring(MSEs, "Qnt50_h6"))
OMsQnt75_h6 <- addMPs(extrair_por_substring(MSEs, "Qnt75_h6"))
OMsQnt25_h7 <- addMPs(extrair_por_substring(MSEs, "Qnt25_h7"))
OMsQnt50_h7 <- addMPs(extrair_por_substring(MSEs, "Qnt50_h7"))
OMsQnt75_h7 <- addMPs(extrair_por_substring(MSEs, "Qnt75_h7"))
OMsQnt25_h8 <- addMPs(extrair_por_substring(MSEs, "Qnt25_h8"))
OMsQnt50_h8 <- addMPs(extrair_por_substring(MSEs, "Qnt50_h8"))
OMsQnt75_h8 <- addMPs(extrair_por_substring(MSEs, "Qnt75_h8"))

######@> MSE list...
MSEList <- list(OMsQnt25_h6, OMsQnt50_h6, OMsQnt75_h6, OMsQnt25_h7,
                OMsQnt50_h7, OMsQnt75_h7, OMsQnt25_h8, OMsQnt50_h8,
                OMsQnt75_h8)

########################################################################
######@> Consolidating results...

######@> Extracting data from MSEs...
MPList <- lapply(MPpaths, GetInfo)

######@> Transform to data.frame...
DF <- do.call("rbind", MPList)

######@> Checking PGK60%...
CalcPGK(DF) ## For all projection period...
CalcPGK(DF, Years = 2028:2035) ## For the middle term...

######@>----------------------------------------------------------------
######@> Time Series Plot (Trajectories)...

######@> Definitions...
plotVars <- c("F_FMSY", "SB_SBMSY", "TAC", "Removals", "Landings")
ribbonCols <- c("darkgray", "black")

######@> Management periods...
periods <- bind_rows(
    data.frame(Year = 2025:2028,
               Period = factor(c(rep(1, 4))),
               y = 1),
    data.frame(Year = 2028:2036,
               Period = factor(c(rep(2, 9))),
               y = 1),
    data.frame(Year = 2036:2055,
               Period = factor(c(rep(3, 20))),
               y = 1))

######@> All OMs Combined...
DFplot <- DF |> tidyr::pivot_longer(dplyr::all_of(plotVars)) |>
  dplyr::group_by(Year, MP, name) |>
  dplyr::summarise(Mean = mean(value),
                   Lower1 = quantile(value, 0.025),
                   Upper1 = quantile(value, 0.975),
                   Lower2 = quantile(value, 0.15),
                   Upper2 = quantile(value, 0.85))
DFplot$name <- factor(DFplot$name, levels = plotVars, ordered = TRUE)
DFplot$MP <- gsub("_1_30", "", DFplot$MP)

######@> SB_SBMSY...
p00 <- ggplot(filter(DFplot, name %in% c("SB_SBMSY")),
              aes(x = Year)) +
    facet_wrap(~ MP, scales = "free", ncol = 4) +
    ## expand_limits(y = 0) +
    geom_ribbon(aes(ymin = Lower1,
                    ymax = Upper1),
                alpha = 0.5, fill = ribbonCols[1]) +
    geom_ribbon(aes(ymin = Lower2,
                    ymax = Upper2),
                alpha = 0.5, fill = ribbonCols[2]) +
    geom_line(aes(y = Mean)) +
    geom_ribbon(data = periods,
                aes(x = Year, ymin = 0, ymax = y - 0.6,
                    fill = Period),
                alpha = 0.5) +
    geom_line(data = periods,
              aes(x = Year, y = y), linetype = "dashed",
              colour = "red") +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 8, 1),
                       limits = c(0, 6)) +
    scale_x_continuous(expand = c(0, 0), limits = c(2025, 2054),
                       breaks = seq(2025, 2055, 10)) +
    labs(x = "Year", y = expression(SSB/SSB[MSY])) +
    my_theme() +
    theme(legend.position = "none")
p00

ggsave(paste0(path02, "Fig_13_SSB_SSBMSY_Trajectory_ver00.tiff"),
       plot = p00, device = "tiff", units = "cm", width = 30,
       height = 25, dpi = 600, bg = "white")

######@> F_FMSY...
p01 <- ggplot(filter(DFplot, name %in% c("F_FMSY")),
              aes(x = Year)) +
    facet_wrap(~ MP, scales = "free", ncol = 4) +
    ## expand_limits(y = 0) +
    geom_ribbon(aes(ymin = Lower1,
                    ymax = Upper1),
                alpha = 0.5, fill = ribbonCols[1]) +
    geom_ribbon(aes(ymin = Lower2,
                    ymax = Upper2),
                alpha = 0.5, fill = ribbonCols[2]) +
    geom_line(aes(y = Mean)) +
    geom_ribbon(data = periods,
                aes(x = Year, ymin = 0, ymax = y - 0.6,
                    fill = Period),
                alpha = 0.5) +
    geom_line(data = periods,
              aes(x = Year, y = y), linetype = "dashed",
              colour = "red") +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 8, 1),
                       limits = c(0, 7)) +
    scale_x_continuous(expand = c(0, 0), limits = c(2025, 2054),
                       breaks = seq(2025, 2055, 10)) +
    labs(x = "Year", y = expression(F/F[MSY])) +
    my_theme() +
    theme(legend.position = "none")
p01

ggsave(paste0(path02, "Fig_14_F_FMSY_Trajectory_ver00.tiff"),
       plot = p01, device = "tiff", units = "cm", width = 30,
       height = 25, dpi = 600, bg = "white")

######@> Removals...
ref <- data.frame(Type = c("10 years", "5 years"),
                  Mean = c(mean(WSKJ_Data@Cat[1, 64:73]),
                           mean(WSKJ_Data@Cat[1, 69:73])))
p02 <- ggplot(filter(DFplot, name %in% c("Removals")),
              aes(x = Year)) +
    facet_wrap(~ MP, scales = "free", ncol = 4) +
    ## expand_limits(y = 0) +
    geom_ribbon(aes(ymin = Lower1,
                    ymax = Upper1),
                alpha = 0.5, fill = ribbonCols[1]) +
    geom_ribbon(aes(ymin = Lower2,
                    ymax = Upper2),
                alpha = 0.5, fill = ribbonCols[2]) +
    geom_line(aes(y = Mean)) +
    geom_ribbon(data = periods,
                aes(x = Year, ymin = 0, ymax = y * 5000,
                    fill = Period),
                alpha = 0.5) +
    geom_hline(yintercept = ref$Mean[2],
               linetype = "dashed", colour = "red") +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 70000, 15000),
                       limits = c(0, 70000)) +
    scale_x_continuous(expand = c(0, 0), limits = c(2025, 2054),
                       breaks = seq(2025, 2055, 10)) +
    labs(x = "Year", y = "Removals (in metric tons)") +
    my_theme() +
    theme(legend.position = "none")
p02

ggsave(paste0(path02, "Fig_15_Removals_Trajectory_ver00.tiff"),
       plot = p02, device = "tiff", units = "cm", width = 30,
       height = 25, dpi = 600, bg = "white")

######@> TAC...
p03 <- ggplot(filter(DFplot, name %in% c("TAC")),
              aes(x = Year)) +
    facet_wrap(~ MP, scales = "free", ncol = 4) +
    ## expand_limits(y = 0) +
    geom_ribbon(aes(ymin = Lower1,
                    ymax = Upper1),
                alpha = 0.5, fill = ribbonCols[1]) +
    geom_ribbon(aes(ymin = Lower2,
                    ymax = Upper2),
                alpha = 0.5, fill = ribbonCols[2]) +
    geom_line(aes(y = Mean)) +
    geom_ribbon(data = periods,
                aes(x = Year, ymin = 0, ymax = y * 5000,
                    fill = Period),
                alpha = 0.5) +
    geom_line(data = periods,
              aes(x = Year, y = y), linetype = "dashed",
              colour = "red") +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 70000, 15000),
                       limits = c(0, 70000)) +
    scale_x_continuous(expand = c(0, 0), limits = c(2025, 2054),
                       breaks = seq(2025, 2055, 10)) +
    labs(x = "Year", y = "Total Allowable Catches (in metric tons)") +
    my_theme() +
    theme(legend.position = "none")
p03

ggsave(paste0(path02, "Fig_16_TAC_Trajectory_ver00.tiff"),
       plot = p03, device = "tiff", units = "cm", width = 30,
       height = 25, dpi = 600, bg = "white")

######@> SSB/SSBMSY By OM...
DFplotbyOM <- DF |>
  dplyr::group_by(Year, MP, Growth, Steepness) |>
  dplyr::summarise(Mean = mean(SB_SBMSY),
                   Lower1 = quantile(SB_SBMSY, 0.025),
                   Upper1 = quantile(SB_SBMSY, 0.975),
                   Lower2 = quantile(SB_SBMSY, 0.15),
                   Upper2 = quantile(SB_SBMSY, 0.85))
DFplotbyOM$MP <- gsub("_1_30", "", DFplotbyOM$MP)

#####@> Visualization...
p04 <- ggplot(DFplotbyOM, aes(x = Year)) +
    facet_grid(Growth ~ Steepness) +
    expand_limits(y = 0) +
    geom_ribbon(aes(ymin = Lower1,
                    ymax = Upper1, group = MP),
                alpha = 0.1, fill = ribbonCols[1]) +
    geom_ribbon(aes(ymin = Lower2,
                    ymax = Upper2, group = MP),
                alpha = 0.1, fill = ribbonCols[2]) +
    geom_line(aes(y = Mean, color = MP)) +
    geom_ribbon(data = periods,
                aes(x = Year, ymin = 0, ymax = y - 0.7,
                    fill = Period),
                alpha = 0.5, show.legend = FALSE) +
    geom_hline(yintercept = 1, linetype = 2, colour = "red") +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 8, 1),
                       limits = c(0, 6)) +
    scale_x_continuous(expand = c(0, 0), limits = c(2025, 2054),
                       breaks = seq(2025, 2055, 10)) +
    labs(x = "Year", y = expression(SSB/SSB[MSY])) +
    my_theme()
p04

ggsave(paste0(path02, "Fig_17_SSB_SSBMSY_Trajectory_by_OM_ver00.tiff"),
       plot = p04, device = "tiff", units = "cm", width = 30,
       height = 25, dpi = 600, bg = "white")

######@> F/FMSY By OM...
DFplotbyOM <- DF |>
  dplyr::group_by(Year, MP, Growth, Steepness) |>
  dplyr::summarise(Mean = mean(F_FMSY),
                   Lower1 = quantile(F_FMSY, 0.025),
                   Upper1 = quantile(F_FMSY, 0.975),
                   Lower2 = quantile(F_FMSY, 0.15),
                   Upper2 = quantile(F_FMSY, 0.85))
DFplotbyOM$MP <- gsub("_1_30", "", DFplotbyOM$MP)

#####@> Visualization...
p05 <- ggplot(DFplotbyOM, aes(x = Year)) +
    facet_grid(Growth ~ Steepness) +
    expand_limits(y = 0) +
    geom_ribbon(aes(ymin = Lower1,
                    ymax = Upper1, group = MP),
                alpha = 0.1, fill = ribbonCols[1]) +
    geom_ribbon(aes(ymin = Lower2,
                    ymax = Upper2, group = MP),
                alpha = 0.1, fill = ribbonCols[2]) +
    geom_line(aes(y = Mean, color = MP)) +
    geom_ribbon(data = periods,
                aes(x = Year, ymin = 0, ymax = y - 0.7,
                    fill = Period),
                alpha = 0.5, show.legend = FALSE) +
    geom_hline(yintercept = 1, linetype = 2, colour = "red") +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 8, 1),
                       limits = c(0, 7)) +
    scale_x_continuous(expand = c(0, 0), limits = c(2025, 2054),
                       breaks = seq(2025, 2055, 10)) +
    labs(x = "Year", y = expression(F/F[MSY])) +
    my_theme()
p05

ggsave(paste0(path02, "Fig_18_F_FMSY_Trajectory_by_OM_ver00.tiff"),
       plot = p05, device = "tiff", units = "cm", width = 30,
       height = 25, dpi = 600, bg = "white")

######@> Removals By OM...
DFplotbyOM <- DF |>
  dplyr::group_by(Year, MP, Growth, Steepness) |>
  dplyr::summarise(Mean = mean(Removals),
                   Lower1 = quantile(Removals, 0.025),
                   Upper1 = quantile(Removals, 0.975),
                   Lower2 = quantile(Removals, 0.15),
                   Upper2 = quantile(Removals, 0.85))
DFplotbyOM$MP <- gsub("_1_30", "", DFplotbyOM$MP)

#####@> Visualization...
p06 <- ggplot(DFplotbyOM, aes(x = Year)) +
    facet_grid(Growth ~ Steepness) +
    expand_limits(y = 0) +
    geom_ribbon(aes(ymin = Lower1,
                    ymax = Upper1, group = MP),
                alpha = 0.1, fill = ribbonCols[1]) +
    geom_ribbon(aes(ymin = Lower2,
                    ymax = Upper2, group = MP),
                alpha = 0.1, fill = ribbonCols[2]) +
    geom_line(aes(y = Mean, color = MP)) +
    geom_ribbon(data = periods,
                aes(x = Year, ymin = 0, ymax = y - 0.7,
                    fill = Period),
                alpha = 0.5, show.legend = FALSE) +
    geom_hline(yintercept = ref$Mean[2], linetype = 2, colour = "red") +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 70000, 15000),
                       limits = c(0, 70000)) +
    scale_x_continuous(expand = c(0, 0), limits = c(2025, 2054),
                       breaks = seq(2025, 2055, 10)) +
    labs(x = "Year", y = "Removals") +
    my_theme()
p06

ggsave(paste0(path02, "Fig_19_Removals_Trajectory_by_OM_ver00.tiff"),
       plot = p06, device = "tiff", units = "cm", width = 30,
       height = 25, dpi = 600, bg = "white")

######@> TAC By OM...
DFplotbyOM <- DF |>
  dplyr::group_by(Year, MP, Growth, Steepness) |>
  dplyr::summarise(Mean = mean(TAC),
                   Lower1 = quantile(TAC, 0.025),
                   Upper1 = quantile(TAC, 0.975),
                   Lower2 = quantile(TAC, 0.15),
                   Upper2 = quantile(TAC, 0.85))
DFplotbyOM$MP <- gsub("_1_30", "", DFplotbyOM$MP)

#####@> Visualization...
p07 <- ggplot(DFplotbyOM, aes(x = Year)) +
    facet_grid(Growth ~ Steepness) +
    expand_limits(y = 0) +
    geom_ribbon(aes(ymin = Lower1,
                    ymax = Upper1, group = MP),
                alpha = 0.1, fill = ribbonCols[1]) +
    geom_ribbon(aes(ymin = Lower2,
                    ymax = Upper2, group = MP),
                alpha = 0.1, fill = ribbonCols[2]) +
    geom_line(aes(y = Mean, color = MP)) +
    geom_ribbon(data = periods,
                aes(x = Year, ymin = 0, ymax = y - 0.7,
                    fill = Period),
                alpha = 0.5, show.legend = FALSE) +
    geom_hline(yintercept = ref$Mean[2], linetype = 2, colour = "red") +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 70000, 15000),
                       limits = c(0, 70000)) +
    scale_x_continuous(expand = c(0, 0), limits = c(2025, 2054),
                       breaks = seq(2025, 2055, 10)) +
    labs(x = "Year", y = "TAC") +
    my_theme()
p07

ggsave(paste0(path02, "Fig_20_TAC_Trajectory_by_OM_ver00.tiff"),
       plot = p07, device = "tiff", units = "cm", width = 30,
       height = 25, dpi = 600, bg = "white")

######@>----------------------------------------------------------------
######@> Kobes Plots...

#####@> Grouping MP by classes...
df00 <- DF %>%
    select(Sim:F_FMSY) %>%
    mutate(MP = gsub("_1_30", "", MP)) %>% 
    mutate(Class = case_when(
               grepl("IR", MP) ~ "Index-based: Index rate",
               grepl("CE", MP) ~ "Index-based: Exploitation rate",
               grepl("SP_", MP) ~
                   "Model-based: Surplus Production",
               grepl("SPAH", MP) ~
                   "Model-based: Surplus Production HCR"))

######@> Estimating Kobe values per year...
tmp <- df00 %>%
    mutate(OF = ifelse(F_FMSY < 1, FALSE, TRUE),
           OFD = ifelse(SB_SBMSY < 1, TRUE, FALSE)) %>%
    filter(Year == 2054)

#####@> Proportions by year...
valdf <- tmp %>%
    group_by(Year, Class, MP) %>%
    summarise(BL = sum(OF == FALSE & OFD == TRUE)/900 * 100,
              BR = sum(OF == FALSE & OFD == FALSE)/900 * 100,
              TL = sum(OF == TRUE & OFD == TRUE)/900 * 100,
              TR = sum(OF == TRUE & OFD == FALSE)/900 * 100)
valdf <- valdf %>% tidyr::pivot_longer(., cols = 4:7)
valdf$x <- -Inf
valdf$y <- -Inf
valdf$y[valdf$name == "TL"] <- Inf
valdf$y[valdf$name == "TR"] <- Inf
valdf$x[valdf$name == "BR"] <- Inf
valdf$x[valdf$name == "TR"] <- Inf
valdf$value <- round(valdf$value, 2)
valdf$value <- paste0(valdf$value, "%")
valdf$hjustvar <- -2
valdf$vjustvar <- -2
valdf$hjustvar[valdf$name == "TL"] <- -1
valdf$hjustvar[valdf$name == "TR"] <- 2
valdf$hjustvar[valdf$name == "BL"] <- -1
valdf$hjustvar[valdf$name == "BR"] <- 2
valdf$vjustvar[valdf$name == "TL"] <- 2
valdf$vjustvar[valdf$name == "TR"] <- 2
valdf$vjustvar[valdf$name == "BL"] <- -2
valdf$vjustvar[valdf$name == "BR"] <- -2

#####@> Colors to figure...
kobe_df <- bind_rows(data.frame(x = c(0, 0, 1, 1),
                                y = c(0, 1, 1, 0),
                                fill = "bl"),
                     data.frame(x = c(1, 1, 3, 3),
                                y = c(0, 1, 1, 0),
                                fill = "br"),
                     data.frame(x = c(0, 0, 1, 1),
                                y = c(1, 3, 3, 1),
                                fill = "tl"),
                     data.frame(x = c(1, 1, 3, 3),
                                y = c(1, 3, 3, 1),
                                fill = "tr"))
kobe_df$alpha <- 0.2

#####@> Figure dots - Last year...
p08 <- ggplot() +
    geom_polygon(data = kobe_df,
                 aes(x = x, y = y, fill = fill, alpha = alpha)) +
    scale_fill_manual(values = c("#F8DC7A", "#67C18B", "#D8775D",
                                 "#FDBD56")) +
    geom_point(data = filter(tmp,
                             Class == "Index-based: Exploitation rate"),
               aes(x = SB_SBMSY, y = F_FMSY), alpha = 0.2,
               size = 4) +
    geom_text(data = filter(valdf,
                            Class == "Index-based: Exploitation rate"),
              fontface = "bold", size = 4,
              aes(x = x, y = y, label = value,
                  hjust = hjustvar, vjust = vjustvar),
              colour = "black") +
    expand_limits(x = c(0, 3), y = c(0, 3)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 3)) +
    geom_hline(yintercept = 1, color = "darkgray", linetype = 2) +
    geom_vline(xintercept = 1, color = "darkgray", linetype = 2) +
    labs(x = expression(SSB/SSB[MSY]), y = expression(F/F[MSY])) +
    facet_grid(Class ~ MP) +
    my_theme() +
    theme(legend.position = "none")
p08

ggsave(paste0(path02, "Fig_21_Kobe_Final_Year_CE_ver00.tiff"),
       plot = p08, device = "tiff", units = "cm", width = 35,
       height = 20, dpi = 600, bg = "white")

p09 <- ggplot() +
    geom_polygon(data = kobe_df,
                 aes(x = x, y = y, fill = fill, alpha = alpha)) +
    scale_fill_manual(values = c("#F8DC7A", "#67C18B", "#D8775D",
                                 "#FDBD56")) +
    geom_point(data = filter(tmp,
                             Class == "Index-based: Index rate"),
               aes(x = SB_SBMSY, y = F_FMSY), alpha = 0.2,
               size = 4) +
    geom_text(data = filter(valdf,
                            Class == "Index-based: Index rate"),
              fontface = "bold", size = 4,
              aes(x = x, y = y, label = value,
                  hjust = hjustvar, vjust = vjustvar),
              colour = "black") +
    expand_limits(x = c(0, 3), y = c(0, 3)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 3)) +
    geom_hline(yintercept = 1, color = "darkgray", linetype = 2) +
    geom_vline(xintercept = 1, color = "darkgray", linetype = 2) +
    labs(x = expression(SSB/SSB[MSY]), y = expression(F/F[MSY])) +
    facet_grid(Class ~ MP) +
    my_theme() +
    theme(legend.position = "none")
p09

ggsave(paste0(path02, "Fig_22_Kobe_Final_Year_IR_ver00.tiff"),
       plot = p09, device = "tiff", units = "cm", width = 35,
       height = 20, dpi = 600, bg = "white")

p10 <- ggplot() +
    geom_polygon(data = kobe_df,
                 aes(x = x, y = y, fill = fill, alpha = alpha)) +
    scale_fill_manual(values = c("#F8DC7A", "#67C18B", "#D8775D",
                                 "#FDBD56")) +
    geom_point(data = filter(tmp,
                             Class == "Model-based: Surplus Production"),
               aes(x = SB_SBMSY, y = F_FMSY), alpha = 0.2,
               size = 4) +
    geom_text(data = filter(valdf,
                            Class == "Model-based: Surplus Production"),
              fontface = "bold", size = 4,
              aes(x = x, y = y, label = value,
                  hjust = hjustvar, vjust = vjustvar),
              colour = "black") +
    expand_limits(x = c(0, 3), y = c(0, 3)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 3)) +
    geom_hline(yintercept = 1, color = "darkgray", linetype = 2) +
    geom_vline(xintercept = 1, color = "darkgray", linetype = 2) +
    labs(x = expression(SSB/SSB[MSY]), y = expression(F/F[MSY])) +
    facet_grid(Class ~ MP) +
    my_theme() +
    theme(legend.position = "none")
p10

ggsave(paste0(path02, "Fig_23_Kobe_Final_Year_SP_ver00.tiff"),
       plot = p10, device = "tiff", units = "cm", width = 35,
       height = 20, dpi = 600, bg = "white")

p11 <- ggplot() +
    geom_polygon(data = kobe_df,
                 aes(x = x, y = y, fill = fill, alpha = alpha)) +
    scale_fill_manual(values = c("#F8DC7A", "#67C18B", "#D8775D",
                                 "#FDBD56")) +
    geom_point(data = filter(tmp,
                             Class == "Model-based: Surplus Production HCR"),
               aes(x = SB_SBMSY, y = F_FMSY), alpha = 0.2,
               size = 4) +
    geom_text(data = filter(valdf,
                            Class == "Model-based: Surplus Production HCR"),
              fontface = "bold", size = 4,
              aes(x = x, y = y, label = value,
                  hjust = hjustvar, vjust = vjustvar),
              colour = "black") +
    expand_limits(x = c(0, 3), y = c(0, 3)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 3)) +
    geom_hline(yintercept = 1, color = "darkgray", linetype = 2) +
    geom_vline(xintercept = 1, color = "darkgray", linetype = 2) +
    labs(x = expression(SSB/SSB[MSY]), y = expression(F/F[MSY])) +
    facet_grid(Class ~ MP) +
    my_theme() +
    theme(legend.position = "none")
p11

ggsave(paste0(path02, "Fig_24_Kobe_Final_Year_SPAH_ver00.tiff"),
       plot = p11, device = "tiff", units = "cm", width = 35,
       height = 20, dpi = 600, bg = "white")

######@> Estimating Kobe values per year...
tmp <- df00 %>%
    mutate(OF = ifelse(F_FMSY < 1, FALSE, TRUE),
           OFD = ifelse(SB_SBMSY < 1, TRUE, FALSE))

#####@> Proportions by year...
valdf <- tmp %>%
    group_by(Year, Class, MP) %>%
    summarise(Yellow = sum(OF == FALSE & OFD == TRUE)/900 * 100,
              Green = sum(OF == FALSE & OFD == FALSE)/900 * 100,
              Red = sum(OF == TRUE & OFD == TRUE)/900 * 100,
              Orange = sum(OF == TRUE & OFD == FALSE)/900 * 100) %>%
    pivot_longer(names_to = "Cond", values_to = "Perc", 4:7) %>%
    mutate(Cond = factor(Cond, levels = c("Green", "Yellow", "Orange",
                                          "Red")))

p12 <- ggplot() +
    geom_area(data = filter(valdf, Year %in% 2025:2054,
                            Class == "Index-based: Exploitation rate"),
              aes(x = Year, y = Perc, fill = rev(Cond)),
              stat = "identity", colour = "black") +
    geom_hline(yintercept = 60, lty = 1, colour = "white", alpha = 0.5,
               linewidth = 1) +
    geom_hline(yintercept = 60, lty = 2, alpha = 0.5) +
    facet_grid(Class~MP) +
    scale_fill_manual(values = rev(c("#67C18B", "#F8DC7A", "#FDBD56",
                                     "#D8775D"))) +
    labs(x = "Year", y = "%") +
    expand_limits(y = c(0, 100)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(2025, 2054, 10),
                       limits = c(2025, 2054)) +
    annotate("rect", xmin = 2025, xmax = 2028, ymin = 0, ymax = 100,
             fill = "white", alpha = 0.5) +
    annotate("text", x = 2026.5, y = 50, label = "Kobe Matrix Period",
             alpha = 0.6, size = 4, angle = 90) +
    my_theme() +
    theme(legend.position = "none")
p12

ggsave(paste0(path02, "Fig_25_Kobe_Time_Serie_CE_ver00.tiff"),
       plot = p12, device = "tiff", units = "cm", width = 35,
       height = 20, dpi = 600, bg = "white")

p13 <- ggplot() +
    geom_area(data = filter(valdf, Year %in% 2025:2054,
                            Class == "Index-based: Index rate"),
              aes(x = Year, y = Perc, fill = rev(Cond)),
              stat = "identity", colour = "black") +
    geom_hline(yintercept = 60, lty = 1, colour = "white", alpha = 0.5,
               linewidth = 1) +
    geom_hline(yintercept = 60, lty = 2, alpha = 0.5) +
    facet_grid(Class~MP) +
    scale_fill_manual(values = rev(c("#67C18B", "#F8DC7A", "#FDBD56",
                                     "#D8775D"))) +
    labs(x = "Year", y = "%") +
    expand_limits(y = c(0, 100)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(2025, 2054, 10),
                       limits = c(2025, 2054)) +
    annotate("rect", xmin = 2025, xmax = 2028, ymin = 0, ymax = 100,
             fill = "white", alpha = 0.5) +
    annotate("text", x = 2026.5, y = 50, label = "Kobe Matrix Period",
             alpha = 0.6, size = 4, angle = 90) +
    my_theme() +
    theme(legend.position = "none")
p13

ggsave(paste0(path02, "Fig_26_Kobe_Time_Serie_IR_ver00.tiff"),
       plot = p13, device = "tiff", units = "cm", width = 35,
       height = 20, dpi = 600, bg = "white")

p14 <- ggplot() +
    geom_area(data = filter(valdf, Year %in% 2025:2054,
                            Class == "Model-based: Surplus Production"),
              aes(x = Year, y = Perc, fill = rev(Cond)),
              stat = "identity", colour = "black") +
    geom_hline(yintercept = 60, lty = 1, colour = "white", alpha = 0.5,
               linewidth = 1) +
    geom_hline(yintercept = 60, lty = 2, alpha = 0.5) +
    facet_grid(Class~MP) +
    scale_fill_manual(values = rev(c("#67C18B", "#F8DC7A", "#FDBD56",
                                     "#D8775D"))) +
    labs(x = "Year", y = "%") +
    expand_limits(y = c(0, 100)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(2025, 2054, 10),
                       limits = c(2025, 2054)) +
    annotate("rect", xmin = 2025, xmax = 2028, ymin = 0, ymax = 100,
             fill = "white", alpha = 0.5) +
    annotate("text", x = 2026.5, y = 50, label = "Kobe Matrix Period",
             alpha = 0.6, size = 4, angle = 90) +
    my_theme() +
    theme(legend.position = "none")
p14

ggsave(paste0(path02, "Fig_27_Kobe_Time_Serie_SP_ver00.tiff"),
       plot = p14, device = "tiff", units = "cm", width = 35,
       height = 20, dpi = 600, bg = "white")

p15 <- ggplot() +
    geom_area(data = filter(valdf, Year %in% 2025:2054,
                            Class == "Model-based: Surplus Production HCR"),
              aes(x = Year, y = Perc, fill = rev(Cond)),
              stat = "identity", colour = "black") +
    geom_hline(yintercept = 60, lty = 1, colour = "white", alpha = 0.5,
               linewidth = 1) +
    geom_hline(yintercept = 60, lty = 2, alpha = 0.5) +
    facet_grid(Class~MP) +
    scale_fill_manual(values = rev(c("#67C18B", "#F8DC7A", "#FDBD56",
                                     "#D8775D"))) +
    labs(x = "Year", y = "%") +
    expand_limits(y = c(0, 100)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(2025, 2054, 10),
                       limits = c(2025, 2054)) +
    annotate("rect", xmin = 2025, xmax = 2028, ymin = 0, ymax = 100,
             fill = "white", alpha = 0.5) +
    annotate("text", x = 2026.5, y = 50, label = "Kobe Matrix Period",
             alpha = 0.6, size = 4, angle = 90) +
    my_theme() +
    theme(legend.position = "none")
p15

ggsave(paste0(path02, "Fig_28_Kobe_Time_Serie_SPAH_ver00.tiff"),
       plot = p15, device = "tiff", units = "cm", width = 35,
       height = 20, dpi = 600, bg = "white")

######@>----------------------------------------------------------------
######@> Performance metrics...

######@> Consolidating general indicators...
PropTab <- bind_cols(
    CalcPGK(DF),
    CalcPGK(DF, Years = 2025:2027)[, 2],
    CalcPGK(DF, Years = 2028:2033)[, 2],
    CalcPGK(DF, Years = 2034:2054)[, 2],
    CalcLRP(DF)[, 2],
    CalcLRP(DF, Years = 2025:2027)[, 2],
    CalcLRP(DF, Years = 2028:2033)[, 2],
    CalcLRP(DF, Years = 2034:2054)[, 2],
    CalcPOF(DF)[, 2],
    CalcPNOF(DF)[, 2],
    CalcAvC(DF)[, 2],
    CalcAvC(DF, Years = 2025:2027)[, 2],
    CalcAvC(DF, Years = 2028:2033)[, 2],
    CalcAvC(DF, Years = 2034:2054)[, 2],
    CalcVarC(DF)[, 2],
    CalcVarC(DF, Years = 2028:2033)[, 2],
    CalcVarC(DF, Years = 2034:2054)[, 2]) %>%
    mutate(MP = gsub("_1_30", "", MP))

guiltplot <- PropTab %>%
    gt() %>%
    fmt_number(decimals = 2,
               columns = c("PGK 2025-2054", "PGK 2025-2027",
                           "PGK 2028-2033",  "PGK 2034-2054",
                           "LRP 2025-2054", "LRP 2025-2027",
                           "LRP 2028-2033", "LRP 2034-2054",
                           "POF 2025-2054", "PNOF 2025-2054",
                           "AvC 2025-2054", "AvC 2025-2027",
                           "AvC 2028-2033", "AvC 2034-2054",
                           "VarC 2025-2054", "VarC 2028-2033",
                           "VarC 2034-2054")) %>%
    data_color(
        columns = c("PGK 2025-2054", "PGK 2025-2027",
                    "PGK 2028-2033",  "PGK 2034-2054",
                    "LRP 2025-2054", "LRP 2025-2027",
                    "LRP 2028-2033", "LRP 2034-2054",
                    "POF 2025-2054", "PNOF 2025-2054",
                    "AvC 2025-2054", "AvC 2025-2027",
                    "AvC 2028-2033", "AvC 2034-2054",
                    "VarC 2025-2054", "VarC 2028-2033",
                    "VarC 2034-2054"),
        colors = scales::col_bin(palette = "BuPu",
                                 domain = NULL, bins = 4,
                                 alpha = 0.5)) %>%
    tab_style(
        style = list(cell_text(weight = "bold", align = "center")),
        locations = cells_column_labels(everything())) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = vars(MP)))
guiltplot


## CalcPGK(DF)

## PGKm <- sapply(MSE_list, function(X) {
##   ## Years 4 - 10 - index 5:11 becuase first projection year is 2025
##   ## before MP is used
##   mean(X@SB_SBMSY[ , , 5:11] > 1 & X@F_FMSY[ , , 5:11] < 1)
## })
## PGKw <- mean(PGKm)

temp00 <- map2(MSEList, seq_along(MSEList), function(mse, i) {
    om <- paste0("MSE", sprintf("%03d", i))
    map2(PMlist, PMs, function(pm_fun, pm_name) {
        pm <- pm_fun(mse)
        mps <- pm@MPs
        val <- data.frame(pm@Prob)
        names(val) <- mps
        val %>%
            mutate(sim = 1:100) %>%
            pivot_longer(names_to = "MP", values_to = "Values",
                         1:length(mps)) %>%
            mutate(OM = om,
                   Name = pm@Name,
                   Caption = pm@Caption,
                   PM = pm_name) %>%
            select(OM, Name, Caption, sim, MP, PM, Values) %>%
            as.data.frame()
    }) %>%
        bind_rows()
}) %>%
    bind_rows()

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
