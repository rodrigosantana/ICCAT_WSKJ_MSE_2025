########################################################################
## Description: Development of the Robustness Tests for the Western
## Atlantic Skipjack tuna MSE...
##
## Maintainer: Datenkraft - ICCAT (TT MSE Sub Group)
## Author: Rodrigo Sant'Ana
## Created: dom jul 13 11:21:56 2025 (-0300)
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

######@> Set the output directory for the Robustness tests...
output.dir <- "06_MSEs/"

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

######@> Loading Historical data...
Hists <- readRDS("03_Hists/HistList.rda")

######@> List management options...
ManagementOptions <- list.dirs("05_TunedMPs", recursive = FALSE,
                               full.names = TRUE)

######@> Select the DataLag and Management Interval options...
MngOption <- ManagementOptions[1] ## DataLag_1_Interval_3

######@> Defining and creating the directory to acomodate the
######@> projections for the robustness tests - By scenarios...

#####@> Scenario 01 - RecDevs increased by 50%...
MSEDir01 <- file.path("06_MSEs", paste0("Robustness_S01_",
                                        basename(MngOption)))

#####@> Scenario 02 - RecDevs with an increase tendency over time...
MSEDir02 <- file.path("06_MSEs", paste0("Robustness_S02_",
                                        basename(MngOption)))

#####@> Scenario 03 - RecDevs with a decrease tendency over time...
MSEDir03 <- file.path("06_MSEs", paste0("Robustness_S03_",
                                        basename(MngOption)))

######@> Listing tuned MPs...
tunedMPs <- list.files(MngOption, full.names = TRUE, recursive = FALSE)
MPnames <- gsub(".mp", "", basename(tunedMPs))

######@> Select MPs to Project...
ProjectMPs <- MPnames

########################################################################
######@> Preparing the Operating Models for Robustness Tests...

######@> Selecting an OM to be used at this time...
Hist005 <- Hists[[5]]
OM005 <- Hist005@OM

######@>----------------------------------------------------------------
######@> Scenario 01 - increase 50% in Recruitment Deviations...

######@> Define a new Recruitment Deviations...

#####@> Looking for the actual structure of the RecDevs...
RecDevs <- Hist005@SampPars$Stock$Perr_y
dim(RecDevs) ## [nsim, maxage + nyears + proyears]
c(OM005@nsim, OM005@maxage + OM005@nyears + OM005@proyears)

#####@> Defining the projection matrix...
ProjInd <- (OM005@maxage + OM005@nyears + 1):dim(RecDevs)[2]
ProjYears <- seq(OM005@CurrentYr + 1, by = 1,
                 length.out = OM005@proyears)

####@> Check if the structures are similar...
length(ProjInd) == OM005@proyears

#####@> Creating a new object to receive the Projection RecDevs...
ProjectionRecDevs <- RecDevs[, ProjInd]

#####@> Extracting the standard deviations from the original projection
#####@> RecDevs...
prevSD <- sd(log(ProjectionRecDevs))

#####@> Creating the first scenario - Increase Variability by 50%...
newSD  <- prevSD * 1.5
NewProjectionRecDevs <- exp(rnorm(OM005@nsim * OM005@proyears, 0,
                                  newSD))
NewProjectionRecDevs <- matrix(NewProjectionRecDevs, OM005@nsim,
                               OM005@proyears)

#####@> Comparing old and new RecDevs...
par(mfrow = c(1, 2))
matplot(ProjYears, t(ProjectionRecDevs), type = "l", ylab = "",
        ylim = c(0, 4))
matplot(ProjYears, t(NewProjectionRecDevs), type = "l", ylab = "",
        ylim = c(0, 4))
par(mfrow = c(1, 1))

######@> Replacing historical simulations with the new RecDevs
######@> Projection...
Hist005S01 <- Hist005
Hist005S01@SampPars$Stock$Perr_y[, ProjInd] <- NewProjectionRecDevs

######@>----------------------------------------------------------------
######@> Scenario 02 - Recruitment Deviations increasing over time...

######@> Creating a function to simulate Recruitment Deviations with
######@> autocorrelation and tendencies over the years...
SimulateRecruitmentAR1 <- function(nsim, nyears,
                                   initSD = 0.6,
                                   finalSD = NULL,
                                   rho = 0.5,
                                   trend = TRUE,
                                   lognormal = TRUE) {
    if (trend) {
        if (is.null(finalSD)) {
            stop("Para usar tendência, especifique finalSD.")
        }
        SDvec <- seq(from = initSD, to = finalSD, length.out = nyears)
    } else {
        SDvec <- rep(initSD, nyears)
    }
    RecDevs <- matrix(NA, nsim, nyears)
    for (i in 1:nsim) {
        eps <- numeric(nyears)
        eps[1] <- rnorm(1, mean = 0, sd = SDvec[1])
        for (t in 2:nyears) {
            eps[t] <- rho * eps[t - 1] +
                rnorm(1, mean = 0, sd = SDvec[t] * sqrt(1 - rho^2))
        }
        RecDevs[i, ] <- if (lognormal) {
                            exp(eps)
                        } else {
                            eps
                        }
    }
    return(RecDevs)
}

######@> Simulating the new RecDevs...
NewProjRecDevs <- SimulateRecruitmentAR1(nsim = 100,
                                         nyears = 30,
                                         initSD = prevSD,
                                         finalSD = 0.6,
                                         rho = 0.5,
                                         trend = TRUE)

######@> Comparing the Recruitment Deviations...
par(mfrow = c(1, 3))
matplot(ProjYears, t(ProjectionRecDevs), type = "l", ylab = "",
        ylim = c(0, 4))
matplot(ProjYears, t(NewProjectionRecDevs), type = "l", ylab = "",
        ylim = c(0, 4))
matplot(ProjYears, t(NewProjRecDevs), type = "l", ylab = "",
        ylim = c(0, 4))
par(mfrow = c(1, 1))

######@> Replacing historical simulations with the new RecDevs
######@> Projection...
Hist005S02 <- Hist005
Hist005S02@SampPars$Stock$Perr_y[, ProjInd] <- NewProjRecDevs

######@>----------------------------------------------------------------
######@> Scenario 03 - Recruitment Deviations decreasing over time...

######@> Simulating the new RecDevs...
NewProjRecDevs02 <- SimulateRecruitmentAR1(nsim = 100,
                                         nyears = 30,
                                         initSD = prevSD,
                                         finalSD = 0.2,
                                         rho = 0.5,
                                         trend = TRUE)

######@> Comparing the Recruitment Deviations...
par(mfrow = c(1, 4))
matplot(ProjYears, t(ProjectionRecDevs), type = "l", ylab = "",
        ylim = c(0, 4))
matplot(ProjYears, t(NewProjectionRecDevs), type = "l", ylab = "",
        ylim = c(0, 4))
matplot(ProjYears, t(NewProjRecDevs), type = "l", ylab = "",
        ylim = c(0, 4))
matplot(ProjYears, t(NewProjRecDevs02), type = "l", ylab = "",
        ylim = c(0, 4))
par(mfrow = c(1, 1))

######@> Replacing historical simulations with the new RecDevs
######@> Projection...
Hist005S03 <- Hist005
Hist005S03@SampPars$Stock$Perr_y[, ProjInd] <- NewProjRecDevs02

########################################################################
######@> Projecting Tuned Management Procedures...

######@> Reading and preparing the Tuned MPs for projections...
for (i in seq_along(ProjectMPs)) {
    ind <- match(ProjectMPs[i], MPnames)
    mp <- readRDS(tunedMPs[ind])$MPout
    assign(ProjectMPs[i], mp, envir = .GlobalEnv)
}

######@> Projecting scenario 01 based on this new RecDevs...
for(mp in ProjectMPs) {
    message("\nProjecting MP: ", mp)
    if (!dir.exists(file.path(MSEDir01, mp)))
        dir.create(file.path(MSEDir01, mp), recursive = TRUE)
    nm <- gsub("OM", "", Hist005S01@OM@Name) |> trimws()
    ## nm <- paste(sprintf("%03d", i), nm, sep="_")
    nm <- paste0(nm, ".mse")
    MSE <- Project(Hist005S01, MPs = mp, parallel = FALSE,
                      silent = TRUE)
    saveRDS(MSE, file.path(MSEDir01, mp, nm))
}

######@> Projecting scenario 02 based on this new RecDevs...
for(mp in ProjectMPs) {
    message("\nProjecting MP: ", mp)
    if (!dir.exists(file.path(MSEDir02, mp)))
        dir.create(file.path(MSEDir02, mp), recursive = TRUE)
    nm <- gsub("OM", "", Hist005S02@OM@Name) |> trimws()
    ## nm <- paste(sprintf("%03d", i), nm, sep="_")
    nm <- paste0(nm, ".mse")
    MSE <- Project(Hist005S02, MPs = mp, parallel = FALSE,
                   silent = TRUE)
    saveRDS(MSE, file.path(MSEDir02, mp, nm))
}

######@> Projecting scenario 03 based on this new RecDevs...
for(mp in ProjectMPs) {
    message("\nProjecting MP: ", mp)
    if (!dir.exists(file.path(MSEDir03, mp)))
        dir.create(file.path(MSEDir03, mp), recursive = TRUE)
    nm <- gsub("OM", "", Hist005S03@OM@Name) |> trimws()
    ## nm <- paste(sprintf("%03d", i), nm, sep="_")
    nm <- paste0(nm, ".mse")
    MSE <- Project(Hist005S03, MPs = mp, parallel = FALSE,
                   silent = TRUE)
    saveRDS(MSE, file.path(MSEDir03, mp, nm))
}

########################################################################
######@> Consolidating Results...

######@>----------------------------------------------------------------
######@> Working with Scenario 01 - Robustness Test...

######@> Loading Robustness Test...

#####@> List of management options...
ManagementOptions <- list.dirs("06_MSEs",
                               recursive = FALSE,
                               full.names = TRUE)

######@> Select the DataLag and Management Interval options...
MngOption <- ManagementOptions[2] ## Scenario 01

######@> Defining the path for the selected Management options...
MPpaths <- list.dirs(MngOption, recursive = FALSE, full.names = TRUE)

#####@> Filtering cases - MPs tuned to 1-30...
MPpaths <- MPpaths[grepl("1_30", MPpaths)]

######@> Loading MSEs - This process can take more than a minute...
MSEs <- lapply(MPpaths, GetMSEs)

#####@> Extracting MSEs based on a specific OM and combine them...
S01 <- addMPs(extrair_por_substring(MSEs, "Qnt50_h7"))

######@> Extracting data from MSEs...
MPList <- lapply(MPpaths, GetInfo)

######@> Transform to data.frame...
DFS01 <- do.call("rbind", MPList)

######@> Checking PGK60%...
CalcPGK(DFS01) ## For all projection period...
CalcPGK(DFS01, Years = 2028:2035) ## For the middle term...

######@> Performance Metric - Consolidating general indicators...
PropTab <- bind_cols(
    CalcPGK(DFS01),
    CalcPGK(DFS01, Years = 2025:2027)[, 2],
    CalcPGK(DFS01, Years = 2028:2033)[, 2],
    CalcPGK(DFS01, Years = 2034:2054)[, 2],
    CalcLRP(DFS01)[, 2],
    CalcLRP(DFS01, Years = 2025:2027)[, 2],
    CalcLRP(DFS01, Years = 2028:2033)[, 2],
    CalcLRP(DFS01, Years = 2034:2054)[, 2],
    CalcPOF(DFS01)[, 2],
    CalcPNOF(DFS01)[, 2],
    CalcAvC(DFS01)[, 2],
    CalcAvC(DFS01, Years = 2025:2027)[, 2],
    CalcAvC(DFS01, Years = 2028:2033)[, 2],
    CalcAvC(DFS01, Years = 2034:2054)[, 2],
    CalcVarC(DFS01)[, 2],
    CalcVarC(DFS01, Years = 2028:2033)[, 2],
    CalcVarC(DFS01, Years = 2034:2054)[, 2]) %>%
    mutate(MP = gsub("_1_30", "", MP))

#####@> Changing names...
names(PropTab) <- gsub(" 2025-2054", "", names(PropTab))
names(PropTab) <- gsub(" 2025-2027", "_short", names(PropTab))
names(PropTab) <- gsub(" 2028-2033", "_mid", names(PropTab))
names(PropTab) <- gsub(" 2034-2054", "_long", names(PropTab))

#####@> Guilt plot...
guiltplot <- PropTab %>%
    gt() %>%
    fmt_number(decimals = 2,
               columns = c("PGK", "PGK_short", "PGK_mid", "PGK_long",
                           "LRP", "LRP_short", "LRP_mid", "LRP_long",
                           "POF", "PNOF", "AvC", "AvC_short", "AvC_mid",
                           "AvC_long", "VarC", "VarC_mid",
                           "VarC_long")) %>%
    data_color(
        columns = c("PGK", "PGK_short", "PGK_mid", "PGK_long", "LRP",
                    "LRP_short", "LRP_mid", "LRP_long", "POF", "PNOF",
                    "AvC", "AvC_short", "AvC_mid", "AvC_long", "VarC",
                    "VarC_mid", "VarC_long"),
        ## colors = scales::col_bin(palette = "BuPu",
        ##                          domain = NULL, bins = 4),
        colors = c("#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        alpha = 0.7, autocolor_text = FALSE) %>%
    tab_style(
        style = list(cell_text(weight = "bold", align = "center")),
        locations = cells_column_labels(everything())) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = c(MP))) %>%
    tab_style(
        style = list(
            cell_borders(sides = c("top", "bottom"),
                         color = "black",
                         weight = px(3))),
        locations = list(
            cells_column_labels(
                columns = gt::everything()))) %>%
    tab_style(
        style = list(
            cell_borders(sides = c("bottom"),
                         color = "black",
                         weight = px(3))),
        locations = list(
            cells_body(rows = 16)))
guiltplot

gtsave(guiltplot,
       filename = "07_Results/06_Robustness_Tests/Fig_38_Guiltyplot_OM05_RTS01_ver00.html")

######@>----------------------------------------------------------------
######@> Working with Scenario 02 - Robustness Test...

######@> Loading Robustness Test...

#####@> List of management options...
ManagementOptions <- list.dirs("06_MSEs",
                               recursive = FALSE,
                               full.names = TRUE)

######@> Select the DataLag and Management Interval options...
MngOption <- ManagementOptions[3] ## Scenario 02

######@> Defining the path for the selected Management options...
MPpaths <- list.dirs(MngOption, recursive = FALSE, full.names = TRUE)

#####@> Filtering cases - MPs tuned to 1-30...
MPpaths <- MPpaths[grepl("1_30", MPpaths)]

######@> Loading MSEs - This process can take more than a minute...
MSEs <- lapply(MPpaths, GetMSEs)

#####@> Extracting MSEs based on a specific OM and combine them...
S02 <- addMPs(extrair_por_substring(MSEs, "Qnt50_h7"))

######@> Extracting data from MSEs...
MPList <- lapply(MPpaths, GetInfo)

######@> Transform to data.frame...
DFS02 <- do.call("rbind", MPList)

######@> Checking PGK60%...
CalcPGK(DFS02) ## For all projection period...
CalcPGK(DFS02, Years = 2028:2035) ## For the middle term...

######@> Performance Metric - Consolidating general indicators...
PropTab <- bind_cols(
    CalcPGK(DFS02),
    CalcPGK(DFS02, Years = 2025:2027)[, 2],
    CalcPGK(DFS02, Years = 2028:2033)[, 2],
    CalcPGK(DFS02, Years = 2034:2054)[, 2],
    CalcLRP(DFS02)[, 2],
    CalcLRP(DFS02, Years = 2025:2027)[, 2],
    CalcLRP(DFS02, Years = 2028:2033)[, 2],
    CalcLRP(DFS02, Years = 2034:2054)[, 2],
    CalcPOF(DFS02)[, 2],
    CalcPNOF(DFS02)[, 2],
    CalcAvC(DFS02)[, 2],
    CalcAvC(DFS02, Years = 2025:2027)[, 2],
    CalcAvC(DFS02, Years = 2028:2033)[, 2],
    CalcAvC(DFS02, Years = 2034:2054)[, 2],
    CalcVarC(DFS02)[, 2],
    CalcVarC(DFS02, Years = 2028:2033)[, 2],
    CalcVarC(DFS02, Years = 2034:2054)[, 2]) %>%
    mutate(MP = gsub("_1_30", "", MP))

#####@> Changing names...
names(PropTab) <- gsub(" 2025-2054", "", names(PropTab))
names(PropTab) <- gsub(" 2025-2027", "_short", names(PropTab))
names(PropTab) <- gsub(" 2028-2033", "_mid", names(PropTab))
names(PropTab) <- gsub(" 2034-2054", "_long", names(PropTab))

#####@> Guilt plot...
guiltplot <- PropTab %>%
    gt() %>%
    fmt_number(decimals = 2,
               columns = c("PGK", "PGK_short", "PGK_mid", "PGK_long",
                           "LRP", "LRP_short", "LRP_mid", "LRP_long",
                           "POF", "PNOF", "AvC", "AvC_short", "AvC_mid",
                           "AvC_long", "VarC", "VarC_mid",
                           "VarC_long")) %>%
    data_color(
        columns = c("PGK", "PGK_short", "PGK_mid", "PGK_long", "LRP",
                    "LRP_short", "LRP_mid", "LRP_long", "POF", "PNOF",
                    "AvC", "AvC_short", "AvC_mid", "AvC_long", "VarC",
                    "VarC_mid", "VarC_long"),
        ## colors = scales::col_bin(palette = "BuPu",
        ##                          domain = NULL, bins = 4),
        colors = c("#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        alpha = 0.7, autocolor_text = FALSE) %>%
    tab_style(
        style = list(cell_text(weight = "bold", align = "center")),
        locations = cells_column_labels(everything())) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = c(MP))) %>%
    tab_style(
        style = list(
            cell_borders(sides = c("top", "bottom"),
                         color = "black",
                         weight = px(3))),
        locations = list(
            cells_column_labels(
                columns = gt::everything()))) %>%
    tab_style(
        style = list(
            cell_borders(sides = c("bottom"),
                         color = "black",
                         weight = px(3))),
        locations = list(
            cells_body(rows = 16)))
guiltplot

gtsave(guiltplot,
       filename = "07_Results/06_Robustness_Tests/Fig_38_Guiltyplot_OM05_RTS02_ver00.html")

######@>----------------------------------------------------------------
######@> Working with Scenario 03 - Robustness Test...

######@> Loading Robustness Test...

#####@> List of management options...
ManagementOptions <- list.dirs("06_MSEs",
                               recursive = FALSE,
                               full.names = TRUE)

######@> Select the DataLag and Management Interval options...
MngOption <- ManagementOptions[4] ## Scenario 03

######@> Defining the path for the selected Management options...
MPpaths <- list.dirs(MngOption, recursive = FALSE, full.names = TRUE)

#####@> Filtering cases - MPs tuned to 1-30...
MPpaths <- MPpaths[grepl("1_30", MPpaths)]

######@> Loading MSEs - This process can take more than a minute...
MSEs <- lapply(MPpaths, GetMSEs)

#####@> Extracting MSEs based on a specific OM and combine them...
S03 <- addMPs(extrair_por_substring(MSEs, "Qnt50_h7"))

######@> Extracting data from MSEs...
MPList <- lapply(MPpaths, GetInfo)

######@> Transform to data.frame...
DFS03 <- do.call("rbind", MPList)

######@> Checking PGK60%...
CalcPGK(DFS03) ## For all projection period...
CalcPGK(DFS03, Years = 2028:2035) ## For the middle term...

######@> Performance Metric - Consolidating general indicators...
PropTab <- bind_cols(
    CalcPGK(DFS03),
    CalcPGK(DFS03, Years = 2025:2027)[, 2],
    CalcPGK(DFS03, Years = 2028:2033)[, 2],
    CalcPGK(DFS03, Years = 2034:2054)[, 2],
    CalcLRP(DFS03)[, 2],
    CalcLRP(DFS03, Years = 2025:2027)[, 2],
    CalcLRP(DFS03, Years = 2028:2033)[, 2],
    CalcLRP(DFS03, Years = 2034:2054)[, 2],
    CalcPOF(DFS03)[, 2],
    CalcPNOF(DFS03)[, 2],
    CalcAvC(DFS03)[, 2],
    CalcAvC(DFS03, Years = 2025:2027)[, 2],
    CalcAvC(DFS03, Years = 2028:2033)[, 2],
    CalcAvC(DFS03, Years = 2034:2054)[, 2],
    CalcVarC(DFS03)[, 2],
    CalcVarC(DFS03, Years = 2028:2033)[, 2],
    CalcVarC(DFS03, Years = 2034:2054)[, 2]) %>%
    mutate(MP = gsub("_1_30", "", MP))

#####@> Changing names...
names(PropTab) <- gsub(" 2025-2054", "", names(PropTab))
names(PropTab) <- gsub(" 2025-2027", "_short", names(PropTab))
names(PropTab) <- gsub(" 2028-2033", "_mid", names(PropTab))
names(PropTab) <- gsub(" 2034-2054", "_long", names(PropTab))

#####@> Guilt plot...
guiltplot <- PropTab %>%
    gt() %>%
    fmt_number(decimals = 2,
               columns = c("PGK", "PGK_short", "PGK_mid", "PGK_long",
                           "LRP", "LRP_short", "LRP_mid", "LRP_long",
                           "POF", "PNOF", "AvC", "AvC_short", "AvC_mid",
                           "AvC_long", "VarC", "VarC_mid",
                           "VarC_long")) %>%
    data_color(
        columns = c("PGK", "PGK_short", "PGK_mid", "PGK_long", "LRP",
                    "LRP_short", "LRP_mid", "LRP_long", "POF", "PNOF",
                    "AvC", "AvC_short", "AvC_mid", "AvC_long", "VarC",
                    "VarC_mid", "VarC_long"),
        ## colors = scales::col_bin(palette = "BuPu",
        ##                          domain = NULL, bins = 4),
        colors = c("#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        alpha = 0.7, autocolor_text = FALSE) %>%
    tab_style(
        style = list(cell_text(weight = "bold", align = "center")),
        locations = cells_column_labels(everything())) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = c(MP))) %>%
    tab_style(
        style = list(
            cell_borders(sides = c("top", "bottom"),
                         color = "black",
                         weight = px(3))),
        locations = list(
            cells_column_labels(
                columns = gt::everything()))) %>%
    tab_style(
        style = list(
            cell_borders(sides = c("bottom"),
                         color = "black",
                         weight = px(3))),
        locations = list(
            cells_body(rows = 16)))
guiltplot

gtsave(guiltplot,
       filename = "07_Results/06_Robustness_Tests/Fig_38_Guiltyplot_OM05_RTS03_ver00.html")

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
