########################################################################
## Description: Management Procedure Tuning Process implemented to find
## each MP to achieve the 60% PGK  in the middle term...
##
## Maintainer: Datenkraft - ICCAT (Tropical Tunas Species Group)
## Author: Rodrigo Sant'Ana & Adrian Hordyk
## Created: ter jun 24 09:25:02 2025 (-0300)
## Version: 0.0.1
##
## URL:
## Doc URL:
##
## Database info:
##
### Commentary:
## https://github.com/Blue-Matter/ClimateTest/
## https://github.com/Blue-Matter/Blue_Shark_MSE/
##
### Code:
########################################################################

########################################################################
######@> Setup R...

######@> Update openMSE R packages (This will update openMSE packages if
######@> there are updates on GitHub)...
source("01_script_openMSE_packages_ver00.R")

######@> Loading R packages...
library(openMSE)
library(tidyverse)
library(snowfall)
library(future)

######@> Defining path to Historical Simulated Data...
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

###@> Loading MPs developed to this MSE...
AllMPs <- getMPNames()

########################################################################
######@> Defining a function to optimize the Tune process...

######@> Function to optimize the PGK60% for each MP...
optPGK60_1_30 <- function(MSE_list) {
    PGKm <- sapply(MSE_list, function(X) {
        ## Years 1 - 30 - index 2:31 becuase first projection year is
        ## 2025 before MP is used
        mean(X@SB_SBMSY[ , , 2:30] > 1 & X@F_FMSY[ , , 2:30] < 1)
    })
    PGKw <- round(mean(PGKm), 3)
    ssq <- (PGKw-0.6)^2
    cat(paste0("*************************\n"))
    cat(paste0("PGKw = ", PGKw, "\n"))
    cat(paste0("SSQ = ", round(ssq, 5), "\n"))
    cat(paste0("*************************\n\n\n"))
    return(list(PGKw = PGKw, ssq = ssq))
}

######@> A generic function that uses optimize to tune a single MP
######@> parameter to minimize a user-specified function (e.g. squared
######@> distance from a mean yield, PGK = 60%, etc.)...
tune_MPRS <- function(Hist_list, MP, MP_parname, interval, minfunc,
                      tol = 1E-2, parallel = FALSE) {
    opt <- optimize(int_tuneRS, interval = interval,
                    MP_parname = MP_parname, MP = MP,
                    Hist_list = Hist_list, minfunc = minfunc,
                    tol = tol, parallel = parallel)
    MPout <- get(MP)
    formals(MPout)[MP_parname] <- opt$minimum
    class(MPout) <- "MP"
    ## cat("Resultados de Otimização:\n")
    ## print(results)
    return(list(MPout = MPout, optimization_results = results))
    ## return(MPout)
}

######@> Internal MSE running function for the tune_MP function (Tom
######@> Carruthers, 2024)...
results <- list()
int_tuneRS <- function(par, MP_parname, MP, Hist_list, minfunc, parallel) {
    assign("MPtest", get(MP))
    formals(MPtest)[[MP_parname]] <- par
    cat(paste0(MP_parname, " = ", round(par, 6), " \n"))
    class(MPtest) <- "MP"
    if (!parallel) {
        MSE_list <- lapply(Hist_list,
                           function(X) Project(X, MPs = "MPtest"))
    } else {
        sfExport("MPtest", "Hist_list", "minfunc")
        MSE_list <- sfLapply(Hist_list,
                             function(X) Project(X, MPs = "MPtest"))
    }
    PGKw <- minfunc(MSE_list)
    result <- list(par = par, PGKw = PGKw$PGKw, SSQ = PGKw$ssq)
    results <<- append(results, list(result))
    return((PGKw$PGKw - 0.6)^2)
}

######@> Custom Tuning function...
DoMPTune <- function(HistList,
                     MPName,
                     TuneInterval = c(0.05, 4),
                     TuneFunction = optPGK60_1_30,
                     Data_Lag = 1,
                     ManagementInterval = 3,
                     Initial_MP_Yr = 2026,
                     tol = 1E-2,
                     parallel = FALSE) {
    message("Tuning: ", MPName)
    tuneMP <- get(MPName)
    formals(tuneMP)$Data_Lag <- Data_Lag
    formals(tuneMP)$Interval <- ManagementInterval
    formals(tuneMP)$Initial_MP_Yr <- Initial_MP_Yr
    class(tuneMP) <- "MP"
    assign(MPName, tuneMP, envir = .GlobalEnv)
    tunedMP <- tune_MPRS(HistList, MP = MPName,
                         MP_parname = "tunepar",
                         interval = TuneInterval,
                         minfunc = TuneFunction,
                         tol = tol,
                         parallel = parallel)
    dirName <- paste0("DataLag_", Data_Lag, "_Interval_",
                      ManagementInterval)
    if (!dir.exists("05_TunedMPs")) {
        dir.create("05_TunedMPs")
    }
    if (!dir.exists(file.path("05_TunedMPs", dirName))) {
        dir.create(file.path("05_TunedMPs", dirName))
    }
    filename <- paste0(MPName, "_1_30.mp")
    saveRDS(tunedMP, file.path("05_TunedMPs", dirName, filename))
}

########################################################################
######@> Applying Tuning Process...

######@>---------------------------------------------------------------- 
######@> Running in parallel - Single MPs...

#####@> Defining the option...
parallel <- TRUE

#####@> Set the variables, data and functions to be exported...
if (parallel) {
    setup(cpus = 9)
    sfExport(list = list("Catchdf",
                         "FixedTAC",
                         "SameTAC",
                         "adjust_TAC",
                         "adjust_TAC2"))
}

######@> Testing for one MP...
DoMPTune(HistList, MPName = "CE1", TuneInterval = c(0.5, 2.5),
         parallel = parallel)

DoMPTune(HistList, MPName = "IR1", TuneInterval = c(0.5, 2.5),
         parallel = parallel)

DoMPTune(HistList, MPName = "SP_01", TuneInterval = c(0.05, 2),
         parallel = parallel)

DoMPTune(HistList, MPName = "SPAH_01", TuneInterval = c(0.05, 4),
         parallel = parallel)

#####@> Stoping parallel...
sfStop()

######@>----------------------------------------------------------------
######@> Running in parallel - Multiple MPs...

#####@> Defining the option...
parallel <- TRUE

#####@> Set the variables, data and functions to be exported...
if (parallel) {
    setup(cpus = 9)
    sfExport(list = list("Catchdf",
                         "FixedTAC",
                         "SameTAC",
                         "adjust_TAC",
                         "adjust_TAC2"))
}

######@> Constant Exploitation MPs...

#####@> Define CE MPs...
CE_MPs <- c("CE1", "CE2", "CE3")

#####@> Running in a loop...
for(mp in CE_MPs) {
    DoMPTune(HistList, MPName = mp, TuneInterval = c(0.5, 2.5),
             parallel = parallel)
}

######@> Index Ratio MPs...

#####@> Define IR MPs...
IR_MPs <- c("IR1", "IR2", "IR3")

#####@> Running in a loop...
for (mp in IR_MPs) {
    DoMPTune(HistList, MPName = mp, TuneInterval = c(0.5, 2.5),
             parallel = parallel)
}

######@> Surplus Production Models MPs...

#####@> Define SP MPs...
SP_MPs <- c("SP_01", "SP_02", "SP_03", "SP_04", "SP_05")

#####@> Running in a loop...
for (mp in SP_MPs) {
    DoMPTune(HistList, MPName = mp, TuneInterval = c(0.05, 2),
             parallel = parallel)
}

######@> Surplus Production Models with AH HCR MPs...

#####@> Define SPAH MPs...
SPAH_MPs <- c("SPAH_01", "SPAH_02", "SPAH_03", "SPAH_04", "SPAH_05")

#####@> Running in a loop...
for (mp in SPAH_MPs) {
    DoMPTune(HistList, MPName = mp, TuneInterval = c(0.05, 4),
             parallel = parallel)
}

#####@> Stoping parallel...
sfStop()




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
