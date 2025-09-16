########################################################################
## Description: Defining the Performance Metrics for the Western
## Atlantic Skipjack tuna MSE...
##
## Maintainer: Datenkraft - ICCAT (Tropical Tuna Species Group)
## Author: Rodrigo Sant'Ana
## Created: mon apr 17 14:52:30 2023 (-0300)
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

## ######@> Update openMSE R packages (This will update openMSE packages if
## ######@> there are updates on GitHub)...
## source("01_script_openMSE_packages_ver00.R")
## 
## ######@> Load some custom functions...
## source("00_script_functions_ver00.R")
## 
## ######@> Load some MP custom functions...
## source("06_script_MP_Internal_Functions_ver00.R")
## 
## ######@> Loading R packages...
## library(dplyr)
## library(ggplot2)
## library(openMSE)

######@> Function to load a list of MSEs...
GetMSEs <- function(MPpath) {
    msefiles <- list.files(MPpath, full.names = TRUE)
    if (length(msefiles) < 1)
        stop("No files found for: ", basename(MPpath))
    MSEList <- list()
    for (j in seq_along(msefiles)) {
        mse <- readRDS(msefiles[j])
        t <- mse@MPs
        tt <- strsplit(mse@Name, "_")[[1]][3:4]
        nome <- paste(t, tt[1], tt[2], sep = "_")
        MSEList[[nome]] <- mse
    }
    MSEList
}

########################################################################
######@> Loading dataset...

## ######@> Management Procedure...
## 
## #####@> List tuned management options...
## ManagementOptionsTune <- list.dirs("05_TunedMPs", recursive = FALSE,
##                                    full.names = TRUE)
## 
## #####@> Select the DataLag and Management Interval options - For tuned
## #####@> MPs...
## MngOptionTune <- ManagementOptionsTune[1] ## DataLag_1_Interval_3
## 
## #####@> Listing tuned MPs...
## tunedMPs <- list.files(MngOptionTune, full.names = TRUE, recursive = FALSE)
## MPnames <- gsub('.mp', '', basename(tunedMPs))
## 
## #####@> Select MPs to Project...
## ProjectMPs <- MPnames
## 
## #####@> Reading and preparing the Tuned MPs for projections...
## for (i in seq_along(ProjectMPs)) {
##     ind <- match(ProjectMPs[i], MPnames)
##     mp <- readRDS(tunedMPs[ind])$MPout
##     assign(ProjectMPs[i], mp, envir = .GlobalEnv)
## }
## 
## ######@> List of management options...
## ManagementOptions <- list.dirs("06_MSEs",
##                                recursive = FALSE,
##                                full.names = TRUE)
## 
## ######@> Select the DataLag and Management Interval options...
## MngOption <- ManagementOptions[1] ## DataLag_1_Interval_3
## 
## ######@> Defining the path for the selected Management options...
## MPpaths <- list.dirs(MngOption, recursive = FALSE, full.names = TRUE)
## 
## #####@> Filtering cases - MPs tuned to 1-30...
## MPpaths <- MPpaths[grepl("1_30", MPpaths)]
## 
## ######@> Loading OMs example of data...
## MSEs <- lapply(MPpaths, GetMSEs)
## 
## ######@> Extracting MSEs based on a specific OM and combine them...
## OMsQnt25_h6 <- extrair_por_substring(MSEs, "Qnt25_h6")

########################################################################
######@> Performance Metrics / Statistics...

######@>----------------------------------------------------------------
######@> Adapted function...
calcMedian <- function(Prob) {
    if ("matrix" %in% class(Prob))
        return(apply(Prob, 2, median, na.rm = TRUE))
    if ("numeric" %in% class(Prob))
        return(median(Prob, na.rm = TRUE))
}

calcMax <- function (Prob) {
    if ("matrix" %in% class(Prob))
        return(apply(Prob, 2, max, na.rm = TRUE))
    if ("numeric" %in% class(Prob))
        return(max(Prob, na.rm = TRUE))
}

firstChange <- function(vec) {
    ll <- length(vec)-1
    if (all(diff(vec)<1E-1))
        return(NA)
    for (i in 1:ll) {
        if (abs(vec[i]-vec[i+1]) > 0.1)
            break()
    }
    i
}

is_GK <- function(x, f, b) {
    nMP <- dim(f)[2]
    out <- (f[x,] < 1 & b[x,] > 1)
    out
}

extrair_por_substring <- function(lista_externa, substring) {
    out <- lapply(lista_externa, function(sublista) {
        nomes_filtrados <- grep(substring, names(sublista),
                                value = TRUE)
        sublista[nomes_filtrados]
    })
    unlist(out)
}

######@>----------------------------------------------------------------
######@> [Management Objectives Res. 22-02] Status...

######@> PGK[short]: Probability of being in the Kobe green quadrant
######@> (i.e., SSB >= SSBMSY and F<FMSY in year 2 - 4)...
PGK_short <- function(MSEobj = NULL, Ref = 1, Yrs = c(2, 4)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
        "PKG_short: Probability of being in the Kobe green quadrant (SB>SBMSY & F<FMSY) in Years 1-3 (2026-2028)"
    PMobj@Caption <- "Prob. Kobe Green Quadrant (2026-2028)"
    PMobj@Ref <- Ref
    tt <- MSEobj@SB_SBMSY[ , , Yrs[1]:Yrs[2]] > 1 &
        MSEobj@F_FMSY[ , , Yrs[1]:Yrs[2]] < 1
    if (is.null(dim(tt)))
        tt <- matrix(tt, nrow = MSEobj@nsim, ncol = 1)
    PMobj@Stat <- tt
    PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
    PMobj@Mean <- calcMean(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(PGK_short) <- 'PM'

######@> PGK[medium]: Probability of being in the Kobe green quadrant
######@> (i.e., SSB >= SSBMSY and F < FMSY in year 5 - 11)...
PGK_med <- function(MSEobj = NULL, Ref = 1, Yrs = c(5, 11)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
        "PKG_med: Probability of being in the Kobe green quadrant (SB>SBMSY & F<FMSY) in Years 4-10 (2029-2034)"
    PMobj@Caption <- "Prob. Kobe Green Quadrant (2029-2034)"
    PMobj@Ref <- Ref
    tt <- MSEobj@SB_SBMSY[ , , Yrs[1]:Yrs[2]] > 1 &
        MSEobj@F_FMSY[ , , Yrs[1]:Yrs[2]] < 1
    if (is.null(dim(tt)))
        tt <- matrix(tt, nrow = MSEobj@nsim, ncol = 1)
    PMobj@Stat <- tt
    PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
    PMobj@Mean <- calcMean(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(PGK_med) <- 'PM'

######@> PGK[long]: Probability of being in the Kobe green quadrant
######@> (i.e., SSB >= SSBMSY and F<FMSY in year 12 - 31)...
PGK_long <- function(MSEobj = NULL, Ref = 1, Yrs = c(12, 31)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
        "PKG_long: Probability of being in the Kobe green quadrant (SB>SBMSY & F<FMSY) in Years 11-30 (2035-2055)"
    PMobj@Caption <- "Prob. Kobe Green Quadrant (2035-2055)"
    PMobj@Ref <- Ref
    tt <- MSEobj@SB_SBMSY[ , , Yrs[1]:Yrs[2]] > 1 &
        MSEobj@F_FMSY[ , , Yrs[1]:Yrs[2]] < 1
    if (is.null(dim(tt)))
        tt <- matrix(tt, nrow = MSEobj@nsim, ncol = 1)
    PMobj@Stat <- tt
    PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
    PMobj@Mean <- calcMean(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(PGK_long) <- 'PM'

######@> PGK[all]: Probability of being in the Kobe green quadrant
######@> (i.e., SSB >= SSBMSY and F<FMSY in all period [30 years])...
PGK <- function(MSEobj = NULL, Ref = 1, Yrs = c(2, 31)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
        "PKG: Probability of being in the Kobe green quadrant (SB>SBMSY & F<FMSY) in Years 1-30 (2026-2055)"
    PMobj@Caption <- "Prob. Kobe Green Quadrant (2026-2055)"
    PMobj@Ref <- Ref
    tt <- MSEobj@SB_SBMSY[ , , Yrs[1]:Yrs[2]] > 1 &
        MSEobj@F_FMSY[ , , Yrs[1]:Yrs[2]] < 1
    if (is.null(dim(tt)))
        tt <- matrix(tt, nrow = MSEobj@nsim, ncol = 1)
    PMobj@Stat <- tt
    PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
    PMobj@Mean <- calcMean(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(PGK) <- 'PM'

######@> POF[all]: Probability of Overfishing (F>FMSY) over all years
######@> [1 - 30 years]
POF <- function(MSEobj = NULL, Ref = 1, Yrs = c(2, 31))  {
    if(!inherits(MSEobj, 'MSE'))
        stop('This PM method is designed for objects of class `MSE`')
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
    "POF: Probability of Overfishing (F>FMSY) over all years (2026-2055)"
    PMobj@Caption <- "Prob. Overfishing (F>FMSY) (2026-2055)"
    PMobj@Ref <- Ref
    PMobj@Stat <- MSEobj@F_FMSY[ , , Yrs[1]:Yrs[2]] > 1
    PMobj@Prob <- calcProb(MSEobj@F_FMSY[ , , Yrs[1]:Yrs[2]] > 1, MSEobj)
    PMobj@Mean <- calcMean(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(POF) <- 'PM'

######@> PNOF[all]: Probability of Not Overfishing (F>FMSY) over all years
######@> [1 - 30 years]
PNOF <- function(MSEobj = NULL, Ref = 1, Yrs = c(2, 31))  {
    if(!inherits(MSEobj, 'MSE'))
        stop('This PM method is designed for objects of class `MSE`')
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
        "PNOF: Probability of Not Overfishing (F>FMSY) over all years (2026-2055)"
    PMobj@Caption <- "Prob. Not Overfishing (F>FMSY) (2026-2055)"
    PMobj@Ref <- Ref
    PMobj@Stat <- MSEobj@F_FMSY[ , , Yrs[1]:Yrs[2]] < 1
    PMobj@Prob <- calcProb(MSEobj@F_FMSY[ , , Yrs[1]:Yrs[2]] < 1,
                           MSEobj)
    PMobj@Mean <- calcMean(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(PNOF) <- 'PM'

######@>----------------------------------------------------------------
######@> [Management Objectives Res. 22-02] Safety...

######@> LRPshort: Probability of breaching the limit reference point
######@> (i.e., SSB < 0.4 * SSBMSY over years 1 - 3)...
LRP_short <- function(MSEobj = NULL, Ref = 0.4, Yrs = c(2, 4)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
        "LRP_short: Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any of the first 3 years (2026-2028)"
    PMobj@Caption <- "Prob. SB < 0.4SBMSY (2026-2028)"
    PMobj@Ref <- Ref
    PMobj@Stat <- MSEobj@SB_SBMSY[, , Yrs[1]:Yrs[2]]
    PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MSEobj)
    Prob  <- array(as.logical(PMobj@Prob), dim = dim(PMobj@Prob))
    PMobj@Mean <- calcMean(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(LRP_short) <- 'PM'

######@> LRPmedium: Probability of breaching the limit reference point
######@> (i.e., SSB < 0.4 * SSBMSY over years 4 - 10)...
LRP_med <- function(MSEobj = NULL, Ref = 0.4, Yrs = c(5, 11)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
        "LRP_med: Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any of years 4-10 (2029-2034)"
    PMobj@Caption <- "Prob. SB < 0.4SBMSY (2029-2034)"
    PMobj@Ref <- Ref
    PMobj@Stat <- MSEobj@SB_SBMSY[, , Yrs[1]:Yrs[2]]
    PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MSEobj)
    Prob  <- array(as.logical(PMobj@Prob), dim = dim(PMobj@Prob))
    PMobj@Mean <- calcMean(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(LRP_med) <- 'PM'

######@> LRPlong: Probability of breaching the limit reference point
######@> (i.e., SSB < 0.4 * SSBMSY over years 4 - 10)...
LRP_long <- function(MSEobj = NULL, Ref = 0.4, Yrs = c(12, 31)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
        "LRP_long: Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any of years 11-30 (2035-2055)"
    PMobj@Caption <- "Prob. SB < 0.4SBMSY (2035-2055)"
    PMobj@Ref <- Ref
    PMobj@Stat <- MSEobj@SB_SBMSY[, , Yrs[1]:Yrs[2]]
    PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MSEobj)
    Prob  <- array(as.logical(PMobj@Prob), dim = dim(PMobj@Prob))
    PMobj@Mean <- calcMean(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(LRP_long) <- 'PM'

######@> LRPall: Probability of breaching the limit reference point
######@> (i.e., SSB < 0.4 * SSBMSY over all years)...
LRP <- function(MSEobj = NULL, Ref = 0.4, Yrs = c(2, 31))  {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
        "LRP: Probability of breaching the limit reference point (SSB<0.4SSB_MSY) over all years (2026-2055)"
    PMobj@Caption <- "Prob. SB < 0.4SBMSY (2026-2055)"
    PMobj@Ref <- Ref
    PMobj@Stat <- MSEobj@SB_SBMSY[, , Yrs[1]:Yrs[2]]
    PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MSEobj)
    Prob  <- array(as.logical(PMobj@Prob), dim = dim(PMobj@Prob))
    PMobj@Mean <- calcMean(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(LRP) <- 'PM'

######@> nLRPshort: Probability of not breaching the limit reference point
######@> (i.e., SSB < 0.4 * SSBMSY over years 1 - 3)...
nLRP_short <- function(MSEobj = NULL, Ref = 0.4, Yrs = c(2, 4)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
        "nLRP_short: Probability of not breaching the limit reference point (SSB<0.4SSB_MSY) in any of the first 3 years (2026-2028)"
    PMobj@Caption <- "Prob. of not breaching LRP (2026-2028)"
    PMobj@Ref <- Ref
    PMobj@Stat <- MSEobj@SB_SBMSY[, , Yrs[1]:Yrs[2]]
    PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
    Prob  <- array(as.logical(PMobj@Prob), dim = dim(PMobj@Prob))
    PMobj@Mean <- calcMean(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(nLRP_short) <- 'PM'

######@> nLRPmedium: Probability of not breaching the limit reference point
######@> (i.e., SSB < 0.4 * SSBMSY over years 4 - 10)...
nLRP_med <- function(MSEobj = NULL, Ref = 0.4, Yrs = c(5, 11)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
        "nLRP_med: Probability of not breaching the limit reference point (SSB<0.4SSB_MSY) in any of years 4-10 (2029-2034)"
    PMobj@Caption <- "Prob. of not breaching LRP (2029-2034)"
    PMobj@Ref <- Ref
    PMobj@Stat <- MSEobj@SB_SBMSY[, , Yrs[1]:Yrs[2]]
    PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
    Prob  <- array(as.logical(PMobj@Prob), dim = dim(PMobj@Prob))
    PMobj@Mean <- calcMean(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(nLRP_med) <- 'PM'

######@> nLRPlong: Probability of not breaching the limit reference point
######@> (i.e., SSB < 0.4 * SSBMSY over years 4 - 10)...
nLRP_long <- function(MSEobj = NULL, Ref = 0.4, Yrs = c(12, 31)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
        "nLRP_long: Probability of not breaching the limit reference point (SSB<0.4SSB_MSY) in any of years 11-30 (2035-2055)"
    PMobj@Caption <- "Prob. of not breaching LRP (2035-2055)"
    PMobj@Ref <- Ref
    PMobj@Stat <- MSEobj@SB_SBMSY[, , Yrs[1]:Yrs[2]]
    PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
    Prob  <- array(as.logical(PMobj@Prob), dim = dim(PMobj@Prob))
    PMobj@Mean <- calcMean(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(nLRP_long) <- 'PM'

######@> nLRPall: Probability of not breaching the limit reference point
######@> (i.e., SSB < 0.4 * SSBMSY over all years)...
nLRP <- function(MSEobj = NULL, Ref = 0.4, Yrs = c(2, 31))  {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
        "nLRP: Probability of not breaching the limit reference point (SSB<0.4SSB_MSY) over all years (2026-2055)"
    PMobj@Caption <- "Prob. of not breaching LRP (2026-2055)"
    PMobj@Ref <- Ref
    PMobj@Stat <- MSEobj@SB_SBMSY[, , Yrs[1]:Yrs[2]]
    PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
    Prob  <- array(as.logical(PMobj@Prob), dim = dim(PMobj@Prob))
    PMobj@Mean <- calcMean(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(nLRP) <- 'PM'

######@>----------------------------------------------------------------
######@> [Management Objectives Res. 22-02] Yield...

######@> AvCshort: Median catches (t) over years 1 - 3...
AvC_short <- function(MSEobj = NULL, Ref = 1, Yrs = c(2, 4)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <- "AvC_short: Median Catch (t) over years 1-3"
    PMobj@Caption <- "Median Catch (t) 2026-2028"
    PMobj@Stat <- MSEobj@Removals[ , , Yrs[1]:Yrs[2]]
    PMobj@Ref <- Ref
    PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
    PMobj@Mean <- calcMedian(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(AvC_short) <- 'PM'

######@> AvCmedium: Median catches (t) over years 4 - 10...
AvC_med <- function(MSEobj = NULL, Ref = 1, Yrs = c(5, 11)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <- "AvC_med: Median Catch (t) over years 4-10"
    PMobj@Caption <- "Median Catch (t) 2029-2034"
    PMobj@Stat <- MSEobj@Removals[ , , Yrs[1]:Yrs[2]]
    PMobj@Ref <- Ref
    PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
    PMobj@Mean <- calcMedian(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(AvC_med) <- 'PM'

######@> AvClong: Median catches (t) over years 11 - 30...
AvC_long <- function(MSEobj = NULL, Ref = 1, Yrs = c(12, 31)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <- "AvC_long: Median Catch (t) over years 11-30"
    PMobj@Caption <- "Median Catch (t) 2035-2055"
    PMobj@Stat <- MSEobj@Removals[ , , Yrs[1]:Yrs[2]]
    PMobj@Ref <- Ref
    PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
    PMobj@Mean <- calcMedian(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(AvC_long) <- 'PM'

######@> Median Catch relative to MSY over years 1-3 (2026-2028)...
rAvC_short <- function(MSEobj = NULL, Ref = 1, Yrs = c(2, 4)) {
    if(!inherits(MSEobj, 'MSE'))
        stop('This PM method is designed for objects of class `MSE`')
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <- 'rAvC_short: Median relative Catch over years 1-3'
    PMobj@Caption <- 'Median relative Catch 2026-2028'
    Stat_y <- MSEobj@Removals[ , , Yrs[1]:Yrs[2]]
    MSYs <- MSEobj@RefPoint$ByYear$MSY
    nyears <- MSEobj@nyears
    MSYs <- MSYs[, nyears + Yrs[1]:Yrs[2]]
    out <- lapply(1:dim(Stat_y)[2], function(i) Stat_y[, i, ]/MSYs)
    out2 <- do.call(abind::abind, c(out, along = 3))
    PMobj@Stat <- apply(out2, c(1, 3), median)
    PMobj@Ref <- Ref
    PMobj@Prob <- calcProb(PMobj@Stat, MSEobj) # no probability to calc
    PMobj@Mean <- calcMedian(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(rAvC_short) <- 'PM'

######@> Median Catch relative to MSY over years 4-10 (2029-2034)...
rAvC_med <- function(MSEobj = NULL, Ref = 1, Yrs = c(5, 11)) {
    if(!inherits(MSEobj, 'MSE'))
        stop('This PM method is designed for objects of class `MSE`')
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <- 'rAvC_med: Median relative Catch over years 4-10'
    PMobj@Caption <- 'Median relative Catch 2029-2034'
    Stat_y <- MSEobj@Removals[ , , Yrs[1]:Yrs[2]]
    MSYs <- MSEobj@RefPoint$ByYear$MSY
    nyears <- MSEobj@nyears
    MSYs <- MSYs[, nyears + Yrs[1]:Yrs[2]]
    out <- lapply(1:dim(Stat_y)[2], function(i) Stat_y[, i, ]/MSYs)
    out2 <- do.call(abind::abind, c(out, along = 3))
    PMobj@Stat <- apply(out2, c(1, 3), median)
    PMobj@Ref <- 1
    PMobj@Prob <- calcProb(PMobj@Stat, MSEobj) # no probability to calc
    PMobj@Mean <- calcMedian(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(rAvC_med) <- 'PM'

######@> Median Catch relative to MSY over years 11-30 (2035-2055)...
rAvC_long <- function(MSEobj = NULL, Ref = 1, Yrs = c(12, 31)) {
    if(!inherits(MSEobj, 'MSE'))
        stop('This PM method is designed for objects of class `MSE`')
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <- 'rAvC_med: Median relative Catch over years 11-30'
    PMobj@Caption <- 'Median relative Catch 2035-2055'
    Stat_y <- MSEobj@Removals[ , , Yrs[1]:Yrs[2]]
    MSYs <- MSEobj@RefPoint$ByYear$MSY
    nyears <- MSEobj@nyears
    MSYs <- MSYs[, nyears + Yrs[1]:Yrs[2]]
    out <- lapply(1:dim(Stat_y)[2], function(i) Stat_y[, i, ]/MSYs)
    out2 <- do.call(abind::abind, c(out, along = 3))
    PMobj@Stat <- apply(out2, c(1, 3), median)
    PMobj@Ref <- 1
    PMobj@Prob <- calcProb(PMobj@Stat, MSEobj) # no probability to calc
    PMobj@Mean <- calcMedian(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(rAvC_long) <- 'PM'

######@>----------------------------------------------------------------
######@> [Management Objectives Res. 22-02] Stability [Based on NSWO]...

######@> VarCmedium: Variation in TAC (%) between management cycles 4 -
######@> 10 years...
VarCmedium <- function(MSEobj = NULL, Ref = 1, Yrs = c(5, 11)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <- "VarCmedium: Mean Variation in TAC (%) between management cycles over years 4-10"
    PMobj@Caption <- "Mean Variation in TAC (%) 2029-2034"
    TAC <- apply(MSEobj@TAC[ , , Yrs[1]:Yrs[2], drop = FALSE],
                 c(1, 2, 3), sum, na.rm = TRUE)
    ## get management cycle
    interval <- min(apply(TAC, 1, firstChange), na.rm = TRUE)
    yrs <- seq_along(Yrs[1]:Yrs[2])
    change_yrs <- seq(1, by = interval, to = max(yrs))
    y1 <- change_yrs[1:(length(change_yrs)-1)]
    y2 <- change_yrs[2:length(change_yrs)]
    if (MSEobj@nMPs > 1) {
        AAVY <- apply(((((TAC[, , y2] - TAC[, , y1]) /
                         TAC[, , y1])^2)^0.5), c(1, 2), median,
                      na.rm = TRUE)
    } else {
        AAVY <- array(apply(((((TAC[, 1, y2] - TAC[, 1, y1]) /
                               TAC[, 1, y1])^2)^0.5), 1, median,
                            na.rm = TRUE))
    }
    PMobj@Stat <- AAVY
    PMobj@Ref <- Ref
    PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
    PMobj@Mean <- calcMedian(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(VarCmedium) <- 'PM'

######@> VarClong: Variation in TAC (%) between management cycles 11 -
######@> 30 years...
VarClong <- function(MSEobj = NULL, Ref = 1, Yrs = c(12, 31)) {
    Yrs <- ChkYrs(Yrs, MSEobj)
    PMobj <- new("PMobj")
    PMobj@Name <-
        "VarClong: Mean Variation in TAC (%) between management cycles over years 11-30"
    PMobj@Caption <- "Mean Variation in TAC (%) 2035-2055"
    TAC <- apply(MSEobj@TAC[ , , Yrs[1]:Yrs[2], drop = FALSE],
                 c(1, 2, 3), sum, na.rm = TRUE)
    ## get management cycle
    interval <- min(apply(TAC, 1, firstChange), na.rm = TRUE)
    yrs <- seq_along(Yrs[1]:Yrs[2])
    change_yrs <- seq(1, by = interval, to = max(yrs))
    y1 <- change_yrs[1:(length(change_yrs)-1)]
    y2 <- change_yrs[2:length(change_yrs)]
    if (MSEobj@nMPs > 1) {
        AAVY <- apply(((((TAC[, , y2] - TAC[, , y1]) /
                         TAC[, , y1])^2)^0.5), c(1, 2), median,
                      na.rm = TRUE)
    } else {
        AAVY <- array(apply(((((TAC[, 1, y2] - TAC[, 1, y1]) /
                               TAC[, 1, y1])^2)^0.5), 1, median,
                            na.rm = TRUE))
    }
    PMobj@Stat <- AAVY
    PMobj@Ref <- Ref
    PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
    PMobj@Mean <- calcMedian(PMobj@Prob)
    PMobj@MPs <- MSEobj@MPs
    PMobj
}
class(VarClong) <- 'PM'

######@> VarCAll: Variation in TAC (%) between management cycles 1 -
######@> 30 years...
VarC <- function(MSEobj = NULL, Ref = 1, Yrs = c(2, 31)) {
    if(!inherits(MSEobj,'MSE'))
    stop('This PM method is designed for objects of class `MSE`')
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <-
      "VarC: Mean Variation in TAC (%) between management cycles over all years"
  PMobj@Caption <- "Mean Variation in TAC (%)"
  TAC <- apply(MSEobj@TAC[ , , Yrs[1]:Yrs[2], drop = FALSE],
               c(1, 2, 3), sum, na.rm = TRUE)
  ## get management cycle
  interval <- min(apply(TAC, 1, firstChange), na.rm = TRUE)
  yrs <- seq_along(Yrs[1]:Yrs[2])
  change_yrs <- seq(1, by = interval, to = max(yrs))
  y1 <- change_yrs[1:(length(change_yrs)-1)]
  y2 <- change_yrs[2:length(change_yrs)]
  if (MSEobj@nMPs > 1) {
      AAVY <- apply(((((TAC[, , y2] - TAC[, , y1]) /
                       TAC[, , y1])^2)^0.5), c(1, 2), median,
                    na.rm = TRUE)
  } else {
      AAVY <- array(apply(((((TAC[, 1, y2] - TAC[, 1, y1]) /
                             TAC[, 1, y1])^2)^0.5), 1, median,
                          na.rm = TRUE))
  }
  PMobj@Stat <- AAVY
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
  PMobj@Mean <- calcMedian(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(VarC) <- 'PM'

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
