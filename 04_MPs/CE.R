########################################################################
## Description: Constant Exploitation Management Procedure - WSKJ MSE...
##
## Maintainer: Datenkraft - ICCAT (Tropical Tunas Species Group)
## Author: Rodrigo Sant'Ana & Adrian Hordyk
## Created: seg jun 23 11:34:19 2025 (-0300)
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

######@> Source functions to be used in each MP...
source("06_script_MP_Internal_Functions_ver00.R")

########################################################################
######@> Constant Exploitation Management Procedure...

######@> Constant Exploitation MPs...
## Arguments:
## - tunepar: tuning value for reference index
## - modifier: base modifier for reference index
## - doSmoother: logical; include smoother?
## - refYear: reference year for historical index
## - mc: max change in TAC between management cycles: c(MaxDecrease,
## MaxIncrease); NA to ignore
## - yrs: smoothing years:
##        first value - nyears before refYear (including refYear)
##        second value - nyears before current year (including current
## year)
## - incHCR: include harvest control rule; logical
ConstantExploitation <- function(x, Data,
                                 Data_Lag = 1,
                                 Interval = 3,
                                 Initial_MP_Yr = 2026,
                                 reps = 1,
                                 tunepar = 1,
                                 modifier = 0.4,
                                 doSmoother = FALSE,
                                 refYear = 2017,
                                 mc = c(0.25, 0.25),
                                 yrs = c(2, 2),
                                 incHCR = FALSE,
                                 ...) {
    Rec <- new("Rec")
    ## Check if TAC needs to be updated...
    if (SameTAC(Initial_MP_Yr, Interval, Data)) {
        Rec@TAC <- Data@MPrec[x]
        Rec <- FixedTAC(Rec, Data) # use actual catches if they are
                                   # available
        return(Rec)
    }
    ## Lag Data...
    Data <- Lag_Data(Data, Data_Lag)
    Data@Year <- Data@Year[1:(length(Data@Year)-Data_Lag)]
    ## smooth combined index
    if (doSmoother) {
        index <- smoothed_index <- Data@Ind[x,]
        smoothed <- stats::smooth(index[!is.na(index)],
                                  kind = "3RS3R",
                                  twiceit = TRUE,
                                  endrule = "copy")
        smoothed_index[!is.na(smoothed_index)] <- smoothed
        Data@Ind[x,] <- smoothed_index
    }
    ## Calculate Historical Relative Exploitation Rate
    yr.ind <- which(Data@Year == refYear)
    hist.yrs <- (yr.ind - yrs[1] + 1):yr.ind
    histER <- mean(Data@Cat[x, hist.yrs]) / mean(Data@Ind[x, hist.yrs])
    ## Calculate Current Relative Exploitation Rate
    current_yr <- length(Data@Ind[x,])
    recent_yrs <- (current_yr - yrs[2] + 1):current_yr
    curER <- mean(Data@Cat[x, recent_yrs]) /
        mean(Data@Ind[x, recent_yrs])
    ## Control Rule
    histInd <- mean(Data@Ind[x, hist.yrs]) * modifier * tunepar
    curInd <- mean(Data@Ind[x, recent_yrs], na.rm = TRUE)
    ind_ratio <- curInd/histInd
    targER <- histER * ind_ratio
    if (incHCR) {
        if (ind_ratio >= 0.8) {
            targER <- histER
        } else if (ind_ratio > 0.5) {
            targER <- histER * (-1.4 + 3 * ind_ratio)
        } else {
            targER <- 0.1 * histER
        }
    }
    ## Exploitation Rate Ratio
    ER_ratio <- targER/curER
    TAC <- ER_ratio  * Data@MPrec[x]
    ## Maximum allowed change in TAC
    ## Rec@TAC <- TAC
    Rec@TAC <- adjust_TAC(TAC, Data@MPrec[x], mc)
    Rec
}

######@> Defining variations of constant exploitation MP...

## #####@> As defined in original structure...
## CE1 <- ConstantExploitation
## class(CE1) <- 'MP'

## #####@> Including smooth function to combined index...
## CE2 <- ConstantExploitation
## formals(CE2)$doSmoother <- TRUE
## class(CE2) <- 'MP'

## #####@> Including more flexibility to TAC variation...
## CE3 <- ConstantExploitation
## formals(CE3)$mc <- c(0.3, 0.3)
## class(CE3) <- 'MP'

#####@> Including default - Final 01...
CE <- ConstantExploitation
class(CE) <- 'MP'

#####@> Including reduction in mc - Final 02...
## CE02 <- ConstantExploitation
## formals(CE02)$mc <- c(0.2, 0.2)
## class(CE02) <- 'MP'

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
