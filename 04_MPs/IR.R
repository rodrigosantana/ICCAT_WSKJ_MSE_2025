########################################################################
## Description: Index Ratio Management Procedure - WSKJ MSE...
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
######@> Index Ratio Management Procedure...

######@> Index Ratio MPs...
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
IndexRatio <- function(x,
                       Data,
                       Data_Lag = 1,
                       Interval = 3,
                       Initial_MP_Yr = 2026,
                       reps = 1,
                       tunepar = 0.9,
                       doSmoother = FALSE,
                       modifier = 1,
                       mc = c(0.25, 0.25),
                       yrs = c(3, 3),
                       refYear = 2020,
                       incHCR = FALSE,
                       ...) {
    Rec <- new("Rec")
    ## Check if TAC needs to be updated...
    if (SameTAC(Initial_MP_Yr, Interval, Data)) {
        Rec@TAC <- Data@MPrec[x]
        Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
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
    ## Historical data...
    yr.ind <- which(Data@Year == refYear)
    hist.yrs <- (yr.ind - yrs[1] + 1):yr.ind
    I.hist <- mean(Data@Ind[x, hist.yrs], na.rm = TRUE) * modifier
    ## Current data...
    yr.ind <- which.max(Data@Year)
    curr.yrs <- (yr.ind - yrs[2]+ 1):yr.ind
    I.curr <- mean(Data@Ind[x, curr.yrs], na.rm = TRUE)
    ## Alpha...
    alpha <- I.curr / I.hist  * tunepar
    ## Catch data...
    Cat <- mean(Data@Cat[x, curr.yrs])
    ## Harvest control rule...
    if (incHCR) {
        if(alpha > 1.2) {
            alpha <- 1.2
        } else if(alpha < 0.8) {
            alpha <- 0.8
        } else {
            alpha <- alpha
        }
    }
    TAC <- alpha  * Cat
    ## Return TAC...
    ## Rec@TAC <- TAC
    Rec@TAC <- adjust_TAC(TAC, Data@MPrec[x], mc)
    Rec
}

######@> Defining variations of index ratio MP...

#####@> As defined in original structure...
IR1 <- IndexRatio
class(IR1) <- 'MP'

#####@> Reduce the flexibility in TAC variations...
IR2 <- IndexRatio
formals(IR2)$mc <- c(0.2, 0.2)
class(IR2) <- 'MP'

#####@> Reduce even more the flexibility in TAC variations...
IR3 <- IndexRatio
formals(IR3)$mc <- c(0.15, 0.15)
class(IR3) <- 'MP'

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
