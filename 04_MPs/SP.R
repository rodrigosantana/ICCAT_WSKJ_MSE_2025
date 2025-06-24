########################################################################
## Description: Surplus Production Model Management Procedure - WSKJ
## MSE...
##
## Maintainer: Datenkraft - ICCAT (Tropical Tunas Species Group)
## Author: Rodrigo Sant'Ana & Adrian Hordyk
## Powered by: Adrian Hordyk
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
## - doSmoother: logical; include smoother?
## - refYear: reference year for historical index
## - mc: max change in TAC between management cycles: c(MaxDecrease,
## MaxIncrease); NA to ignore
## - useHCR: include harvest control rule; logical
SurplusProductionStockStatus <- function(x, Data,
                                         Data_Lag = 1,
                                         Interval = 3,
                                         Initial_MP_Yr = 2026,
                                         reps = 1,
                                         doSmoother = FALSE,
                                         tunepar = 0.6,
                                         mc = c(0.25, 0.25),
                                         useHCR = TRUE,
                                         BMSYTarg = 1.3,
                                         BMSYLim = 0.6,
                                         FMSYTarg = 0.8,
                                         delta1 = 1,
                                         delta2 = 0.5,
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
    ## Estimated Stock Status from SP
    SPrun <- SAMtool::SP(x, Data,
                         prior = list(r = c(0.416, 0.148),
                                      MSY = c(30000, 0.2)),
                         start = list(dep = 0.98, n = 1))
    EstF_FMSY <- tail(SPrun@F_FMSY, 1)
    EstB_BMSY <- SPrun@B_BMSY
    prevER <- tail(Data@Cat[x,], 1) / tail(EstB_BMSY, 2)[1]
    newER <- prevER * (1 + (FMSYTarg - EstF_FMSY)) * tunepar
    EstSS <-  tail(EstB_BMSY, 1)
    Bratio <- EstSS/BMSYTarg
    if (useHCR) {
        if (EstSS>=BMSYLim & EstSS<BMSYTarg) {
            a <- (delta1-delta2)/(BMSYTarg-BMSYLim)
            b <- delta2 -a*BMSYLim
            newER <- newER * a*Bratio+b
            ## Bratio <- seq(BMSYLim, BMSYTarg, by=0.1)
            ## plot(Bratio, a*Bratio+b, type='l', ylim=c(0,1))
        } else if (EstSS<BMSYLim){
            newER <- newER * delta2
        }
    }
    TAC <- newER * tail(EstB_BMSY, 1)[1]
    ## Maximum allowed change in TAC
    TAC <- adjust_TAC(TAC, Data@MPrec[x], mc)
    Rec@TAC <- TAC
    Rec
}

######@> Defining variations of Surplus Production Model MP...

#####@> As defining on the original MP...
SP_01 <- SurplusProductionStockStatus
class(SP_01) <- 'MP'

#####@> Changing the FMSY Target to 0.6...
SP_02 <- SP_01
formals(SP_02)$FMSYTarg <- 0.6
class(SP_02) <- 'MP'

#####@> Changing the FMSY Target to 1...
SP_03 <- SP_01
formals(SP_03)$FMSYTarg <- 1
class(SP_03) <- 'MP'

#####@> Changing the BMSY Target and BMSY Limit...
SP_04 <- SP_01
formals(SP_04)$BMSYTarg <- 1.6
formals(SP_04)$BMSYLim <- 0.8
class(SP_04) <- 'MP'

#####@> Allowing more flexibility in TAC Variation...
SP_05 <- SP_01
formals(SP_05)$mc <- c(0.4, 0.4)
class(SP_05) <- 'MP'

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
