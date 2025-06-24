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
######@> Surplus Production Models with Adrian's HCR Management 
######@> Procedure...

######@> Surplus Production Models with Adrian's HCR MPs...
## Arguments:
## - tunepar: tuning value for reference index
## - doSmoother: logical; include smoother?
## - refYear: reference year for historical index
## - mc: max change in TAC between management cycles: c(MaxDecrease,
## MaxIncrease); NA to ignore
## - useHCR: include harvest control rule; logical
## - BMSYTarg
## - theta1
## - theta2
SurplusProductionHCRAH <- function(x, Data,
                                   Data_Lag = 1,
                                   Interval = 3,
                                   Initial_MP_Yr = 2026,
                                   reps = 1,
                                   doSmoother = FALSE,
                                   tunepar = 0.6,
                                   mc = c(0.25, 0.25),
                                   BMSYTarg = 1.3,
                                   theta1 = 0.25,
                                   theta2 = 0,
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
    ## References...
    Bcurr <- SPrun@B[length(SPrun@B)]
    EstB_BMSY <- SPrun@B_BMSY
    EstSS <-  tail(EstB_BMSY, 2) |> mean()
    Fest <- SPrun@Catch[length(SPrun@Catch)] / Bcurr
    ## HCR Ramp...
    Vt <- (theta1 * ((EstSS/BMSYTarg) - 1)^3) +
        (theta2 * (EstSS/(BMSYTarg - 1)))
    delta <- 1 + (Vt * tunepar)
    Fmort <- as.numeric(delta * Fest)
    TAC <- Fmort * Bcurr
    ## Maximum allowed change in TAC
    TAC <- adjust_TAC(TAC, Data@MPrec[x], mc)
    Rec@TAC <- TAC
    Rec
}

######@> Defining variations of Surplus Production Model MP...

#####@> As defining on the original MP...
SPAH_01 <- SurplusProductionHCRAH
class(SPAH_01) <- 'MP'

#####@> Changing the BMSY Target to 1...
SPAH_02 <- SPAH_01
formals(SPAH_02)$BMSYTarg <- 1
class(SPAH_02) <- 'MP'

#####@> Changing the BMSY Target to 0.9...
SPAH_03 <- SPAH_01
formals(SPAH_03)$BMSYTarg <- 0.9
class(SPAH_03) <- 'MP'

#####@> Changing the BMSY Target to 1.2...
SPAH_04 <- SPAH_01
formals(SPAH_04)$BMSYTarg <- 1.2
class(SPAH_04) <- 'MP'

#####@> Allowing less flexibility in TAC Variation...
SPAH_05 <- SPAH_01
formals(SPAH_05)$mc <- c(0.2, 0.2)
class(SPAH_05) <- 'MP'

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
