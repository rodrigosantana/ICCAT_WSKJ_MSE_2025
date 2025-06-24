########################################################################
## Description: Functions and data preparation to be used in Management
## Procedures designed for the WSKJ MSE...
##
## Maintainer: Datenkraft - ICCAT (Tropical Tuna Species Group)
## Author: Rodrigo Sant'Ana & Adrian Hordyk
##
## Created: seg jun 23 11:08:36 2025 (-0300)
## Version: 
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
######@> Functions to be used in the MPs...

######@> Fixed and Same TAC Functions - copied from SWOMSE R package...
FixedTAC <- function(Rec, Data) {
    df <- Catchdf
    if ((max(Data@Year) + 1) %in% df$Year) {
        ind <- match(max(Data@Year) + 1, df$Year)
        Rec@TAC <- df$Catch[ind]
    }
    Rec
}

SameTAC <- function (Initial_MP_Yr, Interval, Data) {
    if (max(Data@Year) < (Initial_MP_Yr - 1))
        return(TRUE)
    Imp_Years <- seq(Initial_MP_Yr, by = Interval, length.out = 30)
    if (!(max(Data@Year) + 1) %in% Imp_Years)
        return(TRUE)
    FALSE
}

######@> Function to adjust symetric, asymetric or not adjust the TAC...
adjust_TAC <- function(TAC, Last_TAC, mc) {
    delta_TAC <- TAC / Last_TAC
    if(any(is.na(mc))) {
        return(TAC)
    }
    lower_bound <- 1 - ifelse(length(mc) == 2, mc[1], mc)
    upper_bound <- 1 + ifelse(length(mc) == 2, mc[2], mc)
    delta_TAC <- pmax(pmin(delta_TAC, upper_bound), lower_bound)
    return(Last_TAC * delta_TAC)
}

######@> Function to adjust TAC for model based MPs...
adjust_TAC2 <- function(TAC, Last_TAC, mc, B_rel) {
    delta_TAC <- TAC / Last_TAC
    if(any(is.na(mc))) {
        return(TAC)
    }
    lower_bound <- if (length(mc) == 2) 1 - mc[1] else 1 - mc
    upper_bound <- if (length(mc) == 2) 1 + mc[2] else 1 + mc
    if(B_rel > 1) {
        delta_TAC <- pmax(pmin(delta_TAC, upper_bound), lower_bound)
    } else {
        delta_TAC <- pmin(delta_TAC, upper_bound)
    }
    return(Last_TAC * delta_TAC)
}

########################################################################
######@> Preparing Catch Data for 2025...

######@> Loading WSKJ Pre-prepared Data...
WSKJ_Data <- readRDS("01_Data/WSKJ_Data.rda")

######@> Assuming 2025 catch is mean of last 3 years...
Assumed2025Catch <- tail(WSKJ_Data@Cat[1, ], 3) |>
    mean()

#####@> Defining the Catchdf dataset to be included in the MPs...
Catchdf <- data.frame(Year = 2025, Catch = Assumed2025Catch)

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
