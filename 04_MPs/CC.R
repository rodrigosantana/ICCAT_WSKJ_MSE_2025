########################################################################
## Description: Constant Catch Management Procedure - WSKJ MSE...
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
######@> Constant Catch Generic Management Procedure...

######@> Constant Catch MPs...
ConstantCatchGeneric <- function(x, Data, Interval = 3,
                                 Initial_MP_Yr = 2026, TAC = 4E4, ...) {
    Rec <- new("Rec")
    if (SameTAC(Initial_MP_Yr, Interval, Data)) {
        Rec@TAC <- Data@MPrec[x]
        Rec <- FixedTAC(Rec, Data)
        return(Rec)
    }
    Rec@TAC <-TAC
    Rec
}

######@> Defining variations of constant catches...
CC_40kt <- ConstantCatchGeneric
formals(CC_40kt)$TAC <- 4E4
class(CC_40kt) <- 'MP'

CC_30kt <- ConstantCatchGeneric
formals(CC_30kt)$TAC <- 3E4
class(CC_30kt) <- 'MP'

CC_20kt <- ConstantCatchGeneric
formals(CC_20kt)$TAC <- 2E4
class(CC_20kt) <- 'MP'

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
