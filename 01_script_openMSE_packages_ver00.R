########################################################################
## Description: R code for the install or update the OpenMSE
## framework...
##
## Maintainer: Datenkraft - ICCAT (Tropical Tunas Species Group)
## Author: Rodrigo Sant'Ana
## Created: qua jun 18 10:29:49 2025 (-0300)
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
######@> Loading some accessory packages...

######@> List of packages...
library(dplyr)
library(tidyr)

########################################################################
######@> Preparing the OpenMSE framework environment...

######@> Looking for what have already installed...
installed <- pak::lib_status() %>%
    filter(package %in% c("MSEtool", "SAMtool", "openMSE", "r4ss")) %>%
    select(package, version, title, depends) %>%
    as.data.frame()

######@> Looking for upgrades in the listed packages...

#####@> MSEtool (3.7.9999)... Github Development Version...
pak::pkg_install("blue-matter/MSEtool", ask = FALSE)

#####@> SAMtool (1.8.1)...
pak::pkg_install("blue-matter/SAMtool", ask = FALSE)

#####@> openMSE (1.0.1)...
pak::pkg_install("blue-matter/openMSE", ask = FALSE)

#####@> r4ss (1.52.0)...
pak::pkg_install("r4ss/r4ss", ask = FALSE)

######@> Looking for the new versions installed...
new_installed <- pak::lib_status() %>%
    filter(package %in% c("MSEtool", "SAMtool", "openMSE", "r4ss")) %>%
    select(package, version, title, depends) %>%
    as.data.frame()

######@> Comparing old and new...
output <- installed %>%
    left_join(new_installed, by = c("package", "title", "depends")) %>%
    select(package, "old_version" = version.x,
           "new_version" = version.y, title, depends)

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
