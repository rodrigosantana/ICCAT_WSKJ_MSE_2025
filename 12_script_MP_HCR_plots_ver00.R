########################################################################
## Description: Demonstration of the MP HCR behaviour...
##
## Maintainer: Datenkraft - ICCAT (TT MSE Sub Group)
## Author: Rodrigo Sant'Ana
## Created: seg jul 14 14:16:03 2025 (-0300)
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

######@> Loading R packages...
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

######@> Path to save Results...

#####@> MSE projections...
path01 <- "07_Results/05_MSE_Projections/"

########################################################################
######@> Constant Exploitation...

######@> HCR Structure...

#####@> Simulating data...
ind_ratio <- seq(0, 1.2, 0.01)
histER <- 0.3

#####@> HCR Ramp...
targER <- ifelse(ind_ratio >= 0.8,
                 histER,
          ifelse(ind_ratio > 0.5,
                 histER * (-1.4 + 3 * ind_ratio),
                 0.1 * histER))

#####@> Data simulated...
df <- data.frame(ind_ratio, targER)

#####@> Visualization...
p00 <- ggplot(df, aes(x = ind_ratio, y = targER)) +
    geom_line(size = 1.2) +
    geom_vline(xintercept = c(0.5, 0.8), linetype = "dashed",
               color = "gray") +
    scale_y_continuous(breaks = seq(0, 0.4, 0.05), limits = c(0, 0.4)) +
    labs(x = "Recent and historical catch ratio",
         y = "Exploitable target ratio") +
    my_theme()
p00

ggsave(paste0(path01, "Fig_CE_HCR_ver00.tiff"),
       plot = p00, device = "tiff", units = "cm", width = 30,
       height = 25, dpi = 300, bg = "white")

########################################################################
######@> Index Ratio...

######@> HCR Structure...

#####@> Simulating data...
modifier <- 1
tunepar <- 0.9
ind_ratio <- seq(0.4, 1.6, 0.01)

#####@>  Alpha without HCR...
alpha_nHCR <- (ind_ratio / modifier) * tunepar

#####@>  Alpha with HCR truncated...
alpha_HCR <- pmin(pmax(alpha_nHCR, 0.8), 1.2)

#####@> Data simulated...
df <- data.frame(
    ind_ratio = ind_ratio,
    alpha_nHCR = alpha_nHCR,
    alpha_HCR = alpha_HCR)

#####@> Visualization...
p01 <- ggplot(df, aes(x = ind_ratio)) +
    geom_line(aes(y = alpha_nHCR), color = "blue", size = 1.2,
              linetype = "dashed") +
    geom_line(aes(y = alpha_HCR), color = "red", size = 1.2) +
    geom_hline(yintercept = c(0.8, 1.2), linetype = "dotted",
               color = "gray") +
    scale_y_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 1.8)) +
    scale_x_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 1.8)) + 
    labs(x = "Recent and historical index ratio",
         y = "TAC Adjust factor (α)") +
    my_theme()
p01

ggsave(paste0(path01, "Fig_IR_HCR_ver00.tiff"),
       plot = p01, device = "tiff", units = "cm", width = 30,
       height = 25, dpi = 300, bg = "white")

########################################################################
######@> Surplus Production Models...

######@> HCR Structure...

#####@> Simulating data...
BMSYTarg <- 1.3
BMSYLim <- 0.6
delta1 <- 1
delta2 <- 0.5

#####@> Estimating parameters of penalization function...
Br <- seq(0.2, 1.5, 0.01)
a <- (delta1 - delta2) / (BMSYTarg - BMSYLim)
b <- delta2 - a * BMSYLim
pen <- ifelse(Br >= BMSYTarg, 1,
       ifelse(Br >= BMSYLim, a * Br + b, delta2))

#####@> Data simulated...
df <- data.frame(Br = Br, penalty = pen)

#####@> Visualization...
p02 <- ggplot(df, aes(x = Br, y = penalty)) +
    geom_line(size = 1.2, color = "black") +
    geom_vline(xintercept = c(BMSYLim, BMSYTarg), linetype = "dashed") +
    scale_y_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 1.2)) +
    scale_x_continuous(breaks = seq(0, 1.8, 0.2), limits = c(0, 1.8)) +
    labs(x = expression(B/B[MSY]),
         y = "Penalization factor") +
    my_theme()
p02

ggsave(paste0(path01, "Fig_SP_HCR_ver00.tiff"),
       plot = p02, device = "tiff", units = "cm", width = 30,
       height = 25, dpi = 300, bg = "white")

########################################################################
######@> Surplus Production Models with AH HCR...

######@> HCR Structure...

#####@> Simulating data...
theta1 <- 0.25
theta2 <- 0
BMSYTarg <- 1.3
tunepar <- 0.6

#####@> Estimating parameters of penalization function...
Br <- seq(0.2, 2.2, by = 0.01)
Vt <- (theta1 * ((Br / BMSYTarg) - 1)^3) +
    (theta2 * (Br / (BMSYTarg - 1)))
delta <- 1 + (Vt * tunepar)

#####@> Data simulated...
df <- data.frame(B_BMSY = Br, Delta = delta)

#####@> Visualization...
p03 <- ggplot(df, aes(x = B_BMSY, y = Delta)) +
    geom_line(size = 1.2, color = "darkgreen") +
    geom_vline(xintercept = BMSYTarg, linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = 1, linetype = "dotted", color = "black") +
    scale_y_continuous(breaks = seq(0, 1.8, 0.05), limits = c(0.9, 1.1)) +
    scale_x_continuous(breaks = seq(0, 2.2, 0.2), limits = c(0, 2.2)) +
    labs(x = expression(B/B[MSY]),
         y = expression("Relative effort modifier * " * F[t])) +
    my_theme()
p03

ggsave(paste0(path01, "Fig_SPAH_HCR_ver00.tiff"),
       plot = p03, device = "tiff", units = "cm", width = 30,
       height = 25, dpi = 300, bg = "white")


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
