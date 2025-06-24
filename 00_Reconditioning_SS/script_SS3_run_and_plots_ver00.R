########################################################################
## Description: Code for Stock Synthesis Plots...
##
## Maintainer: UNIVALI / EP / LEMA - ICCAT - Sharks group...
## Author: Rodrigo Sant'Ana
## Created: seg jun  9 16:58:26 2025 (-0300)
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

######@> Loading libraries...
library(r4ss)
library(ss3diags)
library(future)
library(future.apply)

######@> Path to folder...
path <- "../WSKJ_Reconditioning_SA"

######@> List folders...
dirs <- list.dirs(path, recursive = FALSE)[grepl("WSKJ",
                                                 list.dirs(path,
                                                           recursive =
                                                               FALSE))]

########################################################################
######@> Running SS3 inside R...

######@> Function to run SS3 directly from R...
do_ss3 <- function(SSdir, doPlots = TRUE) {
    origem <- getwd()
    setwd(SSdir)
    system("./ss3_linux .")
    if(doPlots) {
        check <- dir.exists("plots")
        if(check) {
            system("rm -rf plots")
        }
        SS_plots(SS_output(getwd(), printstats = TRUE, covar = TRUE,
                           forecast = TRUE, covarfile = "covar.sso"))
    }
    setwd(origem)
}

######@> Running SS3 - Single...
do_ss3(dirs[1], doPlots = TRUE)

######@> Running SS3 - Future parallel...

#####@> Setting the number of threads...
plan(multisession, workers = 9)

#####@> Running...
system.time(
    future_lapply(dirs, do_ss3, future.seed = TRUE)
)

#####@> Stoping workers...
plan(sequential)

########################################################################
######@> Running the Diagnostics...

######@> Function to run Diags in parallel...
run_retro_diagnostics <- function(dirs) {
    model_dir <- file.path(getwd(), dirs)
    retro_dir <- file.path(model_dir, "retrospectives")
    ## Rodar retrospectiva
    r4ss::retro(dir = model_dir,
                oldsubdir = "",
                newsubdir = "retrospectives",
                years = 0:-5,
                exe = "ss3_linux",
                verbose = FALSE,
                overwrite = TRUE)
    ## Carregar os modelos retro
    retro_models <- r4ss::SSgetoutput(
                              dirvec = file.path(retro_dir,
                                                 paste0("retro", 0:-5)))
    ## Resumir
    retro_summary <- r4ss::SSsummarize(retro_models)
    endyrvec <- retro_summary$endyrs + 0:-5
    ## Plots - Comparação
    r4ss::SSplotComparisons(retro_summary,
                            plotdir = retro_dir,
                            png = TRUE,
                            endyrvec = endyrvec,
                            legendlabels = paste("Data", 0:-5, "years"))
    r4ss::SSplotComparisons(retro_summary,
                            subplot = c(3, 4),
                            sprtarg = FALSE, btarg = FALSE, minbthresh = FALSE,
                            labels = c("Year", "", "B/Bmsy"),
                            plotdir = retro_dir,
                            png = TRUE,
                            endyrvec = endyrvec,
                            legendlabels = paste("", 0:-5, ""))
    r4ss::SSplotComparisons(retro_summary,
                            subplot = 8,
                            sprtarg = FALSE, btarg = FALSE, minbthresh = FALSE,
                            labels = c("Year", "Spawning biomass (t)",
                                       "Fraction of unfished",
                                       "Age-0 recruits (1,000s)",
                                       "Recruitment deviations",
                                       "Index", "Log index",
                                       "SPR-related quantity",
                                       "Density", 
                                       "Management target",
                                       "Minimum stock size threshold",
                                       "Spawning output",
                                       "Harvest rate"),
                            plotdir = retro_dir,
                            png = FALSE,
                            endyrvec = endyrvec,
                            legendlabels = paste("", 0:-5, ""))
    ## Diagnósticos
    diag_dir <- file.path(model_dir, "Plotdiags")
    dir.create(diag_dir, showWarnings = FALSE)
    ss_out <- r4ss::SS_output(dir = model_dir, printstats = FALSE,
                              covar = TRUE, forecast = TRUE,
                              covarfile = "covar.sso")
    hccomps <- ss3diags::SSretroComps(retro_models)
    hc <- ss3diags::SSmase(retro_summary)
    retro_b <- ss3diags::SShcbias(retro_summary)
    ## Plots de testes de aderência
    jpeg(file.path(diag_dir, "Len_runs_teste_Index_.jpg"), width = 14,
         height = 12, res = 600, units = "in")
    r4ss::sspar(mfrow = c(3, 2), labs = TRUE, plot.cex = 0.9)
    ss3diags::SSplotRunstest(ss_out, add = TRUE, tickEndYr = FALSE)
    dev.off()
    jpeg(file.path(diag_dir, "Len_runs_teste_Length_.jpg"), width = 14,
         height = 12, res = 600, units = "in")
    r4ss::sspar(mfrow = c(3, 2), labs = TRUE, plot.cex = 0.9)
    ss3diags::SSplotRunstest(ss_out, subplots = "len", add = TRUE,
                             tickEndYr = FALSE)
    dev.off()
    ## Residuals
    jpeg(file.path(diag_dir, "Residuals_.jpg"), width = 14, height = 12,
         res = 600, units = "in")
    r4ss::sspar(mfrow = c(1, 2), labs = TRUE, plot.cex = 0.9)
    ss3diags::SSplotJABBAres(ss_out, add = TRUE)
    ss3diags::SSplotJABBAres(ss_out, subplots = "len", add = TRUE)
    dev.off()
    ## Retroplot com forecast
    jpeg(file.path(diag_dir, "Retro_.jpg"), width = 14, height = 10,
         res = 600, units = "in")
    r4ss::sspar(mfrow = c(1, 2), labs = FALSE, plot.cex = 0.8)
    ss3diags::SSplotRetro(retro_summary, add = TRUE,
                          uncertainty = TRUE, showrho = TRUE,
                          forecast = TRUE, labels = "SSB (t)")
    ss3diags::SSplotRetro(retro_summary, subplots = "F", add = TRUE,
                          legendcex = 0.8, forecast = TRUE, labels = "F")
    dev.off()
    ## Hindcast validation
    jpeg(file.path(diag_dir, "Hcxval_Index_.jpg"), width = 14,
         height = 10, res = 600, units = "in")
    r4ss::sspar(mfrow = c(2, 2), labs = TRUE, plot.cex = 0.6)
    ss3diags::SSplotHCxval(retro_summary, add = TRUE)
    dev.off()
    jpeg(file.path(diag_dir, "Hcxval_Length_.jpg"), width = 14,
         height = 10, res = 600, units = "in")
    r4ss::sspar(mfrow = c(2, 2), labs = TRUE, plot.cex = 0.6)
    ss3diags::SSplotHCxval(hccomps, subplots = "len", add = TRUE)
    dev.off()
    ## MVLN Uncertainty
    ## starter <- SSsettingsBratioF(ss_out)
    r4ss::sspar(mfrow = c(1, 1), plot.cex = 0.9)
    mvn <- SSdeltaMVLN(ss_out, plot = FALSE, Fref = "MSY", mc = 50000)
    ## dev.print(jpeg, file.path(diag_dir, "Kobe_.jpg"), width = 14,
    ##           height = 10, res = 600, units = "in")
    jpeg(file.path(diag_dir, "MVLN_Traj_.jpg"), width = 14,
         height = 10, res = 600, units = "in")
    r4ss::sspar(mfrow = c(3, 2), plot.cex = 0.9)
    ss3diags::SSplotEnsemble(mvn$kb, ylabs = mvn$labels, add = TRUE,
                             legend = FALSE)
    dev.off()
    jpeg(file.path(diag_dir, "MVLN_Traj_SSBmsy_.jpg"), width = 14,
         height = 10, res = 600, units = "in")
    r4ss::sspar(mfrow = c(1, 1), plot.cex = 0.9)
    ss3diags::SSplotEnsemble(mvn$kb, subplots = c("stock"))
    dev.off()
    return(invisible(NULL))
}

#####@> Setting the number of threads...
plan(multisession, workers = 9)

#####@> Running...
system.time(
    future_lapply(dirs, run_retro_diagnostics, future.seed = TRUE)
)

#####@> Stoping workers...
## nbrOfWorkers()
plan(sequential)

######@> Re-run WSKJ_EstRec93_Qnt75_h7...
run_retro_diagnostics(dirs[8])

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
