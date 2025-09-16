########################################################################
## Description: Management Procedure Tuning Process implemented to find
## each MP to achieve the 60% PGK  in the middle term...
##
## Maintainer: Datenkraft - ICCAT (Tropical Tunas Species Group)
## Author: Rodrigo Sant'Ana & Adrian Hordyk
## Created: ter jun 24 09:25:02 2025 (-0300)
## Version: 0.0.1
##
## URL:
## Doc URL:
##
## Database info:
##
### Commentary:
## https://github.com/Blue-Matter/ClimateTest/
## https://github.com/Blue-Matter/Blue_Shark_MSE/
##
### Code:
########################################################################

########################################################################
######@> Setup R...

######@> Update openMSE R packages (This will update openMSE packages if
######@> there are updates on GitHub)...
source("01_script_openMSE_packages_ver00.R")

######@> Loading R packages...
library(openMSE)
library(tidyverse)
library(snowfall)
library(abind)
library(future)
library(future.apply)
library(patchwork)
library(progressr)

######@> Defining path to Historical Simulated Data...
path01 <- "03_Hists/"

######@> ggplot theme...
extrafont::loadfonts(device = "postscript")
rgb01 <- "black"
rgb02 <- "black"
seta <- grid::arrow(length = grid::unit(0.2, "cm"), type = "open")
seta2 <- grid::arrow(length = grid::unit(0.2, "cm"), type = "open",
                     ends = "both")
my_theme <- function(base_size = 20, base_family = "Helvetica") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(axis.ticks = element_line(colour = rgb01),
              axis.line = element_line(colour = rgb01, linewidth = 0.2),
              axis.text = element_text(colour = rgb02, size = 20),
              axis.title = element_text(size = 20),
              strip.text = element_text(size = 20,
                                        margin = ggplot2::margin(0.3,
                                                                 0.3,
                                                                 0.3,
                                                                 0.3,
                                                                 "cm"),
                                        face = "bold"),
              legend.background = element_blank(),
              legend.key = element_blank(),
              legend.text = element_text(size = 18),
              legend.title = element_text(size = 20),
              panel.background = element_blank(),
              panel.grid = element_line(linetype = "solid",
                                        linewidth = 0.2,
                                        colour = "gray90"),
              plot.background = element_blank(),
              complete = TRUE)
}

######@> Progress bar setup...
handlers(global = TRUE)
handlers("txtprogressbar")

########################################################################
######@> Loading data...

######@> Historical Simulated Data...
HistList <- readRDS(paste0(path01, "HistList.rda"))

######@> Management Procedure...

#####@>  Source pre-tuned MPs...

####@> Remove MPs from global enviroment...
suppressWarnings(rm(list = avail("MP")))

####@> Load MPs into global environment...
for (fl in list.files("04_MPs", full.names = TRUE)) {
    source(fl)
}

####@> Function to get MP names...
getMPNames <- function() {
    ls(envir = .GlobalEnv)[vapply(ls(envir = .GlobalEnv),
                                  MSEtool:::getclass, logical(1),
                                  classy = 'MP')]
}

###@> Loading MPs developed to this MSE...
AllMPs <- getMPNames()

########################################################################
######@> Defining a function to optimize the Tune process...

######@> Vectorized metric functions...

#####@> PGK...
.PGK_vec <- function(MSE_list, years_idx = 2:31) {
    SB_list <- lapply(MSE_list, \(X) X@SB_SBMSY[ , , years_idx,
                                                drop = FALSE])
    FF_list <- lapply(MSE_list, \(X) X@F_FMSY[ , , years_idx,
                                              drop = FALSE])
    SB <- aperm(abind::abind(SB_list, along = 1), c(1, 2, 3))
    FF <- aperm(abind::abind(FF_list, along = 1), c(1, 2, 3))
    good <- (SB > 1) & (FF < 1)
    dim(good) <- c(dim(good)[1L]*dim(good)[2L], dim(good)[3L])
    mean(rowMeans(good, na.rm = TRUE), na.rm = TRUE)
}

#####@> LRP...
.LRP_any_vec <- function(MSE_list, years_idx = 2:31, thr = 0.4) {
    SB_list <- lapply(MSE_list, \(X) X@SB_SBMSY[ , , years_idx,
                                                drop = FALSE])
    SB <- aperm(abind::abind(SB_list, along = 1), c(1, 2, 3))
    below <- SB < thr
    dim(below) <- c(dim(below)[1L]*dim(below)[2L], dim(below)[3L])
    mean(below, na.rm = TRUE)
    ## mean(apply(below, 1L, any, na.rm = TRUE), na.rm = TRUE)
}

#####@> TAC...
.TAC_mean <- function(MSE_list) {
    tac_list <- lapply(MSE_list, \(X) X@TAC)
    TAC <- aperm(abind::abind(tac_list, along = 1), c(1, 2, 3))
    dim(TAC) <- c(dim(TAC)[1L]*dim(TAC)[2L], dim(TAC)[3L])
    mean(rowMeans(TAC, na.rm = TRUE), na.rm = TRUE)
}

######@> Compute all metrics...
.compute_metrics <- function(MSE_list, years_idx = 2:31, thr = 0.4) {
    PGKw <- .PGK_vec(MSE_list, years_idx = years_idx)
    LRPw <- .LRP_any_vec(MSE_list, years_idx = years_idx, thr = thr)
    TACw <- .TAC_mean(MSE_list)
    list(PGKw = PGKw, LRPw = LRPw, TACw = TACw)
}

######@> Check Project function...
.resolve_Project <- local({
    fun <- NULL
    function() {
        if (!is.null(fun) && is.function(fun)) return(fun)
        if (exists("Project", mode = "function", inherits = TRUE)) {
            fun <<- get("Project")
            return(fun)
        }
        for (pkg in c("MSEtool", "openMSE", "DLMtool")) {
            if (requireNamespace(pkg, quietly = TRUE)) {
                ns <- asNamespace(pkg)
                if (exists("Project", envir = ns, inherits = FALSE)) {
                    fun <<- get("Project", envir = ns)
                    return(fun)
                }
            }
        }
        stop("Function 'Project' not found.")
    }
})

######@> Objective function...
.make_objective <- function(mode = c("PGK","LRP","BOTH"),
                            targets = list(PGK = 0.60, LRP = 0.10),
                            weights = list(PGK = 1, LRP = 1),
                            years_idx = 2:31,
                            thr = 0.4,
                            digits_print = 5,
                            verbose = TRUE,
                            show_tunepar = TRUE,
                            par_label = "tunepar",
                            digits_par = 6) {
    mode <- match.arg(mode)
    function(MSE_list, tunepar = NULL) {
        met  <- .compute_metrics(MSE_list, years_idx = years_idx,
                                 thr = thr)
        PGKw <- round(met$PGKw, digits_print)
        LRPw <- round(met$LRPw, digits_print)
        ssq <- switch(mode,
                      "PGK" = (PGKw - targets$PGK)^2,
                      "LRP" = (LRPw - targets$LRP)^2,
                      "BOTH" = weights$PGK*(PGKw-targets$PGK)^2 +
                           weights$LRP*(LRPw-targets$LRP)^2)
        if (isTRUE(verbose)) {
            cat("*************************\n")
            cat("Mode = ", mode, "\n", sep = "")
            if (isTRUE(show_tunepar) && !is.null(tunepar)) {
                cat(par_label, " = ", signif(tunepar, digits_par), "\n",
                    sep = "")
            }
            cat("PGKw = ", PGKw, " (target=", targets$PGK, ")\n",
                sep = "")
            cat("LRPw (ever-risk) = ", LRPw, " [thr<", thr,
                "] (target=", targets$LRP, ")\n", sep = "")
            if (mode == "BOTH")
                cat("Weights: PGK=", weights$PGK, ", LRP=",
                    weights$LRP, "\n", sep = "")
            cat("SSQ = ", round(ssq, 8), "\n")
            cat("*************************\n\n")
        }
        list(TACw = met$TACw, PGKw = PGKw, LRPw = LRPw, ssq = ssq)
    }
}

######@> Function to register MP as temporary function...
.register_temp_mp <- function(MP_fun, parname, par_value,
                              extra_formals = list()) {
    MPtmp <- MP_fun
    fml <- formals(MPtmp)
    if (!parname %in% names(fml)) stop("Parâmetro '", parname,
                                       "' não existe no MP.")
    fml[[parname]] <- par_value
    for (nm in names(extra_formals)) {
        if (nm %in% names(fml)) fml[[nm]] <- extra_formals[[nm]]
    }
    formals(MPtmp) <- fml
    class(MPtmp) <- "MP"
    tmp_name <- paste0("MPtmp_", as.integer(runif(1, 1e6, 9e6)))
    assign(tmp_name, MPtmp, envir = .GlobalEnv)
    tmp_name
}

######@> Projection function with progress bar...
.project_histories_seq <- function(Hist_list,
                                   mp_name,
                                   show_progress = TRUE,
                                   parallel = TRUE,
                                   sf_cpus = max(1,
                                                 parallel::detectCores()-1),
                                   sf_type = "SOCK",
                                   sf_pkgs = c("MSEtool", "openMSE",
                                               "DLMtool"),
                                   sf_export = NULL) {
    n <- length(Hist_list)
    if (!isTRUE(parallel) || n <= 1L || sf_cpus <= 1L) {
        ProjectFun <- .resolve_Project()
        if (isTRUE(show_progress)) {
            pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
            on.exit(try(close(pb), silent = TRUE), add = TRUE)
        }
        res <- vector("list", n)
        for (i in seq_len(n)) {
            res[[i]] <- ProjectFun(Hist_list[[i]], MPs = mp_name)
            if (isTRUE(show_progress)) utils::setTxtProgressBar(pb, i)
        }
        return(res)
    }
    if (!requireNamespace("snowfall", quietly = TRUE)) {
        stop("Package snowfall not installed!")
    }
    snowfall::sfInit(parallel = TRUE, cpus = sf_cpus, type = sf_type)
    on.exit(try(snowfall::sfStop(), silent = TRUE), add = TRUE)
    invisible(lapply(sf_pkgs, function(pkg)
        try(snowfall::sfLibrary(pkg, character.only = TRUE), silent = TRUE)))
    if (!is.null(sf_export)) {
        miss <- sf_export[!vapply(sf_export, exists, logical(1),
                                  envir = .GlobalEnv, inherits = TRUE)]
        if (length(miss)) stop("Objects not founded in .GlobalEnv: ",
                               paste(miss, collapse = ", "))
        snowfall::sfExport(list = sf_export)
    }
    if (!exists(mp_name, envir = .GlobalEnv, inherits = FALSE)) {
        stop("MP '", mp_name, "' not founded in .GlobalEnv.")
    }
    mp_obj <- get(mp_name, envir = .GlobalEnv)
    snowfall::sfExport("mp_name", "mp_obj")
    sf_project_runner <- function(H) {
        nm <- get("mp_name", envir = .GlobalEnv)
        mp <- get("mp_obj",  envir = .GlobalEnv)
        assign(nm, mp, envir = .GlobalEnv)
        on.exit(try(rm(list = nm, envir = .GlobalEnv), silent = TRUE),
                add = TRUE)
        PF <- if (exists("Project", mode = "function",
                         inherits = TRUE)) {
                  get("Project")
              } else if (requireNamespace("MSEtool", quietly = TRUE)) {
                  get("Project", envir = asNamespace("MSEtool"))
              } else if (requireNamespace("openMSE", quietly = TRUE)) {
                  get("Project", envir = asNamespace("openMSE"))
              } else if (requireNamespace("DLMtool", quietly = TRUE)) {
                  get("Project", envir = asNamespace("DLMtool"))
              } else stop("Project() não encontrado no worker.")
        PF(H, MPs = nm)
    }
    environment(sf_project_runner) <- baseenv()
    snowfall::sfExport("sf_project_runner")
    res <- snowfall::sfLapply(Hist_list, sf_project_runner)
    res
}

######@> Optimization function with log and checkpoint...
tune_MPRS <- function(Hist_list,
                      MP,
                      MP_parname,
                      interval,
                      objective_fun,
                      years_idx = 2:31,
                      tol = 1e-2,
                      extra_formals = list(),
                      log_csv = NULL,
                      checkpoint_rds = NULL,
                      max_evals = Inf,
                      verbose = TRUE,
                      parallel = FALSE,
                      sf_cpus = max(1, parallel::detectCores()-1),
                      sf_type = "SOCK",
                      sf_pkgs = c("MSEtool", "openMSE", "DLMtool"),
                      sf_export = NULL,
                      sf_quiet = TRUE,
                      sf_batch_size = NULL,
                      mp_label = NULL) {
    if (is.null(mp_label)) {
        mp_label <- if (is.character(MP)) MP else "MP"
    }
    MP_fun <- if (is.character(MP)) get(MP, envir = .GlobalEnv) else MP
    results <- list(); eval_count <- 0L
    best <- list(ssq = Inf, par = NA_real_)
    if (isTRUE(parallel)) {
        if (!requireNamespace("snowfall", quietly = TRUE))
            stop("Package snowfall not installed!")
        quiet <- function(expr) invisible(
                                    utils::capture.output(
                                               suppressMessages(
                                                   suppressWarnings(expr))))
        if (isTRUE(sf_quiet)) {
            quiet(snowfall::sfInit(parallel = TRUE, cpus = sf_cpus,
                                   type = sf_type))
        } else {
            snowfall::sfInit(parallel = TRUE, cpus = sf_cpus,
                             type = sf_type)
        }
        on.exit(try(snowfall::sfStop(), silent = TRUE), add = TRUE)
        for (pkg in sf_pkgs) {
            quiet(try(snowfall::sfLibrary(pkg,
                                          character.only = TRUE),
                      silent = TRUE))
        }
        if (!is.null(sf_export) && length(sf_export)) {
            miss <- sf_export[!vapply(sf_export, exists, logical(1),
                                      envir = .GlobalEnv,
                                      inherits = TRUE)]
            if (length(miss))
                stop("Objects exported not founded in .GlobalEnv: ",
                     paste(miss, collapse = ", "))
            quiet(snowfall::sfExport(list = sf_export))
        }
        sf_project_runner <- function(H) {
            nm <- get("mp_name", envir = .GlobalEnv)
            mp <- get("mp_obj",  envir = .GlobalEnv)
            assign(nm, mp, envir = .GlobalEnv)
            on.exit(try(rm(list = nm, envir = .GlobalEnv),
                        silent = TRUE), add = TRUE)
            PF <- if (exists("Project", mode = "function",
                             inherits = TRUE)) {
                      get("Project")
                  } else if (requireNamespace("MSEtool",
                                              quietly = TRUE)) {
                      get("Project", envir = asNamespace("MSEtool"))
                  } else if (requireNamespace("openMSE",
                                              quietly = TRUE)) {
                      get("Project", envir = asNamespace("openMSE"))
                  } else if (requireNamespace("DLMtool",
                                              quietly = TRUE)) {
                      get("Project", envir = asNamespace("DLMtool"))
                  } else stop("Project() not founded.")
            PF(H, MPs = nm)
        }
        environment(sf_project_runner) <- baseenv()
        quiet(snowfall::sfExport("sf_project_runner"))
    }
    .sf_project_with_progress <- function(Hist_list,
                                          show_progress = TRUE) {
        n <- length(Hist_list)
        bs <- sf_batch_size
        if (is.null(bs) || bs <= 0) bs <- max(1L, sf_cpus * 2L)
        idx_list <- split(seq_len(n), ceiling(seq_len(n) / bs))
        res <- vector("list", n)
        if (isTRUE(show_progress)) {
            pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
            on.exit(try(close(pb), silent = TRUE), add = TRUE)
            done <- 0L
        }
        for (I in idx_list) {
            batch <- snowfall::sfLapply(Hist_list[I], sf_project_runner)
            res[I] <- batch
            if (isTRUE(show_progress)) {
                done <- done + length(I)
                utils::setTxtProgressBar(pb, done)
            }
        }
        res
    }
    wrapper <- function(par) {
        eval_count <<- eval_count + 1L
        if (eval_count > max_evals) stop("max_evals atingido")
        t0 <- Sys.time()
        mp_name <- .register_temp_mp(MP_fun, MP_parname, par,
                                     extra_formals)
        on.exit(try(rm(list = mp_name, envir = .GlobalEnv),
                    silent = TRUE), add = TRUE)
        if (isTRUE(parallel)) {
            mp_obj <- get(mp_name, envir = .GlobalEnv)
            invisible(utils::capture.output(
                                 snowfall::sfExport("mp_name",
                                                    "mp_obj")))
            MSE_list <-
                .sf_project_with_progress(Hist_list,
                                          show_progress = isTRUE(verbose))
        } else {
            MSE_list <-
                .project_histories_seq(Hist_list, mp_name,
                                       show_progress = isTRUE(verbose),
                                       parallel = FALSE)
        }
        out <- objective_fun(MSE_list, tunepar = par)
        dur <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        rec <- list(MP = mp_label, par = par, seconds = dur,
                    TACw = out$TACw, PGKw = out$PGKw,
                    LRPw = out$LRPw, SSQ = out$ssq)
        results <<- append(results, list(rec))
        if (!is.null(log_csv)) {
            df <- data.frame(ts = format(Sys.time(),
                                         "%Y-%m-%d %H:%M:%S"),
                             MP = mp_label, par = par, seconds = dur,
                             TACw = out$TACw, PGKw = out$PGKw,
                             LRPw = out$LRPw, SSQ = out$ssq)
            utils::write.table(df, file = log_csv, sep = ",",
                               row.names = FALSE,
                               col.names = !file.exists(log_csv),
                               append = TRUE)
        }
        if (!is.null(checkpoint_rds)) saveRDS(list(results = results,
                                                   best = best),
                                              checkpoint_rds)
        if (out$ssq < best$ssq) best <<- list(ssq = out$ssq, par = par)
        out$ssq
    }
    opt <- tryCatch(
        optimize(f = wrapper, interval = interval, tol = tol),
        error = function(e) {
            message("Interrupted optimization: ", conditionMessage(e))
            list(minimum = best$par, objective = best$ssq)
        })
    MPout <- MP_fun
    fml <- formals(MPout)
    fml[[MP_parname]] <- opt$minimum
    formals(MPout) <- fml
    class(MPout) <- "MP"
    list(MPout = MPout, optimization = opt, trace = results)
}

######@> Main function...
DoMPTune <- function(HistList, MPName,
                     TuneInterval = c(0.05, 4),
                     ObjectiveMode = c("PGK", "LRP", "BOTH"),
                     Target_PGK = 0.60,
                     Target_LRP = 0.10,
                     Weight_PGK = 1,
                     Weight_LRP = 1,
                     years_idx = 2:31,
                     thr = 0.4,
                     Data_Lag = 1,
                     ManagementInterval = 3,
                     Initial_MP_Yr = 2026,
                     tol = 1e-2,
                     output_dir = "05_TunedMPs",
                     verbose = TRUE,
                     max_evals = Inf,
                     parallel = FALSE,
                     sf_cpus = max(1, parallel::detectCores()-1),
                     sf_type = "SOCK",
                     sf_pkgs = c("MSEtool", "openMSE", "DLMtool"),
                     sf_export = NULL,
                     sf_batch_size = NULL,
                     show_tunepar = TRUE,
                     par_label = "tunepar",
                     digits_par = 6) {
    message("Tuning: ", MPName)
    ObjectiveMode <- match.arg(ObjectiveMode)
    obj_fun <- .make_objective(mode = ObjectiveMode,
                               targets = list(PGK = Target_PGK,
                                              LRP = Target_LRP),
                               weights = list(PGK = Weight_PGK,
                                              LRP = Weight_LRP),
                               years_idx = years_idx,
                               thr = thr,
                               verbose = verbose,
                               show_tunepar = show_tunepar,
                               par_label = par_label,
                               digits_par = digits_par)
    extra_formals <- list(Data_Lag = Data_Lag,
                          Interval = ManagementInterval,
                          Initial_MP_Yr = Initial_MP_Yr)
    mode_tag <- switch(ObjectiveMode,
                       "PGK" = paste0("PGK_", Target_PGK),
                       "LRP" = paste0("LRP_", Target_LRP),
                       "BOTH" = paste0("PGK_", Target_PGK, "_LRP_",
                                       Target_LRP))
    dirName <- file.path(output_dir, paste0("DataLag_", Data_Lag,
                                            "_Interval_",
                                            ManagementInterval, "_",
                                            mode_tag))
    if (!dir.exists(output_dir)) dir.create(output_dir,
                                            recursive = TRUE)
    if (!dir.exists(dirName)) dir.create(dirName, recursive = TRUE)
    log_csv <- file.path(dirName, "tune_log.csv")
    checkpoint_rds <- file.path(dirName, "checkpoint.rds")
    filename <- paste0(MPName, "_", min(years_idx), "_",
                       max(years_idx), ".mp")
    tuned <- tune_MPRS(Hist_list = HistList,
                       MP = MPName,
                       MP_parname = "tunepar",
                       interval = TuneInterval,
                       objective_fun = obj_fun,
                       years_idx = years_idx,
                       tol = tol,
                       extra_formals = extra_formals,
                       log_csv = log_csv,
                       checkpoint_rds = checkpoint_rds,
                       max_evals = max_evals,
                       verbose = verbose,
                       parallel = parallel,
                       sf_cpus = sf_cpus,
                       sf_type = sf_type,
                       sf_pkgs = sf_pkgs,
                       sf_export = sf_export,
                       sf_batch_size = sf_batch_size,
                       mp_label = MPName)
    saveRDS(tuned, file.path(dirName, filename))
    invisible(tuned)
}

######@> Compact version...
optPGK60_1_30ER <- function(MSE_list,
                            years_idx = 2:31,
                            thr = 0.4,
                            crit = 0.10,
                            mode = c("PGK","LRP","BOTH"),
                            target_PGK = 0.60,
                            target_LRP = 0.10,
                            weight_PGK = 1,
                            weight_LRP = 1,
                            digits_print = 5,
                            verbose = TRUE) {
    mode <- match.arg(mode)
    met <- .compute_metrics(MSE_list, years_idx = years_idx, thr = thr)
    PGKw <- round(met$PGKw, digits_print)
    LRPw <- round(met$LRPw, digits_print)
    TACw <- met$TACw
    loss_PGK <- (PGKw - target_PGK)^2
    loss_LRP <- (LRPw - target_LRP)^2
    ssq <- switch(mode,
                  "PGK" = loss_PGK,
                  "LRP" = loss_LRP,
                  "BOTH" = weight_PGK*loss_PGK + weight_LRP*loss_LRP)
    if (isTRUE(verbose)) {
        cat("*************************\n")
        cat("Mode = ", mode, "\n", sep = "")
        cat("PGKw = ", PGKw, "  (target = ", target_PGK, ")\n", sep = "")
        cat("LRPw (ever-risk) = ", LRPw,
            "  [thr <", thr, ", crit <", crit,
            "]  (target = ", target_LRP, ")\n", sep = "")
        if (mode == "BOTH") cat("Weights: PGK=", weight_PGK,
                                ", LRP=", weight_LRP, "\n", sep = "")
        cat("SSQ = ", round(ssq, 8), "\n")
        cat("*************************\n\n")
    }
    list(TACw = TACw, PGKw = PGKw, LRPw = LRPw,
         loss = list(PGK = loss_PGK, LRP = loss_LRP), ssq = ssq)
}

########################################################################
######@> Applying Tuning Process...

######@>----------------------------------------------------------------
######@> Running in parallel - Single MPs - LRP 10%...

######@> Constant Exploitation...
DoMPTune(HistList = HistList,
         MPName = "CE",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.10,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Index Ratio...
DoMPTune(HistList = HistList,
         MPName = "IR",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.10,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Surplus Production Model...
DoMPTune(HistList = HistList,
         MPName = "SP",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.10,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Surplus Production Model with Distinct HCR...
DoMPTune(HistList = HistList,
         MPName = "SPAH",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.10,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@>----------------------------------------------------------------
######@> Running in parallel - Single MPs - LRP 15%...

######@> Constant Exploitation...
DoMPTune(HistList = HistList,
         MPName = "CE",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.15,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Index Ratio...
DoMPTune(HistList = HistList,
         MPName = "IR",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.15,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Surplus Production Model...
DoMPTune(HistList = HistList,
         MPName = "SP",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.15,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Surplus Production Model with Distinct HCR...
DoMPTune(HistList = HistList,
         MPName = "SPAH",
         TuneInterval = c(0.05, 5),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.15,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@>----------------------------------------------------------------
######@> Running in parallel - Single MPs - LRP 20%...

######@> Constant Exploitation...
DoMPTune(HistList = HistList,
         MPName = "CE",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.20,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Index Ratio...
DoMPTune(HistList = HistList,
         MPName = "IR",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.20,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Surplus Production Model...
DoMPTune(HistList = HistList,
         MPName = "SP",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.20,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Surplus Production Model with Distinct HCR...
DoMPTune(HistList = HistList,
         MPName = "SPAH",
         TuneInterval = c(0.05, 6),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.20,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@>----------------------------------------------------------------
######@> Running in parallel - Single MPs - LRP 12%...

######@> Constant Exploitation...
DoMPTune(HistList = HistList,
         MPName = "CE",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.12,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Index Ratio...
DoMPTune(HistList = HistList,
         MPName = "IR",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.12,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Surplus Production Model...
DoMPTune(HistList = HistList,
         MPName = "SP",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.12,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Surplus Production Model with Distinct HCR...
DoMPTune(HistList = HistList,
         MPName = "SPAH",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.12,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@>----------------------------------------------------------------
######@> Running in parallel - Single MPs - LRP 18%...

######@> Constant Exploitation...
DoMPTune(HistList = HistList,
         MPName = "CE",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.18,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Index Ratio...
DoMPTune(HistList = HistList,
         MPName = "IR",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.18,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Surplus Production Model...
DoMPTune(HistList = HistList,
         MPName = "SP",
         TuneInterval = c(0.05, 4),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.18,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

######@> Surplus Production Model with Distinct HCR...
DoMPTune(HistList = HistList,
         MPName = "SPAH",
         TuneInterval = c(0.05, 6),
         ObjectiveMode = "LRP",
         Target_PGK = 0.60,
         Target_LRP = 0.18,
         Weight_PGK = 1,
         Weight_LRP = 1,
         years_idx = 2:31,
         thr = 0.4,
         parallel = TRUE,
         sf_cpus = 18,
         sf_type = "SOCK",
         sf_export  = c("Catchdf", "FixedTAC", "SameTAC", "adjust_TAC",
                        "adjust_TAC2"),
         sf_batch_size = 6,
         show_tunepar = TRUE,
         par_label = "tunepar",
         digits_par = 6,
         verbose = TRUE)

########################################################################
######@> Understanding Tune Patterns...

######@> Loading tuned objects...

#####@> Function to read MP's files to R...
import_optimization_results <- function(path = ".",
                                        pattern = "\\.mp$",
                                        mode = "") {
    files <- list.files(path, pattern = pattern, full.names = TRUE)
    purrr::map_dfr(files, \(file) {
        object <- readRDS(file)
        purrr::map_dfr(object$trace, \(trace) {
            data.frame(Tune = trace$par,
                       MP = trace$MP,
                       Time = trace$seconds,
                       PGKw = trace$PGKw,
                       LRPw = trace$LRPw,
                       TACw = trace$TACw,
                       SSQ  = trace$SSQ)}) |>
            dplyr::mutate(source_file =
                              tools::file_path_sans_ext(basename(file)),
                          mode = mode)
    })
}

#####@> Importing tuned MPs...

####@> Simulations and Projection years across 9 OMs...

####@> LRP 0.1 ...
df01 <- import_optimization_results(
    "05_TunedMPs/DataLag_1_Interval_3_LRP_0.1/",
    mode = "LRP 0.1")

####@> LRP 0.12...
df02 <- import_optimization_results(
    "05_TunedMPs/DataLag_1_Interval_3_LRP_0.12/",
    mode = "LRP 0.12")

####@> LRP 0.15...
df03 <- import_optimization_results(
    "05_TunedMPs/DataLag_1_Interval_3_LRP_0.15/",
    mode = "LRP 0.15")

####@> LRP 0.18...
df04 <- import_optimization_results(
    "05_TunedMPs/DataLag_1_Interval_3_LRP_0.18/",
    mode = "LRP 0.18")

####@> LRP 0.2...
df05 <- import_optimization_results(
    "05_TunedMPs/DataLag_1_Interval_3_LRP_0.2/",
    mode = "LRP 0.2")

#####@> Extracting only the final values...

####@> LRP 0.1 ...
mdf01 <- df01 %>%
    group_by(source_file) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    as.data.frame

####@> LRP 0.12 ...
mdf02 <- df02 %>%
    group_by(source_file) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    as.data.frame

####@> LRP 0.15 ...
mdf03 <- df03 %>%
    group_by(source_file) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    as.data.frame

####@> LRP 0.18 ...
mdf04 <- df04 %>%
    group_by(source_file) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    as.data.frame

####@> LRP 0.2 ...
mdf05 <- df05 %>%
    group_by(source_file) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    as.data.frame

#####@> Combining datasets...
case01 <- gtools::smartbind(mdf01, mdf02, mdf03, mdf04, mdf05)

####@> Visualizing - separated...
p00 <- ggplot(data = case01, aes(x = mode, y = PGKw)) +
    geom_hline(yintercept = 0.6, linetype = "dashed",
               colour = "gray") +
    geom_line(aes(group = 1)) +
    geom_point(size = 3) +
    geom_label(aes(y = (PGKw - PGKw) + 0.1,
                  label = paste0(round(TACw), " t")), size = 4) +
    facet_wrap(~MP, ncol = 2) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    labs(x = "Probability of breaching the Limit Reference Point (LRP)",
         y = "Probability of being in the Kobe green quadrant (PGK)") +
    my_theme()
p00

####@> Visualizing - together...
p01 <- ggplot(data = case01, aes(x = mode, y = PGKw, fill = MP,
                                 colour = MP)) +
    geom_hline(yintercept = 0.6, linetype = "dashed",
               colour = "gray") +
    geom_line(aes(group = MP)) +
    geom_point(size = 3) +
    ## geom_label(aes(y = (PGKw - PGKw) + 0.1,
    ##                label = paste0(round(TACw), " t")), size = 4) +
    ## facet_wrap(~MP, ncol = 2) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    labs(x = "Probability of breaching the Limit Reference Point (LRP)",
         y = "Probability of being in the Kobe green quadrant (PGK)",
         fill = "", colour = "") +
    my_theme() +
    theme(legend.position = "inside",
          legend.position.inside = c(0.3, 0.05),
          legend.direction = "horizontal")
p01

p02 <- ggplot(data = case01, aes(x = mode, y = TACw, fill = MP,
                                 colour = MP)) +
    ## geom_hline(yintercept = 0.6, linetype = "dashed",
    ##            colour = "gray") +
    geom_line(aes(group = MP)) +
    geom_point(size = 3) +
    ## geom_label(aes(y = (PGKw - PGKw) + 0.1,
    ##                label = paste0(round(TACw), " t")), size = 4) +
    ## facet_wrap(~MP, ncol = 2) +
    scale_y_continuous(limits = c(0, 40000), expand = c(0, 0)) +
    labs(x = "Probability of breaching the Limit Reference Point (LRP)",
         y = "Total Allowable Catch (t)",
         fill = "", colour = "") +
    my_theme() +
    theme(legend.position = "inside",
          legend.position.inside = c(0.3, 0.05),
          legend.direction = "horizontal")
p02

######@> Exporting figures...
ggsave(paste0("07_Results/04_Tuned_MPs/", "Fig_39_Tune_MPs_ver00.tiff"),
       plot = p00, device = "tiff", units = "cm", width = 35,
       height = 30, dpi = 600, bg = "white", compression = "lzw")

ggsave(paste0("07_Results/04_Tuned_MPs/", "Fig_40_PGK-LRP_Tune_MPs_ver00.tiff"),
       plot = p01, device = "tiff", units = "cm", width = 25,
       height = 20, dpi = 600, bg = "white", compression = "lzw")

ggsave(paste0("07_Results/04_Tuned_MPs/", "Fig_41_TAC-LRP_Tune_MPs_ver00.tiff"),
       plot = p02, device = "tiff", units = "cm", width = 25,
       height = 20, dpi = 600, bg = "white", compression = "lzw")

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
