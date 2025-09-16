########################################################################
## Description: Function created or adapted for using in Western
## Skipjack Management Strategy Evaluation...
##
## Maintainer: Datenkraft - ICCAT (Tropical Tunas Species Group)
## Author: Adrian Hordyk & Rodrigo Sant'Ana
## Created: qua jun 18 10:11:15 2025 (-0300)
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
######@> Function to generate recruitment deviations from truncated
######@> log-normal distribution...

#####@> Log-Normal truncated distribution function...
rtnorm <- function(n, mu, sigma, lower, upper) {
  qnorm(
    runif(n,
          pnorm(lower, mu, sigma),
          pnorm(upper, mu, sigma)),
    mu,
    sigma)
}

#####@> Generate recruitment deviations from Log-Normal truncated
#####@> distribution function...
GenerateRecDevs <- function(OM, truncSD = 2, plot = FALSE) {
    ## rec devs estimated by SS3
    estRecDevs <-
        log(OM@cpars$Perr_y[1, OM@maxage:(OM@maxage+OM@nyears - 1)])
    estRecDevs <- estRecDevs[estRecDevs != 0]
    ## calculate SD and AC from estimated historical rec devs
    SD <- sd(estRecDevs) # OM@Perr[1] # sigmaR
    AC <- acf(estRecDevs, plot = FALSE)$acf[2] # OM@AC[1] lag 1
                                        # autocorrelation
    mu <- -0.5 * SD^2  * (1 - AC) / sqrt(1 - AC^2)
    lower <- mu - truncSD * SD
    upper <- mu + truncSD * SD
    logDevsOrig <- log(OM@cpars$Perr_y)  # recruitment deviations
    dd <- dim(logDevsOrig)
    ## generate rec devs from truncated dist
    set.seed(OM@seed)
    nsim <- OM@nsim
    nyear <- OM@proyears
    nsamp <- nsim * nyear
    logDevs <- array(rtnorm(nsamp, mu, SD, lower, upper),
                     dim = c(nsim, nyear))
    ## add auto-correlation
    lastHistDev <- logDevsOrig[, dd[2] + 1 - OM@proyears]
    for (y in 1:ncol(logDevs)) {
        if (y == 1) {
            logDevs[, y] <- AC * lastHistDev + logDevs[, y] *
                (1 - AC * AC)^0.5
        } else {
            logDevs[, y] <- AC * logDevs[, y - 1] + logDevs[, y] *
                (1 - AC * AC)^0.5
        }
    }
    ## replace values
    OM@cpars$Perr_y[, (dd[2] + 1 - OM@proyears):dd[2]] <- exp(logDevs)
    if (plot) {
        df <- data.frame(EstRecDevs = as.vector(estRecDevs))
        df2 <- data.frame(SimRecDevs = as.vector(logDevs))
        dist <- data.frame(x = c(-1, 1))
        mu <- mean(df$EstRecDevs)
        sd <- sd(df$EstRecDevs)
        SDdf <- data.frame(intercept = c(mu - sd,
                                         mu + sd,
                                         mu - 2 * sd,
                                         mu + 2 * sd),
                       type = c("1 SD", "1 SD", "2 SD", "2 SD"))
        ggplot(df) +
            geom_histogram(aes(x = EstRecDevs, y =..density..),
                           breaks = seq(-1, 1, by = 0.075),
                           colour = "black",
                           fill = "grey") +
            geom_vline(data = SDdf,
                       aes(xintercept = intercept, color = type),
                       linetype = 2) +
            stat_function(fun = dnorm, data = dist, aes(x = x),
                          args = list(mean = mean(df$EstRecDevs),
                                      sd = sd(df$EstRecDevs))) +
            theme_bw()
        ggplot(df2) +
            geom_histogram(aes(x = SimRecDevs, y =..density..),
                           breaks = seq(-1, 1, by = 0.075),
                           colour = "black",
                           fill = "grey") +
            stat_function(fun = dnorm, data = dist, aes(x = x),
                          args = list(mean = mean(df2$SimRecDevs),
                                      sd = sd(df2$SimRecDevs))) +
            theme_bw()
        ## par(mfrow=c(1,2))
        ## lim <- range(estRecDevs) |> abs() |> max()
        ## hist(estRecDevs, main='Estimated Historical',
        ##      xlim=c(-lim,lim))
        ## hist(logDevs, main='Simulated', xlim=c(-lim,lim))
        ## par(mfrow=c(1,2))
        ## histYrs <- (OM@CurrentYr-OM@nyears-1):OM@CurrentYr
        ## projYrs <- seq(OM@CurrentYr+1, by=1, length.out=OM@proyears)
        ## years <- c(histYrs, projYrs)
        ## ind <- (dd[2] - length(years) + 1):dd[2]
        ## matplot(years, t(logDevsOrig[,ind]), type='l', ylim=c(-1.5, 1.5),
        ##         xlab='Year', ylab='log Recruitment Deviations')
        ## mtext("Normal Dist", 3)
        ## matplot(years, t(log(OM@cpars$Perr_y[,ind])), type='l',
        ## ylim=c(-1.5, 1.5), xlab='Year', ylab='')
        ## mtext("Truncated Normal Dist", 3)
    }
    OM
}

#####@> Function to add WSKJ_Data, I_beta = 1, observed catch must be the
#####@> total removals and generate recruitment deviations from trucated
#####@> LN dist to cpars...
update_cpars <- function(OM) {
  OM@cpars$Data <- WSKJ_Data
  OM@cpars$I_beta <- rep(1, OM@nsim)
  OM@cpars$control$ObsCatch <- "Removals"
  OM@cpars$Data@Name <- OM@Name
  OM <- GenerateRecDevs(OM) # generate rec devs from truncated dist
  OM
}

#####@> Function to extract specific text from a string...
extract_obs_se <- function(fleet_name) {
    result <- sub(".*_(Obs|SE)$", "\\1", fleet_name)
    return(result)
}

#####@> Function to remove specific text from a string...
remove_obs_se <- function(fleet_name) {
    result <- sub("_(Obs|SE)$", "", fleet_name)
    return(result)
}

#####@> Function to read Stock Synthesis scenarios...
ler_modelos_ss <- function(path,
                           quantis = c(25, 50, 75),
                           h = 6:8) {
    stopifnot(requireNamespace("purrr"), requireNamespace("dplyr"))
    arquivos <- tidyr::expand_grid(
                           q = quantis,
                           h = h) |>
        dplyr::mutate(
                   path = paste0(path, "WSKJ_EstRec93_Qnt", q, "_h", h),
                   nome = paste0("WSKJ_EstRec93_Qnt", q, "_h", h))
    modelos <- purrr::map(arquivos$path, SS_output)
    names(modelos) <- arquivos$nome
    return(modelos)
}

######@> Function to check openMSE vs SS3 reference points...
compareRefs <- function(Hists, SSdir = "00_Reconditioning_SS") {
    HistList <- readRDS(Hists)
    ReplistList  <- list()
    SSdirs <- list.dirs(SSdir, recursive = FALSE, full.names = TRUE)
    DFList <- list()
    for (i in seq_along(HistList)) {
        Hist <- HistList[[i]]
        omname <- gsub("OM", "", Hist@OM@Name) |>
            trimws()
        SSind <- which(grepl(omname, basename(SSdirs)))
        replist <- r4ss::SS_output(SSdirs[SSind])
        vars <- c("SSB_MSY", "annF_MSY", "Dead_Catch_MSY", "Ret_Catch_MSY")
        df <- replist$derived_quants |>
            dplyr::filter(Label %in% vars) |>
            dplyr::select(Label, SS = Value)
        df$Label <- c("SBMSY", "FMSY", "MSYDead", "MSYRetain")
        df$OM <- c(Hist@Ref$ReferencePoints$SSBMSY[1],
                   Hist@Ref$ReferencePoints$FMSY[1],
                   Hist@Ref$ReferencePoints$MSY[1],
                   Hist@Ref$ReferencePoints$MSY[1])
        df$SS <- round(df$SS, 3)
        df$OM <- round(df$OM, 3)
        df$ratio <- df$SS/df$OM
        df$OMname <- omname
        DFList[[i]] <- df
    }
    do.call("rbind", DFList)
}

######@> Function to extract data from Perr_y SampPars slot...
extrair_perr_hist <- function(HistList) {
    purrr::map_dfr(seq_along(HistList), function(i) {
        hist_i <- HistList[[i]]
        nome <- hist_i@OM@Name
        scen <- paste0("OM", sprintf("%02d", i))
        mage <- HistList[[i]]@OM@maxage
        devs <- t(hist_i@SampPars$Stock$Perr_y)
        devs <- devs[(mage + 1):nrow(devs), ]
        df <- as.data.frame(log(devs)) %>%
            set_names(paste0("Sim_", sprintf("%03d", 1:ncol(.)))) %>%
            mutate(Year = 1952:2055,
                   Name = nome,
                   OM = scen) %>%
            pivot_longer(cols = starts_with("Sim_"),
                         names_to = "Sim",
                         values_to = "Devs") %>%
            arrange(OM, Name, Year)
        return(df)
    })
}

######@> Function to evaluate the RecDevs results...
excedencias_por_OM_detalhada <- function(df,
                                         ref_year = 2025,
                                         sim_col = "Sim",
                                         year_col = "Year",
                                         value_col = "Devs",
                                         om_col = "OM",
                                         nm_col = "Name") {
    df %>%
        group_by(.data[[om_col]], .data[[nm_col]]) %>%
        group_modify(~{
            dados_passado <- .x %>%
                filter(.data[[year_col]] < ref_year)
            dados_futuro  <- .x %>%
                filter(.data[[year_col]] >= ref_year)
            lim_min <- min(dados_passado[[value_col]], na.rm = TRUE)
            lim_max <- max(dados_passado[[value_col]], na.rm = TRUE)
            exced <- dados_futuro %>%
                group_by(.data[[sim_col]]) %>%
                summarise(
                    anos_acima = sum(.data[[value_col]] >
                                     lim_max, na.rm = TRUE),
                    anos_abaixo = sum(.data[[value_col]] <
                                      lim_min, na.rm = TRUE),
                    passou_max = any(.data[[value_col]] >
                                     lim_max, na.rm = TRUE),
                    passou_min = any(.data[[value_col]] <
                                     lim_min, na.rm = TRUE),
                    passou_qualquer = passou_max | passou_min,
                    .groups = "drop") %>%
                mutate(
                    anos_excedidos = anos_acima + anos_abaixo,
                    limite_min = lim_min,
                    limite_max = lim_max)
            return(exced)
        }) -> detalhes
    resumo <- detalhes %>%
        group_by(.data[[om_col]], .data[[nm_col]]) %>%
        summarise(n_simulacoes = n(),
                  n_excedencias = sum(passou_qualquer),
                  n_anos_excedidos = sum(anos_excedidos),
                  proporcao_excedencias = n_excedencias / n_simulacoes,
                  limite_min = first(limite_min),
                  limite_max = first(limite_max),
                  .groups = "drop")
    return(list(resumo = resumo,
                detalhes = detalhes))
}

######@> Functions to extract the trajectories from the historical data
######@> simulations...

#####@> Reference points...
get_Quantities <- function(Hists) {
    temp <- lapply(
        Hists,
        function(hist) {
            cbind.data.frame(
                scenario = str_sub(hist@OM@Name, 4, 31),
                msy = mean(hist@Ref$ByYear$MSY),
                fmsy = mean(hist@Ref$ByYear$FMSY),
                ssbmsy = mean(hist@Ref$ByYear$SSBMSY)
            )})
    temp <- do.call("rbind.data.frame", temp)
    return(temp)
}

#####@> Trajectory values...
get_Find <- function(Hist) {
    value <-
        as.data.frame(stack(as.data.frame(Hist@TSdata$Find)))$values
    year <- rep(1952:2024, each = 100)
    scenario <- Hist@OM@Name
    sim <- dim(Hist@TSdata$Number)[1]
    df <- data.frame(
        scenario = gsub("OM ", "", scenario),
        year = year,
        sim = 1:sim,
        value = value,
        variable = "Find",
        period = "Historical",
        model = "Model 1"
    )
}

get_Quantities02 <- function(Hists, variable = "SSB") {
    if(variable == "SSB") {
        temp <- lapply(
            Hists,
            function(hist) {
                cbind.data.frame(
                    scenario = str_sub(hist@OM@Name, 4, 31),
                    temp02 = get_SSB(hist)
                )})
    } else {
        temp <- lapply(
            Hists,
            function(hist) {
                cbind.data.frame(
                    temp02 = get_Find(hist)
                )})
    }
    temp <- do.call("rbind.data.frame", temp)
    names(temp) <- c("scenario", "year", "sim", "value", "variable",
                     "period", "model")
    return(temp)
}

extract_reference_values <- function(result_list, year_f, year_b) {
    required_slots <- c("Fvalue", "FvalueLower", "FvalueUpper",
                        "Bratio", "BratioLower", "BratioUpper")
    missing <- setdiff(required_slots, names(result_list))
    if (length(missing) > 0) {
        stop(paste("Faltando na lista:",
                   paste(missing, collapse = ", ")))
    }
    f_value <- result_list$Fvalue[result_list$Fvalue$Yr %in% year_f, ] %>%
        select(-Label) %>%
        pivot_longer(names_to = "Scenarios", values_to = "F_avg", 1:9)
    f_lower <-
        result_list$FvalueLower[result_list$FvalueLower$Yr %in% year_f, ] %>%
        select(-Label) %>%
        pivot_longer(names_to = "Scenarios", values_to = "F_Lwr", 1:9)
    f_upper <-
        result_list$FvalueUpper[result_list$FvalueUpper$Yr %in% year_f, ] %>%
        select(-Label) %>%
        pivot_longer(names_to = "Scenarios", values_to = "F_Upr", 1:9)
    b_value <- result_list$Bratio[result_list$Bratio$Yr %in% year_b, ] %>%
        select(-Label) %>%
        mutate(Yr = Yr - 1) %>%
        pivot_longer(names_to = "Scenarios", values_to = "B_avg", 1:9)
    b_lower <-
        result_list$BratioLower[result_list$BratioLower$Yr %in% year_b, ] %>%
        select(-Label) %>%
        mutate(Yr = Yr - 1) %>%
        pivot_longer(names_to = "Scenarios", values_to = "B_Lwr", 1:9)
    b_upper <-
        result_list$BratioUpper[result_list$BratioUpper$Yr %in% year_b, ] %>%
        select(-Label) %>%
        mutate(Yr = Yr - 1) %>%
        pivot_longer(names_to = "Scenarios", values_to = "B_Upr", 1:9)
    out <- f_lower %>%
        left_join(f_value) %>%
        left_join(f_upper) %>%
        left_join(b_lower) %>%
        left_join(b_value) %>%
        left_join(b_upper)
    return(out)
}

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
