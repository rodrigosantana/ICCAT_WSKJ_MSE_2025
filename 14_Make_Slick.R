if (packageVersion('Slick') < '1.0.0')
  install.packages('Slick')

library(Slick)

Slick <- Slick()

Title(Slick) <- "MSE Results for the Western Atlantic Skipjack Tuna Katsuwonus pelamis"

Subtitle(Slick) <- "PRELIMINARY RESULTS"

Date(Slick) <- 'September 2025'

Author(Slick) <- c("Rodrigo Sant’Ana", "Bruno Mourato")
Email(Slick) <- c('rsantana@univali.br')

Institution(Slick) <- "Universidade do Vale do Itajaí"

Introduction(Slick) <- "
The SCRS’s Tropical Tunas Species Group has been developing a management strategy evaluation (MSE) framework for West Atlantic skipjack (SKJ-W) since 2020. In 2015, the Commission called for adoption of a management procedure (MP) for SKJ-W and seven other priority stocks based on an MSE ([Rec. 15-07](https://www.iccat.int/Documents/Recs/compendiopdf-e/2015-07-e.pdf)). This call for an MSE has been echoed in every ICCAT tropical tunas measure since 2016, with [Rec. 16-01](https://www.iccat.int/Documents/Recs/compendiopdf-e/2016-01-e.pdf) setting initial performance indicators for tropical tunas. While the East Atlantic skipjack stock is included in the multispecies MSE with bigeye and yellowfin tunas, western Atlantic skipjack has been earmarked for its own MSE since the Commission adopted the [First Draft Roadmap for the Development of MSE and Harvest Control Rules (HCR)](https://www.iccat.int/com2016/DocENG/PLE_137B_ENG.pdf) in 2016; this is because western skipjack tuna are caught predominantly in a single-stock fishery. 

External experts launched the MSE work in 2020 [(SCRS/140/2020)](https://www.iccat.int/Documents/CVSP/CV077_2020/n_8/CV077080121.pdf) and since then, MSE development has been conducted by the SCRS ([SCRS/2022/097](https://www.iccat.int/Documents/CVSP/CV079_2022/n_1/CV079010384.pdf), [SCRS/2022/180](https://www.iccat.int/Documents/CVSP/CV079_2022/n_1/CV079010851.pdf), [SCRS/2023/169](https://iccat.int/Documents/CVSP/CV080_2023/n_2/CV080020260.pdf), [SCRS/2024/050](https://iccat.int/Documents/CVSP/CV081_2024/n_2/CV08102050.pdf), [SCRS/2024/162](https://iccat.int/Documents/CVSP/CV081_2024/n_2/CV08102162.pdf), [SCRS/2025/087](https://iccat.int/Documents/CVSP/CV082_2025/n_5/CV082050087.pdf), [SCRS/2025/157](https://iccat.int/Documents/CVSP/CV082_2025/n_5/CV082050157.pdf), SCRS/2025/228). The Commission adopted conceptual management objectives for SKJ-W in 2022 ([Res. 22-02](https://www.iccat.int/Documents/Recs/compendiopdf-e/2022-02-e.pdf)) and operationalized them in 2024 ([Rec. 24-04](https://iccat.int/Documents/Recs/compendiopdf-e/2024-04-e.pdf)). Recommendation 24-04 also set a 3-year management cycle and an implementation schedule for the MP and called for final tuning of candidate MPs in 2025. 

The MSE work is now complete and ready for ICCAT to adopt an MP in 2025, in accordance with Rec. 24-04 and the Commission’s workplan [Revised Roadmap for the ICCAT MSE processes adopted by the Commission in 2024](https://www.iccat.int/mse/Docs/MSE_Roadmap_ENG.pdf).

"

MPs(Slick) <- MPs(Code=c('CE',
                         'IR', 
                         'SP', 
                         'SPAH'),
                  
                  Label=c('Constant Exploitation', 
                          'Index Ratio',
                          'Surplus Production',
                          'Surplus Production with non-linear HCR'
                  ),
                  Description=c(' The TAC is adjusted based on the ratio of the estimated current exploitation rate to the mean exploitation rate from 2016 - 2017, with the aim to maintain a constant exploitation rate',
                                'The TAC is adjusted based on the ratio of the mean index over the 3 most recent years to the value of the mean index from 2019 - 2021',
                                'A model-based a surplus production model with a 130-60 hockey stick harvest control rule and an F_Target equal to F_MSY',
                                'The same surplus production model as SP, but with a non-linear harvest control rule')
)

# TODO - add robustness OMs when available
# TODO - add Factor level descriptions

OMs(Slick) <- OMs(
  Factors = data.frame(Factor=c(rep('Growth',3), rep('Steepness', 3), 'Type'),
                       Level=c('Qnt25', 'Qnt50', 'Qnt75',
                               'h6', 'h7', 'h8',
                               'Reference'),
                       
                       Description=''),
  Design = data.frame(Growth=c(rep('Qnt25', 3),
                               rep('Qnt50', 3),
                               rep('Qnt75', 3)),
                      Steepness=c('h6', 'h7', 'h8'),
                      Type='Reference'),
  
  Preset = list(Reference=list(1:3, 1:3,1))
)

LoadMSEObject <- function(MP='CE', Growth='Qnt25', Steepness='h6', LRP=0.1) {
  path <- file.path('06_MSEs', paste0('DataLag_1_Interval_3_LRP_', LRP, '/', MP, '_2_31'))
  fls <- list.files(path)
  ind <- which(grepl(Growth, fls) & grepl(Steepness, fls))
  readRDS(file.path(path, fls[ind]))
}

MSE <- LoadMSEObject()
CurrentYr <- MSE@OM$CurrentYr[1] +1
HistYrs <- seq(CurrentYr, by=-1, length.out=MSE@nyears+1) |> rev()
ProjYrs <- seq(CurrentYr+1, by=1, length.out=MSE@proyears-1) 

nSim <- MSE@nsim
nOM <- nrow(Design(Slick))
nMP <- length(MPs(Slick) |> Code())
nPI <- 3 
nTS <- length(c(HistYrs, ProjYrs))

Timeseries(Slick) <- Timeseries(
  Code=c('SSB/SSB_MSY', 'F/F_MSY', 'Removals'),
  Label=c('SSB/SSB_MSY', 'F/F_MSY', 'Removals (metric tons)'),
  Description = c('Spawning biomass relative to the equilibrium spawning biomass corresponding with maximum sustainable yield',
                  'Fishing mortality relative to the equilibrium fishing mortality corresponding with maximum sustainable yield',
                  'The total removals from the population (landings + dead discards; metric tons)'),
  Time=c(HistYrs, ProjYrs),
  TimeNow = max(HistYrs),
  Value= array(NA, dim=c(nSim, nOM, nMP, nPI, nTS)),
  Target=c(1,NA, NA),
  Limit=c(0.4, 1, NA)
)

Design <- Design(Slick)
MPs <- MPs(Slick) |> Code()

for (i in 1:nrow(Design)) {
  for (j in seq_along(MPs)) {
    MSE <- LoadMSEObject(MP=MPs[j],
                         Growth=Design$Growth[i],
                         Steepness=Design$Steepness[i])
    
    
    # SB/SBMSY
    SSBMSY <- MSE@RefPoint$SSBMSY 
    SSB_hist <- replicate(1, MSE@SSB_hist) |> aperm(c(1,3,2))
    SSB_proj <- MSE@SSB 
    SSB <- abind::abind(SSB_hist, SSB_proj, along=3)
    Slick@Timeseries@Value[,i,j,1,] <- SSB/SSBMSY
    
    # F/FMSY
    FMSY <- MSE@RefPoint$FMSY 
    F_hist <- replicate(1, MSE@FM_hist) |> aperm(c(1,3,2))
    F_proj <- MSE@FM 
    F <- abind::abind(F_hist, F_proj, along=3)
    Slick@Timeseries@Value[,i,j,2,] <- F/FMSY
    
    # Removals
    Removals_hist <- replicate(1, apply(MSE@Hist@TSdata$Removals, 1:2, sum)) |> aperm(c(1,3,2))
    Removals_proj <- MSE@Removals 
    Removals <- abind::abind(Removals_hist, Removals_proj, along=3)
    Slick@Timeseries@Value[,i,j,3,] <- Removals
  }
}


# Boxplot
Boxplot(Slick) <- Boxplot(
  Code = c('VarC', 'SSB/SSB_MSY', 'F/FMSY', 'Removals'),
  Label = c('Average Variability in TAC',  'SSB/SSB_MSY', 'F/F_MSY', 'Removals (metric tons)'),
  Description = c('Average Variability in the change in TAC between management cycles',
    'The mean spawning biomass relative to the equilibrium spawning biomass corresponding with maximum sustainable yield in the first 10 years of the projection period',
                  'The mean fishing mortality relative to the equilibrium fishing mortality corresponding with maximum sustainable yield in the first 10 years of the projection period',
                  'The mean total removals from the population (landings + dead discards; metric tons)'),
  Value = array(NA, dim=c(nSim, nOM, nMP, 4))
)

for (i in 1:nrow(Design)) {
  for (j in seq_along(MPs)) {
    MSE <- LoadMSEObject(MP=MPs[j],
                         Growth=Design$Growth[i],
                         Steepness=Design$Steepness[i])
    
    # Average Variability in TAC
    interval <- 3
    updateYrs <- seq(2, by=interval, to=MSE@proyears)
    TACs <- MSE@TAC[,1, updateYrs]
    ys <- 1:dim(TACs)[2]
    y1 <- ys[1:(length(ys)-1)]
    y2 <- ys[2:length(ys)]
    
    avgdelta <- apply(((((TACs[, y1] - TACs[, y2])/TACs[, y2])^2)^0.5), 1, mean)
    Slick@Boxplot@Value[,i,j,1] <- avgdelta

    # SB/SBMSY
    Slick@Boxplot@Value[,i,j,2] <- apply(MSE@SB_SBMSY[,1,1:10], 1, mean)
    
    # F/FMSY
    Slick@Boxplot@Value[,i,j,3] <- apply(MSE@F_FMSY[,1,1:10], 1, mean)
    
    # Removals
    Slick@Boxplot@Value[,i,j,4] <- apply(MSE@Removals, 1, mean)
  }
}

# Quilt
Quilt(Slick) <- Quilt(
  Code = c('PGK_short', 'PGK_med', 'PGK_long',
           'nLRP_short', 'nLRP_med', 'nLRP_long',
           'PNOF',
           'AvC_short', 'AvC_med', 'AvC_long'),
  Label = c('PGK_short', 'PGK_med', 'PGK_long',
            'nLRP_short', 'nLRP_med', 'nLRP_long',
            'PNOF',
            'AvC_short', 'AvC_med', 'AvC_long'),
  Description = c('Probability of being in the Kobe green quadrant (Years 1 - 3)',
                  'Probability of being in the Kobe green quadrant (Years 4 - 10)',
                  'Probability of being in the Kobe green quadrant (Years 11 - 30)',
                  'Probability of not breaching the limit reference point (Years 1 - 3)',
                  'Probability of not breaching the limit reference point (Years 4 - 10)',
                  'Probability of not breaching the limit reference point (Years 11 - 30)',
                  'Probability of F < FMSY (Years 1 - 30)',
                  'Average catch (metric ton; Years 1 - 3)',
                  'Average catch (metric ton; Years 4 - 10)',
                  'Average catch (metric ton; Years 11 - 30)'
                  ),

  Value=array(NA, dim=c(nOM, nMP, 10))
)

for (i in 1:nrow(Design)) {
  for (j in seq_along(MPs)) {
    MSE <- LoadMSEObject(MP=MPs[j],
                         Growth=Design$Growth[i],
                         Steepness=Design$Steepness[i])
    
    # PGK_short
    yrs <- 2:4
    Slick@Quilt@Value[i,j,1] <- mean(MSE@SB_SBMSY[,1,yrs] > 1 & MSE@F_FMSY[,1,yrs] < 1)
      
    # PGK_med
    yrs <- 5:11
    Slick@Quilt@Value[i,j,2] <- mean(MSE@SB_SBMSY[,1,yrs] > 1 & MSE@F_FMSY[,1,yrs] < 1)
    
    # PGK_long
    yrs <- 12:31
    Slick@Quilt@Value[i,j,3] <- mean(MSE@SB_SBMSY[,1,yrs] > 1 & MSE@F_FMSY[,1,yrs] < 1)
    
    # nLRP_short
    yrs <- 2:4
    Slick@Quilt@Value[i,j,4] <- mean(MSE@SB_SBMSY[,1,yrs] > 0.4)
    
    # nLRP_med
    yrs <- 5:11
    Slick@Quilt@Value[i,j,5] <- mean(MSE@SB_SBMSY[,1,yrs] > 0.4)
    
    # nLRP_long
    yrs <- 12:31
    Slick@Quilt@Value[i,j,6] <- mean(MSE@SB_SBMSY[,1,yrs] > 0.4)
    
    # PNOF
    Slick@Quilt@Value[i,j,7] <- mean(MSE@F_FMSY[,1,2:31] < 1)
    
    # AvC_short
    Slick@Quilt@Value[i,j,8] <- mean(MSE@Catch[,1,2:4])
    
    # AvC_med
    Slick@Quilt@Value[i,j,9] <- mean(MSE@Catch[,1,5:11])
    
    # AvC_long
    Slick@Quilt@Value[i,j,10] <- mean(MSE@Catch[,1,12:31])
  }
}

# Kobe
Kobe(Slick) <- Kobe(Code=c('SB/SBMSY', 'F/FMSY'),
     Label=c('SB/SBMSY','F/FMSY'),
     Description = c('The spawning biomass relative to the equilibrium spawning biomass corresponding with maximum sustainable yield',
                     'The fishing mortality relative to the equilibrium fishing mortality corresponding with maximum sustainable yield'),
     Value=array(NA, dim=c(nSim, nOM, nMP, 2, length(ProjYrs))),
     Time=ProjYrs
)

for (i in 1:nrow(Design)) {
  for (j in seq_along(MPs)) {
    MSE <- LoadMSEObject(MP=MPs[j],
                         Growth=Design$Growth[i],
                         Steepness=Design$Steepness[i])
    
    Slick@Kobe@Value[,i,j,1,] <- MSE@SB_SBMSY[,1,2:31]
    Slick@Kobe@Value[,i,j,2,] <- MSE@F_FMSY[,1,2:31]
  }
}


saveRDS(Slick, 'Western_Atlantic_Skipjack.slick')

# Test App
App(slick=Slick)


# Spider

# Tradeoff



