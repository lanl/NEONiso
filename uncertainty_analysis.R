# uncertainty_analysis.R

# test integration period on one site first.
library(rhdf5)
library(tidyverse)
library(neonUtilities)


time_windows <- c(1, 2, 3, 4, 7, 14, 21, 28, 90, 180, 365, 100000)

data <- list()
plot_out <- list()
plot_amb <- list()

sites <- c(NEONiso::terrestrial_core_sites(),NEONiso::terrestrial_relocatable_sites())

for (j in 1:length(sites)) {
  
  
  #j <- 1
  for (i in 1:length(time_windows)) {
    
    
    # get list of files.
    flist <- list.files(paste0('~/NEONcal/r2_99/carbon_halfwidth_tests/',time_windows[i],'/'), 
                        full.names = TRUE)
    
    glist <- list.files(paste0('~/NEONcal/r2_99/carbon_halfwidth_tests/',time_windows[i],'/'))
    slist <- substr(glist, 10, 13)
    
    
    # figure out which site in slist matches sites[j]
    k <- which(sites[j] == slist)
    
    print(slist)
    print(paste(i,j,k,sites[j]))
    
    tmp0 <- h5read(flist[k], name = paste0('/',slist[k],'/dp01/data/isoCo2/'))
    
    # select just the standards
    tmp1 <- tmp0[c("co2High_09m", "co2Med_09m", "co2Low_09m")]
    
    # this is annoying!
    tmp2 <- list()
    tmp3 <- list()
    
    print("rearrange data")
    
    for (z in 1:length(tmp1)) {
      tmp2[[z]] <- tmp1[[z]]$dlta13CCo2 %>%
        dplyr::select(timeBgn, mean_cal, numSamp, vari, CVcalUcrt, LOOcalUcrt)
      
      tmp3[[z]] <- tmp1[[z]]$dlta13CCo2Refe %>%
        dplyr::select(timeBgn, mean)
    }
    
    print('merging data')
    tmp2 <- do.call(rbind, tmp2)
    tmp3 <- do.call(rbind, tmp3)
    
    print("continue merging data")
    tmp4 <- merge(tmp2, tmp3, by = 'timeBgn', all = TRUE)
    
    plot_out[[i]] <- tmp4 %>%
      dplyr::select(-timeBgn) %>%
      dplyr::rename(cal = mean_cal, ref = mean) %>%
      mutate(diff1 = CVcalUcrt, diff2 = cal - ref, diff3 = LOOcalUcrt, intNum = time_windows[i])
    
    # also collect ambient data:
    tmp91 <- tmp0[!(names(tmp0) %in% c("co2High_09m", "co2Med_09m", "co2Low_09m", "calData"))]
    # this is annoying!
    tmp92 <- list()
    
    for (y in 1:length(tmp91)) {
      tmp92[[y]] <- tmp91[[y]]$dlta13CCo2 %>%
        dplyr::select(mean_cal, mean)
    }
    
    tmp92 <- do.call(rbind, tmp92)
    
    plot_amb[[i]] <- tmp92 %>%
      dplyr::mutate(not_na_cal = !is.na(mean_cal), not_na_obs = !is.na(mean),
                    na_cal     = is.na(mean_cal), na_obs = is.na(mean),
                    window = time_windows[i]) %>%
      dplyr::summarize(total_obs_frac = sum(not_na_cal)/sum(not_na_obs), window = mean(window))
  }
  
  # collapse plot out
  plot_out <- do.call(rbind, plot_out)
  plot_amb <- do.call(rbind, plot_amb)
  
  # filter out really high variance measurements
  plot_out <- plot_out %>%
    dplyr::filter(vari < 0.5)
  
  # make a plot?
  p1 <- ggplot(data = plot_out, aes(x = factor(intNum), y = diff1)) +
    #  geom_violin() +
    #  geom_line(aes(x = intNum, y = mean(diff1))) +
    geom_boxplot() + 
    scale_y_continuous(name = "calibration uncertainty (permil)", limits=c(1e-4,10), trans = "log10") +
    scale_x_discrete(name = 'calibration window (days)')
  
  print(p1)
  
  p2 <- ggplot(data = plot_amb, aes(x = factor(window), y = total_obs_frac)) +
    geom_point()
  
  print(p2)
  
  p3 <- ggplot(data = plot_out, aes(x = factor(intNum), y = abs(diff2))) +
    geom_violin() +
    scale_y_continuous(name = "calibration uncertainty (permil)", limits=c(1e-4,10), trans = "log10") +
    geom_boxplot()
  
  print(p3)
  
  p4 <- ggplot(data = plot_out, aes(x = factor(intNum), y = abs(diff3))) +
    geom_violin() +
    scale_y_continuous(name = "calibration uncertainty (permil)", limits=c(1e-4,10), trans = "log10") +
    geom_boxplot()
  
  print(p4)
  
  pdf(paste0('~/Desktop/testing/',sites[j],'.pdf'), width = 12, height = 8)
  gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)
  dev.off()
  
}