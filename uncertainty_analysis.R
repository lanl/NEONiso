# uncertainty_analysis.R

# test integration period on one site first.
library(rhdf5)
library(tidyverse)
library(neonUtilities)


time_windows <- c(1, 2, 3, 4, 7, 14, 21, 28, 90, 180, 365, 100000)

data <- list()
plot_out <- list()

for (i in 1:length(time_windows)) {
  
  data[[i]] <- h5read(paste0('~/NEONcal/carbon_halfwidth_tests/highlow/', time_windows[i], '/NEON.D01.HARV.DP4.00200.001.nsae.2017-03.basic.20201020T144907Z.h5'),
                      name = '/HARV/dp01/data/isoCo2/co2Med_09m')
 
  # select the columns we need from each data frame
  tmp1 <- data[[i]]$dlta13CCo2 %>%
    dplyr::select(timeBgn, mean_cal, numSamp, vari)
  
  tmp2 <- data[[i]]$dlta13CCo2Refe %>%
    dplyr::select(timeBgn, mean)
  
  tmp3 <- merge(tmp1, tmp2, by = 'timeBgn')
  
  plot_out[[i]] <- tmp3 %>%
    dplyr::rename(cal = mean_cal, ref = mean) %>%
    mutate(diff = cal - ref, intNum = time_windows[i])
   
}

# collapse plot out
plot_out <- do.call(rbind, plot_out)

# filter out really high variance measurements
plot_out <- plot_out %>%
  dplyr::filter(vari < 0.5)

# make a plot?
p1 <- ggplot(data = plot_out, aes(x = factor(intNum), y = diff)) +
  geom_violin() +
  geom_boxplot()

print(p1)

p2 <- ggplot(data = plot_out, aes(x = factor(intNum), y = abs(diff))) +
  geom_violin() +
  geom_boxplot()

print(p2)

p2 <- ggplot(data = plot_out, aes(x = factor(intNum), y = abs(diff))) +
  geom_point(position = 'jitter')

print(p2)




