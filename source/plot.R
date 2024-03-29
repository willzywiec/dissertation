# plot.R
#
# William John Zywiec
# The George Washington University

Plot <- function(x, history) {

  library(ggplot2)
  library(magrittr)
  library(scales)

  # set theme
  new.theme <- theme_gray() + theme(axis.text = element_text(color = 'black', size = 11), text = element_text(color = 'black', family = 'serif', size = 11))
  theme_set(new.theme)

  if (missing('history')) {
    history <- read.csv(paste0(x, '.csv'), header = TRUE)
  } else {
    history <- data.frame(
      epoch = 1:length(history$metrics$val_loss),
      val.loss = history$metrics$val_loss,
      val.mae = history$metrics$val_mean_absolute_error,
      loss = history$metrics$loss,
      mae = history$metrics$mean_absolute_error)
    write.csv(history, file = paste0(x, '.csv'), row.names = FALSE)
  }

  ggplot(history, aes(x = epoch)) +
  geom_line(aes(y = val.mae, color = 'cross-validation data')) +
  geom_line(aes(y = mae, color = 'training data')) +
  geom_point(aes(x = which.min(history$mae), y = min(history$mae), color = 'training minimum')) +
  guides(color = guide_legend(override.aes = list(linetype = c(1, 1, NA), shape = c(NA, NA, 16)))) +
  scale_color_manual('', breaks = c('training data', 'cross-validation data', 'training minimum'), values = c('black', '#a9a9a9', 'red')) +
  scale_x_continuous() +
  scale_y_log10(breaks = c(2e-04, 2e-03, 2e-02, 2e-01), limits = c(2e-04, 2e-01)) +
  theme(
    legend.position = 'bottom',
    legend.spacing.x = unit(0.2, 'cm'),
    legend.text = element_text(size = 11),
    legend.title = element_blank()) +
  ylab('mean absolute error') +
  annotate(
    geom = 'text',
    x = which.min(history$mae),
    y = min(history$mae),
    vjust = 1.9,
    label = format(min(history$mae), digits = 3, scientific = TRUE),
    color = 'red',
    family = 'serif',
    size = 3.5)

  ggsave(paste0(x, '.png'), dpi = 1000, height = 4, width = 6.5) %>% suppressMessages()

}
