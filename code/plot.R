# plot.R
#
# William John Zywiec
# The George Washington University

Plot <- function(file.name, history) {

  # library(ggplot2)
  # library(magrittr)
  # library(scales)

  new.theme <- theme_gray() + theme(axis.text = element_text(color = 'black', size = 10), text = element_text(color = 'black', family = 'serif', size = 10))
  theme_set(new.theme)

  if (missing(history)) {
    history.df <- read.csv(paste0(file.name, '.csv'), header = TRUE)
  } else {
    history.df <- data.frame(
      epoch = 1:length(history$metrics$val_loss),
      val.loss = history$metrics$val_loss,
      val.mae = history$metrics$val_mean_absolute_error,
      loss = history$metrics$loss,
      mae = history$metrics$mean_absolute_error)
    write.csv(history.df, file = paste0(file.name, '.csv'), row.names = FALSE)
  }

  ggplot(history.df, aes(x = epoch)) +
    ggtitle(paste0('model ', file.name)) +
    geom_line(aes(y = mae)) +
    geom_line(aes(y = val.mae), alpha = 0.7, color = '#ff9999') +
    geom_point(aes(x = which.min(history.df$mae), y = min(history.df$mae)), color = 'red') +
    scale_x_continuous() +
    scale_y_log10(breaks = c(2e-04, 2e-03, 2e-02, 2e-01), limits = c(2e-04, 2e-01)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab(bquote(mean ~ absolute ~ error ~ (k[eff]))) +
    annotate(
      geom = 'text',
      x = which.min(history.df$mae),
      y = min(history.df$mae),
      vjust = 1.9,
      label = format(min(history.df$mae), digits = 3, scientific = TRUE),
      color = 'red',
      family = 'serif',
      size = 3.5)

  ggsave(paste0(file.name, '.jpg'), dpi = 1000) %>% suppressMessages()

}
