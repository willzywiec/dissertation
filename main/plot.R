# plot.R
#
# William John Zywiec
# The George Washington University
#
# ...

Plot <- function(history, file.name) {

  # load packages
  library(scales)

  history.df <- data.frame(
    epoch = 1:length(history$metrics$val_loss),
    val.loss = history$metrics$val_loss,
    val.mae = history$metrics$val_mean_absolute_error,
    loss = history$metrics$loss,
    mae = history$metrics$mean_absolute_error)

  new.theme <- theme_gray() + theme(text = element_text(family = 'serif'))

  theme_set(new.theme)

  ggplot(history.df, aes(x = epoch)) +
    geom_line(aes(y = mae)) +
    geom_line(aes(y = val.mae), color = '#FC4E07') +
    scale_x_continuous(breaks = pretty_breaks()) +
    ylab('mean absolute error') +
    ggtitle(file.name)

  suppressMessages(ggsave(paste0(file.name, '.png')))

  ggplot(history.df, aes(x = epoch)) +
    geom_line(aes(y = mae)) +
    geom_line(aes(y = val.mae), color = '#FC4E07') +
    scale_x_continuous(breaks = pretty_breaks(), limits = c(max(history.df$epoch) / 2, NA)) +
    scale_y_continuous(breaks = pretty_breaks(), limits = c(0, max(tail(history.df$val.mae, max(history.df$epoch) / 2)))) +
    ylab('mean absolute error') +
    ggtitle(file.name)

  suppressMessages(ggsave(paste0(file.name, '_1.png')))

  write.csv(history.df, file = paste0(file.name, '.csv'), row.names = FALSE)

}
