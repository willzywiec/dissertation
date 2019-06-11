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
    epoch = 1:length(history$metrics$mean_absolute_error),
    val.loss = history$metrics$val_loss,
    val.mae = history$metrics$val_mean_absolute_error,
    loss = history$metrics$loss,
    mae = history$metrics$mean_absolute_error)

  new.theme <- theme_gray() + theme(text = element_text(family = 'serif'))

  theme_set(new.theme)

  history.plot <- ggplot(history.df, aes(x = epoch)) +
    geom_line(aes(y = mae)) +
    geom_line(aes(y = val.mae), color = '#FC4E07') +
    ylab('mean absolute error') +
    scale_x_continuous(breaks = pretty_breaks()) +
    ggtitle(title)

  print(history.plot)

  write.csv(history.df, file = paste0(file.name, '.csv'), row.names = FALSE)

}
