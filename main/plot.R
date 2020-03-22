# plot.R
#
# William John Zywiec
# The George Washington University

Plot <- function(file.name, history) {

  library(ggplot2)
  library(scales)

  new.theme <- theme_gray() + theme(axis.text = element_text(color = "black", size = 10), text = element_text(color = "black", family = "serif", size = 10))
  theme_set(new.theme)

  if (missing(history)) {
    history.df <- read.csv(paste0(file.name, ".csv"), header = TRUE)
  } else {
    history.df <- data.frame(
      epoch = 1:length(history$metrics$val_loss),
      val.loss = history$metrics$val_loss,
      val.mae = history$metrics$val_mean_absolute_error,
      loss = history$metrics$loss,
      mae = history$metrics$mean_absolute_error)
    write.csv(history.df, file = paste0(file.name, ".csv"), row.names = FALSE)
  }

  ggplot(history.df, aes(x = epoch)) +
    geom_line(aes(y = mae)) +
    geom_line(aes(y = val.mae), color = "#FC4E07") +
    scale_x_continuous(breaks = pretty_breaks()) +
    ylab(bquote(mean ~ absolute ~ error ~ (k[eff])))

  suppressMessages(ggsave(paste0(file.name, ".png")))

  ggplot(history.df, aes(x = epoch)) +
    geom_line(aes(y = mae)) +
    geom_line(aes(y = val.mae), color = "#FC4E07") +
    scale_x_continuous(breaks = pretty_breaks(), limits = c(max(history.df$epoch) / 2, NA)) +
    scale_y_continuous(breaks = pretty_breaks(), limits = c(0, max(tail(history.df$val.mae, max(history.df$epoch) / 2)))) +
    ylab(bquote(mean ~ absolute ~ error ~ (k[eff])))

  suppressMessages(ggsave(paste0(file.name, "-tail.png")))

}
