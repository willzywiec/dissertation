# tabulate.R
#
# William John Zywiec
# The George Washington University
#
# ...

Tabulate <- function() {

  output.files <- list.files(pattern = '\\.o$')

  # load data set
  if (file.exists('data_set.csv')) {

    data.set <- read.csv('data_set.csv', header = TRUE)

    if (nrow(data.set) >= length(output.files)) {
      Subset(na.omit(data.set))
      return(cat('Loaded data_set.csv\n'))
    }

  } 

  if (length(output.files) > 0) {

    mass <- rad <- dim <- ht <- vol <- conc <- hd <- keff <- sd <- numeric()
    form <- mod <- ref <- shape <- character()

    # tabulate data
    for (i in 1:length(output.files)) {
      if (any(readLines(output.files[i]) %>% grep('final result', .))) {
        output <- readLines(output.files[i]) %>% grep('final result', ., value = TRUE) %>% strsplit('\\s+') %>% unlist()
        file.name <- gsub('\\.o', '', output.files[i]) %>% strsplit('_') %>% unlist()
        mass[i] <- as.numeric(file.name[1])
        form[i] <- file.name[2]
        mod[i] <- file.name[3]
        rad[i] <- as.numeric(file.name[4])
        ref[i] <- file.name[5]
        if (ref[i] == 'none') {
          dim[i] <- 0
          shape[i] <- file.name[6]
        } else {
          dim[i] <- as.numeric(file.name[6])
          shape[i] <- file.name[7]
        }
        if (shape[i] == 'sph') {
          ht[i] <- 2 * rad[i]
        } else if (ref[i] == 'none') {
          ht[i] <- as.numeric(file.name[7])
        } else {
          ht[i] <- as.numeric(file.name[8])
        }
        # volume (cc)
        if (shape[i] == 'sph') {
          vol[i] <- (4/3 * pi * rad[i]^3)
        } else if (shape[i] == 'rcc') {
          vol[i] <- (pi * rad[i]^2 * ht[i])
        } else if (shape[i] == 'rpp') {
          vol[i] <- ((2 * rad[i])^2 * ht[i])
        }
        conc[i] <- (mass[i] / vol[i]) # concentration (g/cc)
        hd[i] <- (ht[i] / (2 * rad[i]))
        keff[i] <- as.numeric(output[4])
        sd[i] <- as.numeric(output[5])
      }
    }

    data.set <- data.frame(
      mass = mass,
      form = form,
      mod = mod,
      rad = rad,
      ref = ref,
      dim = dim,
      shape = shape,
      ht = ht,
      vol = vol,
      conc = conc,
      hd = hd,
      keff = keff,
      sd = sd)

    # write data set to file
    write.csv(data.set, file = 'data_set.csv', row.names = FALSE)
    Subset(na.omit(data.set))
    return(cat('Saved data_set.csv\n'))

  }

}
