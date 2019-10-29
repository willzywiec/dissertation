# tabulate.R
#
# William John Zywiec
# The George Washington University
#
# ...

Tabulate <- function(source.dir) {

  if (file.exists('data-set.RData')) {

    data.set <- readRDS('data-set.RData')

  } else {

    # load function
    source(paste0(source.dir, '/split.R'))

    output.files <- list.files(pattern = '\\.o$')

    # load output
    if (file.exists('output.csv')) {
      output <- na.omit(read.csv('output.csv'))
      if (nrow(output) >= length(output.files)) {
        cat('Loaded data.csv\n')
        output <- output[sample(nrow(output)), ]
        data.set <- Split(na.omit(output))
      } else {
        remove(output)
      }
    }

    if (!exists('output') && length(output.files) > 0) {

      mass <- rad <- dim <- ht <- vol <- conc <- hd <- keff <- sd <- numeric()
      form <- mod <- ref <- shape <- character()

      # tabulate data
      for (i in 1:length(output.files)) {

        if (any(grep('final result', readLines(output.files[i])))) {

          # Pu mass (g), form, mod, rad (cm), and ref
          file.name <- gsub('\\.o', '', output.files[i]) %>% strsplit('-') %>% unlist()
          mass[i] <- as.numeric(file.name[1])
          form[i] <- file.name[2]
          mod[i] <- file.name[3]
          rad[i] <- as.numeric(file.name[4])
          ref[i] <- file.name[5]

          # dim (cm) and shape
          if (ref[i] == 'none') {
            dim[i] <- 0
            shape[i] <- file.name[6]
          } else {
            dim[i] <- as.numeric(file.name[6])
            shape[i] <- file.name[7]
          }

          # ht (cm)
          if (shape[i] == 'sph') {
            ht[i] <- 2 * rad[i]
          } else if (ref[i] == 'none') {
            ht[i] <- as.numeric(file.name[7])
          } else {
            ht[i] <- as.numeric(file.name[8])
          }

          # vol (cc)
          if (shape[i] == 'sph') {
            vol[i] <- 4/3 * pi * rad[i]^3
          } else if (shape[i] == 'rcc') {
            vol[i] <- pi * rad[i]^2 * ht[i]
          }

          # conc (g/cc) and h/d
          conc[i] <- (mass[i] / vol[i])
          hd[i] <- (ht[i] / (2 * rad[i]))

          # keff and sd
          final.result <- grep('final result', readLines(output.files[i]), value = TRUE) %>% strsplit('\\s+') %>% unlist()
          keff[i] <- as.numeric(final.result[4])
          sd[i] <- as.numeric(final.result[5])

        }

      }

      output <- data.frame(
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

      output <- output[sample(nrow(output)), ]

      # write output to file
      write.csv(output, file = 'output.csv', row.names = FALSE)
      cat('Saved data.csv\n')
      data.set <- Split(na.omit(output))

    } else if (!exists('data') && length(output.files) == 0) {

      stop('Could not find data or output files\n')

    }

  }

  return(data.set)

}
