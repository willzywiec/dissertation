# build.R
#
# William John Zywiec
# The George Washington University
#
# ...
#
# model parameters
# ----------------
# mass:        Pu mass (95% Pu-239, 5% Pu-240)
#
# form:        alpha-phase Pu metal
#              delta-phase Pu metal
#              Pu oxide
#
# mod(erator): polyethylene
#              sepiolite
#              water
#
# rad(ius):    radius (cm)
#
# ref(lector): aluminum
#              beryllium
#              beryllium oxide
#              carbon steel
#              copper
#              graphite
#              polyethylene
#              stainless steel 304
#              tantalum
#              vanadium
#              water
#
# dim(ension): reflector thickness (cm)
#
# shape:       sphere
#              cylinder
#              rectangular prism
#
# h(eigh)t:    cylinder or rectangular prism height (cm)

Build <- function(mass, form, mod, rad, ref, dim, shape, ht) {

  # load packages
  library(parallel)

  # format numbers
  Format <- function(x) formatC(x, format = 'e', digits = 5)

  # set Pu mass (g) and density (g/cc)
  if (form == 'alpha') {
    pu.mass <- mass
    pu.density <- 19.86
  } else if (form == 'delta') {
    pu.mass <- mass
    pu.density <- 15.9
  } else if (form == 'oxide') {
    pu.mass <- mass
    pu.density <- 11.5
  }

  # calculate volume (cc)
  if (shape == 'sph') {
    vol <- 4/3 * pi * rad^3
  } else if (shape == 'rcc') {
    vol <- pi * rad^2 * ht
  } else if (shape == 'rpp') {
    vol <- rad^2 * ht
  }

  # reset moderator, volume (cc), and radius (cm)
  if (vol <= pu.mass / pu.density) {
    mod <- 'none'
    vol <- pu.mass / pu.density
    if (shape == 'sph') {
      rad <- (3/4 * vol / pi)^(1/3) %>% round(4)
    } else if (shape == 'rcc') {
      rad <- (vol / ht / pi)^(1/3) %>% round(4)
    } else if (shape == 'rpp') {
      rad <- (vol / ht)^(1/2) %>% round(4)
    }
  }

  # reset reflector
  if (dim == 0) {
    ref <- 'none'
  }

  # set polyethylene and water densities (g/cc)
  ch2.density <- 0.96
  h2o.density <- 0.998207

  # set moderator density (g/cc)
  if (mod == 'ch2') {
    mod.density <- ch2.density
  } else if (mod == 'sepiolite') {
    mod.density <- 2.14
  } else if (mod == 'h2o') {
    mod.density <- h2o.density
  } else if (mod == 'none') {
    mod.density <- 0
  }

  # set reflector density (g/cc)
  if (ref == 'al') {
    ref.density <- 2.6989
  } else if (ref == 'be') {
    ref.density <- 1.848
  } else if (ref == 'beo') {
    ref.density <- 3.01
  } else if (ref == 'cs') {
    ref.density <- 7.82
  } else if (ref == 'cu') {
    ref.density <- 8.96
  } else if (ref == 'c') {
    ref.density <- 1.7
  } else if (ref == 'ch2') {
    ref.density <- ch2.density
  } else if (ref == 'ss304') {
    ref.density <- 8.0
  } else if (ref == 'ta') {
    ref.density <- 16.654
  } else if (ref == 'v') {
    ref.density <- 6.0
  } else if (ref == 'h2o') {
    ref.density <- h2o.density
  } else if (ref == 'none') {
    dim <- 0
  }

  # calculate moderator mass (g)
  mod.mass <- mod.density * (vol - pu.mass / pu.density)

  # calculate bulk mass (g)
  bulk.mass <- pu.mass + mod.mass

  # calculate bulk density (g/cc)
  bulk.density <- (pu.mass + mod.mass) / vol

  # calculate fractional mass (g)
  pu.frac <- pu.mass / bulk.mass
  mod.frac <- mod.mass / bulk.mass

  # calculate reflector dimensions (cm)
  ref.rad <- rad + dim
  h2o.rad <- rad + dim + 2.54

  # build title card
  if (ref != 'none' && shape == 'sph') {
    title.card <- paste(mass, form, mod, rad, ref, dim, shape)
  } else if (ref != 'none') {
    title.card <- paste(mass, form, mod, rad, ref, dim, shape, ht)
  } else if (ref == 'none' && shape == 'sph') {
    title.card <- paste(mass, form, mod, rad, ref, shape)
  } else if (ref == 'none') {
    title.card <- paste(mass, form, mod, rad, ref, shape, ht)
  }

  # build cell and surface cards
  if (ref == 'none' || dim == 0) {
    cell.cards <- paste0(
      '1  1 ', -bulk.density %>% Format(), ' -1     imp:n=1',
      '\n2  2 ', -h2o.density %>% Format(), ' +1 -2  imp:n=1',
      '\n3  0 +2', strrep(' ', 18), 'imp:n=0')
    if (shape == 'sph') {
      surface.cards <- paste0(
        '\n1  so  ', rad,
        '\n2  so  ', h2o.rad)
    } else if (shape == 'rcc') {
      surface.cards <- paste0(
        '\n1  rcc  0 0 0 0 0 ', ht, ' ', rad,
        '\n2  rcc  0 0 -2.54 0 0 ', ht + 2.54, ' ', h2o.rad)
    } else if (shape == 'rpp') {
      surface.cards <- paste0(
        '\n1  rpp  ', -rad, ' ', rad, ' ', -rad, ' ', rad, ' 0 ', ht,
        '\n2  rpp  ', -h2o.rad, ' ', h2o.rad, ' ', -h2o.rad, ' ', h2o.rad, ' ', -2.54, ' ', ht + 2.54)
    }
  } else {
    cell.cards <- paste0(
      '1  1 ', -bulk.density %>% Format(), ' -1     imp:n=1',
      '\n2  2 ', -ref.density %>% Format(), ' +1 -2  imp:n=1',
      '\n3  3 ', -h2o.density %>% Format(), ' +2 -3  imp:n=1',
      '\n4  0 +3', strrep(' ', 18), 'imp:n=0')
    if (shape == 'sph') {
      surface.cards <- paste0(
        '\n1  so  ', rad,
        '\n2  so  ', ref.rad,
        '\n3  so  ', h2o.rad)
    } else if (shape == 'rcc') {
      surface.cards <- paste0(
        '\n1  rcc  0 0 0 0 0 ', ht, ' ', rad,
        '\n2  rcc  0 0 ', -dim, ' 0 0 ', ht + 2 * dim, ' ', ref.rad,
        '\n3  rcc  0 0 ', -dim - 2.54, ' 0 0 ', ht + 2 * dim + 5.08, ' ', h2o.rad)
    } else if (shape == 'rpp') {
      surface.cards <- paste0(
        '\n1  rpp  ', -rad, ' ', rad, ' ', -rad, ' ', rad, ' 0 ', ht,
        '\n2  rpp  ', -ref.rad, ' ', ref.rad, ' ', -ref.rad, ' ', ref.rad, ' ', -dim, ' ', ht + dim,
        '\n3  rpp  ', -h2o.rad, ' ', h2o.rad, ' ', -h2o.rad, ' ', h2o.rad, ' ', -dim - 2.54, ' ', ht + dim + 2.54)
    }
  }

  # build material cards
  # material 1
  if (form != 'oxide' && mod == 'ch2') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-0.143716 * mod.frac) %>% Format(), ' $ H-1',
      '\n    6000.80c  ', (-0.856284 * mod.frac) %>% Format(), ' $ C',
      '\n    94239.80c ', (-0.95 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n    94240.80c ', (-0.05 * pu.frac) %>% Format(), ' $ Pu-240')
  } else if (form != 'oxide' && mod == 'sepiolite') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-0.021782 * mod.frac) %>% Format(), ' $ H-1',
      '\n    8016.80c  ', (-0.568029 * mod.frac) %>% Format(), ' $ O-16',
      '\n    12024.80c ', (-0.118555 * mod.frac) %>% Format(), ' $ Mg-24',
      '\n    12025.80c ', (-0.015007 * mod.frac) %>% Format(), ' $ Mg-25',
      '\n    12026.80c ', (-0.016508 * mod.frac) %>% Format(), ' $ Mg-26',
      '\n    14028.80c ', (-0.239830 * mod.frac) %>% Format(), ' $ Si-28',
      '\n    14029.80c ', (-0.012226 * mod.frac) %>% Format(), ' $ Si-29',
      '\n    14030.80c ', (-0.008064 * mod.frac) %>% Format(), ' $ Si-30',
      '\n    94239.80c ', (-0.95 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n    94240.80c ', (-0.05 * pu.frac) %>% Format(), ' $ Pu-240')
  } else if (form != 'oxide' && mod == 'h2o') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-0.111894 * mod.frac) %>% Format(), ' $ H-1',
      '\n    8016.80c  ', (-0.888106 * mod.frac) %>% Format(), ' $ O-16',
      '\n    94239.80c ', (-0.95 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n    94240.80c ', (-0.05 * pu.frac) %>% Format(), ' $ Pu-240')
  } else if (form != 'oxide' && mod == 'none') {
    material.cards <- paste0(
      '\nm1  94239.80c ', -0.95 %>% Format(), ' $ Pu-239',
      '\n    94240.80c ', -0.05 %>% Format(), ' $ Pu-240')
  } else if (form == 'oxide' && mod == 'ch2') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-0.143716 * mod.frac) %>% Format(), ' $ H-1',
      '\n    6000.80c  ', (-0.856284 * mod.frac) %>% Format(), ' $ C',
      '\n    8016.80c  ', (-0.118030 * pu.frac) %>% Format(), ' $ O-16',
      '\n    94239.80c ', (-0.837871 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n    94240.80c ', (-0.044098 * pu.frac) %>% Format(), ' $ Pu-240')
  } else if (form == 'oxide' && mod == 'sepiolite') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-0.021782 * mod.frac) %>% Format(), ' $ H-1',
      '\n    8016.80c  ', (-0.568029 * mod.frac - 0.118030 * pu.frac) %>% Format(), ' $ O-16',
      '\n    12024.80c ', (-0.118555 * mod.frac) %>% Format(), ' $ Mg-24',
      '\n    12025.80c ', (-0.015007 * mod.frac) %>% Format(), ' $ Mg-25',
      '\n    12026.80c ', (-0.016508 * mod.frac) %>% Format(), ' $ Mg-26',
      '\n    14028.80c ', (-0.239830 * mod.frac) %>% Format(), ' $ Si-28',
      '\n    14029.80c ', (-0.012226 * mod.frac) %>% Format(), ' $ Si-29',
      '\n    14030.80c ', (-0.008064 * mod.frac) %>% Format(), ' $ Si-30',
      '\n    94239.80c ', (-0.837871 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n    94240.80c ', (-0.044098 * pu.frac) %>% Format(), ' $ Pu-240')
  } else if (form == 'oxide' && mod == 'h2o') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-0.111894 * mod.frac) %>% Format(), ' $ H-1',
      '\n    8016.80c  ', (-0.888106 * mod.frac - 0.118030 * pu.frac) %>% Format(), ' $ O-16',
      '\n    94239.80c ', (-0.837871 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n    94240.80c ', (-0.044098 * pu.frac) %>% Format(), ' $ Pu-240')
  } else if (form == 'oxide' && mod == 'none') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', -0.118030 %>% Format(), ' $ O-16',
      '\n    94239.80c ', -0.837871 %>% Format(), ' $ Pu-239',
      '\n    94240.80c ', -0.044098 %>% Format(), ' $ Pu-240')
  }
  # materials 2 and 3
  if (ref == 'al') {
    material.cards <- paste0(
      material.cards,
      '\nm2  13027.80c +1 $ Al-27',
      '\nmt2 al27.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n    8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'be') {
    material.cards <- paste0(
      material.cards,
      '\nm2  4009.80c  +1 $ Be-9',
      '\nmt2 be.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n    8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'beo') {
    material.cards <- paste0(
      material.cards,
      '\nm2  4009.80c  +1 $ Be-9',
      '\n    8016.80c  +1 $ O-16',
      '\nmt2 be-o.20t',
      '\n    o-be.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n    8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'cs') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  ', -0.005 %>% Format(), ' $ C',
      '\n    26054.80c ', -0.058208 %>% Format(), ' $ Fe-54',
      '\n    26056.80c ', -0.912913 %>% Format(), ' $ Fe-56',
      '\n    26057.80c ', -0.021094 %>% Format(), ' $ Fe-57',
      '\n    26058.80c ', -0.002786 %>% Format(), ' $ Fe-58',
      '\nm3  1001.80c  +2 $ H-1',
      '\n    8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'cu') {
    material.cards <- paste0(
      material.cards,
      '\nm2  29063.80c ', -0.6915 %>% Format(), ' $ Cu-63',
      '\n    29065.80c ', -0.3085 %>% Format(), ' $ Cu-65',
      '\nm3  1001.80c  +2 $ H-1',
      '\n    8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'c') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  +1 $ C',
      '\nmt2 grph.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n    8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ch2') {
    material.cards <- paste0(
      material.cards,
      '\nm2  1001.80c  +2 $ H-1',
      '\n    6000.80c  +1 $ C',
      '\nmt2 poly.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n    8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ss304') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  ', -0.0004 %>% Format(), ' $ C',
      '\n    14028.80c ', -0.004610 %>% Format(), ' $ Si-28',
      '\n    14029.80c ', -0.000235 %>% Format(), ' $ Si-29',
      '\n    14030.80c ', -0.000155 %>% Format(), ' $ Si-30',
      '\n    15031.80c ', -0.00023 %>% Format(), ' $ P',
      '\n    16032.80c ', -0.000142485 %>% Format(), ' $ S-32',
      '\n    16033.80c ', -0.000001125 %>% Format(), ' $ S-33',
      '\n    16034.80c ', -0.000006375 %>% Format(), ' $ S-34',
      '\n    16036.80c ', -0.000000015 %>% Format(), ' $ S-36',
      '\n    24050.80c ', -0.008256 %>% Format(), ' $ Cr-50',
      '\n    24052.80c ', -0.159199 %>% Format(), ' $ Cr-52',
      '\n    24053.80c ', -0.095010 %>% Format(), ' $ Cr-53',
      '\n    24054.80c ', -0.023650 %>% Format(), ' $ Cr-54',
      '\n    25055.80c ', -0.01 %>% Format(), ' $ Mn',
      '\n    26054.80c ', -0.041051 %>% Format(), ' $ Fe-54',
      '\n    26056.80c ', -0.643837 %>% Format(), ' $ Fe-56',
      '\n    26057.80c ', -0.014877 %>% Format(), ' $ Fe-57',
      '\n    26058.80c ', -0.001965 %>% Format(), ' $ Fe-58',
      '\n    28058.80c ', -0.062971 %>% Format(), ' $ Ni-58',
      '\n    28060.80c ', -0.024256 %>% Format(), ' $ Ni-60',
      '\n    28061.80c ', -0.001055 %>% Format(), ' $ Ni-61',
      '\n    28062.80c ', -0.003362 %>% Format(), ' $ Ni-62',
      '\n    28064.80c ', -0.000857 %>% Format(), ' $ Ni-64',
      '\nm3  1001.80c  +2 $ H-1',
      '\n    8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ta') {
    material.cards <- paste0(
      material.cards,
      '\nm2  73181.80c +1 $ Ta',
      '\nm3  1001.80c  +2 $ H-1',
      '\n    8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'v') {
    material.cards <- paste0(
      material.cards,
      '\nm2  23050.80c ', -0.0025 %>% Format(), ' $ V-50',
      '\n    23051.80c ', -0.9975 %>% Format(), ' $ V-51',
      '\nm3  1001.80c  +2 $ H-1',
      '\n    8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'h2o') {
    material.cards <- paste0(
      material.cards,
      '\nm2  1001.80c  +2 $ H-1',
      '\n    8016.80c  +1 $ O-16',
      '\nmt2 lwtr.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n    8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'none') {
    material.cards <- paste0(
      material.cards,
      '\nm2  1001.80c  +2 $ H-1',
      '\n    8016.80c  +1 $ O-16',
      '\nmt2 lwtr.20t')
  }

  # calculate source coordinates
  source.coord <- (2/3 * rad) %>% round(2)

  # build source cards
  kcode <- 'kcode 10000 1 50 500'

  if (shape == 'sph') {
    source.cards <- paste0(
      kcode,
      '\nksrc  0 0 0',
      '\n      ', source.coord, ' 0 0',
      '\n      0 ', source.coord, ' 0',
      '\n      0 0 ', source.coord,
      '\n      ', -source.coord, ' 0 0',
      '\n      0 ', -source.coord, ' 0',
      '\n      0 0 ', -source.coord)
  } else if (shape == 'rcc' || shape == 'rpp') {
    source.cards <- paste0(
      kcode,
      '\nksrc  0 0 ', (ht / 2) %>% round(2),
      '\n      ', source.coord, ' 0 ', (ht / 2) %>% round(2),
      '\n      0 ', source.coord, ' ', (ht / 2) %>% round(2),
      '\n      0 0 ', (5/6 * ht) %>% round(2),
      '\n      ', -source.coord, ' 0 ', (ht / 2) %>% round(2),
      '\n      0 ', -source.coord, ' ', (ht / 2) %>% round(2),
      '\n      0 0 ', (ht / 6) %>% round(2))
  }

  # write input to file
  file.name <- paste(gsub(' ', '_', title.card)) 
  input.deck <- paste(title.card, 'c', cell.cards, surface.cards, material.cards, 'c', source.cards, 'print', sep = '\n')
  write(input.deck, file = paste0(file.name, '.i'))

  # run MCNP
  system(paste0(
    'C:/MCNP/MCNP_CODE/bin/mcnp6 inp=', file.name, '.i ',
    'outp=', file.name, '.o ',
    'runtpe=', file.name, '.runtpe ',
    'srctp=', file.name, '.srctp ',
    'tasks ', detectCores()))

}
