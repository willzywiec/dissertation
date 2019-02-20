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

  # reset moderator, volume, and radius
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
  ref.dim <- rad + dim
  h2o.dim <- rad + dim + 2.54

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
      '1  1 ', -bulk.density %>% formatC(, format = 'e', digits = 5), ' -1\t imp:n=1\n',
      '2  2 ', -h2o.density %>% formatC(, format = 'e', digits = 5), ' +1 -2  imp:n=1\n',
      '3  0 +2\t\t\t\t\t imp:n=0\n')
    if (shape == 'sph') {
      surface.cards <- paste0(
        '1  so  ', rad, '\n',
        '2  so  ', h2o.dim, '\n')
    } else if (shape == 'rcc') {
      surface.cards <- paste0(
        '1  rcc  0 0 0 0 0 ', ht, ' ', rad, '\n',
        '2  rcc  0 0 -2.54 0 0 ', ht + 2.54, ' ', h2o.dim, '\n')
    } else if (shape == 'rpp') {
      surface.cards <- paste0(
        '1  rpp  ', -rad, ' ', rad, ' ', -rad, ' ', rad, ' 0 ', ht, '\n',
        '2  rpp  ', -h2o.dim, ' ', h2o.dim, ' ', -h2o.dim, ' ', h2o.dim, ' ', -2.54, ' ', ht + 2.54, '\n')
    }
  } else {
    cell.cards <- paste0(
      '1  1 ', -bulk.density %>% formatC(, format = 'e', digits = 5), ' -1\t imp:n=1\n',
      '2  2 ', -ref.density %>% formatC(, format = 'e', digits = 5), ' +1 -2  imp:n=1\n',
      '3  3 ', -h2o.density %>% formatC(, format = 'e', digits = 5), ' +2 -3  imp:n=1\n',
      '4  0 +3\t\t\t\t\t imp:n=0\n')
    if (shape == 'sph') {
      surface.cards <- paste0(
        '1  so  ', rad, '\n',
        '2  so  ', ref.dim, '\n',
        '3  so  ', h2o.dim, '\n')
    } else if (shape == 'rcc') {
      surface.cards <- paste0(
        '1  rcc  0 0 0 0 0 ', ht, ' ', rad, '\n',
        '2  rcc  0 0 ', -dim, ' 0 0 ', ht + 2 * dim, ' ', ref.dim, '\n',
        '3  rcc  0 0 ', -dim - 2.54, ' 0 0 ', ht + 2 * dim + 5.08, ' ', h2o.dim, '\n')
    } else if (shape == 'rpp') {
      surface.cards <- paste0(
        '1  rpp  ', -rad, ' ', rad, ' ', -rad, ' ', rad, ' 0 ', ht, '\n',
        '2  rpp  ', -ref.dim, ' ', ref.dim, ' ', -ref.dim, ' ', ref.dim, ' ', -dim, ' ', ht + dim, '\n',
        '3  rpp  ', -h2o.dim, ' ', h2o.dim, ' ', -h2o.dim, ' ', h2o.dim, ' ', -dim - 2.54, ' ', ht + dim + 2.54, '\n')
    }
  }

  # build material cards
  # material 1
  if (form != 'oxide' && mod == 'ch2') {
    material.cards <- paste0(
      'm1  1001.80c  ', (-0.143716 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ H-1',
      '\n\t6000.80c  ', (-0.856284 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ C',
      '\n\t94239.80c ', (-0.95 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ Pu-239',
      '\n\t94240.80c ', (-0.05 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ Pu-240')
  } else if (form != 'oxide' && mod == 'sepiolite') {
    material.cards <- paste0(
      'm1  1001.80c  ', (-0.021782 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ H-1',
      '\n\t8016.80c  ', (-0.568029 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ O-16',
      '\n\t12024.80c ', (-0.118555 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ Mg-24',
      '\n\t12025.80c ', (-0.015007 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ Mg-25',
      '\n\t12026.80c ', (-0.016508 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ Mg-26',
      '\n\t14028.80c ', (-0.239830 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ Si-28',
      '\n\t14029.80c ', (-0.012226 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ Si-29',
      '\n\t14030.80c ', (-0.008064 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ Si-30',
      '\n\t94239.80c ', (-0.95 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ Pu-239',
      '\n\t94240.80c ', (-0.05 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ Pu-240')
  } else if (form != 'oxide' && mod == 'h2o') {
    material.cards <- paste0(
      'm1  1001.80c  ', (-0.111894 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ H-1',
      '\n\t8016.80c  ', (-0.888106 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ O-16',
      '\n\t94239.80c ', (-0.95 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ Pu-239',
      '\n\t94240.80c ', (-0.05 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ Pu-240')
  } else if (form != 'oxide' && mod == 'none') {
    material.cards <- paste0(
      'm1  94239.80c ', -0.95 %>% formatC(, format = 'e', digits = 5), ' $ Pu-239',
      '\n\t94240.80c ', -0.05 %>% formatC(, format = 'e', digits = 5), ' $ Pu-240')
  } else if (form == 'oxide' && mod == 'ch2') {
    material.cards <- paste0(
      'm1  1001.80c  ', (-0.143716 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ H-1',
      '\n\t6000.80c  ', (-0.856284 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ C',
      '\n\t8016.80c  ', (-0.118030 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ O-16',
      '\n\t94239.80c ', (-0.837871 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ Pu-239',
      '\n\t94240.80c ', (-0.044098 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ Pu-240')
  } else if (form == 'oxide' && mod == 'sepiolite') {
    material.cards <- paste0(
      'm1  1001.80c  ', (-0.021782 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ H-1',
      '\n\t8016.80c  ', (-0.568029 * mod.frac - 0.118030 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ O-16',
      '\n\t12024.80c ', (-0.118555 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ Mg-24',
      '\n\t12025.80c ', (-0.015007 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ Mg-25',
      '\n\t12026.80c ', (-0.016508 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ Mg-26',
      '\n\t14028.80c ', (-0.239830 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ Si-28',
      '\n\t14029.80c ', (-0.012226 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ Si-29',
      '\n\t14030.80c ', (-0.008064 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ Si-30',
      '\n\t94239.80c ', (-0.837871 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ Pu-239',
      '\n\t94240.80c ', (-0.044098 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ Pu-240')
  } else if (form == 'oxide' && mod == 'h2o') {
    material.cards <- paste0(
      'm1  1001.80c  ', (-0.111894 * mod.frac) %>% formatC(, format = 'e', digits = 5), ' $ H-1',
      '\n\t8016.80c  ', (-0.888106 * mod.frac - 0.118030 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ O-16',
      '\n\t94239.80c ', (-0.837871 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ Pu-239',
      '\n\t94240.80c ', (-0.044098 * pu.frac) %>% formatC(, format = 'e', digits = 5), ' $ Pu-240')
  } else if (form == 'oxide' && mod == 'none') {
    material.cards <- paste0(
      'm1  8016.80c  ', -0.118030 %>% formatC(, format = 'e', digits = 5), ' $ O-16',
      '\n\t94239.80c ', -0.837871 %>% formatC(, format = 'e', digits = 5), ' $ Pu-239',
      '\n\t94240.80c ', -0.044098 %>% formatC(, format = 'e', digits = 5), ' $ Pu-240')
  }
  # materials 2 and 3
  if (ref == 'al') {
    material.cards <- paste0(
      material.cards,
      '\nm2  13027.80c +1 $ Al-27',
      '\nmt2 al27.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'be') {
    material.cards <- paste0(
      material.cards,
      '\nm2  4009.80c  +1 $ Be-9',
      '\nmt2 be.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'beo') {
    material.cards <- paste0(
      material.cards,
      '\nm2  4009.80c  +1 $ Be-9',
      '\n\t8016.80c  +1 $ O-16',
      '\nmt2 be-o.20t',
      '\n\to-be.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'cs') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  ', -0.005 %>% formatC(, format = 'e', digits = 5), ' $ C',
      '\n\t26054.80c ', -0.058208 %>% formatC(, format = 'e', digits = 5), ' $ Fe-54',
      '\n\t26056.80c ', -0.912913 %>% formatC(, format = 'e', digits = 5), ' $ Fe-56',
      '\n\t26057.80c ', -0.021094 %>% formatC(, format = 'e', digits = 5), ' $ Fe-57',
      '\n\t26058.80c ', -0.002786 %>% formatC(, format = 'e', digits = 5), ' $ Fe-58',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'cu') {
    material.cards <- paste0(
      material.cards,
      '\nm2  29063.80c ', -0.6915 %>% formatC(, format = 'e', digits = 5), ' $ Cu-63',
      '\n\t29065.80c ', -0.3085 %>% formatC(, format = 'e', digits = 5), ' $ Cu-65',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'c') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  +1 $ C',
      '\nmt2 grph.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ch2') {
    material.cards <- paste0(
      material.cards,
      '\nm2  1001.80c  +2 $ H-1',
      '\n\t6000.80c  +1 $ C',
      '\nmt2 poly.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ss304') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  ', -0.0004 %>% formatC(, format = 'e', digits = 5), ' $ C',
      '\n\t14028.80c ', -0.004610 %>% formatC(, format = 'e', digits = 5), ' $ Si-28',
      '\n\t14029.80c ', -0.000235 %>% formatC(, format = 'e', digits = 5), ' $ Si-29',
      '\n\t14030.80c ', -0.000155 %>% formatC(, format = 'e', digits = 5), ' $ Si-30',
      '\n\t15031.80c ', -0.00023 %>% formatC(, format = 'e', digits = 5), ' $ P',
      '\n\t16032.80c ', -0.000142485 %>% formatC(, format = 'e', digits = 5), ' $ S-32',
      '\n\t16033.80c ', -0.000001125 %>% formatC(, format = 'e', digits = 5), ' $ S-33',
      '\n\t16034.80c ', -0.000006375 %>% formatC(, format = 'e', digits = 5), ' $ S-34',
      '\n\t16036.80c ', -0.000000015 %>% formatC(, format = 'e', digits = 5), ' $ S-36',
      '\n\t24050.80c ', -0.008256 %>% formatC(, format = 'e', digits = 5), ' $ Cr-50',
      '\n\t24052.80c ', -0.159199 %>% formatC(, format = 'e', digits = 5), ' $ Cr-52',
      '\n\t24053.80c ', -0.095010 %>% formatC(, format = 'e', digits = 5), ' $ Cr-53',
      '\n\t24054.80c ', -0.023650 %>% formatC(, format = 'e', digits = 5), ' $ Cr-54',
      '\n\t25055.80c ', -0.01 %>% formatC(, format = 'e', digits = 5), ' $ Mn',
      '\n\t26054.80c ', -0.041051 %>% formatC(, format = 'e', digits = 5), ' $ Fe-54',
      '\n\t26056.80c ', -0.643837 %>% formatC(, format = 'e', digits = 5), ' $ Fe-56',
      '\n\t26057.80c ', -0.014877 %>% formatC(, format = 'e', digits = 5), ' $ Fe-57',
      '\n\t26058.80c ', -0.001965 %>% formatC(, format = 'e', digits = 5), ' $ Fe-58',
      '\n\t28058.80c ', -0.062971 %>% formatC(, format = 'e', digits = 5), ' $ Ni-58',
      '\n\t28060.80c ', -0.024256 %>% formatC(, format = 'e', digits = 5), ' $ Ni-60',
      '\n\t28061.80c ', -0.001055 %>% formatC(, format = 'e', digits = 5), ' $ Ni-61',
      '\n\t28062.80c ', -0.003362 %>% formatC(, format = 'e', digits = 5), ' $ Ni-62',
      '\n\t28064.80c ', -0.000857 %>% formatC(, format = 'e', digits = 5), ' $ Ni-64',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ta') {
    material.cards <- paste0(
      material.cards,
      '\nm2  73181.80c +1 $ Ta',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'v') {
    material.cards <- paste0(
      material.cards,
      '\nm2  23050.80c ', -0.0025 %>% formatC(, format = 'e', digits = 5), ' $ V-50',
      '\n\t23051.80c ', -0.9975 %>% formatC(, format = 'e', digits = 5), ' $ V-51',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'h2o') {
    material.cards <- paste0(
      material.cards,
      '\nm2  1001.80c  +2 $ H-1',
      '\n\t8016.80c  +1 $ O-16',
      '\nmt2 lwtr.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'none') {
    material.cards <- paste0(
      material.cards,
      '\nm2  1001.80c  +2 $ H-1',
      '\n\t8016.80c  +1 $ O-16',
      '\nmt2 lwtr.20t')
  }

  # calculate source coordinate
  source.coord <- (2/3 * rad) %>% round(2)

  # build source cards
  kcode <- 'kcode 10000 1 50 500'

  if (shape == 'sph') {
    source.cards <- paste0(
      kcode,
      '\nksrc  0 0 0',
      '\n\t  ', source.coord, ' 0 0',
      '\n\t  0 ', source.coord, ' 0',
      '\n\t  0 0 ', source.coord,
      '\n\t  ', -source.coord, ' 0 0',
      '\n\t  0 ', -source.coord, ' 0',
      '\n\t  0 0 ', -source.coord)
  } else if (shape == 'rcc' || shape == 'rpp') {
    source.cards <- paste0(
      kcode,
      '\nksrc  0 0 ', (ht / 2) %>% round(2),
      '\n\t  ', source.coord, ' 0 ', (ht / 2) %>% round(2),
      '\n\t  0 ', source.coord, ' ', (ht / 2) %>% round(2),
      '\n\t  0 0 ', (5/6 * ht) %>% round(2),
      '\n\t  ', -source.coord, ' 0 ', (ht / 2) %>% round(2),
      '\n\t  0 ', -source.coord, ' ', (ht / 2) %>% round(2),
      '\n\t  0 0 ', (ht / 6) %>% round(2))
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
