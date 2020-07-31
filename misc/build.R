# build.R
#
# William John Zywiec
# The George Washington University
#
# model parameters
# ----------------
# mass:  Pu mass (95% Pu-239, 5% Pu-240) (g)
#
# form:  alpha-phase Pu
#        delta-phase Pu
#        Pu oxide (PuO2)
#
# mod:   aluminum oxide
#        beryllium
#        beryllium oxide
#        graphite
#        magnesium oxide
#        polyethylene
#        sepiolite
#        silicon dioxide
#        water
#
# rad:   radius (cm)
#
# ref:   aluminum
#        aluminum oxide
#        beryllium
#        beryllium oxide
#        carbon steel
#        copper
#        depleted uranium
#        granite
#        graphite
#        iron
#        lead
#        magnesium oxide
#        molybdenum
#        nickel
#        niobium
#        platinum
#        polyethylene
#        stainless steel 304
#        stainless steel 304L
#        stainless steel 316
#        stainless steel 316L
#        tantalum
#        titanium
#        tungsten
#        vanadium
#        water
#
# thk:   reflector thickness (cm)
#
# shape: rcc
#        sph
#
# ht:    height (cm)

Build <- function(mass, form, mod, rad, ref, thk, shape, ht) {

  library(magrittr)
  library(parallel)

  # set format and precision
  Format <- function(x) formatC(x, format = 'e', digits = 14)

  # set Pu mass (g) and density (g/cc)
  if (form == 'alpha') {
    pu.mass <- mass
    pu.density <- 19.86
  } else if (form == 'puo2') {
    pu.mass <- mass
    pu.density <- 11.5
  }

  # calculate vol (cc)
  if (shape == 'sph') {
    vol <- 4/3 * pi * rad^3
  } else if (shape == 'rcc') {
    vol <- pi * rad^2 * ht
  }
  
  # fix mod, vol (cc), and rad (cm)
  if (vol <= pu.mass / pu.density) {
    mod <- 'none'
    vol <- pu.mass / pu.density
    if (shape == 'sph') {
      rad <- (3/4 * vol / pi)^(1/3)
    } else if (shape == 'rcc') {
      rad <- (vol / ht / pi)^(1/2)
    }
  }

  # fix ref and thk (cm)
  if (ref == 'none' || thk == 0) {
    ref <- 'none'
    thk <- 0
  }

  # set material density (g/cc)
  matl.density <- c(
    'al', 2.6989,
    'al2o3', 3.97,
    'be', 1.848,
    'beo', 3.01,
    'cs', 7.82,
    'cu', 8.96,
    'du', 19.0,
    'granite', 2.69,
    'graphite', 1.7,
    'fe', 7.874,
    'pb', 11.35,
    'mgo', 3.58,
    'mo', 10.22,
    'ni', 8.902,
    'nb', 8.57,
    'pt', 21.45,
    'ch2', 0.965,
    'sepiolite', 2.14,
    'sio2', 2.648,
    'ss304', 8.0,
    'ss304L', 8.0,
    'ss316', 8.0,
    'ss316L', 8.0,
    'ta', 16.654,
    'ti', 4.54,
    'w', 19.3,
    'v', 6.0,
    'h2o', 0.998027,
    'none', 0)

  mod.density <- as.numeric(matl.density[match(mod, matl.density) + 1])
  ref.density <- as.numeric(matl.density[match(ref, matl.density) + 1])

  # calculate mod mass (g)
  mod.mass <- mod.density * (vol - pu.mass / pu.density)

  # calculate average density (g/cc)
  avg.density <- (pu.mass + mod.mass) / vol

  # calculate weighted mass fractions
  pu.wt <- pu.mass / (pu.mass + mod.mass)
  mod.wt <- mod.mass / (pu.mass + mod.mass)

  # calculate ref radii (cm)
  ref.rad <- rad + thk
  h2o.rad <- rad + thk + 2.54

  # build title card
  if (ref != 'none' && shape == 'sph') {
    title.card <- paste(mass, form, mod, rad, ref, thk, shape)
  } else if (ref != 'none') {
    title.card <- paste(mass, form, mod, rad, ref, thk, shape, ht)
  } else if (ref == 'none' && shape == 'sph') {
    title.card <- paste(mass, form, mod, rad, ref, shape)
  } else if (ref == 'none') {
    title.card <- paste(mass, form, mod, rad, ref, shape, ht)
  }

  # build cell and surface cards
  if (ref == 'none' || thk == 0) {
    cell.cards <- paste0(
      '1  1 ', -avg.density %>% Format(), ' -1',
      '\n2  2 ', -as.numeric(matl.density[match('h2o', matl.density) + 1]) %>% Format(), ' +1 -2',
      '\n3  0 +2')
    if (shape == 'sph') {
      surface.cards <- paste0(
        '\n1  so  ', rad,
        '\n2  so  ', h2o.rad)
    } else if (shape == 'rcc') {
      surface.cards <- paste0(
        '\n1  rcc  0 0 0 0 0 ', ht, ' ', rad,
        '\n2  rcc  0 0 -2.54 0 0 ', ht + 2.54, ' ', h2o.rad)
    }
  } else {
    cell.cards <- paste0(
      '1  1 ', -avg.density %>% Format(), ' -1',
      '\n2  2 ', -ref.density %>% Format(), ' +1 -2',
      '\n3  3 ', -as.numeric(matl.density[match('h2o', matl.density) + 1]) %>% Format(), ' +2 -3',
      '\n4  0 +3')
    if (shape == 'sph') {
      surface.cards <- paste0(
        '\n1  so  ', rad,
        '\n2  so  ', ref.rad,
        '\n3  so  ', h2o.rad)
    } else if (shape == 'rcc') {
      surface.cards <- paste0(
        '\n1  rcc  0 0 0 0 0 ', ht, ' ', rad,
        '\n2  rcc  0 0 ', -thk, ' 0 0 ', ht + 2 * thk, ' ', ref.rad,
        '\n3  rcc  0 0 ', -thk - 2.54, ' 0 0 ', ht + 2 * thk + 5.08, ' ', h2o.rad)
    }
  }

  # build material cards
  # material 1
  if (form != 'puo2' && mod == 'al2o3') {
    matl.cards <- paste0(
      '\nm1   8016.80c ', (-4.70679444984350e-01 * mod.wt) %>% Format(), ' $ O-16',
      '\n\t  13027.80c ', (-5.29320555015650e-01 * mod.wt) %>% Format(), ' $ Al-27',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1  al27.22t')
  } else if (form != 'puo2' && mod == 'be') {
    matl.cards <- paste0(
      '\nm1   4009.80c ', (-mod.wt) %>% Format(), ' $ Be-9',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1    be.20t')
  } else if (form != 'puo2' && mod == 'beo') {
    matl.cards <- paste0(
      '\nm1   4009.80c ', (-3.60384984537289e-01 * mod.wt) %>% Format(), ' $ Be-9',
      '\n\t   8016.80c ', (-6.39615015462711e-01 * mod.wt) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1  be-o.20t',
      '\n\t   o-be.20t')
  } else if (form != 'puo2' && mod == 'graphite') {
    matl.cards <- paste0(
      '\nm1   6000.80c ', (-mod.wt) %>% Format(), ' $ C',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1  grph.20t')
  } else if (form != 'puo2' && mod == 'mgo') {
    matl.cards <- paste0(
      '\nm1   8016.80c ', (-3.96896476983704e-01 * mod.wt) %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', (-4.70119114481253e-01 * mod.wt) %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', (-6.19996472798894e-02 * mod.wt) %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', (-7.09847612551540e-02 * mod.wt) %>% Format(), ' $ Mg-26',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240')
  } else if (form != 'puo2' && mod == 'ch2') {
    matl.cards <- paste0(
      '\nm1   1001.80c ', (-1.43701457933504e-01 * mod.wt) %>% Format(), ' $ H-1',
      '\n\t   6000.80c ', (-8.56298542066496e-01 * mod.wt) %>% Format(), ' $ C',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1  poly.20t')
  } else if (form != 'puo2' && mod == 'sepiolite') {
    matl.cards <- paste0(
      '\nm1   1001.80c ', (-2.17829111286340e-02 * mod.wt) %>% Format(), ' $ H-1',
      '\n\t   8016.80c ', (-5.67953140203876e-01 * mod.wt) %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', (-1.16997161651148e-01 * mod.wt) %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', (-1.54296699106214e-02 * mod.wt) %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', (-1.76657688052133e-02 * mod.wt) %>% Format(), ' $ Mg-26',
      '\n\t  14028.80c ', (-2.39030664873055e-01 * mod.wt) %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', (-1.25696735473918e-02 * mod.wt) %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', (-8.57100988006093e-03 * mod.wt) %>% Format(), ' $ Si-30',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240')
  } else if (form != 'puo2' && mod == 'sio2') {
    matl.cards <- paste0(
      '\nm1   8016.80c ', (-5.32481915390405e-01 * mod.wt) %>% Format(), ' $ O-16',
      '\n\t  14028.80c ', (-4.29529075105271e-01 * mod.wt) %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', (-2.25872285300873e-02 * mod.wt) %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', (-1.54017809742357e-02 * mod.wt) %>% Format(), ' $ Si-30',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1  sio2.30t')
  } else if (form != 'puo2' && mod == 'h2o') {
    matl.cards <- paste0(
      '\nm1   1001.80c ', (-1.11914873272364e-01 * mod.wt) %>% Format(), ' $ H-1',
      '\n\t   8016.80c ', (-8.88085126727636e-01 * mod.wt) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1  lwtr.20t')
  } else if (form != 'puo2' && mod == 'none') {
    matl.cards <- paste0(
      '\nm1  94239.80c ', -0.95, ' $ Pu-239',
      '\n\t  94240.80c ', -0.05, ' $ Pu-240')
  } else if (form == 'puo2' && mod == 'al2o3') {
    matl.cards <- paste0(
      '\nm1   8016.80c ', (-4.70679444984350e-01 * mod.wt - 1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  13027.80c ', (-5.29320555015650e-01 * mod.wt) %>% Format(), ' $ Al-27',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1  al27.22t')
  } else if (form == 'puo2' && mod == 'be') {
    matl.cards <- paste0(
      '\nm1   4009.80c ', (-mod.wt) %>% Format(), ' $ Be-9',
      '\n\t   8016.80c ', (-1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1    be.20t')
  } else if (form == 'puo2' && mod == 'beo') {
    matl.cards <- paste0(
      '\nm1   4009.80c ', (-3.60384984537289e-01 * mod.wt) %>% Format(), ' $ Be-9',
      '\n\t   8016.80c ', (-6.39615015462711e-01 * mod.wt - 1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1  be-o.20t',
      '\n\t   o-be.20t')
  } else if (form == 'puo2' && mod == 'graphite') {
    matl.cards <- paste0(
      '\nm1   6000.80c ', (-mod.wt) %>% Format(), ' $ C',
      '\n\t   8016.80c ', (-1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1  grph.20t')
  } else if (form == 'puo2' && mod == 'mgo') {
    matl.cards <- paste0(
      '\nm1   8016.80c ', (-3.96896476983704e-01 * mod.wt - 1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', (-4.70119114481253e-01 * mod.wt) %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', (-6.19996472798894e-02 * mod.wt) %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', (-7.09847612551540e-02 * mod.wt) %>% Format(), ' $ Mg-26',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240')
  } else if (form == 'puo2' && mod == 'ch2') {
    matl.cards <- paste0(
      '\nm1   1001.80c ', (-1.43701457933504e-01 * mod.wt) %>% Format(), ' $ H-1',
      '\n\t   6000.80c ', (-8.56298542066496e-01 * mod.wt) %>% Format(), ' $ C',
      '\n\t   8016.80c ', (-1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1  poly.20t')
  } else if (form == 'puo2' && mod == 'sepiolite') {
    matl.cards <- paste0(
      '\nm1   1001.80c ', (-2.17829111286340e-02 * mod.wt) %>% Format(), ' $ H-1',
      '\n\t   8016.80c ', (-5.67953140203876e-01 * mod.wt - 1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', (-1.16997161651148e-01 * mod.wt) %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', (-1.54296699106214e-02 * mod.wt) %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', (-1.76657688052133e-02 * mod.wt) %>% Format(), ' $ Mg-26',
      '\n\t  14028.80c ', (-2.39030664873055e-01 * mod.wt) %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', (-1.25696735473918e-02 * mod.wt) %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', (-8.57100988006093e-03 * mod.wt) %>% Format(), ' $ Si-30',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240')
  } else if (form == 'puo2' && mod == 'sio2') {
    matl.cards <- paste0(
      '\nm1   8016.80c ', (-5.32481915390405e-01 * mod.wt - 1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  14028.80c ', (-4.29529075105271e-01 * mod.wt) %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', (-2.25872285300873e-02 * mod.wt) %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', (-1.54017809742357e-02 * mod.wt) %>% Format(), ' $ Si-30',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1  sio2.30t')
  } else if (form == 'puo2' && mod == 'h2o') {
    matl.cards <- paste0(
      '\nm1   1001.80c ', (-1.11914873272364e-01 * mod.wt) %>% Format(), ' $ H-1',
      '\n\t   8016.80c ', (-8.88085126727636e-01 * mod.wt - 1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1  lwtr.20t')
  } else if (form == 'puo2' && mod == 'none') {
    matl.cards <- paste0(
      '\nm1   8016.80c ', -1.18003643801257e-01 %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', -8.37896538388806e-01 %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', -4.40998178099372e-02 %>% Format(), ' $ Pu-240')
  }
  # materials 2 and 3
  if (ref == 'al') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2  13027.80c +1 $ Al-27',
      '\nmt2  al27.22t',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'al2o3') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2   8016.80c +3 $ O-16',
      '\n\t  13027.80c +2 $ Al-27',
      '\nmt2  al27.22t',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'be') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2   4009.80c +1 $ Be-9',
      '\nmt2    be.20t',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'beo') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2   4009.80c +1 $ Be-9',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt2  be-o.20t',
      '\n\t   o-be.20t',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'cs') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2   6000.80c ', -0.005000 %>% Format(), ' $ C',
      '\n\t  26054.80c ', -5.61733047504445e-02 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -9.14420210917578e-01 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -2.14956677132355e-02 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -2.91081661874234e-03 %>% Format(), ' $ Fe-58',
      '\nmt2  fe56.22t',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'cu') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2  29063.80c ', -6.84994320997273e-01 %>% Format(), ' $ Cu-63',
      '\n\t  29065.80c ', -3.15005679002727e-01 %>% Format(), ' $ Cu-65',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'du') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2  92234.80c ', -0.000005 %>% Format(), ' $ U-234',
      '\n\t  92235.80c ', -0.002500 %>% Format(), ' $ U-235',
      '\n\t  92238.80c ', -0.997495 %>% Format(), ' $ U-238',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'granite') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2   8016.80c ', -0.484170 %>% Format(), ' $ O-16',
      '\n\t  11023.80c ', -0.027328 %>% Format(), ' $ Na-23',
      '\n\t  12024.80c ', -3.33158242094796e-03 %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', -4.39371488246284e-04 %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', -5.03046090805758e-04 %>% Format(), ' $ Mg-26',
      '\n\t  13027.80c ', -0.076188 %>% Format(), ' $ Al-27',
      '\n\t  14028.80c ', -3.08852992862601e-01 %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', -1.62413525330718e-02 %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', -1.10746546043271e-02 %>% Format(), ' $ Si-30',
      '\n\t  19039.80c ', -3.17324306083595e-02 %>% Format(), ' $ K-39',
      '\n\t  19040.80c ', -4.08330016373087e-06 %>% Format(), ' $ K-40',
      '\n\t  19041.80c ', -2.40748609147673e-03 %>% Format(), ' $ K-41',
      '\n\t  20040.80c ', -1.25509788921567e-02 %>% Format(), ' $ Ca-40',
      '\n\t  20042.80c ', -8.79521381703543e-05 %>% Format(), ' $ Ca-42',
      '\n\t  20043.80c ', -1.87891233810042e-05 %>% Format(), ' $ Ca-43',
      '\n\t  20044.80c ', -2.97632447849203e-04 %>% Format(), ' $ Ca-44',
      '\n\t  20046.80c ', -5.95526856619387e-07 %>% Format(), ' $ Ca-46',
      '\n\t  20048.80c ', -2.90518715861230e-05 %>% Format(), ' $ Ca-48',
      '\n\t  22046.80c ', -1.42165708671302e-04 %>% Format(), ' $ Ti-46',
      '\n\t  22047.80c ', -1.30995193080019e-04 %>% Format(), ' $ Ti-47',
      '\n\t  22048.80c ', -1.32551868873429e-03 %>% Format(), ' $ Ti-48',
      '\n\t  22049.80c ', -9.93028199522752e-05 %>% Format(), ' $ Ti-49',
      '\n\t  22050.80c ', -9.70175895621157e-05 %>% Format(), ' $ Ti-50',
      '\n\t  25055.80c ', -0.000387 %>% Format(), ' $ Mn-55',
      '\n\t  26054.80c ', -1.21690008431742e-03 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -1.98093745189230e-02 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -4.65667454832955e-04 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -6.30579419266243e-05 %>% Format(), ' $ Fe-58',
      '\n\t  82204.80c ', -1.38359612318662e-05 %>% Format(), ' $ Pb-204',
      '\n\t  82206.80c ', -2.40513219139613e-04 %>% Format(), ' $ Pb-206',
      '\n\t  82207.80c ', -2.21625930181222e-04 %>% Format(), ' $ Pb-207',
      '\n\t  82208.80c ', -5.28024889447298e-04 %>% Format(), ' $ Pb-208',
      '\nmt2  al27.22t',
      '\n\t   sio2.30t',
      '\n\t   fe56.22t',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'graphite') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2   6000.80c +1 $ C',
      '\nmt2  grph.20t',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'fe') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2  26054.80c ', -5.64555826637633e-02 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -9.19015287354349e-01 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -2.16036861439553e-02 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -2.92544383793200e-03 %>% Format(), ' $ Fe-58',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'pb') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2  82204.80c ', -1.37808378803449e-02 %>% Format(), ' $ Pb-204',
      '\n\t  82206.80c ', -2.39554999143041e-01 %>% Format(), ' $ Pb-206',
      '\n\t  82207.80c ', -2.20742958347831e-01 %>% Format(), ' $ Pb-207',
      '\n\t  82208.80c ', -5.25921204628783e-01 %>% Format(), ' $ Pb-208',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'mgo') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2   8016.80c ', -3.96896476983704e-01 %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', -4.70119114481253e-01 %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', -6.19996472798894e-02 %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', -7.09847612551540e-02 %>% Format(), ' $ Mg-26',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'mo') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2  42092.80c ', -1.42174367179782e-01 %>% Format(), ' $ Mo-92',
      '\n\t  42094.80c ', -9.05462698581737e-02 %>% Format(), ' $ Mo-94',
      '\n\t  42095.80c ', -1.57498244126087e-01 %>% Format(), ' $ Mo-95',
      '\n\t  42096.80c ', -1.66753727400932e-01 %>% Format(), ' $ Mo-96',
      '\n\t  42097.80c ', -9.64703471057269e-02 %>% Format(), ' $ Mo-97',
      '\n\t  42098.80c ', -2.46265576821488e-01 %>% Format(), ' $ Mo-98',
      '\n\t  42100.80c ', -1.00291467507811e-01 %>% Format(), ' $ Mo-100',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'ni') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2  28058.80c ', -6.71977983510585e-01 %>% Format(), ' $ Ni-58',
      '\n\t  28060.80c ', -2.67758585123918e-01 %>% Format(), ' $ Ni-60',
      '\n\t  28061.80c ', -1.18346279615680e-02 %>% Format(), ' $ Ni-61',
      '\n\t  28062.80c ', -3.83429436164925e-02 %>% Format(), ' $ Ni-62',
      '\n\t  28064.80c ', -1.00858597874365e-02 %>% Format(), ' $ Ni-64',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'nb') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2  41093.80c +1 $ Nb-93',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'pt') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2  78000.40c +1 $ Pt',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'ch2') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2   1001.80c +2 $ H-1',
      '\n\t   6000.80c +1 $ C',
      '\nmt2  poly.20t',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'ss304') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2   6000.80c ', -0.000400 %>% Format(), ' $ C',
      '\n\t  14028.80c ', -4.59371614965391e-03 %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', -2.41565292056552e-04 %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', -1.64718558289537e-04 %>% Format(), ' $ Si-30',
      '\n\t  15031.80c ', -0.000230 %>% Format(), ' $ P-31',
      '\n\t  16032.80c ', -1.42151263009614e-04 %>% Format(), ' $ S-32',
      '\n\t  16033.80c ', -1.15708257503575e-06 %>% Format(), ' $ S-33',
      '\n\t  16034.80c ', -6.69137391313452e-06 %>% Format(), ' $ S-34',
      '\n\t  16036.80c ', -2.80502215984187e-10 %>% Format(), ' $ S-36',
      '\n\t  24050.80c ', -7.93000447879800e-03 %>% Format(), ' $ Cr-50',
      '\n\t  24052.80c ', -1.59028788463595e-01 %>% Format(), ' $ Cr-52',
      '\n\t  24053.80c ', -1.83798150490731e-02 %>% Format(), ' $ Cr-53',
      '\n\t  24054.80c ', -4.66139200853358e-03 %>% Format(), ' $ Cr-54',
      '\n\t  25055.80c ', -0.010000 %>% Format(), ' $ Mn-55',
      '\n\t  26054.80c ', -3.96165760226426e-02 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -6.44900597595167e-01 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -1.51599546777977e-02 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -2.05287170439202e-03 %>% Format(), ' $ Fe-58',
      '\n\t  28058.80c ', -6.21579634747292e-02 %>% Format(), ' $ Ni-58',
      '\n\t  28060.80c ', -2.47676691239624e-02 %>% Format(), ' $ Ni-60',
      '\n\t  28061.80c ', -1.09470308644504e-03 %>% Format(), ' $ Ni-61',
      '\n\t  28062.80c ', -3.54672228452556e-03 %>% Format(), ' $ Ni-62',
      '\n\t  28064.80c ', -9.32942030337874e-04 %>% Format(), ' $ Ni-64',
      '\nmt2  fe56.22t',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'ss304L') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2   6000.80c ', -0.000150 %>% Format(), ' $ C',
      '\n\t  14028.80c ', -4.59371614965391e-03 %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', -2.41565292056552e-04 %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', -1.64718558289537e-04 %>% Format(), ' $ Si-30',
      '\n\t  15031.80c ', -0.000230 %>% Format(), ' $ P-31',
      '\n\t  16032.80c ', -1.42151263009614e-04 %>% Format(), ' $ S-32',
      '\n\t  16033.80c ', -1.15708257503575e-06 %>% Format(), ' $ S-33',
      '\n\t  16034.80c ', -6.69137391313452e-06 %>% Format(), ' $ S-34',
      '\n\t  16036.80c ', -2.80502215984187e-10 %>% Format(), ' $ S-36',
      '\n\t  24050.80c ', -7.93000447879800e-03 %>% Format(), ' $ Cr-50',
      '\n\t  24052.80c ', -1.59028788463595e-01 %>% Format(), ' $ Cr-52',
      '\n\t  24053.80c ', -1.83798150490731e-02 %>% Format(), ' $ Cr-53',
      '\n\t  24054.80c ', -4.66139200853358e-03 %>% Format(), ' $ Cr-54',
      '\n\t  25055.80c ', -0.010000 %>% Format(), ' $ Mn-55',
      '\n\t  26054.80c ', -3.92072730483303e-02 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -6.38237736761848e-01 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -1.50033279532540e-02 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -2.03166223656702e-03 %>% Format(), ' $ Fe-58',
      '\n\t  28058.80c ', -6.71977983510585e-02 %>% Format(), ' $ Ni-58',
      '\n\t  28060.80c ', -2.67758585123918e-02 %>% Format(), ' $ Ni-60',
      '\n\t  28061.80c ', -1.18346279615680e-03 %>% Format(), ' $ Ni-61',
      '\n\t  28062.80c ', -3.83429436164925e-03 %>% Format(), ' $ Ni-62',
      '\n\t  28064.80c ', -1.00858597874365e-03 %>% Format(), ' $ Ni-64',
      '\nmt2  fe56.22t',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'ss316') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2   6000.80c ', -0.000410 %>% Format(), ' $ C',
      '\n\t  14028.80c ', -4.65802817574907e-03 %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', -2.44947206145344e-04 %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', -1.67024618105591e-04 %>% Format(), ' $ Si-30',
      '\n\t  15031.80c ', -0.000230 %>% Format(), ' $ P-31',
      '\n\t  16032.80c ', -1.42151263009614e-04 %>% Format(), ' $ S-32',
      '\n\t  16033.80c ', -1.15708257503575e-06 %>% Format(), ' $ S-33',
      '\n\t  16034.80c ', -6.69137391313452e-06 %>% Format(), ' $ S-34',
      '\n\t  16036.80c ', -2.80502215984187e-10 %>% Format(), ' $ S-36',
      '\n\t  24050.80c ', -7.09526716524032e-03 %>% Format(), ' $ Cr-50',
      '\n\t  24052.80c ', -1.42288915993743e-01 %>% Format(), ' $ Cr-52',
      '\n\t  24053.80c ', -1.64450976754864e-02 %>% Format(), ' $ Cr-53',
      '\n\t  24054.80c ', -4.17071916553004e-03 %>% Format(), ' $ Cr-54',
      '\n\t  25055.80c ', -0.010140 %>% Format(), ' $ Mn-55',
      '\n\t  26054.80c ', -3.77687848020577e-02 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -6.14821227240060e-01 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -1.44528660303061e-02 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -1.95712192757651e-03 %>% Format(), ' $ Fe-58',
      '\n\t  28058.80c ', -8.06373580212703e-02 %>% Format(), ' $ Ni-58',
      '\n\t  28060.80c ', -3.21310302148701e-02 %>% Format(), ' $ Ni-60',
      '\n\t  28061.80c ', -1.42015535538816e-03 %>% Format(), ' $ Ni-61',
      '\n\t  28062.80c ', -4.60115323397910e-03 %>% Format(), ' $ Ni-62',
      '\n\t  28064.80c ', -1.21030317449238e-03 %>% Format(), ' $ Ni-64',
      '\n\t  42092.80c ', -3.55435917949455e-03 %>% Format(), ' $ Mo-92',
      '\n\t  42094.80c ', -2.26365674645434e-03 %>% Format(), ' $ Mo-94',
      '\n\t  42095.80c ', -3.93745610315217e-03 %>% Format(), ' $ Mo-95',
      '\n\t  42096.80c ', -4.16884318502330e-03 %>% Format(), ' $ Mo-96',
      '\n\t  42097.80c ', -2.41175867764317e-03 %>% Format(), ' $ Mo-97',
      '\n\t  42098.80c ', -6.15663942053720e-03 %>% Format(), ' $ Mo-98',
      '\n\t  42100.80c ', -2.50728668769527e-03 %>% Format(), ' $ Mo-100',
      '\nmt2  fe56.22t',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'ss316L') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2   6000.80c ', -0.000300 %>% Format(), ' $ C',
      '\n\t  14028.80c ', -9.18743229930782e-03 %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', -4.83130584113104e-04 %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', -3.29437116579075e-04 %>% Format(), ' $ Si-30',
      '\n\t  15031.80c ', -0.000450 %>% Format(), ' $ P-31',
      '\n\t  16032.80c ', -2.84302526019227e-04 %>% Format(), ' $ S-32',
      '\n\t  16033.80c ', -2.31416515007149e-06 %>% Format(), ' $ S-33',
      '\n\t  16034.80c ', -1.33827478262690e-05 %>% Format(), ' $ S-34',
      '\n\t  16036.80c ', -5.61004431968375e-10 %>% Format(), ' $ S-36',
      '\n\t  24050.80c ', -7.09526716524032e-03 %>% Format(), ' $ Cr-50',
      '\n\t  24052.80c ', -1.42288915993743e-01 %>% Format(), ' $ Cr-52',
      '\n\t  24053.80c ', -1.64450976754864e-02 %>% Format(), ' $ Cr-53',
      '\n\t  24054.80c ', -4.17071916553004e-03 %>% Format(), ' $ Cr-54',
      '\n\t  25055.80c ', -0.020000 %>% Format(), ' $ Mn-55',
      '\n\t  26054.80c ', -3.69191282829680e-02 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -6.00990047165377e-01 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -1.41277305538395e-02 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -1.91309399781563e-03 %>% Format(), ' $ Fe-58',
      '\n\t  28058.80c ', -8.06373580212703e-02 %>% Format(), ' $ Ni-58',
      '\n\t  28060.80c ', -3.21310302148701e-02 %>% Format(), ' $ Ni-60',
      '\n\t  28061.80c ', -1.42015535538816e-03 %>% Format(), ' $ Ni-61',
      '\n\t  28062.80c ', -4.60115323397910e-03 %>% Format(), ' $ Ni-62',
      '\n\t  28064.80c ', -1.21030317449238e-03 %>% Format(), ' $ Ni-64',
      '\n\t  42092.80c ', -3.55435917949455e-03 %>% Format(), ' $ Mo-92',
      '\n\t  42094.80c ', -2.26365674645434e-03 %>% Format(), ' $ Mo-94',
      '\n\t  42095.80c ', -3.93745610315217e-03 %>% Format(), ' $ Mo-95',
      '\n\t  42096.80c ', -4.16884318502330e-03 %>% Format(), ' $ Mo-96',
      '\n\t  42097.80c ', -2.41175867764317e-03 %>% Format(), ' $ Mo-97',
      '\n\t  42098.80c ', -6.15663942053720e-03 %>% Format(), ' $ Mo-98',
      '\n\t  42100.80c ', -2.50728668769527e-03 %>% Format(), ' $ Mo-100',
      '\nmt2  fe56.22t',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'ta') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2  73181.80c +1 $ Ta-181',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'ti') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2  22046.80c ', -7.92009519060180e-02 %>% Format(), ' $ Ti-46',
      '\n\t  22047.80c ', -7.29778234429076e-02 %>% Format(), ' $ Ti-47',
      '\n\t  22048.80c ', -7.38450522971748e-01 %>% Format(), ' $ Ti-48',
      '\n\t  22049.80c ', -5.53219052658915e-02 %>% Format(), ' $ Ti-49',
      '\n\t  22050.80c ', -5.40487964134349e-02 %>% Format(), ' $ Ti-50',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'w') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2  74180.80c ', -1.17457548331097e-03 %>% Format(), ' $ W-180',
      '\n\t  74182.80c ', -2.62270494784839e-01 %>% Format(), ' $ W-182',
      '\n\t  74183.80c ', -1.42406025314416e-01 %>% Format(), ' $ W-183',
      '\n\t  74184.80c ', -3.06581920073334e-01 %>% Format(), ' $ W-184',
      '\n\t  74186.80c ', -2.87566984344099e-01 %>% Format(), ' $ W-186',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'v') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2  23050.80c ', -2.45120335808159e-03 %>% Format(), ' $ V-50',
      '\n\t  23051.80c ', -9.97548796641918e-01 %>% Format(), ' $ V-51',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'h2o') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt2  lwtr.20t',
      '\nm3   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt3  lwtr.20t')
  } else if (ref == 'none') {
    matl.cards <- paste0(
      matl.cards,
      '\nm2   1001.80c +2 $ H-1',
      '\n\t   8016.80c +1 $ O-16',
      '\nmt2  lwtr.20t')
  }

  # build data cards
  if (ref == 'none') {
    imp <- 'imp:n 1 1 0'
  } else {
    imp <- 'imp:n 1 1 1 0'
  }

  kcode <- 'kcode 10000 1 50 500'

  source <- (2/3 * rad) %>% round(2)

  if (shape == 'sph') {
    data.cards <- paste0(
      imp,
      '\n',
      kcode,
      '\nksrc  0 0 0',
      '\n\t    ', source, ' 0 0',
      '\n\t    0 ', source, ' 0',
      '\n\t    0 0 ', source,
      '\n\t    ', -source, ' 0 0',
      '\n\t    0 ', -source, ' 0',
      '\n\t    0 0 ', -source)
  } else if (shape == 'rcc') {
    data.cards <- paste0(
      imp,
      '\n',
      kcode,
      '\nksrc  0 0 ', (ht / 2) %>% round(2),
      '\n\t    ', source, ' 0 ', (ht / 2) %>% round(2),
      '\n\t    0 ', source, ' ', (ht / 2) %>% round(2),
      '\n\t    0 0 ', (5/6 * ht) %>% round(2),
      '\n\t    ', -source, ' 0 ', (ht / 2) %>% round(2),
      '\n\t    0 ', -source, ' ', (ht / 2) %>% round(2),
      '\n\t    0 0 ', (ht / 6) %>% round(2)) 
  }

  # write input to file
  file.name <- paste(gsub(' ', '-', title.card)) 
  input.deck <- paste(title.card, 'c', cell.cards, surface.cards, matl.cards, 'c', data.cards, 'c\nprint', sep = '\n')
  write(input.deck, file = paste0(file.name, '.i'))

  # run MCNP
  # if (!file.exists(paste0(file.name, '.o'))) {
  #   system(paste0(
  #     'C:/MCNP/MCNP_CODE/bin/mcnp6 inp=', file.name, '.i ',
  #     'outp=', file.name, '.o ',
  #     'runtpe=', file.name, '.runtpe ',
  #     'srctp=', file.name, '.srctp ',
  #     'tasks ', detectCores()))
  # }

}
