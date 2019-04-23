# build.R
#
# William John Zywiec
# The George Washington University
#
# ...
#
# model parameters
# ----------------
# mass:  Pu mass (95% Pu-239, 5% Pu-240)
#
# form:  alpha-phase Pu metal
#        delta-phase Pu metal
#        Pu oxide
#
# mod:   aluminum oxide *
#        beryllium *
#        beryllium oxide *
#        graphite *
#        magnesium oxide *
#        polyethylene
#        sepiolite
#        silicon dioxide *
#        water
#
# rad:   radius (cm)
#
# ref:   aluminum
#        aluminum oxide *
#        beryllium
#        beryllium oxide
#        carbon steel
#        copper
#        depleted uranium *
#        granite *
#        graphite
#        iron *
#        lead *
#        magnesium oxide *
#        molybdenum *
#        nickel *
#        niobium *
#        platinum *
#        polyethylene
#        stainless steel 304
#        stainless steel 304L *
#        stainless steel 316 *
#        stainless steel 316L *
#        tantalum
#        titanium *
#        tungsten *
#        vanadium
#        water
#
# dim:   reflector thickness (cm)
#
# shape: sphere
#        right circular cylinder
#        right parallelipiped
#
# ht:    height (cm)

Build <- function(mass, form, mod, rad, ref, dim, shape, ht) {

  # load packages
  library(parallel)

  # format numbers
  Format <- function(x) formatC(x, format = 'e', digits = 14)

  # set Pu mass (g) and density (g/cc)
  if (form == 'alpha') {
    pu.mass <- mass
    pu.density <- 19.86
  } else if (form == 'delta') {
    pu.mass <- mass
    pu.density <- 15.9
  } else if (form == 'puo2') {
    pu.mass <- mass
    pu.density <- 11.5
  }

  # calculate volume (cc)
  if (shape == 'sph') {
    vol <- 4/3 * pi * rad^3
  } else if (shape == 'rcc') {
    vol <- pi * rad^2 * ht
  } else if (shape == 'rpp') {
    vol <- (2 * rad)^2 * ht
  }

  # reset moderator, volume (cc), and radius (cm)
  if (vol <= pu.mass / pu.density) {
    mod <- 'none'
    vol <- pu.mass / pu.density
    if (shape == 'sph') {
      rad <- (3/4 * vol / pi)^(1/3)
    } else if (shape == 'rcc') {
      rad <- (vol / ht / pi)^(1/2)
    } else if (shape == 'rpp') {
      rad <- ((vol / ht)^(1/2) / 2)
    }
  }

  # reset reflector
  if (dim == 0) {
    ref <- 'none'
  }

  # set densities (g/cc)
  al2o3.density <- 3.97
  be.density <- 1.848
  beo.density <- 3.01
  graphite.density <- 1.7
  mgo.density <- 3.58
  ch2.density <- 0.965
  h2o.density <- 0.998207

  # set moderator density (g/cc)
  if (mod == 'al2o3') {
    mod.density <- al2o3.density
  } else if (mod == 'be') {
    mod.density <- be.density
  } else if (mod == 'beo') {
    mod.density <- beo.density
  } else if (mod == 'graphite') {
    mod.density <- graphite.density
  } else if (mod == 'mgo') {
    mod.density <- mgo.density
  } else if (mod == 'ch2') {
    mod.density <- ch2.density
  } else if (mod == 'sepiolite') {
    mod.density <- 2.14
  } else if (mod == 'sio2') {
    mod.density <- 2.648
  } else if (mod == 'h2o') {
    mod.density <- h2o.density
  } else if (mod == 'none') {
    mod.density <- 0
  }

  # set reflector density (g/cc)
  if (ref == 'al') {
    ref.density <- 2.6989
  } else if (ref == 'al2o3') {
    ref.density <- al2o3.density
  } else if (ref == 'be') {
    ref.density <- be.density
  } else if (ref == 'beo') {
    ref.density <- beo.density
  } else if (ref == 'cs') {
    ref.density <- 7.82
  } else if (ref == 'cu') {
    ref.density <- 8.96
  } else if (ref == 'du') {
    ref.density <- 19.0
  } else if (ref == 'granite') {
    ref.density <- 2.69
  } else if (ref == 'graphite') {
    ref.density <- graphite.density
  } else if (ref == 'fe') {
    ref.density <- 7.874
  } else if (ref == 'pb') {
    ref.density <- 11.35
  } else if (ref == 'mgo') {
    ref.density <- mgo.density
  } else if (ref == 'mo') {
    ref.density <- 10.22
  } else if (ref == 'ni') {
    ref.density <- 8.902
  } else if (ref == 'nb') {
    ref.density <- 8.57
  } else if (ref == 'pt') {
    ref.density <- 21.45
  } else if (ref == 'ch2') {
    ref.density <- ch2.density
  } else if (ref == 'ss304' || ref == 'ss304L' || ref == 'ss316' || ref == 'ss316L') {
    ref.density <- 8.0
  } else if (ref == 'ta') {
    ref.density <- 16.654
  } else if (ref == 'ti') {
    ref.density <- 4.54
  } else if (ref == 'w') {
    ref.density <- 19.3
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

  # calculate weighted mass fraction
  pu.wt <- pu.mass / bulk.mass
  mod.wt <- mod.mass / bulk.mass

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
      '\n3  0 +2', strrep(' ', 27), 'imp:n=0')
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
      '\n4  0 +3', strrep(' ', 27), 'imp:n=0')
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
  if (form != 'puo2' && mod == 'al2o3') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', (-4.70679444984350e-01 * mod.wt) %>% Format(), ' $ O-16',
      '\n\t  13027.80c ', (-5.29320555015650e-01 * mod.wt) %>% Format(), ' $ Al-27',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1 al27.22t')
  } else if (form != 'puo2' && mod == 'be') {
    material.cards <- paste0(
      '\nm1  4009.80c  ', (-mod.wt) %>% Format(), ' $ Be-9',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1 be.20t')
  } else if (form != 'puo2' && mod == 'beo') {
    material.cards <- paste0(
      '\nm1  4009.80c  ', (-3.60384984537289e-01 * mod.wt) %>% Format(), ' $ Be-9',
      '\n\t  8016.80c  ', (-6.39615015462711e-01 * mod.wt) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1 be-o.20t',
      '\n\t  o-be.20t')
  } else if (form != 'puo2' && mod == 'graphite') {
    material.cards <- paste0(
      '\nm1  6000.80c  ', (-mod.wt) %>% Format(), ' $ C',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1 grph.20t')
  } else if (form != 'puo2' && mod == 'mgo') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', (-3.96896476983704e-01 * mod.wt) %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', (-4.70119114481253e-01 * mod.wt) %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', (-6.19996472798894e-02 * mod.wt) %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', (-7.09847612551540e-02 * mod.wt) %>% Format(), ' $ Mg-26',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240')
  } else if (form != 'puo2' && mod == 'ch2') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-1.43701457933504e-01 * mod.wt) %>% Format(), ' $ H-1',
      '\n\t  6000.80c  ', (-8.56298542066496e-01 * mod.wt) %>% Format(), ' $ C',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240')
  } else if (form != 'puo2' && mod == 'sepiolite') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-2.17829111286340e-02 * mod.wt) %>% Format(), ' $ H-1',
      '\n\t  8016.80c  ', (-5.67953140203876e-01 * mod.wt) %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', (-1.16997161651148e-01 * mod.wt) %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', (-1.54296699106214e-02 * mod.wt) %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', (-1.76657688052133e-02 * mod.wt) %>% Format(), ' $ Mg-26',
      '\n\t  14028.80c ', (-2.39030664873055e-01 * mod.wt) %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', (-1.25696735473918e-02 * mod.wt) %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', (-8.57100988006093e-03 * mod.wt) %>% Format(), ' $ Si-30',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240')
  } else if (form != 'puo2' && mod == 'sio2') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', (-5.32481915390405e-01 * mod.wt) %>% Format(), ' $ O-16',
      '\n\t  14028.80c ', (-4.29529075105271e-01 * mod.wt) %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', (-2.25872285300873e-02 * mod.wt) %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', (-1.54017809742357e-02 * mod.wt) %>% Format(), ' $ Si-30',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1 sio2.30t')
  } else if (form != 'puo2' && mod == 'h2o') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-1.11914873272364e-01 * mod.wt) %>% Format(), ' $ H-1',
      '\n\t  8016.80c  ', (-8.88085126727636e-01 * mod.wt) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-0.95 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.wt) %>% Format(), ' $ Pu-240')
  } else if (form != 'puo2' && mod == 'none') {
    material.cards <- paste0(
      '\nm1  94239.80c ', -0.95, ' $ Pu-239',
      '\n\t  94240.80c ', -0.05, ' $ Pu-240')
  } else if (form == 'puo2' && mod == 'al2o3') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', (-4.70679444984350e-01 * mod.wt - 1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  13027.80c ', (-5.29320555015650e-01 * mod.wt) %>% Format(), ' $ Al-27',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1 al27.22t')
  } else if (form == 'puo2' && mod == 'be') {
    material.cards <- paste0(
      '\nm1  4009.80c  ', (-mod.wt) %>% Format(), ' $ Be-9',
      '\n\t  8016.80c  ', (-1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1 be.20t')
  } else if (form == 'puo2' && mod == 'beo') {
    material.cards <- paste0(
      '\nm1  4009.80c  ', (-3.60384984537289e-01 * mod.wt) %>% Format(), ' $ Be-9',
      '\n\t  8016.80c  ', (-6.39615015462711e-01 * mod.wt - 1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1 be-o.20t',
      '\n\t  o-be.20t')
  } else if (form == 'puo2' && mod == 'graphite') {
    material.cards <- paste0(
      '\nm1  6000.80c  ', (-mod.wt) %>% Format(), ' $ C',
      '\n\t  8016.80c  ', (-1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1 grph.20t')
  } else if (form == 'puo2' && mod == 'mgo') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', (-3.96896476983704e-01 * mod.wt - 1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', (-4.70119114481253e-01 * mod.wt) %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', (-6.19996472798894e-02 * mod.wt) %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', (-7.09847612551540e-02 * mod.wt) %>% Format(), ' $ Mg-26',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240')
  } else if (form == 'puo2' && mod == 'ch2') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-1.43701457933504e-01 * mod.wt) %>% Format(), ' $ H-1',
      '\n\t  6000.80c  ', (-8.56298542066496e-01 * mod.wt) %>% Format(), ' $ C',
      '\n\t  8016.80c  ', (-1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240')
  } else if (form == 'puo2' && mod == 'sepiolite') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-2.17829111286340e-02 * mod.wt) %>% Format(), ' $ H-1',
      '\n\t  8016.80c  ', (-5.67953140203876e-01 * mod.wt - 1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', (-1.16997161651148e-01 * mod.wt) %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', (-1.54296699106214e-02 * mod.wt) %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', (-1.76657688052133e-02 * mod.wt) %>% Format(), ' $ Mg-26',
      '\n\t  14028.80c ', (-2.39030664873055e-01 * mod.wt) %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', (-1.25696735473918e-02 * mod.wt) %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', (-8.57100988006093e-03 * mod.wt) %>% Format(), ' $ Si-30',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240')
  } else if (form == 'puo2' && mod == 'sio2') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', (-5.32481915390405e-01 * mod.wt - 1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  14028.80c ', (-4.29529075105271e-01 * mod.wt) %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', (-2.25872285300873e-02 * mod.wt) %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', (-1.54017809742357e-02 * mod.wt) %>% Format(), ' $ Si-30',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240',
      '\nmt1 sio2.30t')
  } else if (form == 'puo2' && mod == 'h2o') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-1.11914873272364e-01 * mod.wt) %>% Format(), ' $ H-1',
      '\n\t  8016.80c  ', (-8.88085126727636e-01 * mod.wt - 1.18003643801257e-01 * pu.wt) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-8.37896538388806e-01 * pu.wt) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-4.40998178099372e-02 * pu.wt) %>% Format(), ' $ Pu-240')
  } else if (form == 'puo2' && mod == 'none') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', -1.18003643801257e-01 %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', -8.37896538388806e-01 %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', -4.40998178099372e-02 %>% Format(), ' $ Pu-240')
  }
  # materials 2 and 3
  if (ref == 'al') {
    material.cards <- paste0(
      material.cards,
      '\nm2  13027.80c +1 $ Al-27',
      '\nmt2 al27.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'be') {
    material.cards <- paste0(
      material.cards,
      '\nm2  4009.80c  +1 $ Be-9',
      '\nmt2 be.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'beo') {
    material.cards <- paste0(
      material.cards,
      '\nm2  4009.80c  +1 $ Be-9',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt2 be-o.20t',
      '\n\t  o-be.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'cs') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  ', -0.005000 %>% Format(), ' $ C',
      '\n\t  26054.80c ', -5.61733047504445e-02 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -9.14420210917578e-01 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -2.14956677132355e-02 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -2.91081661874234e-03 %>% Format(), ' $ Fe-58',
      '\nmt2 fe56.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'cu') {
    material.cards <- paste0(
      material.cards,
      '\nm2  29063.80c ', -6.84994320997273e-01 %>% Format(), ' $ Cu-63',
      '\n\t  29065.80c ', -3.15005679002727e-01 %>% Format(), ' $ Cu-65',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'du') {
    material.cards <- paste0(
      material.cards,
      '\nm2  92234.80c ', -0.000005 %>% Format(), ' $ U-234',
      '\n\t  92235.80c ', -0.002500 %>% Format(), ' $ U-235',
      '\n\t  92238.80c ', -0.997495 %>% Format(), ' $ U-238',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'granite') {
    material.cards <- paste0(
      material.cards,
      '\nm2  8016.80c  ', -0.484170 %>% Format(), ' $ O-16',
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
      '\nmt2 al27.22t',
      '\n\t  sio2.30t',
      '\n\t  fe56.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'graphite') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  +1 $ C',
      '\nmt2 grph.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'fe') {
    material.cards <- paste0(
      material.cards,
      '\nm2  26054.80c ', -5.64555826637633e-02 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -9.19015287354349e-01 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -2.16036861439553e-02 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -2.92544383793200e-03 %>% Format(), ' $ Fe-58',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'pb') {
    material.cards <- paste0(
      material.cards,
      '\nm2  82204.80c ', -1.37808378803449e-02 %>% Format(), ' $ Pb-204',
      '\n\t  82206.80c ', -2.39554999143041e-01 %>% Format(), ' $ Pb-206',
      '\n\t  82207.80c ', -2.20742958347831e-01 %>% Format(), ' $ Pb-207',
      '\n\t  82208.80c ', -5.25921204628783e-01 %>% Format(), ' $ Pb-208',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'mgo') {
    material.cards <- paste0(
      material.cards,
      '\nm2  8016.80c  ', -3.96896476983704e-01 %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', -4.70119114481253e-01 %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', -6.19996472798894e-02 %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', -7.09847612551540e-02 %>% Format(), ' $ Mg-26',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'mo') {
    material.cards <- paste0(
      material.cards,
      '\nm2  42092.80c ', -1.42174367179782e-01 %>% Format(), ' $ Mo-92',
      '\n\t  42094.80c ', -9.05462698581737e-02 %>% Format(), ' $ Mo-94',
      '\n\t  42095.80c ', -1.57498244126087e-01 %>% Format(), ' $ Mo-95',
      '\n\t  42096.80c ', -1.66753727400932e-01 %>% Format(), ' $ Mo-96',
      '\n\t  42097.80c ', -9.64703471057269e-02 %>% Format(), ' $ Mo-97',
      '\n\t  42098.80c ', -2.46265576821488e-01 %>% Format(), ' $ Mo-98',
      '\n\t  42100.80c ', -1.00291467507811e-01 %>% Format(), ' $ Mo-100',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ni') {
    material.cards <- paste0(
      material.cards,
      '\nm2  28058.80c ', -6.71977983510585e-01 %>% Format(), ' $ Ni-58',
      '\n\t  28060.80c ', -2.67758585123918e-01 %>% Format(), ' $ Ni-60',
      '\n\t  28061.80c ', -1.18346279615680e-02 %>% Format(), ' $ Ni-61',
      '\n\t  28062.80c ', -3.83429436164925e-02 %>% Format(), ' $ Ni-62',
      '\n\t  28064.80c ', -1.00858597874365e-02 %>% Format(), ' $ Ni-64',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'nb') {
    material.cards <- paste0(
      material.cards,
      '\nm2  41093.80c +1 $ Nb-93',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'pt') {
    material.cards <- paste0(
      material.cards,
      '\nm2  78190.80c ', -1.36327092879217e-04 %>% Format(), ' $ Pt-190',
      '\n\t  78192.80c ', -7.69505938997349e-03 %>% Format(), ' $ Pt-192',
      '\n\t  78194.80c ', -3.27785500043873e-01 %>% Format(), ' $ Pt-194',
      '\n\t  78195.80c ', -3.38123997074903e-01 %>% Format(), ' $ Pt-195',
      '\n\t  78196.80c ', -2.53567915304668e-01 %>% Format(), ' $ Pt-196',
      '\n\t  78198.80c ', -7.26912010937033e-02 %>% Format(), ' $ Pt-198',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ch2') {
    material.cards <- paste0(
      material.cards,
      '\nm2  1001.80c  +2 $ H-1',
      '\n\t  6000.80c  +1 $ C',
      '\nmt2 poly.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ss304') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  ', -0.000400 %>% Format(), ' $ C',
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
      '\nmt2 fe56.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ss304L') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  ', -0.000150 %>% Format(), ' $ C',
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
      '\nmt2 fe56.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ss316') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  ', -0.000410 %>% Format(), ' $ C',
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
      '\nmt2 fe56.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ss316L') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  ', -0.000300 %>% Format(), ' $ C',
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
      '\nmt2 fe56.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ta') {
    material.cards <- paste0(
      material.cards,
      '\nm2  73181.80c +1 $ Ta-181',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ti') {
    material.cards <- paste0(
      material.cards,
      '\nm2  22046.80c ', -7.92009519060180e-02 %>% Format(), ' $ Ti-46',
      '\n\t  22047.80c ', -7.29778234429076e-02 %>% Format(), ' $ Ti-47',
      '\n\t  22048.80c ', -7.38450522971748e-01 %>% Format(), ' $ Ti-48',
      '\n\t  22049.80c ', -5.53219052658915e-02 %>% Format(), ' $ Ti-49',
      '\n\t  22050.80c ', -5.40487964134349e-02 %>% Format(), ' $ Ti-50',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'w') {
    material.cards <- paste0(
      material.cards,
      '\nm2  74180.80c ', -1.17457548331097e-03 %>% Format(), ' $ W-180',
      '\n\t  74182.80c ', -2.62270494784839e-01 %>% Format(), ' $ W-182',
      '\n\t  74183.80c ', -1.42406025314416e-01 %>% Format(), ' $ W-183',
      '\n\t  74184.80c ', -3.06581920073334e-01 %>% Format(), ' $ W-184',
      '\n\t  74186.80c ', -2.87566984344099e-01 %>% Format(), ' $ W-186',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'v') {
    material.cards <- paste0(
      material.cards,
      '\nm2  23050.80c ', -2.45120335808159e-03 %>% Format(), ' $ V-50',
      '\n\t  23051.80c ', -9.97548796641918e-01 %>% Format(), ' $ V-51',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'h2o') {
    material.cards <- paste0(
      material.cards,
      '\nm2  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt2 lwtr.20t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'none') {
    material.cards <- paste0(
      material.cards,
      '\nm2  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
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
      '\n\t    ', source.coord, ' 0 0',
      '\n\t    0 ', source.coord, ' 0',
      '\n\t    0 0 ', source.coord,
      '\n\t    ', -source.coord, ' 0 0',
      '\n\t    0 ', -source.coord, ' 0',
      '\n\t    0 0 ', -source.coord)
  } else if (shape == 'rcc' || shape == 'rpp') {
    source.cards <- paste0(
      kcode,
      '\nksrc  0 0 ', (ht / 2) %>% round(2),
      '\n\t    ', source.coord, ' 0 ', (ht / 2) %>% round(2),
      '\n\t    0 ', source.coord, ' ', (ht / 2) %>% round(2),
      '\n\t    0 0 ', (5/6 * ht) %>% round(2),
      '\n\t    ', -source.coord, ' 0 ', (ht / 2) %>% round(2),
      '\n\t    0 ', -source.coord, ' ', (ht / 2) %>% round(2),
      '\n\t    0 0 ', (ht / 6) %>% round(2))
  }

  # build f4 tally and stop card
  f4.tally <- paste0(
    'f4:n 1',
    '\nfm4 -1 1 -6 -7',
    '\nsd4 1',
    '\ne4 6.25e-07 0.1 20',
    '\nstop f4 5e-04')

  # write input to file
  file.name <- paste(gsub(' ', '_', title.card)) 
  input.deck <- paste(title.card, 'c', cell.cards, surface.cards, material.cards, 'c', source.cards, 'c', f4.tally, 'c\nprint', sep = '\n')
  write(input.deck, file = paste0(file.name, '.i'))

  # run MCNP
  system(paste0(
    'C:/MCNP/MCNP_CODE/bin/mcnp6 inp=', file.name, '.i ',
    'outp=', file.name, '.o ',
    'runtpe=', file.name, '.runtpe ',
    'srctp=', file.name, '.srctp ',
    'tasks ', detectCores()))

}
