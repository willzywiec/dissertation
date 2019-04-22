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

  # set polyethylene and water densities (g/cc)
  ch2.density <- 0.965
  h2o.density <- 0.998207

  # set moderator density (g/cc)
  if (mod == 'al2o3') {
    mod.density <- 3.97
  } else if (mod == 'be') {
    mod.density <- 1.848
  } else if (mod == 'beo') {
    mod.density <- 3.01
  } else if (mod == 'c') {
    mod.density <- 1.7
  } else if (mod == 'mgo') {
    mod.density <- 3.58
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
    ref.density <- 3.97
  } else if (ref == 'be') {
    ref.density <- 1.848
  } else if (ref == 'beo') {
    ref.density <- 3.01
  } else if (ref == 'cs') {
    ref.density <- 7.82
  } else if (ref == 'cu') {
    ref.density <- 8.96
  } else if (ref == 'du') {
    ref.density <- 19.0
  } else if (ref == 'granite') {
    ref.density <- 2.69
  } else if (ref == 'c') {
    ref.density <- 1.7
  } else if (ref == 'fe') {
    ref.density <- 7.874
  } else if (ref == 'pb') {
    ref.density <- 11.35
  } else if (ref == 'mgo') {
    ref.density <- 3.58
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
  if (form != 'oxide' && mod == 'al2o3') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', (-0.470679444984350 * mod.frac) %>% Format(), ' $ O-16',
      '\n\t  13027.80c ', (-0.529320555015650 * mod.frac) %>% Format(), ' $ Al-27',
      '\n\t  94239.80c ', (-0.95 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.frac) %>% Format(), ' $ Pu-240',
      '\nmt1 al27.22t')
  } else if (form != 'oxide' && mod == 'be') {
    material.cards <- paste0(
      '\nm1  4009.80c  ', (-mod.frac) %>% Format(), ' $ Be-9',
      '\n\t  94239.80c ', (-0.95 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.frac) %>% Format(), ' $ Pu-240',
      '\nmt1 be.20t')
  } else if (form != 'oxide' && mod == 'beo') {
    material.cards <- paste0(
      '\nm1  4009.80c  ', (-0.360384984537289 * mod.frac) %>% Format(), ' $ Be-9',
      '\n\t  8016.80c  ', (-0.639615015462711 * mod.frac) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-0.95 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.frac) %>% Format(), ' $ Pu-240',
      '\nmt1 be-o.20t',
      '\n\t  o-be.20t')
  } else if (form != 'oxide' && mod == 'c') {
    material.cards <- paste0(
      '\nm1  6000.80c  ', (-mod.frac) %>% Format(), ' $ C',
      '\n\t  94239.80c ', (-0.95 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.frac) %>% Format(), ' $ Pu-240',
      '\nmt1 grph.20t')
  } else if (form != 'oxide' && mod == 'mgo') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', (-0.396896476983704 * mod.frac) %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', (-0.470119114481253 * mod.frac) %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', (-0.061999647279889 * mod.frac) %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', (-0.070984761255154 * mod.frac) %>% Format(), ' $ Mg-26',
      '\n\t  94239.80c ', (-0.95 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.frac) %>% Format(), ' $ Pu-240')
  } else if (form != 'oxide' && mod == 'ch2') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-0.143701457933504 * mod.frac) %>% Format(), ' $ H-1',
      '\n\t  6000.80c  ', (-0.856298542066496 * mod.frac) %>% Format(), ' $ C',
      '\n\t  94239.80c ', (-0.95 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.frac) %>% Format(), ' $ Pu-240')
  } else if (form != 'oxide' && mod == 'sepiolite') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-0.021782911128634 * mod.frac) %>% Format(), ' $ H-1',
      '\n\t  8016.80c  ', (-0.567953140203876 * mod.frac) %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', (-0.116997161651148 * mod.frac) %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', (-0.015429669910621 * mod.frac) %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', (-0.017665768805213 * mod.frac) %>% Format(), ' $ Mg-26',
      '\n\t  14028.80c ', (-0.239030664873055 * mod.frac) %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', (-0.012569673547392 * mod.frac) %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', (-0.008571009880061 * mod.frac) %>% Format(), ' $ Si-30',
      '\n\t  94239.80c ', (-0.95 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.frac) %>% Format(), ' $ Pu-240')
  } else if (form != 'oxide' && mod == 'sio2') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', (-0.532481915390405 * mod.frac) %>% Format(), ' $ O-16',
      '\n\t  14028.80c ', (-0.429529075105271 * mod.frac) %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', (-0.022587228530087 * mod.frac) %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', (-0.015401780974236 * mod.frac) %>% Format(), ' $ Si-30',
      '\n\t  94239.80c ', (-0.95 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.frac) %>% Format(), ' $ Pu-240',
      '\nmt1 sio2.30t')
  } else if (form != 'oxide' && mod == 'h2o') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-0.111914873272364 * mod.frac) %>% Format(), ' $ H-1',
      '\n\t  8016.80c  ', (-0.888085126727636 * mod.frac) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-0.95 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.05 * pu.frac) %>% Format(), ' $ Pu-240')
  } else if (form != 'oxide' && mod == 'none') {
    material.cards <- paste0(
      '\nm1  94239.80c ', -0.95 %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', -0.05 %>% Format(), ' $ Pu-240')
  } else if (form == 'oxide' && mod == 'al2o3') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', (-0.470679444984350 * mod.frac - 0.118003643801257 * pu.frac) %>% Format(), ' $ O-16',
      '\n\t  13027.80c ', (-0.529320555015650 * mod.frac) %>% Format(), ' $ Al-27',
      '\n\t  94239.80c ', (-0.837896538388806 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.044099817809937 * pu.frac) %>% Format(), ' $ Pu-240',
      '\nmt1 al27.22t')
  } else if (form == 'oxide' && mod == 'be') {
    material.cards <- paste0(
      '\nm1  4009.80c  ', (-mod.frac) %>% Format(), ' $ Be-9',
      '\n\t  8016.80c  ', (-0.118003643801257 * pu.frac) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-0.837896538388806 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.044099817809937 * pu.frac) %>% Format(), ' $ Pu-240',
      '\nmt1 be.20t')
  } else if (form == 'oxide' && mod == 'beo') {
    material.cards <- paste0(
      '\nm1  4009.80c  ', (-0.360384984537289 * mod.frac) %>% Format(), ' $ Be-9',
      '\n\t  8016.80c  ', (-0.639615015462711 * mod.frac - 0.118003643801257 * pu.frac) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-0.837896538388806 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.044099817809937 * pu.frac) %>% Format(), ' $ Pu-240',
      '\nmt1 be-o.20t',
      '\n\t  o-be.20t')
  } else if (form == 'oxide' && mod == 'c') {
    material.cards <- paste0(
      '\nm1  6000.80c  ', (-mod.frac) %>% Format(), ' $ C',
      '\n\t  8016.80c  ', (-0.118003643801257 * pu.frac) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-0.837896538388806 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.044099817809937 * pu.frac) %>% Format(), ' $ Pu-240',
      '\nmt1 grph.20t')
  } else if (form == 'oxide' && mod == 'mgo') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', (-0.396896476983704 * mod.frac - 0.118003643801257 * pu.frac) %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', (-0.470119114481253 * mod.frac) %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', (-0.061999647279889 * mod.frac) %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', (-0.070984761255154 * mod.frac) %>% Format(), ' $ Mg-26',
      '\n\t  94239.80c ', (-0.837896538388806 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.044099817809937 * pu.frac) %>% Format(), ' $ Pu-240')
  } else if (form == 'oxide' && mod == 'ch2') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-0.143701457933504 * mod.frac) %>% Format(), ' $ H-1',
      '\n\t  6000.80c  ', (-0.856298542066496 * mod.frac) %>% Format(), ' $ C',
      '\n\t  8016.80c  ', (-0.118003643801257 * pu.frac) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-0.837896538388806 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.044099817809937 * pu.frac) %>% Format(), ' $ Pu-240')
  } else if (form == 'oxide' && mod == 'sepiolite') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-0.021782911128634 * mod.frac) %>% Format(), ' $ H-1',
      '\n\t  8016.80c  ', (-0.567953140203876 * mod.frac - 0.118003643801257 * pu.frac) %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', (-0.116997161651148 * mod.frac) %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', (-0.015429669910621 * mod.frac) %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', (-0.017665768805213 * mod.frac) %>% Format(), ' $ Mg-26',
      '\n\t  14028.80c ', (-0.239030664873055 * mod.frac) %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', (-0.012569673547392 * mod.frac) %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', (-0.008571009880061 * mod.frac) %>% Format(), ' $ Si-30',
      '\n\t  94239.80c ', (-0.837896538388806 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.044099817809937 * pu.frac) %>% Format(), ' $ Pu-240')
  } else if (form == 'oxide' && mod = 'sio2') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', (-0.532481915390405 * mod.frac - 0.118003643801257 * pu.frac) %>% Format(), ' $ O-16',
      '\n\t  14028.80c ', (-0.429529075105271 * mod.frac) %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', (-0.022587228530087 * mod.frac) %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', (-0.015401780974236 * mod.frac) %>% Format(), ' $ Si-30',
      '\n\t  94239.80c ', (-0.837896538388806 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.044099817809937 * pu.frac) %>% Format(), ' $ Pu-240',
      '\nmt1 sio2.30t')
  } else if (form == 'oxide' && mod == 'h2o') {
    material.cards <- paste0(
      '\nm1  1001.80c  ', (-0.111914873272364 * mod.frac) %>% Format(), ' $ H-1',
      '\n\t  8016.80c  ', (-0.888085126727636 * mod.frac - 0.118003643801257 * pu.frac) %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', (-0.837896538388806 * pu.frac) %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', (-0.044099817809937 * pu.frac) %>% Format(), ' $ Pu-240')
  } else if (form == 'oxide' && mod == 'none') {
    material.cards <- paste0(
      '\nm1  8016.80c  ', -0.118003643801257 %>% Format(), ' $ O-16',
      '\n\t  94239.80c ', -0.837896538388806 %>% Format(), ' $ Pu-239',
      '\n\t  94240.80c ', -0.044099817809937 %>% Format(), ' $ Pu-240')
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
      '\nm2  6000.80c  ', -0.005 %>% Format(), ' $ C',
      '\n\t  26054.80c ', -0.056173304750445 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -0.914420210917578 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -0.021495667713236 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -0.002910816618742 %>% Format(), ' $ Fe-58',
      '\nmt2 fe56.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'cu') {
    material.cards <- paste0(
      material.cards,
      '\nm2  29063.80c ', -0.684994320997273 %>% Format(), ' $ Cu-63',
      '\n\t  29065.80c ', -0.315005679002727 %>% Format(), ' $ Cu-65',
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
      '\n\t  12024.80c ', -0.003331582420948 %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', -0.000439371488246 %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', -0.000503046090806 %>% Format(), ' $ Mg-26',
      '\n\t  13027.80c ', -0.076188 %>% Format(), ' $ Al-27',
      '\n\t  14028.80c ', -0.308852992862601 %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', -0.016241352533072 %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', -0.011074654604327 %>% Format(), ' $ Si-30',
      '\n\t  19039.80c ', -0.031732430608360 %>% Format(), ' $ K-39',
      '\n\t  19040.80c ', -0.000004083300164 %>% Format(), ' $ K-40',
      '\n\t  19041.80c ', -0.002407486091477 %>% Format(), ' $ K-41',
      '\n\t  20040.80c ', -0.012550978892157 %>% Format(), ' $ Ca-40',
      '\n\t  20042.80c ', -0.000087952138170 %>% Format(), ' $ Ca-42',
      '\n\t  20043.80c ', -0.000018789123381 %>% Format(), ' $ Ca-43',
      '\n\t  20044.80c ', -0.000297632447849 %>% Format(), ' $ Ca-44',
      '\n\t  20046.80c ', -0.000000595526857 %>% Format(), ' $ Ca-46',
      '\n\t  20048.80c ', -0.000029051871586 %>% Format(), ' $ Ca-48',
      '\n\t  22046.80c ', -0.000142165708671 %>% Format(), ' $ Ti-46',
      '\n\t  22047.80c ', -0.000130995193080 %>% Format(), ' $ Ti-47',
      '\n\t  22048.80c ', -0.001325518688734 %>% Format(), ' $ Ti-48',
      '\n\t  22049.80c ', -0.000099302819952 %>% Format(), ' $ Ti-49',
      '\n\t  22050.80c ', -0.000097017589562 %>% Format(), ' $ Ti-50',
      '\n\t  25055.80c ', -0.000387 %>% Format(), ' $ Mn-55',
      '\n\t  26054.80c ', -0.001216900084317 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -0.019809374518923 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -0.000465667454833 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -0.000063057941927 %>% Format(), ' $ Fe-58',
      '\n\t  82204.80c ', -0.000013835961232 %>% Format(), ' $ Pb-204',
      '\n\t  82206.80c ', -0.000240513219140 %>% Format(), ' $ Pb-206',
      '\n\t  82207.80c ', -0.000221625930181 %>% Format(), ' $ Pb-207',
      '\n\t  82208.80c ', -0.000528024889447 %>% Format(), ' $ Pb-208',
      '\nmt2 al27.22t',
      '\n\t  sio2.30t',
      '\n\t  fe56.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'c') {
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
      '\nm2  26054.80c ', -0.056455582663763 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -0.919015287354349 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -0.021603686143955 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -0.002925443837932 %>% Format(), ' $ Fe-58',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'pb') {
    material.cards <- paste0(
      material.cards,
      '\nm2  82204.80c ', -0.013780837880345 %>% Format(), ' $ Pb-204',
      '\n\t  82206.80c ', -0.239554999143041 %>% Format(), ' $ Pb-206',
      '\n\t  82207.80c ', -0.220742958347831 %>% Format(), ' $ Pb-207',
      '\n\t  82208.80c ', -0.525921204628783 %>% Format(), ' $ Pb-208',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'mgo') {
    material.cards <- paste0(
      material.cards,
      '\nm2  8016.80c  ', -0.396896476983704 %>% Format(), ' $ O-16',
      '\n\t  12024.80c ', -0.470119114481253 %>% Format(), ' $ Mg-24',
      '\n\t  12025.80c ', -0.061999647279889 %>% Format(), ' $ Mg-25',
      '\n\t  12026.80c ', -0.070984761255154 %>% Format(), ' $ Mg-26',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'mo') {
    material.cards <- paste0(
      material.cards,
      '\nm2  42092.80c ', -0.142174367179782 %>% Format(), ' $ Mo-92',
      '\n\t  42094.80c ', -0.090546269858174 %>% Format(), ' $ Mo-94',
      '\n\t  42095.80c ', -0.157498244126087 %>% Format(), ' $ Mo-95',
      '\n\t  42096.80c ', -0.166753727400932 %>% Format(), ' $ Mo-96',
      '\n\t  42097.80c ', -0.096470347105727 %>% Format(), ' $ Mo-97',
      '\n\t  42098.80c ', -0.246265576821488 %>% Format(), ' $ Mo-98',
      '\n\t  42100.80c ', -0.100291467507811 %>% Format(), ' $ Mo-100',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ni') {
    material.cards <- paste0(
      material.cards,
      '\nm2  28058.80c ', -0.671977983510585 %>% Format(), ' $ Ni-58',
      '\n\t  28060.80c ', -0.267758585123918 %>% Format(), ' $ Ni-60',
      '\n\t  28061.80c ', -0.011834627961568 %>% Format(), ' $ Ni-61',
      '\n\t  28062.80c ', -0.038342943616493 %>% Format(), ' $ Ni-62',
      '\n\t  28064.80c ', -0.010085859787437 %>% Format(), ' $ Ni-64',
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
      '\nm2  78190.80c ', -0.000136327092879 %>% Format(), ' $ Pt-190',
      '\n\t  78192.80c ', -0.007695059389973 %>% Format(), ' $ Pt-192',
      '\n\t  78194.80c ', -0.327785500043873 %>% Format(), ' $ Pt-194',
      '\n\t  78195.80c ', -0.338123997074903 %>% Format(), ' $ Pt-195',
      '\n\t  78196.80c ', -0.253567915304668 %>% Format(), ' $ Pt-196',
      '\n\t  78198.80c ', -0.072691201093703 %>% Format(), ' $ Pt-198',
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
      '\nm2  6000.80c  ', -0.0004 %>% Format(), ' $ C',
      '\n\t  14028.80c ', -0.004593716149654 %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', -0.000241565292057 %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', -0.000164718558290 %>% Format(), ' $ Si-30',
      '\n\t  15031.80c ', -0.00023 %>% Format(), ' $ P',
      '\n\t  16032.80c ', -0.000142151263010 %>% Format(), ' $ S-32',
      '\n\t  16033.80c ', -0.000001157082575 %>% Format(), ' $ S-33',
      '\n\t  16034.80c ', -0.000006691373913 %>% Format(), ' $ S-34',
      '\n\t  16036.80c ', -0.000000000280502 %>% Format(), ' $ S-36',
      '\n\t  24050.80c ', -0.007930004478798 %>% Format(), ' $ Cr-50',
      '\n\t  24052.80c ', -0.159028788463595 %>% Format(), ' $ Cr-52',
      '\n\t  24053.80c ', -0.018379815049073 %>% Format(), ' $ Cr-53',
      '\n\t  24054.80c ', -0.004661392008534 %>% Format(), ' $ Cr-54',
      '\n\t  25055.80c ', -0.01 %>% Format(), ' $ Mn',
      '\n\t  26054.80c ', -0.039616576022643 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -0.644900597595167 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -0.015159954677798 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -0.002052871704392 %>% Format(), ' $ Fe-58',
      '\n\t  28058.80c ', -0.062157963474729 %>% Format(), ' $ Ni-58',
      '\n\t  28060.80c ', -0.024767669123962 %>% Format(), ' $ Ni-60',
      '\n\t  28061.80c ', -0.001094703086445 %>% Format(), ' $ Ni-61',
      '\n\t  28062.80c ', -0.003546722284526 %>% Format(), ' $ Ni-62',
      '\n\t  28064.80c ', -0.000932942030338 %>% Format(), ' $ Ni-64',
      '\nmt2 fe56.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ss304L') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  ', -0.00015 %>% Format(), ' $ C',
      '\n\t  14028.80c ', -0.004593716149654 %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', -0.000241565292057 %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', -0.000164718558290 %>% Format(), ' $ Si-30',
      '\n\t  15031.80c ', -0.00023 %>% Format(), ' $ P',
      '\n\t  16032.80c ', -0.000142151263010 %>% Format(), ' $ S-32',
      '\n\t  16033.80c ', -0.000001157082575 %>% Format(), ' $ S-33',
      '\n\t  16034.80c ', -0.000006691373913 %>% Format(), ' $ S-34',
      '\n\t  16036.80c ', -0.000000000280502 %>% Format(), ' $ S-36',
      '\n\t  24050.80c ', -0.007930004478798 %>% Format(), ' $ Cr-50',
      '\n\t  24052.80c ', -0.159028788463595 %>% Format(), ' $ Cr-52',
      '\n\t  24053.80c ', -0.018379815049073 %>% Format(), ' $ Cr-53',
      '\n\t  24054.80c ', -0.004661392008534 %>% Format(), ' $ Cr-54',
      '\n\t  25055.80c ', -0.01 %>% Format(), ' $ Mn',
      '\n\t  26054.80c ', -0.039616576022643 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -0.644900597595167 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -0.015159954677798 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -0.002052871704392 %>% Format(), ' $ Fe-58',
      '\n\t  28058.80c ', -0.067197798351059 %>% Format(), ' $ Ni-58',
      '\n\t  28060.80c ', -0.026775858512392 %>% Format(), ' $ Ni-60',
      '\n\t  28061.80c ', -0.001183462796157 %>% Format(), ' $ Ni-61',
      '\n\t  28062.80c ', -0.003834294361649 %>% Format(), ' $ Ni-62',
      '\n\t  28064.80c ', -0.001008585978744 %>% Format(), ' $ Ni-64',
      '\nmt2 fe56.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ss316') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  ', -0.00041 %>% Format(), ' $ C',
      '\n\t  14028.80c ', -0.004658028175749 %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', -0.000244947206145 %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', -0.000167024618106 %>% Format(), ' $ Si-30',
      '\n\t  15031.80c ', -0.00023 %>% Format(), ' $ P',
      '\n\t  16032.80c ', -0.000142151263010 %>% Format(), ' $ S-32',
      '\n\t  16033.80c ', -0.000001157082575 %>% Format(), ' $ S-33',
      '\n\t  16034.80c ', -0.000006691373913 %>% Format(), ' $ S-34',
      '\n\t  16036.80c ', -0.000000000280502 %>% Format(), ' $ S-36',
      '\n\t  24050.80c ', -0.007095267165240 %>% Format(), ' $ Cr-50',
      '\n\t  24052.80c ', -0.142288915993743 %>% Format(), ' $ Cr-52',
      '\n\t  24053.80c ', -0.016445097675486 %>% Format(), ' $ Cr-53',
      '\n\t  24054.80c ', -0.004170719165530 %>% Format(), ' $ Cr-54',
      '\n\t  25055.80c ', -0.01014 %>% Format(), ' $ Mn',
      '\n\t  26054.80c ', -0.037768784802058 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -0.614821227240060 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -0.014452866030306 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -0.001957121927577 %>% Format(), ' $ Fe-58',
      '\n\t  28058.80c ', -0.080637358021270 %>% Format(), ' $ Ni-58',
      '\n\t  28060.80c ', -0.032131030214870 %>% Format(), ' $ Ni-60',
      '\n\t  28061.80c ', -0.001420155355388 %>% Format(), ' $ Ni-61',
      '\n\t  28062.80c ', -0.004601153233979 %>% Format(), ' $ Ni-62',
      '\n\t  28064.80c ', -0.001210303174492 %>% Format(), ' $ Ni-64',
      '\n\t  42092.80c ', -0.003554359179495 %>% Format(), ' $ Mo-92',
      '\n\t  42094.80c ', -0.002263656746454 %>% Format(), ' $ Mo-94',
      '\n\t  42095.80c ', -0.003937456103152 %>% Format(), ' $ Mo-95',
      '\n\t  42096.80c ', -0.004168843185023 %>% Format(), ' $ Mo-96',
      '\n\t  42097.80c ', -0.002411758677643 %>% Format(), ' $ Mo-97',
      '\n\t  42098.80c ', -0.006156639420537 %>% Format(), ' $ Mo-98',
      '\n\t  42100.80c ', -0.002507286687695 %>% Format(), ' $ Mo-100',
      '\nmt2 fe56.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ss316L') {
    material.cards <- paste0(
      material.cards,
      '\nm2  6000.80c  ', -0.0003 %>% Format(), ' $ C',
      '\n\t  14028.80c ', -0.009187432299308 %>% Format(), ' $ Si-28',
      '\n\t  14029.80c ', -0.000483130584113 %>% Format(), ' $ Si-29',
      '\n\t  14030.80c ', -0.000329437116579 %>% Format(), ' $ Si-30',
      '\n\t  15031.80c ', -0.00045 %>% Format(), ' $ P',
      '\n\t  16032.80c ', -0.000284302526019 %>% Format(), ' $ S-32',
      '\n\t  16033.80c ', -0.000002314165150 %>% Format(), ' $ S-33',
      '\n\t  16034.80c ', -0.000013382747826 %>% Format(), ' $ S-34',
      '\n\t  16036.80c ', -0.000000000561004 %>% Format(), ' $ S-36',
      '\n\t  24050.80c ', -0.007095267165240 %>% Format(), ' $ Cr-50',
      '\n\t  24052.80c ', -0.142288915993743 %>% Format(), ' $ Cr-52',
      '\n\t  24053.80c ', -0.016445097675486 %>% Format(), ' $ Cr-53',
      '\n\t  24054.80c ', -0.004170719165530 %>% Format(), ' $ Cr-54',
      '\n\t  25055.80c ', -0.02 %>% Format(), ' $ Mn',
      '\n\t  26054.80c ', -0.036919128282968 %>% Format(), ' $ Fe-54',
      '\n\t  26056.80c ', -0.600990047165377 %>% Format(), ' $ Fe-56',
      '\n\t  26057.80c ', -0.014127730553840 %>% Format(), ' $ Fe-57',
      '\n\t  26058.80c ', -0.001913093997816 %>% Format(), ' $ Fe-58',
      '\n\t  28058.80c ', -0.080637358021270 %>% Format(), ' $ Ni-58',
      '\n\t  28060.80c ', -0.032131030214870 %>% Format(), ' $ Ni-60',
      '\n\t  28061.80c ', -0.001420155355388 %>% Format(), ' $ Ni-61',
      '\n\t  28062.80c ', -0.004601153233979 %>% Format(), ' $ Ni-62',
      '\n\t  28064.80c ', -0.001210303174492 %>% Format(), ' $ Ni-64',
      '\n\t  42092.80c ', -0.003554359179495 %>% Format(), ' $ Mo-92',
      '\n\t  42094.80c ', -0.002263656746454 %>% Format(), ' $ Mo-94',
      '\n\t  42095.80c ', -0.003937456103152 %>% Format(), ' $ Mo-95',
      '\n\t  42096.80c ', -0.004168843185023 %>% Format(), ' $ Mo-96',
      '\n\t  42097.80c ', -0.002411758677643 %>% Format(), ' $ Mo-97',
      '\n\t  42098.80c ', -0.006156639420537 %>% Format(), ' $ Mo-98',
      '\n\t  42100.80c ', -0.002507286687695 %>% Format(), ' $ Mo-100',
      '\nmt2 fe56.22t',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ta') {
    material.cards <- paste0(
      material.cards,
      '\nm2  73181.80c +1 $ Ta',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'ti') {
    material.cards <- paste0(
      material.cards,
      '\nm2  22046.80c ', -0.079200951906018 %>% Format(), ' $ Ti-46',
      '\n\t  22047.80c ', -0.072977823442908 %>% Format(), ' $ Ti-47',
      '\n\t  22048.80c ', -0.738450522971748 %>% Format(), ' $ Ti-48',
      '\n\t  22049.80c ', -0.055321905265892 %>% Format(), ' $ Ti-49',
      '\n\t  22050.80c ', -0.054048796413435 %>% Format(), ' $ Ti-50',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'w') {
    material.cards <- paste0(
      material.cards,
      '\nm2  74180.80c ', -0.001174575483311 %>% Format(), ' $ W-180',
      '\n\t  74182.80c ', -0.262270494784839 %>% Format(), ' $ W-182',
      '\n\t  74183.80c ', -0.142406025314416 %>% Format(), ' $ W-183',
      '\n\t  74184.80c ', -0.306581920073334 %>% Format(), ' $ W-184',
      '\n\t  74186.80c ', -0.287566984344099 %>% Format(), ' $ W-186',
      '\nm3  1001.80c  +2 $ H-1',
      '\n\t  8016.80c  +1 $ O-16',
      '\nmt3 lwtr.20t')
  } else if (ref == 'v') {
    material.cards <- paste0(
      material.cards,
      '\nm2  23050.80c ', -0.002451203358082 %>% Format(), ' $ V-50',
      '\n\t  23051.80c ', -0.997548796641918 %>% Format(), ' $ V-51',
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
