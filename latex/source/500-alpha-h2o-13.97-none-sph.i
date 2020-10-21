500 alpha h2o 13.97 none sph
c
c cell cards
1  1 -1.03960849037536e+00 -1    $ Pu-H2O sphere density = 1.0396 g/cm^3
2  2 -9.98027000000000e-01 +1 -2 $ H2O reflector density = 0.9980 g/cm^3
3  0 +2

c surface cards
1  so  13.97 $ Pu-H2O sphere radius = 13.97 cm (5.5 in)
2  so  16.51 $ H2O reflector radius = 16.51 cm (6.5 in)

c material cards
m1   1001.80c -1.07201735161097e-01 $ H-1
	   8016.80c -8.50684665694693e-01 $ O-16
	  94239.80c -4.00079191869996e-02 $ Pu-239
	  94240.80c -2.10567995721051e-03 $ Pu-240
mt1  lwtr.20t
m2   1001.80c +2 $ H-1
	   8016.80c +1 $ O-16
mt2  lwtr.20t
c
imp:n 1 1 0
c
c source cards
kcode 10000 1 50 500
ksrc  0 0 0
	    9.31 0 0
	    0 9.31 0
	    0 0 9.31
	    -9.31 0 0
	    0 -9.31 0
	    0 0 -9.31
c
print
