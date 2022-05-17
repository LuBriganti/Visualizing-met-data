# libraries

require(here)

#Define temporal and spatial resolution 

lons<-seq(0, 360, by=1)
nx=length(lons)
lats<-seq(-90, 90, by=1)
ny=length(lats)
Plvl<-c(1000 ,975, 950, 925, 900, 850, 800, 750, 700, 650, 600, 550, 500, 450, 400, 350, 300, 250, 200, 150, 100)
nz=length(Plvl)
vars<-c("temp","u","v","omega","geop")
nvars=length(vars)
time=c("00","12","24","48")
nt=length(time)
nadata=9.999E+20
N=nx*ny*nz*nvars

#Creating array and fitting file data into it

datos<-list.files(here("DATOS","tema_05"))
arreglo=array(0,c(nx,ny,nz,nvars,4),dimnames = list(lons, # Name for longitudes
                                                    lats, # Name for lats
                                                    Plvl, # Name for level pressures
                                                    vars, # Name for variables 
                                                    time  # Name for time
                                                    )
                                                  )

for (i in 1:4) {
  RAW=readBin(here("DATOS","tema_05",datos[i]), "numeric", n=N, size=4, endian="little")
  arreglo[,,,,i]=array(RAW,c(nx,ny,nz,nvars))
  }


# Checking new array

dimnames(arreglo)



