#lib

library(mapdata)
require("here")
library("fields")
library("ggplot2")
library("maps")
library("RColorBrewer")
library("scales")
library("gridExtra")
library("gganimate")
library("gifski")


##Def list of coords and data of map subster(southamerica) for thickness and geop

geop1000<-vector("list",4)
espesor<-vector("list",4)
coord_espesor<-vector("list",4)  
coord_geop1000<-vector("list",4)  

for (i in 1:4)  { 
  geop1000[[i]]<-arreglo[,,which(Plvl==1000),"geop",i]  
  espesor[[i]]<- arreglo[,,which(Plvl==500),"geop",i] - arreglo[,,which(Plvl==1000),"geop",i]
  coord_espesor[[i]]<-list(x=lons, y=lats, z=espesor[[i]])
  coord_geop1000[[i]]<-list(x=lons, y=lats, z=geop1000[[i]])
}


grid<-list(x=seq(lons[181],lons[361], by = 0.2 ), y = seq(lats[which(lats==-90)],lats[which(lats==0)],   by = 0.2)) #grid de america del sur que quiero interpolar

# fake downscalling 

reticula_geop1000<-vector("list", 4)
reticula_espesor<-vector("list", 4)

for ( j in 1:4) { 
  reticula_espesor[[j]]<-interp.surface.grid(coord_espesor[[j]],grid)
  reticula_geop1000[[j]]<-interp.surface.grid(coord_geop1000[[j]],grid)
  
}


# Creating df

df_geop1000<-vector("list",4)
df_espesor<-vector("list",4)

for ( i in 1:4 ) { 
  
  df_geop1000[[i]]<-data.frame(x = rep(reticula_geop1000[[i]]$x , length(reticula_geop1000[[i]]$y)), 
                               y = rep(reticula_geop1000[[i]]$y , each = length(reticula_geop1000[[i]]$x)), z=matrix(reticula_geop1000[[i]]$z, length(reticula_geop1000[[i]]$x) * length(reticula_geop1000[[i]]$y)))
  
  df_espesor[[i]]<-data.frame(x = rep(reticula_espesor[[i]]$x , length(reticula_espesor[[i]]$y)), 
                              y = rep(reticula_espesor[[i]]$y , each = length(reticula_espesor[[i]]$x)), z=matrix(reticula_espesor[[i]]$z,length(reticula_espesor[[i]]$x) *    length(reticula_espesor[[i]]$y)))
  
}


#prepare some layers for plotting

map.world <- map_data("world2")
my_fill1 <- scale_fill_gradientn(name = expression("espesor"),
                                 colours = rev(brewer.pal(7, "RdYlBu")), breaks = seq(4700,
                                                                                      5900,100), limits = c(4700, 5900))
titulos_grafico<-c("espesor 1000 hPa - 00Z.06nov.2006","espesor 1000 hPa - 12Z.06nov.2006","espesor 1000 hPa 00Z.07nov.2006","espesor 1000 hPa 12Z.07nov.2006")

#---
#title: '"Geopotencial 1000 y espesor 1000/500 (b.i)"'
#author: "Lucia Briganti"
#date: "March 8, 2019"
#---

# Plotting

save<-c("espesor1.png","espesor2.png","espesor3.png","espesor4.png")

for ( i in 1:4) { 
  
  plot<- ggplot(df_espesor[[i]],aes(x=x, y=y)) +  ##ploteo la reticula correspondiente a omega
    geom_tile(aes(fill=df_espesor[[i]]$z)) +  #relleno con la columna z la grilla
    ggtitle(titulos_grafico[i]) +
    my_fill1 +   
    geom_map(data=map.world, map = map.world, aes(map_id=region), fill="NA", color="black", inherit.aes = F,size=0.2)  +
    coord_quickmap( xlim = range(df_espesor[[i]]$x), ylim = range(df_espesor[[i]]$y), expand = FALSE)  +    
    scale_x_continuous(limits = range(df_espesor[[i]]$x)) +  
    scale_y_continuous(limits = range(df_espesor[[i]]$y))  +
    geom_contour(data = df_geop1000[[i]],aes(x=x, y=y, z=z), breaks=seq(0,300,50), color = "black" , size= 0.2) +
    geom_contour(data = df_geop1000[[i]],aes(x=x, y=y, z=z), breaks=seq(-500,-10,50), color = "black" , size= 0.2, linetype="dashed") + 
    xlab("longitud") +
    ylab("latitud")
  
  ggsave(filename=here("Salidas", save[i]), dpi = 300)
  
}


# GIF for omega

plot<-c(here("Salidas","espesor1.png"),here("Salidas","espesor2.png"),here("Salidas","espesor3.png"),
        here("Salidas","espesor4.png"))

library(gifski)
gifski(png_files = plot, gif_file = "GIF1000hPa.gif", width = 800, height = 600,
       delay = 1, loop = TRUE, progress = TRUE)

```