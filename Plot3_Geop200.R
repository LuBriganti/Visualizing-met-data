#---
#title: "Geopotencial 200 hPa e intensidad del viento (b.iii)"
#author: "Lucia Briganti"
#date: "March 10, 2019"
#---
  
  
#Select necesary data from array   

geop200<-vector("list",4)
intensidad<-vector("list",4)
coord_intensidad<-vector("list",4)  
coord_geop200<-vector("list",4)  

for (i in 1:4)  { 
  geop200[[i]]<-arreglo[,,which(Plvl==200),"geop",i]  
  intensidad[[i]]<- sqrt((arreglo[,,"200","u",i]^2)+(arreglo[,,"200","u",i]^2))
  coord_intensidad[[i]]<-list(x=lons, y=lats, z=intensidad[[i]])
  coord_geop200[[i]]<-list(x=lons, y=lats, z=geop200[[i]])
}


grid<-list(x=seq(lons[181],lons[361], by = 0.2 ), y = seq(lats[which(lats==-90)],lats[which(lats==0)],   by = 0.2)) #grid de america del sur que quiero interpolar

# fake downscalling
reticula_geop200<-vector("list", 4)
reticula_intensidad<-vector("list", 4)

for ( j in 1:4) { 
  reticula_intensidad[[j]]<-interp.surface.grid(coord_intensidad[[j]],grid)
  reticula_geop200[[j]]<-interp.surface.grid(coord_geop200[[j]],grid)
  
}


#df for plotting

df_geop200<-vector("list",4) 
df_intensidad<-vector("list",4)

for ( i in 1:4 ) { 
  
  df_geop200[[i]]<-data.frame(x = rep(reticula_geop200[[i]]$x , length(reticula_geop200[[i]]$y)), 
                              y = rep(reticula_geop200[[i]]$y , each = length(reticula_geop200[[i]]$x)), z=matrix(reticula_geop200[[i]]$z,length(reticula_geop200[[i]]$x) *    length(reticula_geop200[[i]]$y)))
  
  df_intensidad[[i]]<-data.frame(x = rep(reticula_intensidad[[i]]$x , length(reticula_intensidad[[i]]$y)), 
                                 y = rep(reticula_intensidad[[i]]$y , each = length(reticula_intensidad[[i]]$x)), z=matrix(reticula_intensidad[[i]]$z,length(reticula_intensidad[[i]]$x) *    length(reticula_intensidad[[i]]$y)))
  
}


#prepare some layers for the plot

map.world <- map_data("world2")
my_fill3 <- scale_fill_gradientn(name = expression("int. viento"),
                                 colours = brewer.pal(9, "Greens"), breaks = seq(0,                                                                                                          130,10), limits = c(0, 130))

titulos_grafico3<-c("intensidad del viento 200 hPa - 00Z.06nov.2006","intensidad del viento 200 hPa - 12Z.06nov.2006","intensidad del viento 200 hPa 00Z.07nov.2006","intensidad del viento 200 hPa 12Z.07nov.2006")


# Plotting

save<-c("intensidad1.png","intensidad2.png","intensidad3.png","intensidad4.png")

for ( i in 1:4) { 
  
  plot<- ggplot(df_intensidad[[i]],aes(x=x, y=y)) +  ##ploteo la reticula correspondiente a omega
    geom_tile(aes(fill=df_intensidad[[i]]$z)) +  #relleno con la columna z la grilla
    ggtitle(titulos_grafico3[i]) +
    my_fill3 +   
    geom_map(data=map.world, map = map.world, aes(map_id=region), fill="NA", color="black", inherit.aes = F,size=0.2)  +
    coord_quickmap( xlim = range(df_intensidad[[i]]$x), ylim = range(df_intensidad[[i]]$y), expand = FALSE)  +
    scale_x_continuous(limits = range(df_intensidad[[i]]$x)) +  
    scale_y_continuous(limits = range(df_intensidad[[i]]$y))  +
    geom_contour(data = df_geop200[[i]],aes(x=x, y=y, z=z), breaks=seq(10000,12500,by=100), color = "black" , size= 0.2) +
    xlab("longitud") +
    ylab("latitud")
  
  ggsave(filename=here("Salidas",save[i]), dpi = 300)
  
  
}


# GIF

plot<-c(here("Salidas","intensidad1.png"),here("Salidas","intensidad2.png"),here("Salidas","intensidad3.png"),
        here("Salidas","intensidad4.png"))

gifski(png_files = plot, gif_file = "intensidad.gif", width = 800, height = 600,
       delay = 1, loop = TRUE, progress = TRUE)

`







