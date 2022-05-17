#---
#title: "Geopotential height and omega at 700 hPa"
#author: "Lucia Briganti"
#date: "March 8, 2019"
#---

#datos geop y omega al nivel de 700      
#creating list of geop and omega at 700 hPa and from there -> lons*lat*geop700 y lons*lat*omega700

geop700<-vector("list",4)
omega700<-vector("list",4)
coord_omega<-vector("list",4)  
coord_geop700<-vector("list",4)  

for (i in 1:4)  { 
  geop700[[i]]<-arreglo[,,which(Plvl==700),"geop",i]  
  omega700[[i]]<-arreglo[,,which(Plvl==700),"omega",i]  
  coord_omega[[i]]<-list(x=lons, y=lats, z=omega700[[i]])
  coord_geop700[[i]]<-list(x=lons, y=lats, z=geop700[[i]])
}

grid<-list(x=seq(lons[181],lons[361], by = 0.2 ), y = seq(lats[which(lats==-90)],lats[which(lats==0)],   by = 0.2)) #grid de america del sur que quiero interpolar

# fake downscalling
reticula_geop700<-vector("list", 4)
reticula_omega<-vector("list", 4)

for ( j in 1:4) { 
  reticula_omega[[j]]<-interp.surface.grid(coord_omega[[j]],grid)
  reticula_geop700[[j]]<-interp.surface.grid(coord_geop700[[j]],grid)
  
}



##prepare df for ggplot

df_geop700<-vector("list",4)
df_omega<-vector("list",4)

for ( i in 1:4 ) { 
  
  df_geop700[[i]]<-data.frame(x = rep(reticula_geop700[[i]]$x , length(reticula_geop700[[i]]$y)), 
                              y = rep(reticula_geop700[[i]]$y , each = length(reticula_geop700[[i]]$x)), z=matrix(reticula_geop700[[i]]$z,length(reticula_geop700[[i]]$x) *    length(reticula_geop700[[i]]$y)))
  
  df_omega[[i]]<-data.frame(x = rep(reticula_omega[[i]]$x , length(reticula_omega[[i]]$y)), 
                            y = rep(reticula_omega[[i]]$y , each = length(reticula_omega[[i]]$x)), z=matrix(reticula_omega[[i]]$z,length(reticula_omega[[i]]$x) *    length(reticula_omega[[i]]$y)))
  
}

#prepare some layers of the plot

map.world <- map_data("world2")  # layer del mapa
my_fill2 <- scale_fill_gradientn(name = expression("OMEGA"),   # el relleno de la reticula ( en este caso omega)
                                 colours = brewer.pal(11, "PiYG"), breaks = seq(-2.6, 2.6, by=0.6), limits = c(-2.6, 2.6))
titulos_grafico2<-c("omega 700 hPa - 00Z.06nov.2006","omega 700 hPa - 12Z.06nov.2006","omega 700 hPa 00Z.07nov.2006","omega 700 hPa 12Z.07nov.2006")




# Plotting

save<-c("omega1.png","omega2.png","omega3.png","omega4.png")

for ( i in 1:4) { 
  
  plot<- ggplot(df_omega[[i]],aes(x=x, y=y)) +  ##ploteo la reticula correspondiente a omega
    geom_tile(aes(fill=df_omega[[i]]$z)) +  #relleno con la columna z la grilla
    ggtitle(titulos_grafico2[i]) +
    my_fill2 +   
    geom_map(data=map.world, map = map.world, aes(map_id=region), fill="NA", color="black", inherit.aes = F,size=0.2)  +
    coord_quickmap( xlim = range(df_omega[[i]]$x), ylim = range(df_omega[[i]]$y), expand = FALSE)  +
    scale_x_continuous(limits = range(df_omega[[i]]$x)) +  
    scale_y_continuous(limits = range(df_omega[[i]]$y))  +
    geom_contour(data = df_geop700[[i]],aes(x=x, y=y, z=z), breaks=seq(2300,3200,by=100), color = "black" , size= 0.2) +
    xlab("longitud") +
    ylab("latitud")
  
  ggsave(filename=here("Salidas",save[i]), dpi = 300)
  
  
}

# GIF for geop at 700 hpa


plot<-c(here("Salidas","omega1.png"),here("Salidas","omega2.png"),here("Salidas","omega3.png"),
        here("Salidas","omega4.png")) #Genero el vector con los archivos a gifear

gifski(png_files = plot, gif_file = "omega.gif", width = 800, height = 600,
       delay = 2, loop = TRUE, progress = TRUE)



