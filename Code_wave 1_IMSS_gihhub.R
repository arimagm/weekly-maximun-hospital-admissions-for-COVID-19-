# Link para descrga de datos de covid: https://www.gob.mx/salud/documentos/datos-abiertos-152127


options(encoding="latin1")  #UTF-8

################## Valores extremos

rm(list = ls())

library(dplyr)
library(readxl)
library(tools)
library(tidyverse)       # %>%
library(xtable)          # xtable

library(SpatialExtremes) # latent
library(spBayesSurv)
library(BayesX)

library(timetk)          # summarise_by_time
library(CARBayesST)
library(CARBayesdata)

library(rgdal)           # readOGR Para importar shapefiles. 
library(geoR)            # pred_grid
library(fields)          # no instalado
library(coda)
library(leaflet)
library(spdep)
library(sp)
library("survival")

library(GGally)
library(ggplot2)
library(ggrepel)
library(classInt)     # classIntervals
library(plotly)       # ggplotly
library(gghighlight)  # gghighlight

library(xtable)       # xtable latex
library(mxmaps)       # df_mxmunicipio
library(plotly)       # ggplotly
library(RColorBrewer) # brewer.pal
library(forcats)
library(tidyr)        #  pivot_wider

library(writexl)      # write_xlsx

library(snpar)       # runs.test
library(posterior)
library(coda)
library(paletteer) # paletteer_c
library(metR)  # geom_contour_fill()

################################################################################
#################################  Shape #######################################


#--- Shape de la CDMX


n.est<-9 # readOGR

shape_cdmx <-st_read("C://Documetos MGM_OS_Dell//Documentos Maria_Dell_OS//Analisis de valores extremos espacial//conjunto_de_datos//09mun.shp",options = "ENCODING=LATIN1") # encoding="latin1"
                   
shape_cdmx <- st_transform(shape_cdmx, sp::CRS("+proj=longlat +datum=WGS84 +no_defs")) # sp::spTransform

shape_cdmx_df<- shape_cdmx

centroides_shape_cdmx <- st_coordinates(st_centroid(shape_cdmx))


# --- Contorno del estado sin municipios: Shape de Mexico con sus 32 estados (no hay municipios)

shape_est_pais <-st_read("C:/Documetos MGM_OS_Dell/Documentos Maria_Dell_OS/Analisis de valores extremos espacial/conjunto_de_datos", layer="areas_geoestadisticas_estatales")

shape_est_pais$CVE_ENT<-as.numeric(shape_est_pais$CVE_ENT)

shape_est <- shape_est_pais[shape_est_pais$CVE_ENT==n.est,] # Elecci�n del estado

shape_est <- st_transform(shape_est, sp::CRS("+proj=longlat +datum=WGS84 +no_defs")) # Cambio de coordenadas

shape_est_df<-fortify(shape_est)  # Contorno del estado  shape_est_df

borde<-cbind(shape_est_df[,1],shape_est_df[,2])

plot(borde)
plot(shape_est_df)

shape_poli_cdmx <-st_read("C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell_OS\\Analisis de valores extremos espacial\\Poligonos de alcaldias de la cdmx\\poligonos_alcaldias_cdmx.shp")

plot(shape_poli_cdmx)
#View(shape_poli_cdmx@data)

##############################################################################################
################################## Hospitales ########################################

#Histórico de Capacidad hospitalaria: https://datos.cdmx.gob.mx/dataset/capacidad-hospitalaria

#--- Nombres de las alcaldías

nom_alcaldias <-c("Azcapotzalco","Coyoacán", "Cuajimalpa de Morelos", "Gustavo A. Madero","Iztacalco", "Iztapalapa",
                  "La Magdalena Contreras","Milpa Alta","Álvaro Obregón", "Tláhuac", "Tlalpan","Xochimilco","Benito Juárez", 
                  "Cuauhtémoc", "Miguel Hidalgo","Venustiano Carranza") 



#--- Mapa de los hospitales IMSS y SSA

imss_ssa<-read.csv("C:/Documetos MGM_OS_Dell/Documentos Maria_Dell_OS/Analisis de valores extremos espacial/imss_ssa.csv")


#View(imss_ssa)

#--- Etiquetas 

centroides_shape<-st_coordinates(st_centroid(shape_cdmx))

nombres_mun<-data.frame(long=centroides_shape[,1],
                        lat=centroides_shape[,2],
                        lab=nom_alcaldias,
                        CVE_MUN=shape_cdmx$CVE_MUN)


imss_ssa<-imss_ssa[1:20,] #28


##################################################################################################################
#################################   Población   #########################################################


#---- Población de la CDMX


dat_cdmx<-df_mxmunicipio_2020%>%
          filter(state_code=="09")%>%
          mutate(CVE_MUN=(municipio_code),Porcentaje=round(pop/sum(pop)*100,2),
                 municipio_name=fct_reorder(municipio_name,pop,.desc = TRUE)) # Ordena las barras en orden ascendente

ggplot(dat_cdmx, aes(x = municipio_name, y = pop, fill = municipio_name)) + 
  geom_bar(stat = "identity",fill="#B0C4DE",width = 0.8)+ 
  theme_gray()+
  theme_bw()+
  labs(x=" ", y="Population")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,hjust = 1.0,vjust = 0.4),legend.position ="none")+  # angle=90,hjust = 1.0,vjust = 1.0
  geom_text(                                      # Total de población sobre las barras
            aes(label = pop),
            position = "stack",
           # hjust = 0.1,
            vjust =  -0.3,
            size = 3,
            #angle=25,
            color = "black")

ggsave("C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell_OS\\Analisis de valores extremos espacial\\Articulo_valores extremos\\Fig_1.eps",dpi = 320,device=cairo_ps)# .eps quita las curvas grices


#--- Mapa de la CDMX con la población

#--- Etiquetas 

nombres_mun<-data.frame(long=centroides_shape_cdmx[,1],
                         lat=centroides_shape_cdmx[,2],
                         lab=shape_cdmx$NOMGEO,
                         CVE_MUN=shape_cdmx$CVE_MUN)


intervalo <- classIntervals(dat_cdmx$Porcentaje, fixedBreaks=c(1.66,3.0,5.0,9.0,12.0,20.0),style="fixed")
punt.cor<-intervalo$brks
offs <- 0.0000001 # para que no aparezca NA
punt.cor[1] <- punt.cor[1] - offs 
punt.cor[length(punt.cor)] <- punt.cor[length(punt.cor)] + offs 


dat_cdmx <-dat_cdmx%>%
           mutate(Porcentaje_cut = cut(Porcentaje, punt.cor,dig.lab = 3)) 

#-- shape_cdmx_df


mapa_final <- shape_cdmx %>%
              left_join(dat_cdmx, by = "CVE_MUN")

ggplot(data = mapa_final) +
      geom_sf(aes(fill = Porcentaje_cut), color = "black")+
      scale_fill_brewer(palette = "Blues") +
      geom_text_repel(data=nombres_mun, aes(long, lat, label=lab),size=2.5)+
      labs(x="Longitude",
           y="Latitude",
           fill="Percentage of population, 2020")+
      coord_sf(crs = 4326, # maya cuadrada
               label_graticule="NW",
               label_axes = list(right="N", bottom="E") # coloca ejer derecho y superior
                   )+   

      theme(
             legend.position = c(.55, 1),
             legend.justification = c("right", "top"),
             legend.box.just = "right",
             legend.margin = margin(6, 6, 6, 6),
             legend.background = element_blank(),
             legend.box.background = element_blank()
            )+
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4)))+
  theme_bw()

ggsave("C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell_OS\\Analisis de valores extremos espacial\\Articulo_valores extremos\\Fig_2.eps",dpi = 320,device=cairo_ps)# .eps quita las curvas grices


###########################################################################################

# --- Centroides

centroides_mun<-data.frame(CVE_MUN=shape_cdmx$CVE_MUN, 
                           long=centroides_shape_cdmx[,1],
                           lat=centroides_shape_cdmx[,2] )

centroides_mun<-centroides_mun%>%
                arrange(CVE_MUN) # ordena en orden desendente


#--- Fecha del dia 24 del mes 11 del año 2021 (año-mes-dia)

DATOS_220702_COVID19_MEXICO<-read.csv("C:/Documetos MGM_OS_Dell/Documentos Maria_Dell_OS/datos_abiertos_covid19_23_05_2022/220702COVID19MEXICO.csv")



dim(datos) # 16638854 


res_lab  <-1   # Positivo covid # RESULTADO_LAB
tp_hosp  <-2   # hospitalizados  #TIPO_PACIENTE
               # SECTOR

datos<-DATOS_220702_COVID19_MEXICO%>%
        dplyr::select(RESULTADO_LAB, TIPO_PACIENTE, SECTOR, ENTIDAD_RES, MUNICIPIO_RES, FECHA_INGRESO)%>%
               mutate(FECHA_INGRESO=as.Date(FECHA_INGRESO),
                      ENTIDAD_RES=as.numeric(ENTIDAD_RES),
                      CVE_MUN=as.numeric(MUNICIPIO_RES))%>% 
        dplyr::filter( RESULTADO_LAB==res_lab,             # Positivo covid
                       TIPO_PACIENTE==tp_hosp,             # Hospitalizados
                       ENTIDAD_RES==n.est,                 # CDMX
                       MUNICIPIO_RES!=999)



datos_github<-DATOS_220702_COVID19_MEXICO%>%
        dplyr::filter( RESULTADO_LAB==res_lab,             # Positivo covid
                       TIPO_PACIENTE==tp_hosp,             # Hospitalizados
                       ENTIDAD_RES==n.est,                 # CDMX
                       MUNICIPIO_RES!=999)

View(datos)
#write_xlsx(datos_github, "C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell_OS\\Analisis de valores extremos espacial\\Articulo_valores extremos\\220702COVID19MEXICO.xlsx")


#--- Número de casos por institución de salud


dat_var_sector <- datos%>%
                  dplyr::filter(FECHA_INGRESO<= "2022-03-06")%>% # Fecha de término de la 4ta ola
                  group_by(CVE_MUN,SECTOR)%>%
                  summarise(Total=n())


dat_var_sector<-as.data.frame(dat_var_sector)


xtable(data.frame(t(dat_var_sector$Total))) # Número de casos por instituci�n



#--- Totales para la CDMX por institucion


dat_var_sector <- datos%>%
                   dplyr::filter(FECHA_INGRESO<= "2022-03-06")%>% # Fecha de término de la 4ta ola
                   group_by(CVE_MUN,SECTOR)%>%
                   table()


dat_var_sector<-as.data.frame(dat_var_sector)

res<-dat_var_sector%>%
     group_by(SECTOR)%>%
     summarise(Total=sum(Freq))

res


#---- Análisis casos del IMSS

sec_imss <- 4 

cov_pos<-datos%>%
         dplyr::filter(SECTOR==sec_imss)%>%  
         mutate(Year=as.numeric(substr(FECHA_INGRESO,start=1,stop=4)),
                Mes =as.numeric(substr(FECHA_INGRESO,start=6,stop=7)),
                Dia =as.numeric(substr(FECHA_INGRESO,start=9,stop=10)))

dim(cov_pos)  # 33608  


#View(cov_pos)
##-----------------------------------------

#-- Usando la función summarise_by_time: ola 1 para la IMSS

#-- Alcaldías con los máximos de las semanas epidemiológicas

dat_hosp_imss<-cov_pos%>%
               dplyr::filter(FECHA_INGRESO>= "2020-02-17" &  FECHA_INGRESO<= "2020-09-27")%>%  # Fecha de la 1er ola
               dplyr:: group_by(SECTOR, CVE_MUN, FECHA_INGRESO)%>%
               count()%>%
               summarise_by_time( .date_var = FECHA_INGRESO,
                                  .by       = "week",  # Agrupa por semana   # hay que hacerlo con un día
                                  VMax      = max(n))%>%  # Obtiene el máximo   # hay que checar que este agrupando por día, de lo contario hay que agrupar dos veces  
              mutate(MesDia=as.numeric(paste0(str_sub(FECHA_INGRESO, start = 6, end = 7),str_sub(FECHA_INGRESO, start = 9, end = 10))))


dat_hosp_imss$Alcaldias<-recode(as.factor(dat_hosp_imss$CVE_MUN),
                               "2"="Azcapotzalco",     "3"="Coyoacán",     "4"="Cuajimalpa de Morelos",   "5"="Gustavo A. Madero",
                               "6"="Iztacalco",        "7"="Iztapalapa",   "8"="La Magdalena Contreras",  "9"="Milpa Alta",
                               "10"="Álvaro Obregón",  "11"="Tláhuac",     "12"="Tlalpan",                "13"="Xochimilco",
                               "14"="Benito Juárez",   "15"="Cuauhtémoc",  "16"="Miguel Hidalgo",         "17"="Venustiano Carranza") 

dim(dat_hosp_imss)

max(dat_hosp_imss$FECHA_INGRESO)

min(dat_hosp_imss$FECHA_INGRESO)

View(dat_hosp_imss)

#--- Curvas de las Alcaldías

dat_hosp_imss %>%
mutate(Alcaldias= fct_reorder2(Alcaldias, FECHA_INGRESO, VMax))%>%
ggplot(aes(FECHA_INGRESO, VMax, color=Alcaldias )) +
       geom_point() + 
       geom_line()+
       gghighlight(max(VMax) > 15,use_direct_label = FALSE)+
       scale_x_date(breaks =dat_hosp_imss$FECHA_INGRESO, 
                    date_labels="%d %b %y") +
       scale_y_continuous(breaks = c(1,5, 10, 15, 20, 25, 30, 35, 40))+
       labs(title = "", x = "Weeks",y = "Weekly maximum hospital admissions", colour ="Districts") +
       theme_gray()+
       theme_bw()+
       theme(text = element_text(size = 10), 
             axis.text.x = element_text(angle =90,hjust = 1, vjust = 0.4))


ggsave("C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell_OS\\Analisis de valores extremos espacial\\Articulo_valores extremos\\Fig3.eps",dpi = 320,device=cairo_ps)# .eps quita las curvas grices


######################################
# Pasar a formato ancho

View(dat_hosp_imss)  # Datos para el análisis

datos_anchos <- dat_hosp_imss[,c(3,4,6)] %>%
                pivot_wider(
                             names_from = Alcaldias,   
                             values_from = VMax   
                           )%>%
                mutate(Fecha = as.character(FECHA_INGRESO))%>% 
                select(-FECHA_INGRESO)%>%
                relocate(Fecha, .before=Azcapotzalco)%>% 
                arrange(Fecha) %>%
                mutate(across(where(is.numeric), ~ coalesce(.x, 0)))

str(datos_anchos)

View(datos_anchos)

xtable(datos_anchos) # Resultados en formato latex


#------------------------------------------------
# Evaluación de la dependencia temporal

dat <- data.frame(X=rep(0,16,),pV=rep(0,16))

j<-1

for(i in 2:17)
   {
      dat[j,1]  <- runs.test(as.vector(datos_anchos[[i]]))$statistic  # Box.test(na.omit(datos_anchos[,i]), lag = 3, type = "Box-Pierce")[1] # estadística
      dat[j,2]  <- runs.test(as.vector(datos_anchos[[i]]))$p.value  # Box.test(na.omit(datos_anchos[,i]), lag = 3, type = "Box-Pierce")[3] # p-valor

      j<-j+1
   }

res<-dat%>%
     mutate(Distrito=t(colnames(datos_anchos))[-1])%>%
     relocate(Distrito,.before=X)



xtable(res,digits = 3)


################33

class(datos_anchos)


write_xlsx(datos_anchos, "C:/Documetos MGM_OS_Dell/Documentos Maria_Dell_OS/Analisis de valores extremos espacial/Articulo_valores extremos/data_1st.xlsx")



################### Figure 4
#-- Datos

dat<-as.matrix( datos_anchos[,-1])

dat_olamax<- data.frame(long=centroides_mun$long, 
                        lat=centroides_mun$lat,
                        Max=apply(dat, 2,max))  # uso de la matriz de datos

dim(dat_olamax)


#--- Etiquetas 

centroides_shape<-st_coordinates(st_centroid(shape_cdmx))

nom_municipios<-data.frame(long=centroides_shape[,1],
                           lat=centroides_shape[,2],
                           lab=nom_alcaldias,
                           CVE_MUN=shape_cdmx$CVE_MUN)


shape_cdmx%>%
          ggplot()+
          geom_sf(fill="white",color="grey30")+  # Datos del Shape
          geom_point(data=dat_olamax, aes(x=long,y=lat, color=Max), size = 3)+       # Datos del data frame    
          coord_sf(crs = 4326, # maya cuadrada
                   label_graticule="NW",
                   label_axes = list(right="N", bottom="E") # coloca ejer derecho y superior
                   )+                                              # Colores
          scale_color_gradient(low = "yellow", high = "red") +
          geom_text_repel(data=nom_municipios, aes(long, lat, label=lab),size=2.5)+     # Etiquetas
          labs(x="Longitud",y="Latitud",color="Maximun")+
          labs(x="Longitude",y="Latitude")+
          theme_bw()

ggsave("C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell_OS\\Analisis de valores extremos espacial\\Articulo_valores extremos\\Fig_4.eps",dpi = 320,device=cairo_ps)# .eps quita las curvas grices

###########################################################################################
#-------------------------- Modelo base

coord <- cbind(lon =centroides_mun$long, lat =centroides_mun$lat)

rm(mc1,mc2,hyper,prop,start)

loc.form   <- y ~ 1+ lon
scale.form <- y ~ 1+ lat
shape.form <- y ~ 1


#-- Distribuciones a priori

hyper <- list()  # hiperpar�metros: par�metros del Proceso Gaussiano

# betas
hyper$betaMeans <- list(loc   = c(0.5, 0.5),  # Normal multivariada  beta_mu
                        scale = c(0.5, 0.5),  # Normal multivariada  beta_tau
                        shape = 0.5 )         # Normal multivariada  beta_xi

# covariance matrix

hyper$betaIcov  <- list(loc   = solve(diag(c(10, 10))),    # 
                        scale = solve(diag(c(10, 10))),    # 
                        shape = solve(diag(c(10),
                                           1, 1) )) # dimensión

# sigma

hyper$sills     <- list(loc   = c(1, 20),     # InvGamma(a, b)   sigma_mu   (1,20) (1,1)*  0.110
                        scale = c(1, 20),     # InvGamma(a, b)   sigma_tau 
                        shape = c(1, 20))     # InvGamma(a, b)   sigma_xi

#  phi
hyper$ranges    <- list(loc   = c(1, 10),     # Gamma    phi_mu      Gamma(1, 10)=Exp(10)
                        scale = c(1, 10),     # Gamma    phi_tau     Gamma(1, 10)=Exp(10)
                        shape = c(1, 10))     # Gamma    phi_xi      Gamma(1, 10)=Exp(10)

# kappa
hyper$smooths   <- list(loc   = c(1, 10),     # Gamma    kappa_mu    Gamma(1, 10)=Exp(10) 
                        scale = c(1, 10),     # Gamma    kappa_tau   Gamma(1, 10)=Exp(10)
                        shape = c(1, 10))     # Gamma    kappa_xi    Gamma(1, 10)=Exp(10)


#--- Instrumental function

set.seed(100)
prop <- list( gev     = c(0.5, 0.5, 0.5),  # betas
              sills   = c(1, 1, 1),        # <-- CAMBIO: 0 evita que 'shape1_sill' varíe
              ranges  = c(1, 1, 1),        # Parameter: phi  
              smooths = c(0.5, 0.5, 0.5 ))   # 
                         # eta, tau, xi
                         # location scale, shape 

#--- Initial values

start <- list(sills   = c(0.5, 0.5, 0.5),                    #  5 5 5
              ranges  = c(0.5, 0.5, 0.5),                    #  5 5 5       # phi  
              smooths = c(0.5, 0.5, 0.5),                    # 0.5 0.5 0.5  # shape 
              beta    = list(loc   = c(0.5, 0.5),            # 5 5          # start values for regression coeffients
                             scale = c(log(0.5), log(0.5)),  # 5 5          # scale
                             shape = 0.5)                    # 5            # shape
             )


#--- Modelo espacial

set.seed(100)
mc_sen1 <- latent(dat, 
              coord,                  #       
              cov.mod="powexp",       #  whitmat  cauchy  powexp bessel 
              loc.form = loc.form, 
              scale.form = scale.form,
              shape.form = shape.form,
            
              hyper = hyper, 
              prop = prop,
              start = start,

              n = 10000,
              burn.in =  2500,
              thin = 200) # 


round(DIC(mc_sen1),3)


#################################################
# ----- Modelo 1

coord <- cbind(lon =centroides_mun$long, lat =centroides_mun$lat)

#--- Spatial linear model for the mean of the latent processes

rm(mc1,mc2,hyper,prop,start)

# Matriz de correlaci�n para los 3 procesos gaussianos: powexp

loc.form   <- y ~ 1+ lon
scale.form <- y ~ 1+ lat
shape.form <- y ~ 1


#-- Distribuciones a priori

hyper <- list()  # hiperpar�metros: par�metros del Proceso Gaussiano

# sigma

hyper$sills     <- list(loc   = c(10, 1),     # InvGamma(a, b)   sigma_mu
                        scale = c(10, 1),     # InvGamma(a, b)   sigma_tau
                        shape = c(10, 1))     # InvGamma(a, b)   sigma_xi

#  phi
hyper$ranges    <- list(loc   = c(10, 1),     # Gamma    phi_mu
                        scale = c(10, 1),     # Gamma    phi_tau
                        shape = c(10, 1))     # Gamma    phi_xi

# kappa
hyper$smooths   <- list(loc   = c(10, 1),     # Gamma    kappa_mu   
                        scale = c(10, 1),     # Gamma    kappa_tau
                        shape = c(10, 1))     # Gamma    kappa_xi

# betas
hyper$betaMeans <- list(loc   = c(0.5, 0.5),  # Normal multivariada  beta_mu
                        scale = c(0.5, 0.5),  # Normal multivariada  beta_tau
                        shape = 0.5 )         # Normal multivariada  beta_xi

# covariance matrix

hyper$betaIcov  <- list(loc   = solve(diag(c(1, 1))),    # 
                        scale = solve(diag(c(1, 1))),    # 
                        shape = solve(diag(c(1), 1, 1))) # 


#-- We will use an exponential covariance function so the jump sizes for
#-- the shape parameter of the covariance function are null.

#--- Instrumental function

set.seed(100)
prop <- list( gev     = c(0.5, 0.5, 0.5),  # betas
              sills   = c(1, 1, 1),        # <-- CAMBIO: 0 evita que 'shape1_sill' varíe,, 1,1,1
              ranges  = c(1, 1, 1),        # Parameter: phi  
              smooths = c(0.5, 0.5, 0.5 ))   # 
                         # eta, tau, xi
                         # location scale, shape 

#--- Initial values

start <- list(sills   = c(0.5, 0.5, 0.5),                    #  5 5 5
              ranges  = c(0.5, 0.5, 0.5),                    #  5 5 5       # phi  
              smooths = c(0.5, 0.5, 0.5),                    # 0.5 0.5 0.5  # shape 
              beta    = list(loc   = c(0.5, 0.5),            # 5 5          # start values for regression coeffients
                             scale = c(log(0.5), log(0.5)),  # 5 5          # scale
                             shape = 0.5)                    # 5            # shape
             )


#--- Modelo espacial

set.seed(100)
mc1 <- latent(dat, 
              coord,                  #       
              cov.mod="powexp",       # powexp whitmat  cauchy bessel 
              loc.form = loc.form, 
              scale.form = scale.form,
              shape.form = shape.form,
            
              hyper = hyper, 
              prop = prop,
              start = start,

              n = 10000,
              burn.in =  2500,
              thin = 200) # 


round(DIC(mc1),3)



###################################################################3

#-------------------------- Modelo 2


loc.form   <- y ~ 1+ lon
scale.form <- y ~ 1+ lat
shape.form <- y ~ 1


#-- Distribuciones a priori

hyper <- list()  # hiperpar�metros: par�metros del Proceso Gaussiano

# betas
hyper$betaMeans <- list(loc   = c(0.5, 0.5),  # Normal multivariada  beta_mu
                        scale = c(0.5, 0.5),  # Normal multivariada  beta_tau
                        shape = 0.5 )         # Normal multivariada  beta_xi

# covariance matrix

hyper$betaIcov  <- list(loc   = solve(diag(c(100, 100))),    # 
                        scale = solve(diag(c(100, 100))),    # 
                        shape = solve(diag(c(100),
                                           1, 1) )) # dimensión

# sigma

hyper$sills     <- list(loc   = c(10, 1),     # InvGamma(a, b)   sigma_mu
                        scale = c(10, 1),     # InvGamma(a, b)   sigma_tau
                        shape = c(10, 1))     # InvGamma(a, b)   sigma_xi

#  phi
hyper$ranges    <- list(loc   = c(20/2, 2),     # Gamma    phi_mu
                        scale = c(20/2, 2),     # Gamma    phi_tau
                        shape = c(20/2, 2))     # Gamma    phi_xi

# kappa
hyper$smooths   <- list(loc   = c(20/2, 2),     # Gamma    kappa_mu   
                        scale = c(20/2, 2),     # Gamma    kappa_tau
                        shape = c(20/2, 2))     # Gamma    kappa_xi



#--- Instrumental function
set.seed(100)
prop <- list( gev     = c(0.5, 0.5, 0.5),  # betas
              sills   = c(1, 1, 1),        # <-- CAMBIO: 0 evita que 'shape1_sill' varíe
              ranges  = c(1, 1, 1),        # Parameter: phi  
              smooths = c(0.5, 0.5, 0.5 ))   # 
                         # eta, tau, xi
                         # location scale, shape 

#--- Initial values

start <- list(sills   = c(0.5, 0.5, 0.5),                    #  5 5 5
              ranges  = c(0.5, 0.5, 0.5),                    #  5 5 5       # phi  
              smooths = c(0.5, 0.5, 0.5),                    # 0.5 0.5 0.5  # shape 
              beta    = list(loc   = c(0.5, 0.5),            # 5 5          # start values for regression coeffients
                             scale = c(log(0.5), log(0.5)),  # 5 5          # scale
                             shape = 0.5)                    # 5            # shape
             )


#--- Modelo espacial

set.seed(100)
mc_sen2 <- latent(dat, 
              coord,                  #       
              cov.mod="powexp",       #  whitmat  cauchy  powexp bessel 
              loc.form = loc.form, 
              scale.form = scale.form,
              shape.form = shape.form,
            
              hyper = hyper, 
              prop = prop,
              start = start,

              n = 10000,
              burn.in =  2500,
              thin = 200) # 


round(DIC(mc_sen2),3)


#########################################
round(DIC(mc1),3)

mc1<- mc_sen1


loc1_beta0  <-mc1$chain.loc[,1]
loc1_beta1  <-mc1$chain.loc[,2]
loc1_sill   <-mc1$chain.loc[,3]
loc1_range  <-mc1$chain.loc[,4]
loc1_smooth <-mc1$chain.loc[,5]

scale1_beta0  <-mc1$chain.scale[,1]
scale1_beta1  <-mc1$chain.scale[,2]
scale1_sill   <-mc1$chain.scale[,3]
scale1_range  <-mc1$chain.scale[,4]
scale1_smooth <-mc1$chain.scale[,5]

shape1_beta0  <-mc1$chain.shape[,1]
shape1_sill   <-mc1$chain.shape[,2]
shape1_range  <-mc1$chain.shape[,3]
shape1_smooth <-mc1$chain.shape[,4]

length(loc1_sill) # longitud de la cadena


#--- Mediana

mo1 <-round(quantile(loc1_beta0,    prob=0.5), 5) #  beta0
mo2 <-round(quantile(loc1_beta1,    prob=0.5), 5) #  beta1
mo3 <-round(quantile(loc1_sill,     prob=0.5), 5) #  loc
mo4 <-round(quantile(loc1_range,    prob=0.5), 5) #  Scale
mo5 <-round(quantile(loc1_smooth,   prob=0.5), 5) #  Shape

mo6 <-round(quantile(scale1_beta0,  prob=0.5), 5) #  beta0
mo7 <-round(quantile(scale1_beta1,  prob=0.5), 5) #  beta1
mo8 <-round(quantile(scale1_sill,   prob=0.5), 5) #  loc
mo9 <-round(quantile(scale1_range,  prob=0.5), 5) #  Scale
mo10<-round(quantile(scale1_smooth, prob=0.5), 5) #  Shape

mo11<-round(quantile(shape1_beta0,  prob=0.5), 5) #  beta0
mo12<-round(quantile(shape1_sill,   prob=0.5), 5) #  loc
mo13<-round(quantile(shape1_range,  prob=0.5), 5) #  Scale
mo14<-round(quantile(shape1_smooth, prob=0.5), 5) #  Shape


#--- Intervalos

int1 <-round(quantile(loc1_beta0,  prob=c(0.025, 0.975)), 5) #  beta0
int2 <-round(quantile(loc1_beta1,  prob=c(0.025, 0.975)), 5) #  beta1
int3 <-round(quantile(loc1_sill,   prob=c(0.025, 0.975)), 5) #  loc
int4 <-round(quantile(loc1_range,  prob=c(0.025, 0.975)), 5) #  Scale
int5 <-round(quantile(loc1_smooth, prob=c(0.025, 0.975)), 5) #  Shape

int6 <-round(quantile(scale1_beta0,  prob=c(0.025, 0.975)), 5) #  beta0
int7 <-round(quantile(scale1_beta1,  prob=c(0.025, 0.975)),5) #  beta1
int8 <-round(quantile(scale1_sill,   prob=c(0.025, 0.975)), 5) #  loc
int9 <-round(quantile(scale1_range,  prob=c(0.025, 0.975)), 5) #  Scale
int10<-round(quantile(scale1_smooth, prob=c(0.025, 0.975)), 5) #  Shape

int11<-round(quantile(shape1_beta0,  prob=c(0.025, 0.975)), 5) #  beta0
int12<-round(quantile(shape1_sill,   prob=c(0.025, 0.975)), 5) #  loc
int13<-round(quantile(shape1_range,  prob=c(0.025, 0.975)), 5) #  Scale
int14<-round(quantile(shape1_smooth, prob=c(0.025, 0.975)), 5) #  Shape

esti<-rbind(c( mo1[1], int1[1], int1[2]),
      c( mo2[1], int2[1], int2[2]),
      c( mo3[1], int3[1], int3[2]),
      c( mo4[1], int4[1], int4[2]),
      c( mo5[1], int5[1], int5[2]),
      c( mo6[1], int6[1], int6[2]),
      c( mo7[1], int7[1], int7[2]),
      c( mo8[1], int8[1], int8[2]),
      c( mo9[1], int9[1], int9[2]),
      c(mo10[1],int10[1],int10[2]),
      c(mo11[1],int11[1],int11[2]),
      c(mo12[1],int12[1],int12[2]),
      c(mo13[1],int13[1],int13[2]),
      c(mo14[1],int14[1],int14[2]))


#-------- Diagnostic Metrics

sd(loc1_beta0)

muestras_beta <- draws_matrix(loc1_beta0 = loc1_beta0,
                              loc1_beta1=loc1_beta1,
                              loc1_sill =loc1_sill,
                              loc1_range =loc1_range,
                              loc1_smooth=loc1_smooth,

                              scale1_beta0 =scale1_beta0,
                              scale1_beta1 =scale1_beta1,
                              scale1_sill  = scale1_sill,
                              scale1_range =scale1_range,
                              scale1_smooth=scale1_smooth,

                              shape1_beta0 =shape1_beta0,
                              shape1_sill  = shape1_sill,
                              shape1_range = shape1_range,
                              shape1_smooth=shape1_smooth)



res_cri<-summarise_draws(muestras_beta, rhat, ess_bulk, ess_tail,mcse_mean )

va_min<-round(apply(res_cri[,3:4],1,min)/10000,3)

res_fin<-cbind(esti,res_cri[,2:5], va_min );res_fin

xtable(res_fin,digits = 3) # Resultados en formato latex


#--- Cadenas

postscript("C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell_OS\\Analisis de valores extremos espacial\\Articulo_valores extremos\\Fig_9.eps",
           onefile = FALSE ,      # Obligatorio para formato EPS correcto
           horizontal = FALSE
        )# .eps quita las curvas grices
par(mfrow = c(3, 3))
plot(seq(1,10000,1), loc1_sill,type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(sigma[eta]))
lines(loc1_sill,col="darkblue",lwd=1)

plot(seq(1,10000,1),  loc1_range,type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(phi[eta]))
lines(loc1_range, col="darkblue",lwd=1)

plot(seq(1,10000,1), loc1_smooth,type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(kappa[eta]))
lines(loc1_smooth,col="darkblue",lwd=1)

plot(seq(1,10000,1),   scale1_sill,type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(sigma[tau]))
lines(scale1_sill,  col="darkblue",lwd=1)

plot(seq(1,10000,1),  scale1_range, type = "n", xlab=" ",ylab=" ",cex.main = 3, main=expression(phi[tau]))
lines(scale1_range, col="darkblue",lwd=1)

plot(x=seq(1,10000,1), scale1_smooth, type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(kappa[tau]),
 ylim=c(min(scale1_smooth),max(scale1_smooth))
)
lines(scale1_smooth, col="darkblue",lwd=1)

plot(seq(1,10000,1), shape1_sill,type = "n", xlab=" ",ylab=" ",cex.main = 3,   main=expression(sigma[xi]))
lines(shape1_sill,col="darkblue",lwd=1)

plot(seq(1,10000,1),  shape1_range,type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(phi[xi]))
lines(shape1_range, col="darkblue",lwd=1)

plot(seq(1,10000,1), shape1_smooth,type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(kappa[xi]))
lines(shape1_smooth,col="darkblue",lwd=1)
dev.off() 

#--- Histogramas

postscript("C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell_OS\\Analisis de valores extremos espacial\\Articulo_valores extremos\\Fig_10.eps",
           onefile = FALSE ,      # Obligatorio para formato EPS correcto
           horizontal = FALSE
        )# .eps quita las curvas grices
par(mfrow = c(3, 3))
hist(loc1_sill,  xlab="",col="darkblue",cex.main = 3, main=expression(sigma[eta]))
hist(loc1_range, xlab="",col="darkblue",cex.main = 3, main=expression(phi[eta]))
hist(loc1_smooth,xlab="",col="darkblue",cex.main = 3,main=expression(kappa[eta]))

hist(scale1_sill, xlab="",col="darkblue",cex.main = 3, main=expression(sigma[tau]))
hist(scale1_range,xlab="",col="darkblue",cex.main = 3, main=expression(phi[tau]))
hist(scale1_smooth,xlab="",col="darkblue",cex.main = 3,main=expression(kappa[tau]))

hist(shape1_sill, xlab="",col="darkblue",cex.main = 3, main=expression(sigma[xi]))
hist(shape1_range,xlab="",col="darkblue",cex.main = 3, main=expression(phi[xi]))
hist(shape1_smooth,xlab="",col="darkblue",cex.main = 3,main=expression(kappa[xi]))
dev.off() 



################################
# Creación del mapa

borde <-as.matrix(cbind( as.data.frame(st_coordinates(shape_est))$X, as.data.frame(st_coordinates(shape_est))$Y ))

plot(borde[,1],borde[,2])

x.grid <- seq(min(borde[,1]),max(borde[,1]),length=20)  # Genera una maya de 50X50 estimaciones
y.grid <- seq(min(borde[,2]),max(borde[,2]),length=20)


res<-map.latent(mc1, 
                x.grid,
                y.grid,
                param = "quant",
                ret.per=8,       # period of 8 weeks
                level=0.95,
                plot.contour=F,
                show.data=T,
                col =  brewer.pal(7,"YlOrRd"),  # heat.colors(5), terrain.colors(64),
                fun = median 
              )


############## Mapa


nivel_retorno_punto  <- res$post.sum
intervalo_inferior   <- res$ci.low
intervalo_superior   <- res$ci.up



rango_global <- range(c(intervalo_inferior, nivel_retorno_punto, intervalo_superior), na.rm = TRUE)
cortes_escala <- pretty(rango_global, n = 12)
num_colores <- length(cortes_escala) - 1
colores_continuos <- colorRampPalette(brewer.pal(9, "YlOrRd"))(num_colores)



filled.contour(
                x.grid, y.grid, intervalo_inferior, 
                col = colores_continuos,
                levels = cortes_escala, # Forzar los cortes limpios
                cex.lab = 0.9,
                xlab = "Longitude",                    
                ylab = "Latitude",  
 key.axes = {
    axis(4, cex.axis = 1.5) 
  },
  
plot.axes = {

    ticks_x <- axTicks(1)
    ticks_y <- axTicks(2)
    axis(1, at = ticks_x, labels = paste0(abs(ticks_x), "°W"), cex.axis = 0.8)
    axis(2, at = ticks_y, labels = paste0(ticks_y, "°N"), cex.axis = 0.8)
    contour(x.grid, y.grid, intervalo_inferior, add = TRUE, col = "black", lwd = 2,labcex = 1.5)
  } 
)



filled.contour(
  x.grid, y.grid, nivel_retorno_punto, 
  col = colores_continuos,
  levels = cortes_escala,
  cex.lab = 0.9,
  xlab = "Longitude",                    
  ylab = "Latitude",  
 key.axes = {
    axis(4, cex.axis = 1.5) 
  },
  plot.axes = {
     ticks_x <- axTicks(1)
    ticks_y <- axTicks(2)
    axis(1, at = ticks_x, labels = paste0(abs(ticks_x), "°W"), cex.axis = 0.8)
    axis(2, at = ticks_y, labels = paste0(ticks_y, "°N"), cex.axis = 0.8)
    contour(x.grid, y.grid, nivel_retorno_punto, add = TRUE, col = "black",lwd = 2,labcex = 1.5)
  }
)


filled.contour(
  x.grid, y.grid, intervalo_superior, 
  col = colores_continuos,
  levels = cortes_escala,
  cex.lab =0.9,
  xlab = "Longitude",                    
  ylab = "Latitude",  
 key.axes = {
    axis(4, cex.axis = 1.5) 
  },
  plot.axes = {
    ticks_x <- axTicks(1)
    ticks_y <- axTicks(2)
    axis(1, at = ticks_x, labels = paste0(abs(ticks_x), "°W"), cex.axis = 0.8)
    axis(2, at = ticks_y, labels = paste0(ticks_y, "°N"), cex.axis = 0.8)
    contour(x.grid, y.grid, intervalo_superior, add = TRUE,col = "black",cex = 8,labcex = 1.5,
lwd = 2)
  }
)

