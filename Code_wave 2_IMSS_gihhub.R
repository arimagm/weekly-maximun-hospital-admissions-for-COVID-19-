# Link para descrga de datos de covid: https://www.gob.mx/salud/documentos/datos-abiertos-152127
# https://www.nsgrantham.com/reorder-legend-ggplot2

Versión usada  del R: 4.3.2

################## Valores extremos

library(SpatialExtremes)
library(timetk) #     summarise_by_time
library("fields") #no instalado

library(tidyverse) # %>%

library("CARBayesST")
library(mxmaps) # df_mxmunicipio
library(dplyr)

library(ggplot2)
library(ggrepel)
library("GGally")
library(rlang)
library("sp")
library(coda)

library(geoR)      # pred_grid
library(rgeos)
library(spBayesSurv)
library("coda")
library("survival")
library("spBayesSurv")
library("BayesX")
library(readxl)
library(knitr)     # kable
library(xtable)    # xtable
library(classInt)  # classIntervals
library(rgdal)     # readOGR Para importar shapefiles. 
library("CARBayesdata")
library(leaflet)
library("spdep")
library(rgdal)
library(xtable)    # xtable

library(writexl)              # write_xlsx

library(pacman)
library(RColorBrewer)
col = brewer.pal(7,"YlOrRd")
library(gghighlight) # gghighlight
library("Cairo")  #ggsave
library(tidyr)
library(posterior)
library(coda)
library(metR)  # geom_contour_fill()
library(pracma)



# --- Shape del estado con sus municipios

n.est<-9

shape_muni_pais <-st_read("C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell\\Investigacion_MGM\\TRABAJOS de Investigacion\\T_Cruz\\Manuel_Dengue-20221028T125149Z-001\\Manuel_Dengue\\conjunto_de_datos", layer="areas_geoestadisticas_municipales")

shape_muni_pais$CVE_ENT<-as.numeric(shape_muni_pais$CVE_ENT)

shape_muni <- shape_muni_pais[shape_muni_pais$CVE_ENT==n.est,] # Elecci n del estado
shape_muni <- st_transform(shape_muni, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))

shape_muni_df<-ggplot2::fortify(shape_muni,region="CVE_MUN") # E muy importante colocar el argumento region


#--- Shape de la CDMX

shape_cdmx <-st_read("C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell_OS\\Analisis de valores extremos espacial\\conjunto_de_datos", layer="09mun")
shape_cdmx <- st_transform(shape_cdmx, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
shape_cdmx_df<-ggplot2::fortify(shape_cdmx, region="CVE_MUN")






# --- Contorno del estado: Shape de Mexico con sus 32 estados (no hay municipios)

shape_est_pais <-readOGR("C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell\\Investigacion_MGM\\TRABAJOS de Investigacion\\T_Cruz\\Manuel_Dengue-20221028T125149Z-001\\Manuel_Dengue\\conjunto_de_datos", layer="areas_geoestadisticas_estatales")

shape_est_pais$CVE_ENT<-as.numeric(shape_est_pais$CVE_ENT)
shape_est <- shape_est_pais[shape_est_pais$CVE_ENT==n.est,] # Elecci n del estado
shape_est <- sp::spTransform(shape_est, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))

shape_est_df<-fortify(shape_est)  # Contorno del estado

borde<-cbind(shape_est_df[,1],shape_est_df[,2])




#--- Etiquetas 

nom_alcaldias <-c("Azcapotzalco","Coyoacán", "Cuajimalpa de Morelos", "Gustavo A. Madero","Iztacalco", "Iztapalapa",
                  "La Magdalena Contreras","Milpa Alta","Álvaro Obregón", "Tláhuac", "Tlalpan","Xochimilco","Benito Juárez", 
                   "Cuauhtémoc", "Miguel Hidalgo","Venustiano Carranza") 





# --- Centroides

centroides<-st_coordinates(st_centroid(shape_muni))

centroides_mun<-data.frame(CVE_MUN=shape_muni$CVE_MUN, long=centroides[,1], lat=centroides[,2] )

centroides_mun<-centroides_mun%>%
                arrange(CVE_MUN) # ordena en orden desendente

View(centroides_mun)

#--- Fecha del dia 24 del mes 11 del a o 2021 (a o-mes-dia)

datos<-read.csv("C:/Documetos MGM_OS_Dell/Documentos Maria_Dell_OS/datos_abiertos_covid19_23_05_2022/220702COVID19MEXICO.csv")


dim(datos) # 16638854 


datos$ENTIDAD_RES<-as.numeric(datos$ENTIDAD_RES)
datos$MUNICIPIO_RES<-as.numeric(datos$MUNICIPIO_RES)


#--- Positivos de Covid-19 del estado n.est

res_lab  <-1   # Positivo covid
tp_hosp  <-2   # hospitalizados 

#--- N mero de casos por instituci n

dat_var_sector <- datos%>%
                  dplyr::select(RESULTADO_LAB, TIPO_PACIENTE, ENTIDAD_RES,SECTOR, MUNICIPIO_RES,FECHA_INGRESO)%>%
                  dplyr::filter(RESULTADO_LAB==res_lab, TIPO_PACIENTE==tp_hosp, ENTIDAD_RES==n.est, MUNICIPIO_RES!=999)%>%
                  mutate(FECHA_INGRESO=as.Date(FECHA_INGRESO))%>% 
                  dplyr::filter(FECHA_INGRESO<= "2022-03-06")%>%
                  group_by(MUNICIPIO_RES,SECTOR)%>%
                  summarise(Total=n())

View(dat_var_sector)


dat_var_sector<-as.data.frame(dat_var_sector)


xtable(data.frame(t(dat_var_sector$Total))) # N mero de casos por instituci n



#--- Totales para la CDMX por institucion

 dat_var_sector <- datos%>%
                   dplyr::select(RESULTADO_LAB, TIPO_PACIENTE, ENTIDAD_RES,SECTOR, MUNICIPIO_RES,FECHA_INGRESO)%>%
                   dplyr::filter(RESULTADO_LAB==res_lab, TIPO_PACIENTE==tp_hosp, ENTIDAD_RES==n.est, MUNICIPIO_RES!=999)%>%
                   mutate(FECHA_INGRESO=as.Date(FECHA_INGRESO))%>% 
                   dplyr::filter(FECHA_INGRESO<= "2022-03-06")%>%
                   group_by(MUNICIPIO_RES,SECTOR)%>%
                   table()

#View( dat_var_sector)

dat_var_sector<-as.data.frame(dat_var_sector)

 res<-dat_var_sector%>%
      group_by(SECTOR)%>%
      summarise(Total=sum(Freq))

  res


#--- Totales por ola para la CDMX


dat_ola_1 <- datos%>%
                  dplyr::select(RESULTADO_LAB, TIPO_PACIENTE, ENTIDAD_RES,SECTOR, MUNICIPIO_RES,FECHA_INGRESO)%>%
                  dplyr::filter(RESULTADO_LAB==res_lab, TIPO_PACIENTE==tp_hosp, ENTIDAD_RES==n.est, MUNICIPIO_RES!=999)%>%
                  mutate(FECHA_INGRESO=as.Date(FECHA_INGRESO))%>% 
                  dplyr::filter(FECHA_INGRESO>= "2020-02-17" &  FECHA_INGRESO<= "2020-09-27")%>%
                  group_by(MUNICIPIO_RES)%>%
                  count()

View(dat_ola_1)

sum(dat_ola_1$n)

dat_ola_2 <- datos%>%
                  dplyr::select(RESULTADO_LAB, TIPO_PACIENTE, ENTIDAD_RES,SECTOR, MUNICIPIO_RES,FECHA_INGRESO)%>%
                  dplyr::filter(RESULTADO_LAB==res_lab, TIPO_PACIENTE==tp_hosp, ENTIDAD_RES==n.est, MUNICIPIO_RES!=999)%>%
                  mutate(FECHA_INGRESO=as.Date(FECHA_INGRESO))%>% 
                  dplyr::filter(FECHA_INGRESO>= "2020-09-28" &  FECHA_INGRESO<= "2021-04-18")%>%
                  group_by(MUNICIPIO_RES)%>%
                  count()

View(dat_ola_2)
sum(dat_ola_2$n)

dat_ola_3 <- datos%>%
                  dplyr::select(RESULTADO_LAB, TIPO_PACIENTE, ENTIDAD_RES,SECTOR, MUNICIPIO_RES,FECHA_INGRESO)%>%
                  dplyr::filter(RESULTADO_LAB==res_lab, TIPO_PACIENTE==tp_hosp, ENTIDAD_RES==n.est, MUNICIPIO_RES!=999)%>%
                  mutate(FECHA_INGRESO=as.Date(FECHA_INGRESO))%>% 
                  dplyr::filter(FECHA_INGRESO>= "2021-06-07" &  FECHA_INGRESO<= "2021-10-24")%>%
                  group_by(MUNICIPIO_RES)%>%
                  count()

View(dat_ola_3)
sum(dat_ola_3$n)


dat_ola_4 <- datos%>%
                  dplyr::select(RESULTADO_LAB, TIPO_PACIENTE, ENTIDAD_RES,SECTOR, MUNICIPIO_RES,FECHA_INGRESO)%>%
                  dplyr::filter(RESULTADO_LAB==res_lab, TIPO_PACIENTE==tp_hosp, ENTIDAD_RES==n.est, MUNICIPIO_RES!=999)%>%
                  mutate(FECHA_INGRESO=as.Date(FECHA_INGRESO))%>% 
                  dplyr::filter(FECHA_INGRESO>= "2021-12-20" &  FECHA_INGRESO<= "2022-03-06")%>%
                  group_by(MUNICIPIO_RES)%>%
                  count()



sum(dat_ola_4$n)


#--- Selecci n de la instituci n IMSS 

#--- Datos hasta la 4ta ola


datos.6.mar.2022<-datos%>%
                  mutate(FECHA_INGRESO=as.Date(FECHA_INGRESO))%>% 
                  dplyr::filter(FECHA_INGRESO<= "2022-03-06")

sec_imss <- 4 

cov_pos<-datos.6.mar.2022%>%
         dplyr::select(RESULTADO_LAB,TIPO_PACIENTE,SECTOR,ENTIDAD_RES,TIPO_PACIENTE,MUNICIPIO_RES,FECHA_INGRESO)%>%
         dplyr::filter(RESULTADO_LAB==res_lab, TIPO_PACIENTE==tp_hosp, SECTOR==sec_imss, ENTIDAD_RES==n.est, MUNICIPIO_RES!=999)%>%  # RESULTADO_LAB==1 es igual a positivo covid.
         rename(CVE_MUN=MUNICIPIO_RES)%>% 
         mutate(Year=as.numeric(substr(FECHA_INGRESO,start=1,stop=4)),
                Mes =as.numeric(substr(FECHA_INGRESO,start=6,stop=7)),
                Dia =as.numeric(substr(FECHA_INGRESO,start=9,stop=10)))

dim(cov_pos)  # 16627  

#-- Usando la función summarise_by_time: ola 2 para la IMSS


dat_hosp_imss<-cov_pos%>%
              dplyr::filter(FECHA_INGRESO>= "2020-09-28" &  FECHA_INGRESO<= "2021-04-18")%>%
              group_by(SECTOR, CVE_MUN, FECHA_INGRESO)%>%
              count()%>%
              summarise_by_time(
                             .date_var = FECHA_INGRESO,
                             .by       = "week", 
                              VMax    = max(n))




dat_hosp_imss$Alcaldias<-recode(as.factor(dat_hosp_imss$CVE_MUN),
                               "2"="Azcapotzalco",     "3"="Coyoacán",     "4"="Cuajimalpa de Morelos",   "5"="Gustavo A. Madero",
                               "6"="Iztacalco",        "7"="Iztapalapa",   "8"="La Magdalena Contreras",  "9"="Milpa Alta",
                              "10"="Álvaro Obregón",   "11"="Tláhuac",     "12"="Tlalpan",                "13"="Xochimilco",
                              "14"="Benito Juárez",    "15"="Cuauhtémoc",  "16"="Miguel Hidalgo",         "17"="Venustiano Carranza") 



dim(dat_hosp_imss)


#--- Gráficos  curvas de las Alcaldías



ggplot(dat_hosp_imss, aes(FECHA_INGRESO, VMax, colour = fct_reorder2(Alcaldias, FECHA_INGRESO, VMax))) +
       geom_point() + 
       geom_line()+
        gghighlight(max(VMax) > 15,use_direct_label = FALSE)+
       scale_x_date(date_labels="%d %b %y", breaks =dat_hosp_imss$FECHA_INGRESO) +
       scale_y_continuous(breaks = c(5, 10, 15, 20, 25, 30, 35, 40))+
       labs(title = "", x = "Weeks",y = "Weekly maximum hospital admissions", colour ="Districts") +
       theme_gray()+
       theme_bw()+
       theme(text = element_text(size = 10), axis.text.x = element_text(angle =90, hjust = 1, vjust = 0.4))


ggsave("C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell_OS\\Analisis de valores extremos espacial\\Articulo_valores extremos\\Fig_6.eps",dpi = 320,device=cairo_ps)# .eps quita las curvas grices



### Formato ancho

datos_ancho <- dat_hosp_imss[,c(3,4,5)] %>%
                pivot_wider(
                            names_from = Alcaldias,   
                            values_from = VMax   
                           )%>%
                mutate(Fecha = as.character(FECHA_INGRESO))%>%
                dplyr::select(-FECHA_INGRESO)%>%
                relocate(Fecha,.before=Azcapotzalco)%>%
                mutate(across(where(is.numeric), ~ coalesce(.x, 0)))

View(datos_ancho)

######################################
# Pasar a formato ancho



View(dat_hosp_imss)


datos_anchos <- dat_hosp_imss[,c(3,4,6)] %>%
                pivot_wider(
                             names_from = Alcaldias,   
                             values_from = VMax   
                           )%>%
                mutate(Fecha = as.character(FECHA_INGRESO))%>% 
                select(-FECHA_INGRESO)%>%
                relocate(Fecha, .before=Azcapotzalco)%>% 
                slice(30, 29, 1:(n()))%>% 
                mutate(across(where(is.numeric), ~ coalesce(.x, 0)))

dim(datos_anchos)


xtable(datos_anchos) # Resultados en formato latex
dim(datos_anchos)


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



xtable(res,digits = 5)




# Identificación de los máximos


lista_max<-list()

for(i in 2:17)
{
   lista_max[[i]]<-dat_hosp_imss%>%
                 filter(CVE_MUN==i)
                
}

lista_max


#--- Verificando si se tienen todos los grupos

a<-as.numeric(centroides_mun$CVE_MUN)

b<-dat_hosp_imss$CVE_MUN

setdiff(a,b) # Grupos que faltam


#--- Base de datos construida

DatMax<-dat_hosp_imss

matrix_max <- matrix(0,30,16)


loc<-as.numeric(levels(as.factor(dat_hosp_imss$CVE_MUN)))

pro<-30
j<-1

for(i in loc)
   {

     if(length(DatMax[DatMax$CVE_MUN==i,]$VMax)==pro)
        {
         matrix_max[ ,j]<-DatMax[DatMax$CVE_MUN==i,]$VMax  # Selecciona la variable VMax
        }
     else
       {
          a<-as.matrix(DatMax[DatMax$CVE_MUN==i,]$VMax,nrow=1)

          while(dim(a)[1]<pro)
           {
             a <-  rbind(a,0)
           }

        matrix_max[ ,j]<-a
       }

     j<-j+1  
   }


dat<-matrix_max
dat



dat_save<-data.frame(dat)

write_xlsx(dat_save, "C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell_OS\\Analisis de valores extremos espacial\\Articulo_valores extremos\\data_2nd.xlsx")



#-- Datos

dat_olamax<- data.frame(long=centroides_mun$long, lat=centroides_mun$lat, Max=apply(dat, 2,max))


#--- Etiquetas 

nom_alcaldias <-c("Azcapotzalco","Coyoacán", "Cuajimalpa de Morelos", "Gustavo A. Madero","Iztacalco", "Iztapalapa",
                  "La Magdalena Contreras","Milpa Alta","Álvaro Obregón", "Tláhuac", "Tlalpan","Xochimilco","Benito Juárez", 
                   "Cuauhtémoc", "Miguel Hidalgo","Venustiano Carranza") 

centroides_shape<-st_coordinates(st_centroid(shape_cdmx))

dim(centroides_shape)

nombres_mun<-data.frame(long=centroides_shape[,1],
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
                   )+                                      
          scale_color_gradient(low = "yellow", high = "red") +
          geom_text_repel(data=nombres_mun, aes(long, lat, label=lab),size=2.5)+     # Etiquetas
          labs(x="Longitud",y="Latitud",color="Maximun")+
          labs(x="Longitude",y="Latitude")+
          theme_bw()

ggsave("C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell_OS\\Analisis de valores extremos espacial\\Articulo_valores extremos\\Fig_7.eps",dpi = 320,device=cairo_ps)# .eps quita las curvas grices


#################################################################################
#---------- Ajustando el  modelo



coord <- cbind(lon =centroides_mun$long, lat =centroides_mun$lat)

#--- Spatial linear model for the mean of the latent processes

rm(mc1,mc2,hyper,prop,start)


#-------------------------- Basel model 

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

hyper$sills     <- list(loc   = c(1, 20),     # InvGamma(a, b)   sigma_mu   Valor bueno: 1,20
                        scale = c(1, 20),     # InvGamma(a, b)   sigma_tau
                        shape = c(1, 20))     # InvGamma(a, b)   sigma_xi

#  phi
hyper$ranges    <- list(loc   = c(1, 10),     # Gamma    phi_mu      Gamma(1,10)=Exp(10)
                        scale = c(1, 10),     # Gamma    phi_tau     Gamma(1,10)=Exp(10)
                        shape = c(1, 10))     # Gamma    phi_xi       Gamma(1,10)=Exp(10)

# kappa
hyper$smooths   <- list(loc   = c(1, 10),     # Gamma    kappa_mu    Gamma(1,10)=Exp(10)
                        scale = c(1, 10),     # Gamma    kappa_tau   Gamma(1,10)=Exp(10)
                        shape = c(1, 10))     # Gamma    kappa_xi   Gamma(1,10)=Exp(10)



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


round(DIC(mc_sen1),3)



#####################  Modelo 1

# Matriz de correlaci n para los 3 procesos gaussianos: powexp

loc.form   <- y ~ 1+ lon
scale.form <- y ~ 1+ lat
shape.form <- y ~ 1

set.seed(100)  # semilla

#-- Distribuciones a priori


hyper <- list()  # hiperpar metros: par metros del Proceso Gaussiano


# sigma

hyper$sills     <- list(loc   = c(10, 1),   # InvGamma(a, b)   sigma_mu
                        scale = c(10, 1),   # InvGamma(a, b)   sigma_tau
                        shape = c(10, 1))   # InvGamma(a, b)   sigma_xi

#  phi
hyper$ranges    <- list(loc   = c(10, 1),      # Gamma       phi_mu
                        scale = c(10, 1),      # Gamma       phi_tau
                        shape = c(10, 1))      # Gamma       phi_xi

# kappa
hyper$smooths   <- list(loc   = c(10, 1),    # Gamma    kappa_mu   
                        scale = c(10, 1),    # Gamma    kappa_tau
                        shape = c(10, 1))    # Gamma    kappa_xi


hyper$betaMeans <- list(loc   = c(0.5, 0.5),     # Normal multivariada  beta_mu
                        scale = c(0.5, 0.5),     # Normal multivariada  beta_tau
                        shape = 0.5 )            # Normal multivariada  beta_xi

# matriz de covarianzas

hyper$betaIcov  <- list(loc   = solve(diag(c(1, 1))),    # 
                        scale = solve(diag(c(1, 1))),    # 
                        shape = solve(diag(c(1), 1, 1))) # 


#-- We will use an exponential covariance function so the jump sizes for
#-- the shape parameter of the covariance function are null.

#--- Funcion instrumental

set.seed(100)  # semilla
prop <- list( gev     = c(0.5, 0.5, 0.5),  # Parameter: location scale, shape 
              sills   = c(1, 1, 1),
              ranges  = c(1, 1, 1),        # Parameter: phi  
              smooths = c(0.5, 0.5, 0.5))     # Parametros funci n de covarianza


#--- Valores iniciales
set.seed(100)  # semilla
start <- list(sills   = c(0.5, 0.5, 0.5),           #  5 5 5
              ranges  = c(.5, .5, .5),                 #  5 5 5    # phi  
              smooths = c(0.5, 0.5, 0.5),           # 0.5 0.5 0.5    # shape 
              beta    = list(loc   = c(0.5, 0.5),   # 5 5             # start values for regression coeffients
                             scale = c(log(0.5), log(0.5)),   # 5 5
                             shape = 0.5)           #5
             )

#--- Modelo espacial



set.seed(100)  # semilla
mc1 <- latent(dat, 
              coord,                 # sin función 2611.47                           # no conv. 3 cad     
              cov.mod="powexp",      # powexp (2611.47 ), whitmat (2618.612), cauchy ( 2607.733 ), bessel
              loc.form = loc.form,   
              scale.form = scale.form,
              shape.form = shape.form,
            
              hyper = hyper, 
              prop = prop,
              start = start,

              n = 10000,
              burn.in = 2500,
              thin = 200)

round(DIC(mc1),3)

set.seed(100)  # semilla
mc2 <- latent(dat, 
              coord,
              cov.mod="powexp",
              loc.form = loc.form, 
              scale.form = scale.form,
              shape.form = shape.form,
            
              hyper = hyper, 
              prop = prop,
              start = start,

              n = 10000,
              burn.in = 2500,
              thin = 200)

round(DIC(mc2),3)




#########################################################################
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


round(DIC(mc_sen2),3)


####################################
#---   Criterios de diagnóstico


mc1<-mc_sen1

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



#--- mediana

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

esti<-rbind(c(mo1[1], int1[1], int1[2]),
            c(mo2[1], int2[1], int2[2]),
            c(mo3[1], int3[1], int3[2]),
            c(mo4[1], int4[1], int4[2]),
            c(mo5[1], int5[1], int5[2]),
            c(mo6[1], int6[1], int6[2]),
            c(mo7[1], int7[1], int7[2]),
            c(mo8[1], int8[1], int8[2]),
            c(mo9[1], int9[1], int9[2]),
            c(mo10[1],int10[1],int10[2]),
            c(mo11[1],int11[1],int11[2]),
            c(mo12[1],int12[1],int12[2]),
            c(mo13[1],int13[1],int13[2]),
            c(mo14[1],int14[1],int14[2]))


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

res_fin<-cbind(esti,res_cri[,2:5], va_min )

xtable(res_fin,digits = 3) # Resultados en formato latex



#--- Cadenas


postscript("C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell_OS\\Analisis de valores extremos espacial\\Articulo_valores extremos\\Fig_11.eps",
           onefile = FALSE,       # Obligatorio para formato EPS correcto

           horizontal = FALSE
)
par(mfrow = c(3, 3))
plot(seq(1,10000,1), loc1_sill,type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(sigma[eta]))
lines(loc1_sill,col="darkblue",lwd=1)

plot(seq(1,10000,1),  loc1_range,type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(phi[eta]))
lines(loc1_range, col="darkblue",lwd=1)

plot(seq(1,10000,1), loc1_smooth,type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(kappa[eta]))
lines(loc1_smooth,col="darkblue",lwd=1)

plot(seq(1,10000,1),   scale1_sill,type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(sigma[tau]))
lines(scale1_sill,  col="darkblue",lwd=1)

plot(seq(1,10000,1),  scale1_range,type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(phi[tau]))
lines(scale1_range, col="darkblue",lwd=1)

plot(seq(1,10000,1), scale1_smooth,type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(kappa[tau]))
lines(scale1_smooth,col="darkblue",lwd=1)

plot(seq(1,10000,1), shape1_sill,type = "n", xlab=" ",ylab=" ",cex.main = 3,   main=expression(sigma[xi]))
lines(shape1_sill,col="darkblue",lwd=1)

plot(seq(1,10000,1),  shape1_range,type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(phi[xi]))
lines(shape1_range, col="darkblue",lwd=1)

plot(seq(1,10000,1), shape1_smooth,type = "n",xlab=" ",ylab=" ",cex.main = 3, main=expression(kappa[xi]))
lines(shape1_smooth,col="darkblue",lwd=1)
dev.off()



#--- Histogramas

postscript("C:\\Documetos MGM_OS_Dell\\Documentos Maria_Dell_OS\\Analisis de valores extremos espacial\\Articulo_valores extremos\\Fig_12.eps",
           onefile = FALSE ,      # Obligatorio para formato EPS correcto
           horizontal = FALSE
)
par(mfrow = c(3, 3))
hist(loc1_sill, col="darkblue",  cex.main = 3, xlab="", main=expression(sigma[eta]))
hist(loc1_range,col="darkblue", cex.main = 3,xlab="", main=expression(phi[eta]))
hist(loc1_smooth,col="darkblue",cex.main = 3,xlab="",main=expression(kappa[eta]))

hist(scale1_sill,col="darkblue", cex.main = 3,xlab="", main=expression(sigma[tau]))
hist(scale1_range,col="darkblue",cex.main = 3,xlab="", main=expression(phi[tau]))
hist(scale1_smooth,col="darkblue",cex.main = 3,xlab="",main=expression(kappa[tau]))

hist(shape1_sill,col="darkblue", cex.main = 3,xlab="", main=expression(sigma[xi]))
hist(shape1_range,col="darkblue",cex.main = 3,xlab="", main=expression(phi[xi]))
hist(shape1_smooth,col="darkblue",cex.main = 3,xlab="",main=expression(kappa[xi]))
dev.off()

#------ Criterios estadísticos


####################################
plot(borde)


x.grid <- seq(min(borde[,1]),max(borde[,1]),length=20)  # corre con 10, 15 
y.grid <- seq(min(borde[,2]),max(borde[,2]),length=20)

#--- Mapa

options(error = recover) 
res<-map.latent(mc1,    # list
                x.grid,
                y.grid,
                param = "quant",
                ret.per=8,  # revisar 
                level=0.95,
                plot.contour=F,
               # show.data=T,    
                thin=20,      
                col =  brewer.pal(7,"YlOrRd"), 
                fun = median 
               )                                                                                                                                                                                                   



# 2. Extraer las matrices de incertidumbre

nivel_retorno_punto  <- res$post.sum
intervalo_inferior   <- res$ci.low
intervalo_superior   <- res$ci.up


###########################################33
par(mfrow = c(1, 1)) 


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

