#Examen Final: Analisis de datos estadisticos en R -----------------------------

#Camilo Riquelme Horta

#Carga de paquetes

pacman::p_load(haven, tidyverse, sjmisc, sjPlot)

#Carga de datos

data = read_sav("input/data/ene-2022-04-mam.sav")

#Exploración de variables ------------------------------------------------------

names(data)
head(data)

sjPlot::view_df(data, encoding = "UTF-8")

frq(data$mes_central)
frq(data$cine) #nivel educacional
frq(data$sexo) #sexo
frq(data$c2_1_1) #horas diarias de trabajo
frq(data$c2_1_2) #días a la semana de trabajo
frq(data$c2_1_3) #horas semanales de trabajo
frq(data$region) #region
frq(data$b1) #grupo ocupacional según ciuo 08

#Procesamiento -----------------------------------------------------------------

  data_proc = data%>%
    filter(edad>=18, proveedor==1)%>%
    mutate_at(vars(c2_1_1, c2_1_2, c2_1_3), 
            ~(car::recode(., recodes = c("c(888, 8888, 999, 9999) = NA"))))%>%
    mutate(edad_tr = case_when(edad>=18 & edad<=29 ~"Jovenes", #Tramos etarios según características de la población INE
                               edad>=30 & edad<=59 ~"Adultos",
                               edad>=60 ~"Adultos mayores"),
           sexo = case_when(sexo==1 ~"Hombre", sexo==2~"Mujer"),
           cine = case_when(cine>=3 & cine<=4 ~"Educacion Basica", #Niveles educacionales según distintos reportes encuesta ESI y encuesta ENE
                            cine==5 ~"Educacion Secundaria",
                            cine==6 ~"Educacion tecnica",
                            cine==7 ~"Educacion Universitaria",
                            cine>=8 & cine<=9 ~"Postitulos, maestrías, doctorado",
                            TRUE~NA_character_),
           b1 = case_when(b1==1~"Directores, gerentes y administradores",
                          b1==2~"Profesionales, científicos e intelectuales",
                          b1==3~"Técnicos y profesionales de nivel medio",
                          b1==4~"Personal de apoyo administrativo",
                          b1==5~"Trabajadores de los servicios y vendedores de comercios y mercados",
                          b1==6~"Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros",
                          b1==7~"Artesanos y operarios de oficios",
                          b1==8~"Operadores de instalaciones, maquinas y ensambladores",
                          b1==9~"Ocupaciones elementales", TRUE~NA_character_),
           region = case_when(region==1~"Tarapaca",
                              region==2~"Antofagasta",
                              region==3~"Atacama",
                              region==4~"Coquimbo",
                              region==5~"Valparaiso",
                              region==6~"O'Higgins",
                              region==7~"Maule",
                              region==8~"Biobio",
                              region==9~"La Araucania",
                              region==10~"Los Lagos",
                              region==11~"Aysen",
                              region==12~"Magallanes",
                              region==13~"Metropolitana",
                              region==14~"Los Rios",
                              region==15~"Arica y Parinacota",
                              region==16~"Ñuble", TRUE~NA_character_),
           proveedor = case_when(proveedor==1~"Proveedor principal", 
                                 TRUE~NA_character_))%>%
    group_by(region)%>%
    mutate(prom_hrs_drs_reg = mean(c2_1_1, na.rm = T),
           prom_hrs_sem_reg = mean(c2_1_3, na.rm = T),
           prom_dias_tr_reg = mean(c2_1_2, na.rm = T))%>%
    ungroup()%>%
    group_by(sexo)%>%
    mutate(prom_hrs_drs_sexo = mean(c2_1_1, na.rm = T),
           prom_hrs_sem_sexo = mean(c2_1_3, na.rm = T),
           prom_dias_tr_sexo = mean(c2_1_2, na.rm = T))%>%
    ungroup()%>%
    select(id_identificacion, ano_encuesta, mes_central, sexo, edad, edad_tr, 
           region, cine, proveedor, b1, prom_hrs_drs_reg, prom_hrs_sem_reg, 
           prom_dias_tr_reg, prom_hrs_drs_sexo, prom_hrs_sem_sexo, 
           prom_dias_tr_sexo, estrato, fact_cal)

#Revision de variables procesadas ----------------------------------------------

frq(data_proc$edad) #edad por tramos
frq(data_proc$cine) #nivel educacional
frq(data_proc$sexo) #sexo
frq(data_proc$region) #region
frq(data_proc$b1) #grupo ocupacional según ciuo 08
frq(data_proc$proveedor)
frq(data_proc$prom_hrs_drs_reg) #promedio horas diarias de trabajo por region
frq(data_proc$prom_hrs_sem_reg) #promedio horas semanales de trabajo por region
frq(data_proc$prom_dias_tr_reg) #promedio dias de trabajo a la semana por region
frq(data_proc$prom_hrs_drs_sexo) #promedio horas diarias de trabajo por sexo
frq(data_proc$prom_hrs_sem_sexo) #promedio horas semanales de trabajo por sexo
frq(data_proc$prom_dias_tr_sexo) #promedio dias de trabajo a la semana por sexo

sjPlot::view_df(data_proc, encoding = "UTF-8")

#Guardar datos -----------------------------------------------------------------

saveRDS(data_proc, file = "output/data/datos_proc.rds")
