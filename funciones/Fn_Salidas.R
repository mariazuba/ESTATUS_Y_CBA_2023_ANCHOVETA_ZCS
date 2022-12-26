
# ARREGLO DATOS PARA SALIDAS DE TABLAS Y FIGURAS ----
rm(list=ls(all=T))

library(stringr) # para arreglo de archivo .dat
library(tidyverse)
library(kableExtra) # genera tablas
library(ggplot2) # genera figuras
library(ggthemes) # para ggplot
library(patchwork) # para unir gráficos de ggplot
library(dplyr)  # para usar melt
library(reshape) # para usar melt
library(here)


dir.0   <- here() # directorio de trabajo 
dir.1   <- paste(dir.0,"/codigos_admb",sep="") # carpeta de códigos ADMB 
dir.fun <- paste(dir.0,"/funciones/",sep="") # carpeta de funciones utilizadas en este informe
source(paste(dir.fun,"functions.R",sep="")) # funciones para leer .dat y .rep
source(paste(dir.fun,"Fn_PBRs.R",sep="")) # funciones para leer .dat y .rep


setwd(dir.1)

admb_dat  <- list.files(pattern=".dat")
admb_rep  <- list.files(pattern=".rep")
admb_std  <- list.files(pattern=".std")

admb_sept <- str_sub(admb_dat[3], 1, 6)
admb_mar  <- str_sub(admb_dat[1], 1, 6)
admb_jul  <- str_sub(admb_dat[2], 1, 6)

carpetaCBA_sept <- "/CBA_sept/"
carpetaCBA_mar  <- "/CBA_mar/"
carpetaCBA_jul  <- "/CBA_jul/"

# ASESORÍA DE SEPTIEMBRE ----
data1        <- lisread(admb_dat[3]) 
names(data1) <- str_trim(names(data1), side="right")
dat1         <- data1
rep1         <- reptoRlist(admb_rep[3])
std1         <- read.table(admb_std[3],header=T,sep="",na="NA",fill=T) 

# ASESORÍA DE MARZO ----
data2        <- lisread(admb_dat[1]) 
names(data2) <- str_trim(names(data2), side="right")
dat2         <- data2
rep2         <- reptoRlist(admb_rep[1])
std2         <- read.table(admb_std[1],header=T,sep="",na="NA",fill=T) 

# ASESORÍA DE JULIO ----
data3        <- lisread(admb_dat[2]) 
names(data3) <- str_trim(names(data3), side="right")
dat3         <- data3
rep3         <- reptoRlist(admb_rep[2])
std3         <- read.table(admb_std[2],header=T,sep="",na="NA",fill=T) 

# AÑOS BIOLOGICO ANCHOVETA ----
yearsb  <- c("1996/97","1997/98","1998/99","1999/00","2000/01","2001/02","2002/03",
             "2003/04","2004/05","2005/06","2006/07","2007/08","2008/09","2009/10",
             "2010/11","2011/12","2012/13","2013/14","2014/15","2015/16","2016/17",
             "2017/18","2018/19","2019/20",'2020/21','2021/22','2022/23')

years1  <- rep1$years
nyears1 <- length(years1)

years2  <- rep2$years
nyears2 <- length(years2)

years3  <- rep3$years
nyears3 <- length(years3)

age     <- seq(0,4,1)                                            
nage    <- length(age)   


#  1. AJUSTES INDICES DE ABUNDANCIA ><> ><> ><> ><> ----
cvBcV   <-0.30
cvBcO   <-0.30
cvdes   <-0.01

names_ind<- c('Crucero_verano', 
              'Crucero_otoño',
              'Crucero_huevos', 
              'Desembarques') 
#Observados
indobs_sept  <- data.frame(rep1$reclasobs,
                         rep1$pelacesobs,
                         rep1$mphobs,
                         rep1$desembarqueobs) %>% 
                na_if(0) %>% 
                magrittr::set_colnames(names_ind) %>%
                mutate(Asesoria='Hito 1: septiembre',type='observado',yrs= years1) %>% 
                melt(id.var=c('yrs','type', 'Asesoria'))  

indobs_marzo <- data.frame(rep2$reclasobs,
                          rep2$pelacesobs,
                          rep2$mphobs,
                          rep2$desembarqueobs) %>% 
                na_if(0)%>% 
                magrittr::set_colnames(names_ind) %>%
                mutate(Asesoria='Hito 2: marzo',type='observado',yrs= years2) %>% 
                melt(id.var=c('yrs','type', 'Asesoria'))  

indobs_julio<- data.frame(rep3$reclasobs,
                          rep3$pelacesobs,
                          rep3$mphobs,
                          rep3$desembarqueobs) %>% 
                na_if(0)%>% 
                magrittr::set_colnames(names_ind)%>%
                mutate(Asesoria='Hito 3: julio',type='observado',yrs= years3) %>% 
                melt(id.var=c('yrs','type', 'Asesoria'))  
#Predichos 

indpred_sept<- data.frame(rep1$reclaspred, 
                          rep1$pelacespred, 
                          rep1$mphpred,
                          rep1$desembarquepred) %>% 
                magrittr::set_colnames(names_ind) %>% 
                mutate (Asesoria='Hito 1: septiembre',type='predicho',yrs= years1)  %>% 
                melt(id.var=c('yrs','type', 'Asesoria'))

indpred_marzo <- data.frame(rep2$reclaspred,
                           rep2$pelacespred,
                           rep2$mphpred,
                           rep2$desembarquepred) %>% 
                 magrittr::set_colnames(names_ind) %>% 
                 mutate (Asesoria='Hito 2: marzo',type='predicho',yrs= years2)  %>% 
                 melt(id.var=c('yrs','type', 'Asesoria'))
                
indpred_julio <- data.frame(rep3$reclaspred,
                           rep3$pelacespred, 
                           rep3$mphpred,
                           rep3$desembarquepred) %>% 
                 magrittr::set_colnames(names_ind) %>% 
                 mutate (Asesoria='Hito 3: julio',type='predicho',yrs= years3)  %>% 
                 melt(id.var=c('yrs','type', 'Asesoria'))

base1 <- merge(indobs_sept, merge(indpred_sept, indpred_marzo, all = TRUE), all = TRUE)  


# 2. AJUSTES COMPOSICIONES DE EDAD  ><> ><> ><> ><> ----
# FLOTA ----
# Observado

a<-identical(rep1$pf_obs,rep2$pf_obs)
if(a==FALSE){
  pf_obs<-rbind(rep1$pf_obs,rep(0,nage))
  pf_pred<-rbind(rep1$pf_pred,rep(0,nage))} else{
  pf_obs  <-rep1$pf_obs
  pf_pred <-rep1$pf_pred}


obsf_sept  <- as.data.frame(pf_obs) %>% 
              magrittr::set_colnames(age)%>% 
              mutate(yrs=years2,
                     Asesoria='Hito 1: septiembre',
                     flota='flota',
                     type='observado') %>% 
              melt(id.vars=c('yrs','Asesoria','flota','type'))

obsf_marzo  <- as.data.frame(rep2$pf_obs) %>% 
               magrittr::set_colnames(age)%>% 
               mutate(yrs=years2, 
                      Asesoria='Hito 2: marzo',
                      flota='flota',
                      type='observado') %>% 
               melt(id.vars=c('yrs','Asesoria','flota','type')) 

obsf_julio  <- as.data.frame(rep3$pf_obs) %>% 
               magrittr::set_colnames(age)%>% 
               mutate(yrs=years3,
                      Asesoria='Hito 3: julio',
                      flota='flota',
                      type='observado') %>% 
               melt(id.vars=c('yrs','Asesoria','flota','type')) 

# Predicho
predf_sept <- as.data.frame(pf_pred) %>% 
              magrittr::set_colnames(age)%>% 
              mutate(yrs=years2,
                     Asesoria='Hito 1: septiembre',
                     flota='flota',
                     type='predicho')%>%
              melt(id.vars=c('yrs','Asesoria','flota','type')) 

predf_marzo <- as.data.frame(rep2$pf_pred ) %>% 
               magrittr::set_colnames(age)%>% 
               mutate(yrs=years2,
                      Asesoria='Hito 2: marzo',
                      flota='flota',
                      type='predicho')%>%
               melt(id.vars=c('yrs','Asesoria','flota','type'))

predf_julio <- as.data.frame(rep3$pf_pred) %>% 
               magrittr::set_colnames(age)%>% 
               mutate(yrs=years3,
                      Asesoria='Hito 3: julio',
                      flota='flota',
                      type='predicho')%>%
               melt(id.vars=c('yrs','Asesoria','flota','type'))

#CRUCERO DE VERANO ----
a<-identical(rep1$pobs_RECLAS,rep2$pobs_RECLAS)
if(a==FALSE){
  pobs_RECLAS<-rbind(rep1$pobs_RECLAS,rep(0,nage))
  ppred_RECLAS<-rbind(rep1$ppred_RECLAS,rep(0,nage))} else{
    pobs_RECLAS  <-rep1$pobs_RECLAS
    ppred_RECLAS <-rep1$ppred_RECLAS}

# Observado
obsr_sept  <- as.data.frame(pobs_RECLAS) %>% 
              magrittr::set_colnames(age)%>% 
              mutate(yrs=years2,
                     Asesoria='Hito 1: septiembre',
                     flota='Crucero_verano',
                     type='observado')%>%
              melt(id.vars=c('yrs','Asesoria','flota','type'))

obsr_marzo <- as.data.frame(rep2$pobs_RECLAS) %>% 
              magrittr::set_colnames(age)%>% 
              mutate(yrs=years2, 
                     Asesoria='Hito 2: marzo',
                     flota='Crucero_verano',
                     type='observado') %>% 
               melt(id.vars=c('yrs','Asesoria','flota','type')) 

obsr_julio <- as.data.frame(rep3$pobs_RECLAS) %>% 
              magrittr::set_colnames(age)%>% 
              mutate(yrs=years3,
                     Asesoria='Hito 3: julio',
                     flota='Crucero_verano',
                     type='observado') %>% 
              melt(id.vars=c('yrs','Asesoria','flota','type')) 

# Predicho
predr_sept <- as.data.frame(ppred_RECLAS) %>% 
             magrittr::set_colnames(age)%>% 
             mutate(yrs=years2,
             Asesoria='Hito 1: septiembre',
             flota='Crucero_verano',
             type='predicho')%>%
             melt(id.vars=c('yrs','Asesoria','flota','type')) 

predr_marzo <- as.data.frame(rep2$ppred_RECLAS) %>% 
               magrittr::set_colnames(age)%>% 
               mutate(yrs=years2,
               Asesoria='Hito 2: marzo',
               flota='Crucero_verano',
               type='predicho')%>%
               melt(id.vars=c('yrs','Asesoria','flota','type'))

predr_julio <- as.data.frame(rep3$ppred_RECLAS) %>% 
               magrittr::set_colnames(age)%>% 
               mutate(yrs=years3,
               Asesoria='Hito 3: julio',
               flota='Crucero_verano',
               type='predicho')%>%
               melt(id.vars=c('yrs','Asesoria','flota','type'))

#CRUCERO DE OTOÑO ----

a<-identical(rep1$pobs_PELACES,rep2$pobs_PELACES)
if(a==FALSE){
  pobs_PELACES<-rbind(rep1$pobs_PELACES,rep(0,nage))
  ppred_PELACES<-rbind(rep1$ppred_PELACES,rep(0,nage))} else{
    pobs_PELACES<-rep1$pobs_PELACES
    ppred_PELACES<-rep1$ppred_PELACES}

# Observado
obsp_sept  <- as.data.frame(pobs_PELACES) %>% 
  magrittr::set_colnames(age)%>% 
  mutate(yrs=years2,
         Asesoria='Hito 1: septiembre',
         flota='Crucero_otoño',
         type='observado')%>%
  melt(id.vars=c('yrs','Asesoria','flota','type'))

obsp_marzo  <- as.data.frame(rep2$pobs_PELACES) %>% 
  magrittr::set_colnames(age)%>% 
  mutate(yrs=years2, 
         Asesoria='Hito 2: marzo',
         flota='Crucero_otoño',
         type='observado') %>% 
  melt(id.vars=c('yrs','Asesoria','flota','type'))

obsp_julio  <- as.data.frame(rep3$pobs_PELACES) %>% 
  magrittr::set_colnames(age)%>% 
  mutate(yrs=years3,
         Asesoria='Hito 3: julio',
         flota='Crucero_otoño',
         type='observado') %>% 
  melt(id.vars=c('yrs','Asesoria','flota','type'))

# Predicho
predp_sept <- as.data.frame(ppred_PELACES) %>% 
  magrittr::set_colnames(age)%>% 
  mutate(yrs=years2,
         Asesoria='Hito 1: septiembre',
         flota='Crucero_otoño',
         type='predicho')%>%
  melt(id.vars=c('yrs','Asesoria','flota','type')) 

predp_marzo <- as.data.frame(rep2$ppred_PELACES) %>% 
  magrittr::set_colnames(age)%>% 
  mutate(yrs=years2,
         Asesoria='Hito 2: marzo',
         flota='Crucero_otoño',
         type='predicho')%>%
  melt(id.vars=c('yrs','Asesoria','flota','type'))

predp_julio <- as.data.frame(rep3$ppred_PELACES) %>% 
  magrittr::set_colnames(age)%>% 
  mutate(yrs=years3,
         Asesoria='Hito 3: julio',
         flota='Crucero_otoño',
         type='predicho')%>%
  melt(id.vars=c('yrs','Asesoria','flota','type'))

# 3. RESIDUOS ><> ><> ><> ><> ----
# índices de abundancia ----

Res_sept <- data.frame(Res=(log(indobs_sept$value)-log(indpred_sept$value)),
                       Pred=log(indpred_sept$value))

Res_marzo <- data.frame(Res=(log(indobs_marzo$value)-log(indpred_marzo$value)),
                        Pred=log(indpred_marzo$value))

Res_julio <- data.frame(Res=(log(indobs_julio$value)-log(indpred_julio$value)),
                        Pred=log(indpred_julio$value))


# Descripción de los datos para Metodología----

# sobre índices ----
year<-format(dat2$Ind[,1],nsmall=0, big.mark="")
recl<-dat2$Ind[,2]
pela<-dat2$Ind[,4]
mpdh<-dat2$Ind[,6]

dataInd<-data.frame(year,recl,pela,mpdh)

#  sobre datos de captura ----
bioyear<-yearsb

desemYdesc_previo<-c(350755,77701,442110,56441,14545,235359,
                     269955,359681,431902,328805,639364,411747,
                     362871,311530,167758,66681,60226,58785,
                     57116,71774,51957,67425,138520,160799,209506,203330,NA)

desc_previo       <- c(rep(1,4),rep(1.04,15),rep(1.02,1),rep(1.06,1),1.01,rep(1.02,5))
desembarque       <- desemYdesc_previo/desc_previo
desc_actualizado  <- c(rep(1,4),rep(1.04,17),1.014,1.021,1.018,1.02,1.02,1.02)
CapturaDescartada <- -(1-desc_actualizado)*desembarque
CapturaTotal      <- desembarque+CapturaDescartada

porcDesc_actualizado<-c(rep("0\\%",4),rep("4\\%",17),rep("1,4\\%",1),rep("2,1\\%",1),rep("1,8\\%",1),rep("2\\%",3))

dataDes_y_descar<-data.frame("Año"=bioyear,
                             "Desembarques"=round(desembarque,0),
                             "Pdescarte"=porcDesc_actualizado,
                             "Capturadesc"=round(CapturaDescartada,0),
                             "Capturatotal"=round(CapturaTotal,0))



# 4. COMPARACIÓN CON ASESORÍAS PREVIAS ><> ><> ><> ><> ----

dir<-paste(dir.0,"/rep_AsesoriasPrevias",sep="")

sept20 <-paste(dir,"/MAE0920b.rep",sep="")
mar21  <-paste(dir,"/MAE0321b.rep",sep="")
jul21  <-paste(dir,"/MAE0721b.rep",sep="")
sept21 <-paste(dir,"/MAE0921b.rep",sep="")
mar22  <-paste(dir,"/MAE322.rep",sep="")
jul22  <-paste(dir.1,"/MAE722.rep",sep="")
sept22 <-paste(dir.1,"/MAE922.rep",sep="")

rep_sept20 <- reptoRlist(sept20)
rep_mar21  <- reptoRlist(mar21)
rep_jul21  <- reptoRlist(jul21)
rep_sept21 <- reptoRlist(sept21)
rep_mar22  <- reptoRlist(mar22)
rep_jul22  <- reptoRlist(jul22)
rep_sept22 <- reptoRlist(sept22)

years  <- rep_sept22$years
nyears <- length(years)                                                                
x  <-c(years,rev(years))
x1 <-c(years[1],years[nyears]+1,nyears+1/2) #xaxp
x2 <-c(years[1]-1,years[nyears]+1) #xlim

Rtcomp <- data.frame(x=years,
                     Rt_sept20=c(rep_sept20$Reclutas,NA,NA),
                     Rt_mar21=c(rep_mar21$Reclutas,NA),
                     Rt_jul21=c(rep_jul21$Reclutas,NA),
                     Rt_sept21=c(rep_sept21$Reclutas,NA),
                     Rt_mar22=c(rep_mar22$Reclutas),
                     Rt_jul22=c(rep_jul22$Reclutas),
                     Rt_sept22=c(rep_sept22$Reclutas))

SSBtcomp <- data.frame(x=years,
                       SSBt_sept20=c(rep_sept20$SSB,NA,NA),
                       SSBt_mar21=c(rep_mar21$SSB,NA),
                       SSBt_jul21=c(rep_jul21$SSB,NA),
                       SSBt_sept21=c(rep_sept21$SSB,NA),
                       SSBt_mar22=c(rep_mar22$SSB),
                       SSBt_jul22=c(rep_jul22$SSB),
                       SSBt_sept22=c(rep_sept22$SSB))

Ftcomp <- data.frame(x=years,
                     Ft_sept20=c(rep_sept20$Ftot,NA,NA),
                     Ft_mar21=c(rep_mar21$Ftot,NA),
                     Ft_jul21=c(rep_jul21$Ftot,NA),
                     Ft_sept21=c(rep_sept21$Ftot,NA),
                     Ft_mar22=c(rep_mar22$Ftot),
                     Ft_jul22=c(rep_jul22$Ftot),
                     Ft_sept22=c(rep_sept22$Ftot))

year_retros <- c('2022_sept','2022_julio','2022_marzo','2021_sept',
                 "2021_julio","2021_marzo","2020_sept")
nretros <-7

# 5. PERFIL VEROSIMILITUD ><> ><> ><> ><> ----

dir<-paste(dir.0,"/Verosimilitud_sept",sep="")
setwd(dir)
admb<-"/MAE922"

casos <-23
logRo    <- rep(0,casos)
likeval  <- matrix(ncol=15,nrow=casos)
slikeval <- matrix(ncol=16,nrow=casos)

#
for(i in 1:casos){
  report      <- reptoRlist(paste(dir,admb,i,".rep",sep=""))
  logRo[i]    <- report$log_Ro
  likeval[i,] <- report$likeval}

like    <- data.frame(round(likeval,3),Total=apply(likeval,1,sum))
minLik  <- apply(like,2,min)  

# busca el mínimo
for(i in 1:16){
  slikeval[,i]<-like[,i]-minLik[i]
}    # Estandarización

names<-c("Ro","Bio_Reclas","Bio_Pelaces","Desembarques","Bio_Mph","C.Edad_Flota",
         "C.Edad_Recl","C.Edad_Pel","prepPelTall","DesvRt","qreclas","qpela","PenFt",
         "PenFspr","NA","NA","Total")
# Tabla verosimilitud
TLk1 <- data.frame(exp(logRo),like);colnames(TLk1)<-names
# Tabla estandarizada
TLk2<- data.frame(exp(logRo),slikeval);colnames(TLk2)<-names

Ro_reclas  <- TLk2$Ro[TLk2$Bio_Reclas==0]
Ro_pelaces <- TLk2$Ro[TLk2$Bio_Pelaces==0]
Ro_desemb  <- min(TLk2$Ro[TLk2$Desembarques==0])
Ro_MPDH    <- ifelse(sum(TLk2$Bio_Mph)>0,0,TLk2$Ro[TLk2$Bio_Mph==0])
Ro_propF   <- TLk2$Ro[TLk2$C.Edad_Flota==0]
Ro_propRecl<- TLk2$Ro[TLk2$C.Edad_Recl==0]
Ro_propPel <- TLk2$Ro[TLk2$C.Edad_Pel==0]
Ro         <- TLk2$Ro[TLk2$Total==0]

names_res<-c("Bio_Reclas","Bio_Pelaces","Desembarques","Bio_MPDH","C.Edad_Flota","C.Edad_Reclas","C.Edad_Pelaces")
res<-c((Ro_reclas-Ro),
       (Ro_pelaces-Ro),
       (Ro_desemb-Ro),
       (Ro_MPDH-Ro),
       (Ro_propF-Ro),
       (Ro_propRecl-Ro),
       (Ro_propPel-Ro))

residuos<-res/Ro
#datares<-data.frame(names_res,residuos)

setwd(dir.1)

# 7. VARIABLES POBLACIONALES ><> ><> ><> ><> ----
# HITO 1: SEPTIEMBRE ----

Rt1      <- c(subset(std1,name=="Reclutas")$value,NA) 
Rt1std   <- c(subset(std1,name=="Reclutas")$std,NA)
BT1      <- c(subset(std1,name=="BT")$value,NA)   
BT1std   <- c(subset(std1,name=="BT")$std,NA)
BD1      <- c(subset(std1,name=="SSB")$value,NA)   
BD1std   <- c(subset(std1,name=="SSB")$std,NA)
Ft1      <- c(subset(std1,name=="log_Ft")$value,NA)   
Ft1std   <- c(subset(std1,name=="log_Ft")$std,NA)

VarPobSep<- data.frame(x=years2, 
                       Rt1=Rt1,
                       BT1=BT1,
                       BD1=BD1,
                       Ft1=exp(Ft1), 
                       lowerRt1 = (Rt1-1.96*Rt1std), 
                       upperRt1 = (Rt1+1.96*Rt1std),
                       lowerBT1 = (BT1-1.96*BT1std), 
                       upperBT1 = (BT1+1.96*BT1std),
                       lowerBD1 = (BD1-1.96*BD1std), 
                       upperBD1 = (BD1+1.96*BD1std),
                       lowerFt1 = exp(Ft1-1.96*Ft1std), 
                       upperFt1 = exp(Ft1+1.96*Ft1std))
# HITO 2: MARZO----
years2<-rep2$years
nyears2<-length(years2)

Rt2      <- subset(std2,name=="Reclutas")$value 
Rt2std   <- subset(std2,name=="Reclutas")$std
BT2      <- subset(std2,name=="BT")$value   
BT2std   <- subset(std2,name=="BT")$std
BD2      <- subset(std2,name=="SSB")$value   
BD2std   <- subset(std2,name=="SSB")$std
Ft2      <- subset(std2,name=="log_Ft")$value   
Ft2std   <- subset(std2,name=="log_Ft")$std

VarPobMar<- data.frame(x=years2, 
                       Rt2=Rt2,
                       BT2=BT2,
                       BD2=BD2,
                       Ft2=exp(Ft2), 
                       lowerRt2 = (Rt2 -1.96*Rt2std), 
                       upperRt2 = (Rt2+1.96*Rt2std),
                       lowerBT2 = (BT2 -1.96*BT2std), 
                       upperBT2 = (BT2+1.96*BT2std),
                       lowerBD2 = (BD2 -1.96*BD2std), 
                       upperBD2 = (BD2+1.96*BD2std),
                       lowerFt2 = exp(Ft2 -1.96*Ft2std), 
                       upperFt2 = exp(Ft2+1.96*Ft2std))
# HITO 3: JULIO----
years3  <- rep3$years
nyears3 <- length(years3)

Rt3      <- subset(std3,name=="Reclutas")$value 
Rt3std   <- subset(std3,name=="Reclutas")$std
BT3      <- subset(std3,name=="BT")$value   
BT3std   <- subset(std3,name=="BT")$std
BD3      <- subset(std3,name=="SSB")$value   
BD3std   <- subset(std3,name=="SSB")$std
Ft3      <- subset(std3,name=="log_Ft")$value   
Ft3std   <- subset(std3,name=="log_Ft")$std

VarPobJul<- data.frame(x=years3, 
                       Rt3=Rt3,
                       BT3=BT3,
                       BD3=BD3,
                       Ft3=exp(Ft3), 
                       lowerRt3 = (Rt3 -1.96*Rt3std), 
                       upperRt3 = (Rt3 +1.96*Rt3std),
                       lowerBT3 = (BT3 -1.96*BT3std), 
                       upperBT3 = (BT3 +1.96*BT3std),
                       lowerBD3 = (BD3 -1.96*BD3std), 
                       upperBD3 = (BD3 +1.96*BD3std),
                       lowerFt3 = exp(Ft3 -1.96*Ft3std), 
                       upperFt3 = exp(Ft3 +1.96*Ft3std))

# 6. ANÁLISIS RETROSPECTIVO ><> ><> ><> ><> ----
dir<-paste(dir.0,"/Retrospectivo_sept",sep="")
setwd(dir)
admb<-str_sub(admb_dat[3], 1, 6)


retros2  <- seq(1,5)
nretros2 <- length(retros2)
year_retros2<-as.factor(years1[(nyears1-(nretros2-1)):nyears1])

retroR      <- matrix(0,nrow=nyears1,ncol=nretros2+1)
retroBD     <- matrix(0,nrow=nyears1,ncol=nretros2+1)
retroF      <- matrix(0,nrow=nyears1,ncol=nretros2+1)

for(i in 1:length(retros2)){
  rep<- reptoRlist(paste(admb,"s",i,".rep",sep=""))
  retroR[,i+1] <- c(rep$Reclutas,rep(NA,i-1))
  retroBD[,i+1] <- c(rep$SSB,rep(NA,i-1))
  retroF[,i+1]  <- c(rep$Ftot,rep(NA,i-1)) }


# retrospectivo relativo (cálculo)
mohn.r       <- rep(NA, nretros2)
rel.diff.r   <- matrix(NA, nrow=nyears1, ncol=(nretros2))
mohn.ssb     <- rep(NA, nretros2)
rel.diff.ssb <- matrix(NA, nrow=nyears1, ncol=(nretros2))
mohn.f       <- rep(NA, nretros2)
rel.diff.f   <- matrix(NA, nrow=nyears1, ncol=(nretros2))

for(j in 1:nretros2){
  rel.diff.r[,j]   <- (retroR[,(j+1)]-retroR[,2])/retroR[,2]
  mohn.r[j]        <- rel.diff.r[(nyears1-j),j]
  rel.diff.ssb[,j] <- (retroBD[,(j+1)]-retroBD[,2])/retroBD[,2]
  mohn.ssb[j]      <- rel.diff.ssb[(nyears1-j),j]
  rel.diff.f[,j]   <- (retroF[,(j+1)]-retroF[,2])/retroF[,2]
  mohn.f[j]        <- rel.diff.f[(nyears1-j),j]}

ave.mohn.r    <- mean(mohn.r)
ave.mohn.ssb  <- mean(mohn.ssb)
ave.mohn.f    <- mean(mohn.f)

# Arreglo datos

#Para retrospectivo tradicional
Rt_retro<- data.frame(x=years1, 
                      y1=retroR[,2],
                      y2=retroR[,3],
                      y3=retroR[,4],
                      y4=retroR[,5],
                      y5=retroR[,6],
                      lower = (Rt1 -1.96*Rt1std),
                      upper = (Rt1 +1.96*Rt1std))
BD_retro<- data.frame(x=years1,
                      y1=retroBD[,2],
                      y2=retroBD[,3],
                      y3=retroBD[,4],
                      y4=retroBD[,5],
                      y5=retroBD[,6],
                      lower = (BD1 -1.96*BD1std),
                      upper = (BD1 +1.96*BD1std))
Ft_retro<- data.frame(x=years1,
                      y1=retroF[,2],
                      y2=retroF[,3],
                      y3=retroF[,4],
                      y4=retroF[,5],
                      y5=retroF[,6], 
                      lower = exp(Ft1-1.96*Ft1std),
                      upper = exp(Ft1+1.96*Ft1std))

#Para restrospectivo relativo
Rt_retroRel<- data.frame(x=years1,
                         y1=rel.diff.r[,1],
                         y2=rel.diff.r[,2],
                         y3=rel.diff.r[,3],
                         y4=rel.diff.r[,4],
                         y5=rel.diff.r[,5])
BD_retroRel<- data.frame(x=years1,
                         y1=rel.diff.ssb[,1],
                         y2=rel.diff.ssb[,2],
                         y3=rel.diff.ssb[,3],
                         y4=rel.diff.ssb[,4],
                         y5=rel.diff.ssb[,5])
Ft_retroRel<- data.frame(x=years1, 
                         y1=rel.diff.f[,1],
                         y2=rel.diff.f[,2],
                         y3=rel.diff.f[,3],
                         y4=rel.diff.f[,4],
                         y5=rel.diff.f[,5])

# 8. PUNTOS BIOLÓGICOS DE REFERENCIA ><> ><> ><> ><> ----

# HITO 1: SEPTIEMBRE----
#PBR año biologico
Amax        <- dat1$nedades 
Fmort 	    <- seq(0,3.5,0.02)           
nf          <- length(Fmort) 
R0 		      <- 1  
#datos de entrada 
Dat<-list()
Dat$M		    <- dat1$par[5]             
Dat$Tspw	  <- dat1$Dt[3]              
Dat$Mad	    <- dat1$madurezsexual        
Dat$Wmed	  <- colMeans(dat1$Wmed)      
Dat$Wini	  <- colMeans(dat1$Wini) 
Dat$Sel     <- rep1$S_f[1,] 

Rmed1        <- mean(Rt1,na.rm = T)           
Bmed1        <- mean(BD1,na.rm = T)         
Fmedian1     <- exp(median(Ft1,na.rm = T))

Bobj         <-c(.85,.80,.60,.55,.52,.50,.45,.40,.30,.325,0.425)
Fobj      	 <- optim(par=rep(0.,11),fn=SPRFpbr,method='BFGS')

SPR1 		     <- SPRFmort(Rmed1,c(0,Fobj$par,Fmedian1,rep1$Ftot[25]),Amax,Dat) 
pSPR_Fmh1    <- as.numeric(SPR1[13,4])     # Paso 2: Cálculo de la curva SPR
pB_Fmh1      <- pSPR_Fmh1-0.05          # Paso 3: Aproximación obtención de %BD(Fmh)
SPRcurv1 		 <- SPRFmort(R0,Fmort,Amax,Dat) 
# PASOS
Bo1           <- rep1$SSBpbr[1]        # Paso 4: Obtenci?n de Bo
BRMS1         <- rep1$SSBpbr[3]        # Paso 5: Obtenci?n de Brms = 60%SPRo = 55%Bo
FRMS1         <- rep1$Fs[2]
BLIM1         <- Bo1*0.275             # Paso 6: Obtenci?n de Blim = 20%Bo 
FLIM1         <- rep1$Fs[3]            # Paso 6: Obtenci?n de Flim = 30%SPRo
SpB1          <- BD1                   # BD serie hist?rica de evaluaci?n de stock 
SpBSE1        <- BD1std                # desviaci?n estandar BD
ln_Fyr1       <- Ft1                   # logaritmo de Ft
ln_FSE1       <- Ft1std                # logaritmo de la desviaci?n standar de Ft

# HITO 2: MARZO----
#PBR año biologico
Amax        <- dat2$nedades 
Fmort 	    <- seq(0,3.5,0.02)           
nf          <- length(Fmort) 
R0 		      <- 1  
#datos de entrada 
Dat<-list()
Dat$M		    <- dat2$par[5]             
Dat$Tspw	  <- dat2$Dt[3]              
Dat$Mad	    <- dat2$madurezsexual        
Dat$Wmed	  <- colMeans(dat2$Wmed)      
Dat$Wini	  <- colMeans(dat2$Wini) 
Dat$Sel     <- rep2$S_f[1,] 

Rmed2        <- mean(Rt2)           
Bmed2        <- mean(BD2)         
Fmedian2     <- exp(median(Ft2))

Bobj         <-c(.85,.80,.60,.55,.52,.50,.45,.40,.30,.325,0.425)
Fobj         <- optim(par=rep(0.,11),fn=SPRFpbr,method='BFGS')

SPR2 		     <- SPRFmort(Rmed2,c(0,Fobj$par,Fmedian2,rep2$Ftot[25]),Amax,Dat) 
pSPR_Fmh2    <- as.numeric(SPR2[13,4])  # Paso 2: Cálculo de la curva SPR
pB_Fmh2      <- pSPR_Fmh2-0.05      # Paso 3: Aproximación obtención de %BD(Fmh)
SPRcurv2 		 <- SPRFmort(R0,Fmort,Amax,Dat) 
# PASOS
Bo2           <- rep2$SSBpbr[1]       # Paso 4: Obtenci?n de Bo
BRMS2         <- rep2$SSBpbr[3]       # Paso 5: Obtenci?n de Brms = 60%SPRo = 55%Bo
FRMS2         <- rep2$Fs[2]
BLIM2         <- Bo2*0.275            # Paso 6: Obtenci?n de Blim = 20%Bo 
FLIM2         <- rep2$Fs[3]           # Paso 6: Obtenci?n de Flim = 30%SPRo
SpB2          <- BD2                  # BD serie hist?rica de evaluaci?n de stock 
SpBSE2        <- BD2std               # desviaci?n estandar BD
ln_Fyr2       <- Ft2                  # logaritmo de Ft
ln_FSE2       <- Ft2std    

# HITO 3: JULIO----
#PBR año biologico
Amax        <- dat3$nedades 
Fmort 	    <- seq(0,3.5,0.02)           
nf          <- length(Fmort) 
R0 		      <- 1  
#datos de entrada 
Dat<-list()
Dat$M		    <- dat3$par[5]             
Dat$Tspw	  <- dat3$Dt[3]              
Dat$Mad	    <- dat3$madurezsexual        
Dat$Wmed	  <- colMeans(dat3$Wmed)      
Dat$Wini	  <- colMeans(dat3$Wini) 
Dat$Sel     <- rep3$S_f[1,] 

Rmed3        <- mean(Rt3)           
Bmed3        <- mean(BD3)         
Fmedian3     <- exp(median(Ft3))

Bobj         <-c(.85,.80,.60,.55,.52,.50,.45,.40,.30,.325,0.425)
Fobj      	 <- optim(par=rep(0.,11),fn=SPRFpbr,method='BFGS')

SPR3 		     <- SPRFmort(Rmed3,c(0,Fobj$par,Fmedian3,rep3$Ftot[25]),Amax,Dat) 
pSPR_Fmh3    <- as.numeric(SPR3[13,4])   # Paso 2: Cálculo de la curva SPR
pB_Fmh3      <- pSPR_Fmh3-0.05    # Paso 3: Aproximación obtención de %BD(Fmh)
SPRcurv3 		 <- SPRFmort(R0,Fmort,Amax,Dat) 
# PASOS
Bo3           <- rep3$SSBpbr[1]       # Paso 4: Obtenci?n de Bo
BRMS3         <- rep3$SSBpbr[3]       # Paso 5: Obtenci?n de Brms = 60%SPRo = 55%Bo
FRMS3         <- rep3$Fs[2]
BLIM3         <- Bo3*0.275            # Paso 6: Obtenci?n de Blim = 20%Bo 
FLIM3         <- rep3$Fs[3]           # Paso 6: Obtenci?n de Flim = 30%SPRo
SpB3          <- BD3                  # BD serie hist?rica de evaluaci?n de stock 
SpBSE3        <- BD3std              # desviaci?n estandar BD
ln_Fyr3       <- Ft3                 # logaritmo de Ft
ln_FSE3       <- Ft3std              # logaritmo de la desviaci?n standar de Ft

# 9. ESTATUS ><> ><> ><> ><> ----
# HITO 1: SEPTIEMBRE ----
#para serie histórica----
Rpr1     <- c(subset(std1,name=="RPRequ3")$value); 
Rpr1std  <- c(subset(std1,name=="RPRequ3")$std)
Frpr1    <- c(subset(std1,name=="Frpr")$value); 
Frpr1std <- c(subset(std1,name=="Frpr")$std)

EstatusSep<- data.frame(x=years1, 
                        Rpr1=Rpr1,
                        Frpr1=Frpr1,
                        lowerRpr1  = (Rpr1 - 1.96*Rpr1std ), 
                        upperRpr1   = (Rpr1 +1.96*Rpr1std ),
                        lowerFrpr1 = (Frpr1 -1.96*Frpr1std), 
                        upperFrpr1 = (Frpr1 +1.96*Frpr1std))
#Para densidad de probabilidad último año----
rprSEPT     <-subset(std1,name=="RPRequ3")$value[nyears1]
rprSEPTstd  <-subset(std1,name=="RPRequ3")$std[nyears1]
FrprSEPT    <-subset(std1,name=="Frpr")$value[nyears1]
FrprSEPTstd <-subset(std1,name=="Frpr")$std[nyears1]
# biomasa desovante vs BDrms----
xbs1 <-rnorm(1000, mean = rprSEPT, sd = rprSEPTstd)
xbs  <-seq(min(xbs1),max(xbs1),0.005)
ybs  <-dnorm(xbs, mean = rprSEPT, sd =rprSEPTstd)
icbs <-qnorm(c(0.05,0.95,0.5),rprSEPT,rprSEPTstd)
# mortalidad por pesca vs Frms----
xfs1 <- rnorm(1000, mean = FrprSEPT, sd = FrprSEPTstd)
xfs  <-seq(min(xfs1),max(xfs1),0.005)
yfs  <-dnorm(xfs, mean = FrprSEPT, sd =FrprSEPTstd)
icfs <-qnorm(c(0.05,0.95,0.5),FrprSEPT,FrprSEPTstd)
#distribución probabilidad----
xxbs<- c(xbs[xbs>=icbs[1]&xbs<=icbs[2]],rev(xbs[xbs>=icbs[1]&xbs<=icbs[2]]))
yybs<- c(ybs[xbs>=icbs[1]&xbs<=icbs[2]],rep(0,length(ybs[xbs>=icbs[1]&xbs<=icbs[2]])))
xxfs<- c(xfs[xfs>=icfs[1]&xfs<=icfs[2]],rev(xfs[xfs>=icfs[1]&xfs<=icfs[2]]))
yyfs<- c(yfs[xfs>=icfs[1]&xfs<=icfs[2]],rep(0,length(yfs[xfs>=icfs[1]&xfs<=icfs[2]])))

densb_bs  <- data.frame(x=xxbs, y=yybs , t=rep('a', length(xxbs)), r=seq(1,length(xxbs),1))
densb_fs  <- data.frame(x=xxfs, y=yyfs , t=rep('a', length(xxfs)), r=seq(1,length(xxfs),1))

# *Probabilidad de estar bajo BRMS* #Asesoría  #P(BD<BDrms)---- 
pa_sept <-pnorm(0.9,rprSEPT,rprSEPTstd,lower.tail = TRUE,log.p = F)
# *Probabilidad de estar bajo FRMS* #Asesoría  #P(F>Frms)----
pb_sept <-1-pnorm(1.1,FrprSEPT,FrprSEPTstd,lower.tail = TRUE,log.p = F)
# *Probabilidad de estar en zona de sobreexplotacion*  #Asesoría  #P(BD<BDrms)---- 
pc_sept <-pnorm(0.9,rprSEPT,rprSEPTstd,lower.tail = TRUE,log.p = F)-pnorm(0.5,
                   rprSEPT,rprSEPTstd,lower.tail = TRUE,log.p = F)
# *Probabilidad de estar en zona de colapso* #Asesoría  #P(BD<BDrms) ----
pd_sept <-pnorm(0.5,rprSEPT,rprSEPTstd,lower.tail = TRUE,log.p = F)
# *Probailidad de sobrepesca* #Asesoría  #P(F>Frms)----
pe_sept <-1-pnorm(1.1,FrprSEPT,FrprSEPTstd,lower.tail = TRUE,log.p = F)


# HITO 2: MARZO----
#para serie histórica ----
Rpr2     <- c(subset(std2,name=="RPRequ3")$value); 
Rpr2std  <- c(subset(std2,name=="RPRequ3")$std)
Frpr2    <- c(subset(std2,name=="Frpr")$value); 
Frpr2std <- c(subset(std2,name=="Frpr")$std)

EstatusMar<- data.frame(x=years1, 
                        Rpr2=Rpr2,
                        Frpr2=Frpr2,
                        lowerRpr2  = (Rpr2  -1.96*Rpr2std ), 
                        upperRpr2  = (Rpr2  +1.96*Rpr2std ),
                        lowerFrpr2 = (Frpr2 -1.96*Frpr2std), 
                        upperFrpr2 = (Frpr2 +1.96*Frpr2std))
#Para densidad de probabilidad último año ----
rprMAR     <-subset(std2,name=="RPRequ3")$value[nyears1]
rprMARstd  <-subset(std2,name=="RPRequ3")$std[nyears1]
FrprMAR    <-subset(std2,name=="Frpr")$value[nyears1]
FrprMARstd <-subset(std2,name=="Frpr")$std[nyears1]
# biomasa desovante vs BDrms ----
xbm1  <-rnorm(1000, mean = rprMAR, sd = rprMARstd)
xbm   <-seq(min(xbm1),max(xbm1),0.005)
ybm   <-dnorm(xbm, mean = rprMAR, sd =rprMARstd)
icbm  <-qnorm(c(0.05,0.95,0.5),rprMAR,rprMARstd)
# mortalidad por pesca vs Frms ----
xfm1  <- rnorm(1000, mean = FrprMAR, sd = FrprMARstd)
xfm   <-seq(min(xfm1),max(xfm1),0.005)
yfm   <-dnorm(xfm, mean = FrprMAR, sd =FrprMARstd)
icfm  <-qnorm(c(0.05,0.95,0.5),FrprMAR,FrprMARstd)
#distribución probabilidad ----
xxbm  <- c(xbm[xbm>=icbm[1]&xbm<=icbm[2]],rev(xbm[xbm>=icbm[1]&xbm<=icbm[2]]))
yybm  <- c(ybm[xbm>=icbm[1]&xbm<=icbm[2]],rep(0,length(ybm[xbm>=icbm[1]&xbm<=icbm[2]])))
xxfm  <- c(xfm[xfm>=icfm[1]&xfm<=icfm[2]],rev(xfm[xfm>=icfm[1]&xfm<=icfm[2]]))
yyfm  <- c(yfm[xfm>=icfm[1]&xfm<=icfm[2]],rep(0,length(yfm[xfm>=icfm[1]&xfm<=icfm[2]])))
densb_bm  <- data.frame(x=xxbm, y=yybm , t=rep('a', length(xxbm)), r=seq(1,length(xxbm),1))
densb_fm  <- data.frame(x=xxfm, y=yyfm , t=rep('a', length(xxfm)), r=seq(1,length(xxfm),1))

# *Probabilidad de estar bajo BRMS* #Asesoría  #P(BD<BDrms) ---- 
pa_mar <-pnorm(0.9,rprMAR,rprMARstd,lower.tail = TRUE,log.p = F)
# *Probabilidad de estar bajo FRMS* #Asesoría  #P(F>Frms) ----
pb_mar <-1-pnorm(1.1,FrprMAR,FrprMARstd,lower.tail = TRUE,log.p = F)
# *Probabilidad de estar en zona de sobreexplotacion*  #Asesoría  #P(BD<BDrms) ----
pc_mar <-pnorm(0.9,rprMAR,rprMARstd,lower.tail = TRUE,log.p = F)-pnorm(0.5,
                   rprMAR,rprMARstd,lower.tail = TRUE,log.p = F)
# *Probabilidad de estar en zona de colapso* #Asesoría  #P(BD<BDrms) ----
pd_mar <-pnorm(0.5,rprMAR,rprMARstd,lower.tail = TRUE,log.p = F)
# *Probailidad de sobrepesca* #Asesoría  #P(F>Frms) ----
pe_mar <-1-pnorm(1.1,FrprMAR,FrprMARstd,lower.tail = TRUE,log.p = F)


# HITO 3: JULIO ----
#para serie histórica ----
Rpr3     <- c(subset(std3,name=="RPRequ3")$value); 
Rpr3std  <- c(subset(std3,name=="RPRequ3")$std)
Frpr3    <- c(subset(std3,name=="Frpr")$value); 
Frpr3std <- c(subset(std3,name=="Frpr")$std)

EstatusJul<- data.frame(x=years1, 
                        Rpr3=Rpr3,
                        Frpr3=Frpr3,
                        lowerRpr3  = (Rpr3  -1.96*Rpr3std ), 
                        upperRpr3  = (Rpr3  +1.96*Rpr3std ),
                        lowerFrpr3 = (Frpr3 -1.96*Frpr3std), 
                        upperFrpr3 = (Frpr3 +1.96*Frpr3std))
#Para densidad de probabilidad último año ----
rprJUL     <- subset(std3,name=="RPRequ3")$value[nyears1]
rprJULstd  <- subset(std3,name=="RPRequ3")$std[nyears1]
FrprJUL    <- subset(std3,name=="Frpr")$value[nyears1]
FrprJULstd <- subset(std3,name=="Frpr")$std[nyears1]
# biomasa desovante vs BDrms----
xbj1  <- rnorm(1000, mean = rprJUL, sd = rprJULstd)
xbj   <- seq(min(xbj1),max(xbj1),0.005)
ybj   <- dnorm(xbj, mean = rprJUL, sd =rprJULstd)
icbj  <- qnorm(c(0.05,0.95,0.5),rprJUL,rprJULstd)
# mortalidad por pesca vs Frms----
xfj1  <- rnorm(1000, mean = FrprJUL, sd = FrprJULstd)
xfj   <- seq(min(xfj1),max(xfj1),0.005)
yfj   <- dnorm(xfj, mean = FrprJUL, sd =FrprJULstd)
icfj  <- qnorm(c(0.05,0.95,0.5),FrprJUL,FrprJULstd)
#distribución probabilidad----
xxbj  <- c(xbj[xbj>=icbj[1]&xbj<=icbj[2]],rev(xbj[xbj>=icbj[1]&xbj<=icbj[2]]))
yybj  <- c(ybj[xbj>=icbj[1]&xbj<=icbj[2]],rep(0,length(ybj[xbj>=icbj[1]&xbj<=icbj[2]])))
xxfj  <- c(xfj[xfj>=icfj[1]&xfj<=icfj[2]],rev(xfj[xfj>=icfj[1]&xfj<=icfj[2]]))
yyfj  <- c(yfj[xfj>=icfj[1]&xfj<=icfj[2]],rep(0,length(yfj[xfj>=icfj[1]&xfj<=icfj[2]])))
densb_bj  <- data.frame(x=xxbj, y=yybj , t=rep('a', length(xxbj)), r=seq(1,length(xxbj),1))
densb_fj  <- data.frame(x=xxfj, y=yyfj , t=rep('a', length(xxfj)), r=seq(1,length(xxfj),1))

# *Probabilidad de estar bajo BRMS* #Asesoría  #P(BD<BDrms) ----
pa_jul <- pnorm(0.9,rprJUL,rprJULstd,lower.tail = TRUE,log.p = F)
# *Probabilidad de estar bajo FRMS* #Asesoría  #P(F>Frms) ----
pb_jul <- 1-pnorm(1.1,FrprJUL,FrprJULstd,lower.tail = TRUE,log.p = F)
# *Probabilidad de estar en zona de sobreexplotacion*  #Asesoría  #P(BD<BDrms)  ----
pc_jul <- pnorm(0.9,rprJUL,rprJULstd,lower.tail = TRUE,log.p = F)-pnorm(0.5,
                   rprJUL,rprJULstd,lower.tail = TRUE,log.p = F)
# *Probabilidad de estar en zona de colapso* #Asesoría  #P(BD<BDrms) ----
pd_jul <- pnorm(0.5,rprJUL,rprJULstd,lower.tail = TRUE,log.p = F)
# *Probailidad de sobrepesca* #Asesoría  #P(F>Frms) ----
pe_jul <- 1-pnorm(1.1,FrprJUL,FrprJULstd,lower.tail = TRUE,log.p = F)


# 10. PROYECCION ><> ><> ><> ><>  ----

# RESULTADOS DE PROYECCIÓN ASESORÍA DE SEPTIEMBRE (HITO 1) ----

dira<-paste(dir.0,carpetaCBA_sept,sep="")
setwd(dira)

reps1a     <- reptoRlist(paste(admb_sept,"11.rep",sep=""))  
reps2a     <- reptoRlist(paste(admb_sept,"12.rep",sep="")) 
reps3a     <- reptoRlist(paste(admb_sept,"13.rep",sep="")) 


stds1     <- read.table(paste(dir.0,carpetaCBA_sept,admb_sept,"11.std", sep=''),header=T,sep="",na="NA",fill=T) 
stds2     <- read.table(paste(dir.0,carpetaCBA_sept,admb_sept,"12.std", sep=''),header=T,sep="",na="NA",fill=T) 
stds3     <- read.table(paste(dir.0,carpetaCBA_sept,admb_sept,"13.std", sep=''),header=T,sep="",na="NA",fill=T) 

bds1     <- subset(stds1,name=="BD_p0")$value ; 
bds1std  <- subset(stds1,name=="BD_p0")$std #reclutamiento medios
bds2     <- subset(stds2,name=="BD_p0")$value ; 
bds2std  <- subset(stds2,name=="BD_p0")$std #reclutamiento 2018
bds3     <- subset(stds3,name=="BD_p0")$value ; 
bds3std  <- subset(stds3,name=="BD_p0")$std #reclutamiento 2012

RpRps1     <- subset(stds1,name=="RPR_p0")$value ; 
RpRps1std  <- subset(stds1,name=="RPR_p0")$std #reclutamiento medios
RpRps2     <- subset(stds2,name=="RPR_p0")$value ; 
RpRps2std  <- subset(stds2,name=="RPR_p0")$std #reclutamiento 2018
RpRps3     <- subset(stds3,name=="RPR_p0")$value ; 
RpRps3std  <- subset(stds3,name=="RPR_p0")$std #reclutamiento 2012

cs1     <- subset(stds1,name=="YTP_p0")$value ; 
cs1std  <- subset(stds1,name=="YTP_p0")$std #reclutamiento medios
cs2     <- subset(stds2,name=="YTP_p0")$value ; 
cs2std  <- subset(stds2,name=="YTP_p0")$std #reclutamiento 2018
cs3     <- subset(stds3,name=="YTP_p0")$value ; 
cs3std  <- subset(stds3,name=="YTP_p0")$std #reclutamiento 2012

# RECLUTAMIENTO ESTIMADO ULTIMO AÑO EVALUACIÓN (AÑO ACTUAL) ----
RTs0s     <- subset(stds1,name=="Reclutas")$value[nyears1] ; 
RTs0s_std  <- subset(stds1,name=="Reclutas")$std[nyears1] 

#BIOMASA DESOVANTE ESTIMADA ULTIMO AÑO EVALUACIÓN ----
bds0s     <- subset(stds1,name=="SSB")$value[nyears1] ; 
bds0s_std  <- subset(stds1,name=="SSB")$std[nyears1] 

# aporte del grupo de edad 0 (reclutamiento) año actual ----
C1eryearR1act<-round(reps1a$YTP_r0W_actual[1]/sum(reps1a$YTP_r0W_actual),2)
C1eryearR1act2<-round(reps1a$YTP_r0W_actual[2]/sum(reps1a$YTP_r0W_actual),2)

# aporte del grupo de edad 0 (reclutamiento) 1er año proyectado ----
C1eryearR1<-round(reps1a$YTP_p0W_proyectada[1,1]/sum(reps1a$YTP_p0W_proyectada[1,]),2)
C1eryearR2<-round(reps2a$YTP_p0W_proyectada[1,1]/sum(reps2a$YTP_p0W_proyectada[1,]),2)
C1eryearR3<-round(reps3a$YTP_p0W_proyectada[1,1]/sum(reps3a$YTP_p0W_proyectada[1,]),2)

# aporte del grupo de edad 1 (reclutamiento) 1er año proyectado ----
C1eryearR1a<-round(reps1a$YTP_p0W_proyectada[1,2]/sum(reps1a$YTP_p0W_proyectada[1,]),2)
C1eryearR2a<-round(reps2a$YTP_p0W_proyectada[1,2]/sum(reps2a$YTP_p0W_proyectada[1,]),2)
C1eryearR3a<-round(reps3a$YTP_p0W_proyectada[1,2]/sum(reps3a$YTP_p0W_proyectada[1,]),2)

# aporte del grupo de edad 0 (reclutamiento) 2do año proyectado ----
C1eryearR12<-round(reps1a$YTP_p0W_proyectada[2,1]/sum(reps1a$YTP_p0W_proyectada[2,]),2)
C1eryearR22<-round(reps2a$YTP_p0W_proyectada[2,1]/sum(reps2a$YTP_p0W_proyectada[2,]),2)
C1eryearR32<-round(reps3a$YTP_p0W_proyectada[2,1]/sum(reps3a$YTP_p0W_proyectada[2,]),2)

# aporte del grupo de edad 1  2do año proyectado ----
C1eryearR12a<-round(reps1a$YTP_p0W_proyectada[2,2]/sum(reps1a$YTP_p0W_proyectada[2,]),2)
C1eryearR22a<-round(reps2a$YTP_p0W_proyectada[2,2]/sum(reps2a$YTP_p0W_proyectada[2,]),2)
C1eryearR32a<-round(reps3a$YTP_p0W_proyectada[2,2]/sum(reps3a$YTP_p0W_proyectada[2,]),2)

# ESTATUS PROYECTADO ----

# PRIMER AÑO PROYECTADO ----
### *Probabilidad de estar bajo BRMS* ----
pa1<-pnorm(0.9,RpRps1[1],RpRps1std[1],lower.tail = TRUE,log.p = F)
pa2<-pnorm(0.9,RpRps2[1],RpRps2std[1],lower.tail = TRUE,log.p = F)
pa3<-pnorm(0.9,RpRps3[1],RpRps3std[1],lower.tail = TRUE,log.p = F)
### *Probabilidad de estar en zona de sobreexplotacion* ----
pc1<-pnorm(0.9,RpRps1[1],RpRps1std[1],lower.tail = TRUE,log.p = F)-pnorm(0.5,
               RpRps1[1],RpRps1std[1],lower.tail = TRUE,log.p = F)
pc2<-pnorm(0.9,RpRps2[1],RpRps2std[1],lower.tail = TRUE,log.p = F)-pnorm(0.5,
               RpRps2[1],RpRps2std[1],lower.tail = TRUE,log.p = F)
pc3<-pnorm(0.9,RpRps3[1],RpRps3std[1],lower.tail = TRUE,log.p = F)-pnorm(0.5,
               RpRps3[1],RpRps3std[1],lower.tail = TRUE,log.p = F)
### *Probabilidad de estar en zona de colapso*----
pd1<-pnorm(0.5,RpRps1[1],RpRps1std[1],lower.tail = TRUE,log.p = F)
pd2<-pnorm(0.5,RpRps2[1],RpRps2std[1],lower.tail = TRUE,log.p = F)
pd3<-pnorm(0.5,RpRps3[1],RpRps3std[1],lower.tail = TRUE,log.p = F)

# SEGUNDO AÑO PROYECTADO ----

### *Probabilidad de estar bajo BRMS* ----
pa12<-pnorm(0.9,RpRps1[2],RpRps1std[2],lower.tail = TRUE,log.p = F)
pa22<-pnorm(0.9,RpRps2[2],RpRps2std[2],lower.tail = TRUE,log.p = F)
pa32<-pnorm(0.9,RpRps3[2],RpRps3std[2],lower.tail = TRUE,log.p = F)
### *Probabilidad de estar en zona de sobreexplotacion* ----
pc12<-pnorm(0.9,RpRps1[2],RpRps1std[2],lower.tail = TRUE,log.p = F)-pnorm(0.5,
                RpRps1[2],RpRps1std[2],lower.tail = TRUE,log.p = F)
pc22<-pnorm(0.9,RpRps2[2],RpRps2std[2],lower.tail = TRUE,log.p = F)-pnorm(0.5,
                RpRps2[2],RpRps2std[2],lower.tail = TRUE,log.p = F)
pc32<-pnorm(0.9,RpRps3[2],RpRps3std[2],lower.tail = TRUE,log.p = F)-pnorm(0.5,
                RpRps3[2],RpRps3std[2],lower.tail = TRUE,log.p = F)
### *Probabilidad de estar en zona de colapso* ----
pd12<-pnorm(0.5,RpRps1[2],RpRps1std[2],lower.tail = TRUE,log.p = F)
pd22<-pnorm(0.5,RpRps2[2],RpRps2std[2],lower.tail = TRUE,log.p = F)
pd32<-pnorm(0.5,RpRps3[2],RpRps3std[2],lower.tail = TRUE,log.p = F)

# CBA INICIAL ----
n<-3
q       <- seq(0.1,0.5,0.1)  # niveles de riesgo (cuantiles)                                
nq      <- length(q)                                                                                   
CBA_sept     <- matrix(ncol=nq,nrow=n)
CBAp_sept    <- rep(0,n)
CBApstd_sept <- rep(0,n)

buffer   <- matrix(ncol=nq,nrow=n)
descarte <- matrix(ncol=nq,nrow=n)

for(i in 1:n){
  std     <- read.table(paste(admb_sept,"1",i,".std",sep=""),header=T,sep="",na="NA",fill=T) 
  CBAp_sept[i]    <-subset(std,name=="CBA_c0")$value[1]
  CBApstd_sept[i] <-subset(std,name=="CBA_c0")$std[1]
  for(j in 1:nq){CBA_sept[i,j]<-qnorm(q[j],CBAp_sept[i],CBApstd_sept[i])}}

for(i in 1:n){for(j in 1:nq){	
  buffer[i,j]<-round(1-CBA_sept[i,j]/CBA_sept[i,5],2)}}

# CBA INICIAL MENOS DESCARTE ----
n<-3
q       <- seq(0.1,0.5,0.1)  # niveles de riesgo (cuantiles)                                
nq      <- length(q)                                                                                   
CBAd_sept     <- matrix(ncol=nq,nrow=n)
CBApd_sept    <- rep(0,n)
CBApdstd_sept <- rep(0,n)


for(i in 1:n){
  std     <- read.table(paste(admb_sept,"1",i,".std",sep=""),header=T,sep="",na="NA",fill=T) 
  CBApd_sept[i]    <-subset(std,name=="CBA_c0d")$value[1]
  CBApdstd_sept[i] <-subset(std,name=="CBA_c0d")$std[1]
  for(j in 1:nq){CBAd_sept[i,j]<-qnorm(q[j],CBApd_sept[i],CBApdstd_sept[i])}}



