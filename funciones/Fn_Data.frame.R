
# ARREGLO DATOS PARA SALIDAS DE TABLAS Y FIGURAS ----

# función datos índices observados ----
indobs_Fig1<-function(archivo.Rdata,Hitoasesoria){
  load(paste(dir.Rdata,archivo.Rdata,sep=""))
  
  indobs  <- data.frame(reclasobs,
                        pelacesobs,
                        mphobs,
                        desembarqueobs) %>% 
    na_if(0) %>% 
    magrittr::set_colnames(names_ind) %>%
    mutate(Asesoria=Hitoasesoria,type='observado',yrs= years) %>% 
    melt(id.var=c('yrs','type', 'Asesoria'))  
  indobs
}

# función datos índices predichos ----
indpred_Fig1<-function(archivo.Rdata,Hitoasesoria){
  load(paste(dir.Rdata,archivo.Rdata,sep=""))
  
  indpred  <- data.frame(reclaspred,
                        pelacespred,
                        mphpred,
                        desembarquepred) %>% 
    na_if(0) %>% 
    magrittr::set_colnames(names_ind) %>%
    mutate(Asesoria=Hitoasesoria,type='predicho',yrs= years) %>% 
    melt(id.var=c('yrs','type', 'Asesoria'))  
  indpred
}

# función datos residuos índices ----
resind_Fig2<-function(indobs,indpred){
Res_H1 <- indobs %>%
              mutate(
              Res=(log(indobs$value)-log(indpred$value)),
              Pred=log(indpred$value))
}  

# función datos de composición de edad ----
propE_Fig3<-function(archivo.Rdata,flota,type,Hitoasesoria){
  load(paste(dir.Rdata,archivo.Rdata,sep=""))

  if(flota=='Flota'&type=='observado'){propEdad<-pfobs}
  if(flota=='Flota'&type=='predicho'){propEdad<-pfpred}
  
  if(flota=='Crucero_verano'&type=='observado'){propEdad<-pRobs}
  if(flota=='Crucero_verano'&type=='predicho'){propEdad<-pRpred}
  
  if(flota=='Crucero_otoño'&type=='observado'){propEdad<-pPpred}
  if(flota=='Crucero_otoño'&type=='predicho'){propEdad<-pPpred}
  
  
propE  <- as.data.frame(propEdad) %>% 
         magrittr::set_colnames(age)%>% 
         mutate(yrs=years,
         Asesoria=Hitoasesoria,
         flota=flota,
         type=type) %>% 
        melt(id.vars=c('yrs','Asesoria','flota','type'))
propE
}


# funcion datos variables poblacionales

Varpobl<-function(archivo.Rdata,hito){
  load(paste(dir.Rdata,archivo.Rdata,sep=""))
  
Rt<- data.frame(x=years,y=Rt,lower = (Rt-1.96*Rtstd),upper = (Rt+1.96*Rtstd))%>% 
     mutate(indicador='Rt')
BT<- data.frame(x=years,y=BT,lower = (BT-1.96*BTstd),upper = (BT+1.96*BTstd))%>% 
  mutate(indicador='BT')
BD<- data.frame(x=years,y=BD,lower = (BD-1.96*BDstd),upper = (BD+1.96*BDstd))%>% 
  mutate(indicador='BD')
Ft<- data.frame(x=years,y=exp(Ft),lower = exp(Ft-1.96*Ftstd),upper = exp(Ft+1.96*Ftstd))%>% 
  mutate(indicador='Ft')

Var<-rbind(Rt,BT,BD,Ft) %>%mutate(Hito=hito)%>% melt(id.vars=c('x','Hito','indicador','lower','upper'))

}


# # 4. COMPARACIÓN CON ASESORÍAS PREVIAS ><> ><> ><> ><> ----
# 
# dir<-paste(dir.0,"/rep_AsesoriasPrevias",sep="")
# 
# sept20 <-paste(dir,"/MAE0920b.rep",sep="")
# mar21  <-paste(dir,"/MAE0321b.rep",sep="")
# jul21  <-paste(dir,"/MAE0721b.rep",sep="")
# sept21 <-paste(dir,"/MAE0921b.rep",sep="")
# mar22  <-paste(dir,"/MAE322.rep",sep="")
# jul22  <-paste(dir.1,"/MAE722.rep",sep="")
# sept22 <-paste(dir.1,"/MAE922.rep",sep="")
# 
# rep_sept20 <- reptoRlist(sept20)
# rep_mar21  <- reptoRlist(mar21)
# rep_jul21  <- reptoRlist(jul21)
# rep_sept21 <- reptoRlist(sept21)
# rep_mar22  <- reptoRlist(mar22)
# rep_jul22  <- reptoRlist(jul22)
# rep_sept22 <- reptoRlist(sept22)
# 
# years  <- rep_sept22$years
# nyears <- length(years)                                                                
# x  <-c(years,rev(years))
# x1 <-c(years[1],years[nyears]+1,nyears+1/2) #xaxp
# x2 <-c(years[1]-1,years[nyears]+1) #xlim
# 
# Rtcomp <- data.frame(x=years,
#                      Rt_sept20=c(rep_sept20$Reclutas,NA,NA),
#                      Rt_mar21=c(rep_mar21$Reclutas,NA),
#                      Rt_jul21=c(rep_jul21$Reclutas,NA),
#                      Rt_sept21=c(rep_sept21$Reclutas,NA),
#                      Rt_mar22=c(rep_mar22$Reclutas),
#                      Rt_jul22=c(rep_jul22$Reclutas),
#                      Rt_sept22=c(rep_sept22$Reclutas))
# 
# SSBtcomp <- data.frame(x=years,
#                        SSBt_sept20=c(rep_sept20$SSB,NA,NA),
#                        SSBt_mar21=c(rep_mar21$SSB,NA),
#                        SSBt_jul21=c(rep_jul21$SSB,NA),
#                        SSBt_sept21=c(rep_sept21$SSB,NA),
#                        SSBt_mar22=c(rep_mar22$SSB),
#                        SSBt_jul22=c(rep_jul22$SSB),
#                        SSBt_sept22=c(rep_sept22$SSB))
# 
# Ftcomp <- data.frame(x=years,
#                      Ft_sept20=c(rep_sept20$Ftot,NA,NA),
#                      Ft_mar21=c(rep_mar21$Ftot,NA),
#                      Ft_jul21=c(rep_jul21$Ftot,NA),
#                      Ft_sept21=c(rep_sept21$Ftot,NA),
#                      Ft_mar22=c(rep_mar22$Ftot),
#                      Ft_jul22=c(rep_jul22$Ftot),
#                      Ft_sept22=c(rep_sept22$Ftot))
# 
# year_retros <- c('2022_sept','2022_julio','2022_marzo','2021_sept',
#                  "2021_julio","2021_marzo","2020_sept")
# nretros <-7
# 
# # 5. PERFIL VEROSIMILITUD ><> ><> ><> ><> ----
# 
# dir<-paste(dir.0,"/Verosimilitud_sept",sep="")
# setwd(dir)
# admb<-"/MAE922"
# 
# casos <-23
# logRo    <- rep(0,casos)
# likeval  <- matrix(ncol=15,nrow=casos)
# slikeval <- matrix(ncol=16,nrow=casos)
# 
# #
# for(i in 1:casos){
#   report      <- reptoRlist(paste(dir,admb,i,".rep",sep=""))
#   logRo[i]    <- report$log_Ro
#   likeval[i,] <- report$likeval}
# 
# like    <- data.frame(round(likeval,3),Total=apply(likeval,1,sum))
# minLik  <- apply(like,2,min)  
# 
# # busca el mínimo
# for(i in 1:16){
#   slikeval[,i]<-like[,i]-minLik[i]
# }    # Estandarización
# 
# names<-c("Ro","Bio_Reclas","Bio_Pelaces","Desembarques","Bio_Mph","C.Edad_Flota",
#          "C.Edad_Recl","C.Edad_Pel","prepPelTall","DesvRt","qreclas","qpela","PenFt",
#          "PenFspr","NA","NA","Total")
# # Tabla verosimilitud
# TLk1 <- data.frame(exp(logRo),like);colnames(TLk1)<-names
# # Tabla estandarizada
# TLk2<- data.frame(exp(logRo),slikeval);colnames(TLk2)<-names
# 
# Ro_reclas  <- TLk2$Ro[TLk2$Bio_Reclas==0]
# Ro_pelaces <- TLk2$Ro[TLk2$Bio_Pelaces==0]
# Ro_desemb  <- min(TLk2$Ro[TLk2$Desembarques==0])
# Ro_MPDH    <- ifelse(sum(TLk2$Bio_Mph)>0,0,TLk2$Ro[TLk2$Bio_Mph==0])
# Ro_propF   <- TLk2$Ro[TLk2$C.Edad_Flota==0]
# Ro_propRecl<- TLk2$Ro[TLk2$C.Edad_Recl==0]
# Ro_propPel <- TLk2$Ro[TLk2$C.Edad_Pel==0]
# Ro         <- TLk2$Ro[TLk2$Total==0]
# 
# names_res<-c("Bio_Reclas","Bio_Pelaces","Desembarques","Bio_MPDH","C.Edad_Flota","C.Edad_Reclas","C.Edad_Pelaces")
# res<-c((Ro_reclas-Ro),
#        (Ro_pelaces-Ro),
#        (Ro_desemb-Ro),
#        (Ro_MPDH-Ro),
#        (Ro_propF-Ro),
#        (Ro_propRecl-Ro),
#        (Ro_propPel-Ro))
# 
# residuos<-res/Ro
# #datares<-data.frame(names_res,residuos)
# 
# setwd(dir.1)
# 
# 
# 
# 
# 
# # 6. ANÁLISIS RETROSPECTIVO ><> ><> ><> ><> ----
# # dir<-paste(dir.0,"/Retrospectivo_sept",sep="")
# # setwd(dir)
# # admb<-str_sub(admb_dat[3], 1, 6)
# # 
# # 
# # retros2  <- seq(1,5)
# # nretros2 <- length(retros2)
# # year_retros2<-as.factor(years1[(nyears1-(nretros2-1)):nyears1])
# # 
# # retroR      <- matrix(0,nrow=nyears1,ncol=nretros2+1)
# # retroBD     <- matrix(0,nrow=nyears1,ncol=nretros2+1)
# # retroF      <- matrix(0,nrow=nyears1,ncol=nretros2+1)
# # 
# # for(i in 1:length(retros2)){
# #   rep<- reptoRlist(paste(admb,"s",i,".rep",sep=""))
# #   retroR[,i+1] <- c(rep$Reclutas,rep(NA,i-1))
# #   retroBD[,i+1] <- c(rep$SSB,rep(NA,i-1))
# #   retroF[,i+1]  <- c(rep$Ftot,rep(NA,i-1)) }
# # 
# # 
# # # retrospectivo relativo (cálculo)
# # mohn.r       <- rep(NA, nretros2)
# # rel.diff.r   <- matrix(NA, nrow=nyears1, ncol=(nretros2))
# # mohn.ssb     <- rep(NA, nretros2)
# # rel.diff.ssb <- matrix(NA, nrow=nyears1, ncol=(nretros2))
# # mohn.f       <- rep(NA, nretros2)
# # rel.diff.f   <- matrix(NA, nrow=nyears1, ncol=(nretros2))
# # 
# # for(j in 1:nretros2){
# #   rel.diff.r[,j]   <- (retroR[,(j+1)]-retroR[,2])/retroR[,2]
# #   mohn.r[j]        <- rel.diff.r[(nyears1-j),j]
# #   rel.diff.ssb[,j] <- (retroBD[,(j+1)]-retroBD[,2])/retroBD[,2]
# #   mohn.ssb[j]      <- rel.diff.ssb[(nyears1-j),j]
# #   rel.diff.f[,j]   <- (retroF[,(j+1)]-retroF[,2])/retroF[,2]
# #   mohn.f[j]        <- rel.diff.f[(nyears1-j),j]}
# # 
# # ave.mohn.r    <- mean(mohn.r)
# # ave.mohn.ssb  <- mean(mohn.ssb)
# # ave.mohn.f    <- mean(mohn.f)
# # 
# # # Arreglo datos
# # 
# # #Para retrospectivo tradicional
# # Rt_retro<- data.frame(x=years1, 
# #                       y1=retroR[,2],
# #                       y2=retroR[,3],
# #                       y3=retroR[,4],
# #                       y4=retroR[,5],
# #                       y5=retroR[,6],
# #                       lower = (Rt1 -1.96*Rt1std),
# #                       upper = (Rt1 +1.96*Rt1std))
# # BD_retro<- data.frame(x=years1,
# #                       y1=retroBD[,2],
# #                       y2=retroBD[,3],
# #                       y3=retroBD[,4],
# #                       y4=retroBD[,5],
# #                       y5=retroBD[,6],
# #                       lower = (BD1 -1.96*BD1std),
# #                       upper = (BD1 +1.96*BD1std))
# # Ft_retro<- data.frame(x=years1,
# #                       y1=retroF[,2],
# #                       y2=retroF[,3],
# #                       y3=retroF[,4],
# #                       y4=retroF[,5],
# #                       y5=retroF[,6], 
# #                       lower = exp(Ft1-1.96*Ft1std),
# #                       upper = exp(Ft1+1.96*Ft1std))
# # 
# # #Para restrospectivo relativo
# # Rt_retroRel<- data.frame(x=years1,
# #                          y1=rel.diff.r[,1],
# #                          y2=rel.diff.r[,2],
# #                          y3=rel.diff.r[,3],
# #                          y4=rel.diff.r[,4],
# #                          y5=rel.diff.r[,5])
# # BD_retroRel<- data.frame(x=years1,
# #                          y1=rel.diff.ssb[,1],
# #                          y2=rel.diff.ssb[,2],
# #                          y3=rel.diff.ssb[,3],
# #                          y4=rel.diff.ssb[,4],
# #                          y5=rel.diff.ssb[,5])
# # Ft_retroRel<- data.frame(x=years1, 
# #                          y1=rel.diff.f[,1],
# #                          y2=rel.diff.f[,2],
# #                          y3=rel.diff.f[,3],
# #                          y4=rel.diff.f[,4],
# #                          y5=rel.diff.f[,5])
# 
# # 8. PUNTOS BIOLÓGICOS DE REFERENCIA ><> ><> ><> ><> ----
# 
# # # HITO 1: SEPTIEMBRE----
# nyears<-dat1$nanos
# #PBR año biologico
# Amax        <- dat1$nedades 
# Fmort 	    <- seq(0,3.5,0.02)           
# nf          <- length(Fmort) 
# R0 		      <- 1  
# #datos de entrada 
# Dat<-list()
# Dat$M		    <- dat1$par[5]             
# Dat$Tspw	  <- dat1$Dt[3]              
# Dat$Mad	    <- dat1$madurezsexual        
# Dat$Wmed	  <- colMeans(dat1$Wmed)      
# Dat$Wini	  <- colMeans(dat1$Wini) 
# Dat$Sel     <- rep1$S_f[1,] 
# 
# Rmed        <- mean(Rt1,na.rm = T)           
# Bmed        <- mean(BD1,na.rm = T)         
# Fmedian     <- exp(median(Ft1,na.rm = T))
# Fstatuquo   <-rep1$Ftot[nyears]
# 
# Bobj        <-c(.85,.80,.60,.55,.52,.50,.45,.40,.30,.325,0.425)
# Fobj      	<- optim(par=rep(0.,11),fn=SPRFpbr,method='BFGS')
# 
# SPR1 		    <- SPRFmort(Rmed,c(0,Fobj$par,Fmedian,Fstatuquo),Amax,Dat) 
# pSPR_Fmh1    <- as.numeric(SPR1[13,4])     # Paso 2: Cálculo de la curva SPR
# pB_Fmh1      <- pSPR_Fmh1-0.05          # Paso 3: Aproximación obtención de %BD(Fmh)
# SPRcurv1 		<- SPRFmort(R0,Fmort,Amax,Dat) 
# 
# # # HITO 2: MARZO----
# nyears<-dat2$nanos
# # #PBR año biologico
# Amax        <- dat2$nedades
# Fmort 	    <- seq(0,3.5,0.02)
# nf          <- length(Fmort)
# R0 		      <- 1
# # #datos de entrada 
# Dat<-list()
# Dat$M		    <- dat2$par[5]
# Dat$Tspw	  <- dat2$Dt[3]
# Dat$Mad	    <- dat2$madurezsexual
# Dat$Wmed	  <- colMeans(dat2$Wmed)
# Dat$Wini	  <- colMeans(dat2$Wini)
# Dat$Sel     <- rep2$S_f[1,]
# 
# Rmed2        <- mean(Rt2)
# Bmed2        <- mean(BD2)
# Fmedian2     <- exp(median(Ft2))
# 
# Bobj         <-c(.85,.80,.60,.55,.52,.50,.45,.40,.30,.325,0.425)
# Fobj         <- optim(par=rep(0.,11),fn=SPRFpbr,method='BFGS')
# 
# SPR2 		     <- SPRFmort(Rmed2,c(0,Fobj$par,Fmedian2,rep2$Ftot[nyears]),Amax,Dat)
# pSPR_Fmh2    <- as.numeric(SPR2[13,4])  # Paso 2: Cálculo de la curva SPR
# pB_Fmh2      <- pSPR_Fmh2-0.05      # Paso 3: Aproximación obtención de %BD(Fmh)
# SPRcurv2 		 <- SPRFmort(R0,Fmort,Amax,Dat)
# 
# # # HITO 3: JULIO----
# nyears<-dat3$nanos
# # #PBR año biologico
# Amax        <- dat3$nedades
# Fmort 	    <- seq(0,3.5,0.02)
# nf          <- length(Fmort)
# R0 		      <- 1
# # #datos de entrada 
# Dat<-list()
# Dat$M		    <- dat3$par[5]
# Dat$Tspw	  <- dat3$Dt[3]
# Dat$Mad	    <- dat3$madurezsexual
# Dat$Wmed	  <- colMeans(dat3$Wmed)
# Dat$Wini	  <- colMeans(dat3$Wini)
# Dat$Sel     <- rep3$S_f[1,]
# 
# Rmed3        <- mean(Rt3)
# Bmed3        <- mean(BD3)
# Fmedian3     <- exp(median(Ft3))
# 
# Bobj         <-c(.85,.80,.60,.55,.52,.50,.45,.40,.30,.325,0.425)
# Fobj      	 <- optim(par=rep(0.,11),fn=SPRFpbr,method='BFGS')
# 
# SPR3 		     <- SPRFmort(Rmed3,c(0,Fobj$par,Fmedian3,rep3$Ftot[nyears]),Amax,Dat)
# pSPR_Fmh3    <- as.numeric(SPR3[13,4])   # Paso 2: Cálculo de la curva SPR
# pB_Fmh3      <- pSPR_Fmh3-0.05    # Paso 3: Aproximación obtención de %BD(Fmh)
# SPRcurv3 		 <- SPRFmort(R0,Fmort,Amax,Dat)
# 
# 
# # Funcion PBRproxy ----
# 
# PBRproxy<-function(pSPR_Fmh,pB_Fmh,rep,Rt,BD,BDstd,Ft,Ftstd,Hito,dir.Rdata){
# Bmed        <- mean(BD,na.rm = T)         
# Fmedian     <- exp(median(Ft,na.rm = T))
# 
# # PASOS
# Bo           <- rep$SSBpbr[1]        # Paso 4: Obtenci?n de Bo
# BRMS         <- rep$SSBpbr[3]        # Paso 5: Obtenci?n de Brms = 60%SPRo = 55%Bo
# FRMS         <- rep$Fs[2]
# BLIM         <- Bo*0.275             # Paso 6: Obtenci?n de Blim = 20%Bo 
# FLIM         <- rep$Fs[3]            # Paso 6: Obtenci?n de Flim = 30%SPRo
# SpB          <- BD                   # BD serie hist?rica de evaluaci?n de stock 
# SpBSE        <- BDstd                # desviaci?n estandar BD
# ln_Fyr       <- Ft                   # logaritmo de Ft
# ln_FSE       <- Ftstd                # logaritmo de la desviaci?n standar de Ft
# 
# save(Bmed,Fmedian,
#      pSPR_Fmh,pB_Fmh,
#      Bo,BRMS,FRMS,BLIM,FLIM,SpB,SpBSE,ln_Fyr,ln_FSE,
#      file=paste(dir.Rdata,'PBRproxy',Hito,'.RData',sep=""))
# 
# }
# 
# 
# # Funcion genera datos para tabla Pasos PBRproxy ----
# TablaPBRproxy<-function(Rdata){
#   
#   Hito<-c(round(Bmed/10^3,0),
#            formatC(round(Fmedian,2), decimal.mark = ","), 
#            formatC(pSPR_Fmh*100, decimal.mark = ","), 
#            formatC(60, decimal.mark = ","),
#            formatC(pB_Fmh*100, decimal.mark = ","),
#            formatC(55, decimal.mark = ","),
#            round(Bo/10^3,0),
#            round(BRMS/10^3,0),
#            round(BLIM/10^3,0))
#   
#  return(Hito) 
# }
# 
# 
# 
# 
# # # PASOS
# # Bo3           <- rep3$SSBpbr[1]       # Paso 4: Obtenci?n de Bo
# # BRMS3         <- rep3$SSBpbr[3]       # Paso 5: Obtenci?n de Brms = 60%SPRo = 55%Bo
# # FRMS3         <- rep3$Fs[2]
# # BLIM3         <- Bo3*0.275            # Paso 6: Obtenci?n de Blim = 20%Bo 
# # FLIM3         <- rep3$Fs[3]           # Paso 6: Obtenci?n de Flim = 30%SPRo
# # SpB3          <- BD3                  # BD serie hist?rica de evaluaci?n de stock 
# # SpBSE3        <- BD3std              # desviaci?n estandar BD
# # ln_Fyr3       <- Ft3                 # logaritmo de Ft
# # ln_FSE3       <- Ft3std              # logaritmo de la desviaci?n standar de Ft
# 
# # 9. ESTATUS ><> ><> ><> ><> ----
# # HITO 1: SEPTIEMBRE ----
# #para serie histórica----
# Rpr1     <- c(subset(std1,name=="RPRequ3")$value); 
# Rpr1std  <- c(subset(std1,name=="RPRequ3")$std)
# Frpr1    <- c(subset(std1,name=="Frpr")$value); 
# Frpr1std <- c(subset(std1,name=="Frpr")$std)
# 
# EstatusSep<- data.frame(x=years1, 
#                         Rpr=Rpr1,
#                         Frpr=Frpr1,
#                         lowerRpr  = (Rpr1 - 1.96*Rpr1std ), 
#                         upperRpr   = (Rpr1 +1.96*Rpr1std ),
#                         lowerFrpr = (Frpr1 -1.96*Frpr1std), 
#                         upperFrpr = (Frpr1 +1.96*Frpr1std))
# 
# #Para densidad de probabilidad último año----
# rprSEPT     <-subset(std1,name=="RPRequ3")$value[nyears1]
# rprSEPTstd  <-subset(std1,name=="RPRequ3")$std[nyears1]
# FrprSEPT    <-subset(std1,name=="Frpr")$value[nyears1]
# FrprSEPTstd <-subset(std1,name=="Frpr")$std[nyears1]
# # biomasa desovante vs BDrms----
# xbs1 <-rnorm(1000, mean = rprSEPT, sd = rprSEPTstd)
# xbs  <-seq(min(xbs1),max(xbs1),0.005)
# ybs  <-dnorm(xbs, mean = rprSEPT, sd =rprSEPTstd)
# icbs <-qnorm(c(0.05,0.95,0.5),rprSEPT,rprSEPTstd)
# # mortalidad por pesca vs Frms----
# xfs1 <- rnorm(1000, mean = FrprSEPT, sd = FrprSEPTstd)
# xfs  <-seq(min(xfs1),max(xfs1),0.005)
# yfs  <-dnorm(xfs, mean = FrprSEPT, sd =FrprSEPTstd)
# icfs <-qnorm(c(0.05,0.95,0.5),FrprSEPT,FrprSEPTstd)
# #distribución probabilidad----
# xxbs<- c(xbs[xbs>=icbs[1]&xbs<=icbs[2]],rev(xbs[xbs>=icbs[1]&xbs<=icbs[2]]))
# yybs<- c(ybs[xbs>=icbs[1]&xbs<=icbs[2]],rep(0,length(ybs[xbs>=icbs[1]&xbs<=icbs[2]])))
# xxfs<- c(xfs[xfs>=icfs[1]&xfs<=icfs[2]],rev(xfs[xfs>=icfs[1]&xfs<=icfs[2]]))
# yyfs<- c(yfs[xfs>=icfs[1]&xfs<=icfs[2]],rep(0,length(yfs[xfs>=icfs[1]&xfs<=icfs[2]])))
# 
# densb_bs  <- data.frame(x=xxbs, y=yybs , t=rep('a', length(xxbs)), r=seq(1,length(xxbs),1))
# densb_fs  <- data.frame(x=xxfs, y=yyfs , t=rep('a', length(xxfs)), r=seq(1,length(xxfs),1))
# 
# # *Probabilidad de estar bajo BRMS* #Asesoría  #P(BD<BDrms)---- 
# pa_sept <-pnorm(0.9,rprSEPT,rprSEPTstd,lower.tail = TRUE,log.p = F)
# # *Probabilidad de estar bajo FRMS* #Asesoría  #P(F>Frms)----
# pb_sept <-1-pnorm(1.1,FrprSEPT,FrprSEPTstd,lower.tail = TRUE,log.p = F)
# # *Probabilidad de estar en zona de sobreexplotacion*  #Asesoría  #P(BD<BDrms)---- 
# pc_sept <-pnorm(0.9,rprSEPT,rprSEPTstd,lower.tail = TRUE,log.p = F)-pnorm(0.5,
#                    rprSEPT,rprSEPTstd,lower.tail = TRUE,log.p = F)
# # *Probabilidad de estar en zona de colapso* #Asesoría  #P(BD<BDrms) ----
# pd_sept <-pnorm(0.5,rprSEPT,rprSEPTstd,lower.tail = TRUE,log.p = F)
# # *Probailidad de sobrepesca* #Asesoría  #P(F>Frms)----
# pe_sept <-1-pnorm(1.1,FrprSEPT,FrprSEPTstd,lower.tail = TRUE,log.p = F)
# 
# 
# # HITO 2: MARZO----
# #para serie histórica ----
# Rpr2     <- c(subset(std2,name=="RPRequ3")$value); 
# Rpr2std  <- c(subset(std2,name=="RPRequ3")$std)
# Frpr2    <- c(subset(std2,name=="Frpr")$value); 
# Frpr2std <- c(subset(std2,name=="Frpr")$std)
# 
# EstatusMar<- data.frame(x=years2, 
#                         Rpr=Rpr2,
#                         Frpr=Frpr2,
#                         lowerRpr  = (Rpr2  -1.96*Rpr2std ), 
#                         upperRpr  = (Rpr2  +1.96*Rpr2std ),
#                         lowerFrpr = (Frpr2 -1.96*Frpr2std), 
#                         upperFrpr = (Frpr2 +1.96*Frpr2std))
# 
# #Para densidad de probabilidad último año ----
# rprMAR     <-subset(std2,name=="RPRequ3")$value[nyears1]
# rprMARstd  <-subset(std2,name=="RPRequ3")$std[nyears1]
# FrprMAR    <-subset(std2,name=="Frpr")$value[nyears1]
# FrprMARstd <-subset(std2,name=="Frpr")$std[nyears1]
# # biomasa desovante vs BDrms ----
# xbm1  <-rnorm(1000, mean = rprMAR, sd = rprMARstd)
# xbm   <-seq(min(xbm1),max(xbm1),0.005)
# ybm   <-dnorm(xbm, mean = rprMAR, sd =rprMARstd)
# icbm  <-qnorm(c(0.05,0.95,0.5),rprMAR,rprMARstd)
# # mortalidad por pesca vs Frms ----
# xfm1  <- rnorm(1000, mean = FrprMAR, sd = FrprMARstd)
# xfm   <-seq(min(xfm1),max(xfm1),0.005)
# yfm   <-dnorm(xfm, mean = FrprMAR, sd =FrprMARstd)
# icfm  <-qnorm(c(0.05,0.95,0.5),FrprMAR,FrprMARstd)
# #distribución probabilidad ----
# xxbm  <- c(xbm[xbm>=icbm[1]&xbm<=icbm[2]],rev(xbm[xbm>=icbm[1]&xbm<=icbm[2]]))
# yybm  <- c(ybm[xbm>=icbm[1]&xbm<=icbm[2]],rep(0,length(ybm[xbm>=icbm[1]&xbm<=icbm[2]])))
# xxfm  <- c(xfm[xfm>=icfm[1]&xfm<=icfm[2]],rev(xfm[xfm>=icfm[1]&xfm<=icfm[2]]))
# yyfm  <- c(yfm[xfm>=icfm[1]&xfm<=icfm[2]],rep(0,length(yfm[xfm>=icfm[1]&xfm<=icfm[2]])))
# densb_bm  <- data.frame(x=xxbm, y=yybm , t=rep('a', length(xxbm)), r=seq(1,length(xxbm),1))
# densb_fm  <- data.frame(x=xxfm, y=yyfm , t=rep('a', length(xxfm)), r=seq(1,length(xxfm),1))
# 
# # *Probabilidad de estar bajo BRMS* #Asesoría  #P(BD<BDrms) ---- 
# pa_mar <-pnorm(0.9,rprMAR,rprMARstd,lower.tail = TRUE,log.p = F)
# # *Probabilidad de estar bajo FRMS* #Asesoría  #P(F>Frms) ----
# pb_mar <-1-pnorm(1.1,FrprMAR,FrprMARstd,lower.tail = TRUE,log.p = F)
# # *Probabilidad de estar en zona de sobreexplotacion*  #Asesoría  #P(BD<BDrms) ----
# pc_mar <-pnorm(0.9,rprMAR,rprMARstd,lower.tail = TRUE,log.p = F)-pnorm(0.5,
#                    rprMAR,rprMARstd,lower.tail = TRUE,log.p = F)
# # *Probabilidad de estar en zona de colapso* #Asesoría  #P(BD<BDrms) ----
# pd_mar <-pnorm(0.5,rprMAR,rprMARstd,lower.tail = TRUE,log.p = F)
# # *Probailidad de sobrepesca* #Asesoría  #P(F>Frms) ----
# pe_mar <-1-pnorm(1.1,FrprMAR,FrprMARstd,lower.tail = TRUE,log.p = F)
# 
# 
# # HITO 3: JULIO ----
# #para serie histórica ----
# Rpr3     <- c(subset(std3,name=="RPRequ3")$value); 
# Rpr3std  <- c(subset(std3,name=="RPRequ3")$std)
# Frpr3    <- c(subset(std3,name=="Frpr")$value); 
# Frpr3std <- c(subset(std3,name=="Frpr")$std)
# 
# EstatusJul<- data.frame(x=years1, 
#                         Rpr=Rpr3,
#                         Frpr=Frpr3,
#                         lowerRpr  = (Rpr3  -1.96*Rpr3std ), 
#                         upperRpr  = (Rpr3  +1.96*Rpr3std ),
#                         lowerFrpr = (Frpr3 -1.96*Frpr3std), 
#                         upperFrpr = (Frpr3 +1.96*Frpr3std))
# 
# #Para densidad de probabilidad último año ----
# rprJUL     <- subset(std3,name=="RPRequ3")$value[nyears1]
# rprJULstd  <- subset(std3,name=="RPRequ3")$std[nyears1]
# FrprJUL    <- subset(std3,name=="Frpr")$value[nyears1]
# FrprJULstd <- subset(std3,name=="Frpr")$std[nyears1]
# # biomasa desovante vs BDrms----
# xbj1  <- rnorm(1000, mean = rprJUL, sd = rprJULstd)
# xbj   <- seq(min(xbj1),max(xbj1),0.005)
# ybj   <- dnorm(xbj, mean = rprJUL, sd =rprJULstd)
# icbj  <- qnorm(c(0.05,0.95,0.5),rprJUL,rprJULstd)
# # mortalidad por pesca vs Frms----
# xfj1  <- rnorm(1000, mean = FrprJUL, sd = FrprJULstd)
# xfj   <- seq(min(xfj1),max(xfj1),0.005)
# yfj   <- dnorm(xfj, mean = FrprJUL, sd =FrprJULstd)
# icfj  <- qnorm(c(0.05,0.95,0.5),FrprJUL,FrprJULstd)
# #distribución probabilidad----
# xxbj  <- c(xbj[xbj>=icbj[1]&xbj<=icbj[2]],rev(xbj[xbj>=icbj[1]&xbj<=icbj[2]]))
# yybj  <- c(ybj[xbj>=icbj[1]&xbj<=icbj[2]],rep(0,length(ybj[xbj>=icbj[1]&xbj<=icbj[2]])))
# xxfj  <- c(xfj[xfj>=icfj[1]&xfj<=icfj[2]],rev(xfj[xfj>=icfj[1]&xfj<=icfj[2]]))
# yyfj  <- c(yfj[xfj>=icfj[1]&xfj<=icfj[2]],rep(0,length(yfj[xfj>=icfj[1]&xfj<=icfj[2]])))
# densb_bj  <- data.frame(x=xxbj, y=yybj , t=rep('a', length(xxbj)), r=seq(1,length(xxbj),1))
# densb_fj  <- data.frame(x=xxfj, y=yyfj , t=rep('a', length(xxfj)), r=seq(1,length(xxfj),1))
# 
# # *Probabilidad de estar bajo BRMS* #Asesoría  #P(BD<BDrms) ----
# pa_jul <- pnorm(0.9,rprJUL,rprJULstd,lower.tail = TRUE,log.p = F)
# # *Probabilidad de estar bajo FRMS* #Asesoría  #P(F>Frms) ----
# pb_jul <- 1-pnorm(1.1,FrprJUL,FrprJULstd,lower.tail = TRUE,log.p = F)
# # *Probabilidad de estar en zona de sobreexplotacion*  #Asesoría  #P(BD<BDrms)  ----
# pc_jul <- pnorm(0.9,rprJUL,rprJULstd,lower.tail = TRUE,log.p = F)-pnorm(0.5,
#                    rprJUL,rprJULstd,lower.tail = TRUE,log.p = F)
# # *Probabilidad de estar en zona de colapso* #Asesoría  #P(BD<BDrms) ----
# pd_jul <- pnorm(0.5,rprJUL,rprJULstd,lower.tail = TRUE,log.p = F)
# # *Probailidad de sobrepesca* #Asesoría  #P(F>Frms) ----
# pe_jul <- 1-pnorm(1.1,FrprJUL,FrprJULstd,lower.tail = TRUE,log.p = F)
# 
# 
# # 10. PROYECCION ><> ><> ><> ><>  ----
# 
# # RESULTADOS DE PROYECCIÓN ASESORÍA DE SEPTIEMBRE (HITO 1) ----
# 
# 
# 
# 
# # Arreglos para figuras proyeccion Fig18 ----
# 
# 
#   datafig18<-function(dir.0,carpetaCBA,admb,yearProy,SSB,BRMS,desembarquepred){
# 
#    # carpetaCBA<-carpetaCBA_sept
#   #  admb<-admb_sept
#     
#     source(paste(dir.fun,"Fn_CBA.R",sep=""))
#     CreaDataProybase(dir.0,carpetaCBA,admb)
#     
#     
#     dira<-paste(dir.0,carpetaCBA,sep="")
#     setwd(dira)
#      load('dataProybase.RData')
#      
# Rtp<-reps1a$Reclutas
# Rs1<-reps1a$Np[1]
# Rs2<-reps2a$Np[1]
# Rs3<-reps3a$Np[1]
# yearsp<-c(reps1a$years,yearProy)
# 
# dataRproy<-data.frame(S1=c(Rtp,rep(Rs1,2)),
#                       S2=c(Rtp,rep(Rs2,2)),
#                       S3=c(Rtp,rep(Rs3,2)),
#                       base=c(Rtp,rep(NA,2))) %>% 
#            mutate(years=yearsp,indicador='Rt') %>% 
#            melt(id.vars=c('years','indicador'))
# 
# 
# dataBDproy<-data.frame(S1=c(SSB,bds1)/1000,
#                        S2=c(SSB,bds2)/1000,
#                        S3=c(SSB,bds3)/1000,
#                        base=c(SSB,rep(NA,2))/1000) %>% 
#             mutate(years=yearsp,indicador='BD')%>% 
#             melt(id.vars=c('years','indicador'))
# 
# 
# dataBD_rmsproy<-data.frame(S1=round(c(SSB,bds1)/BRMS,1),
#                            S2=round(c(SSB,bds2)/BRMS,1),
#                            S3=round(c(SSB,bds3)/BRMS,1),
#                            base=round(c(SSB/BRMS,rep(NA,2)),1)) %>% 
#                 mutate(years=yearsp,indicador='BD_BDrms')%>% 
#                 melt(id.vars=c('years','indicador'))
#  
# dataCt_proy<-data.frame(S1=c(desembarquepred,cs1)/1000,
#                         S2=c(desembarquepred,cs2)/1000,
#                         S3=c(desembarquepred,cs3)/1000,
#                         base=c(desembarquepred,rep(NA,2))/1000) %>% 
#              mutate(years=yearsp,indicador='Ct')%>% 
#              melt(id.vars=c('years','indicador'))
# 
# DataProy<-rbind(dataRproy,dataBDproy,dataBD_rmsproy,dataCt_proy)
# 
# 
# }
# # 
# # 
# # # Arreglos para tablas ----
# # 
# # # RECLUTAMIENTO ESTIMADO ULTIMO AÑO EVALUACIÓN (AÑO ACTUAL) ----
# # RTs0s     <- subset(stds1,name=="Reclutas")$value[nyears1] ; 
# # RTs0s_std  <- subset(stds1,name=="Reclutas")$std[nyears1] 
# # 
# # #BIOMASA DESOVANTE ESTIMADA ULTIMO AÑO EVALUACIÓN ----
# # bds0s     <- subset(stds1,name=="SSB")$value[nyears1] ; 
# # bds0s_std  <- subset(stds1,name=="SSB")$std[nyears1] 
# # 
# # # aporte del grupo de edad 0 (reclutamiento) año actual ----
# # C1eryearR1act<-round(reps1a$YTP_r0W_actual[1]/sum(reps1a$YTP_r0W_actual),2)
# # C1eryearR1act2<-round(reps1a$YTP_r0W_actual[2]/sum(reps1a$YTP_r0W_actual),2)
# # 
# # # aporte del grupo de edad 0 (reclutamiento) 1er año proyectado ----
# # C1eryearR1<-round(reps1a$YTP_p0W_proyectada[1,1]/sum(reps1a$YTP_p0W_proyectada[1,]),2)
# # C1eryearR2<-round(reps2a$YTP_p0W_proyectada[1,1]/sum(reps2a$YTP_p0W_proyectada[1,]),2)
# # C1eryearR3<-round(reps3a$YTP_p0W_proyectada[1,1]/sum(reps3a$YTP_p0W_proyectada[1,]),2)
# # 
# # # aporte del grupo de edad 1 (reclutamiento) 1er año proyectado ----
# # C1eryearR1a<-round(reps1a$YTP_p0W_proyectada[1,2]/sum(reps1a$YTP_p0W_proyectada[1,]),2)
# # C1eryearR2a<-round(reps2a$YTP_p0W_proyectada[1,2]/sum(reps2a$YTP_p0W_proyectada[1,]),2)
# # C1eryearR3a<-round(reps3a$YTP_p0W_proyectada[1,2]/sum(reps3a$YTP_p0W_proyectada[1,]),2)
# # 
# # # aporte del grupo de edad 0 (reclutamiento) 2do año proyectado ----
# # C1eryearR12<-round(reps1a$YTP_p0W_proyectada[2,1]/sum(reps1a$YTP_p0W_proyectada[2,]),2)
# # C1eryearR22<-round(reps2a$YTP_p0W_proyectada[2,1]/sum(reps2a$YTP_p0W_proyectada[2,]),2)
# # C1eryearR32<-round(reps3a$YTP_p0W_proyectada[2,1]/sum(reps3a$YTP_p0W_proyectada[2,]),2)
# # 
# # # aporte del grupo de edad 1  2do año proyectado ----
# # C1eryearR12a<-round(reps1a$YTP_p0W_proyectada[2,2]/sum(reps1a$YTP_p0W_proyectada[2,]),2)
# # C1eryearR22a<-round(reps2a$YTP_p0W_proyectada[2,2]/sum(reps2a$YTP_p0W_proyectada[2,]),2)
# # C1eryearR32a<-round(reps3a$YTP_p0W_proyectada[2,2]/sum(reps3a$YTP_p0W_proyectada[2,]),2)
# # 
# # # ESTATUS PROYECTADO ----
# # 
# # # PRIMER AÑO PROYECTADO ----
# # ### *Probabilidad de estar bajo BRMS* ----
# # pa1<-pnorm(0.9,RpRps1[1],RpRps1std[1],lower.tail = TRUE,log.p = F)
# # pa2<-pnorm(0.9,RpRps2[1],RpRps2std[1],lower.tail = TRUE,log.p = F)
# # pa3<-pnorm(0.9,RpRps3[1],RpRps3std[1],lower.tail = TRUE,log.p = F)
# # ### *Probabilidad de estar en zona de sobreexplotacion* ----
# # pc1<-pnorm(0.9,RpRps1[1],RpRps1std[1],lower.tail = TRUE,log.p = F)-pnorm(0.5,
# #                RpRps1[1],RpRps1std[1],lower.tail = TRUE,log.p = F)
# # pc2<-pnorm(0.9,RpRps2[1],RpRps2std[1],lower.tail = TRUE,log.p = F)-pnorm(0.5,
# #                RpRps2[1],RpRps2std[1],lower.tail = TRUE,log.p = F)
# # pc3<-pnorm(0.9,RpRps3[1],RpRps3std[1],lower.tail = TRUE,log.p = F)-pnorm(0.5,
# #                RpRps3[1],RpRps3std[1],lower.tail = TRUE,log.p = F)
# # ### *Probabilidad de estar en zona de colapso*----
# # pd1<-pnorm(0.5,RpRps1[1],RpRps1std[1],lower.tail = TRUE,log.p = F)
# # pd2<-pnorm(0.5,RpRps2[1],RpRps2std[1],lower.tail = TRUE,log.p = F)
# # pd3<-pnorm(0.5,RpRps3[1],RpRps3std[1],lower.tail = TRUE,log.p = F)
# # 
# # # SEGUNDO AÑO PROYECTADO ----
# # 
# # ### *Probabilidad de estar bajo BRMS* ----
# # pa12<-pnorm(0.9,RpRps1[2],RpRps1std[2],lower.tail = TRUE,log.p = F)
# # pa22<-pnorm(0.9,RpRps2[2],RpRps2std[2],lower.tail = TRUE,log.p = F)
# # pa32<-pnorm(0.9,RpRps3[2],RpRps3std[2],lower.tail = TRUE,log.p = F)
# # ### *Probabilidad de estar en zona de sobreexplotacion* ----
# # pc12<-pnorm(0.9,RpRps1[2],RpRps1std[2],lower.tail = TRUE,log.p = F)-pnorm(0.5,
# #                 RpRps1[2],RpRps1std[2],lower.tail = TRUE,log.p = F)
# # pc22<-pnorm(0.9,RpRps2[2],RpRps2std[2],lower.tail = TRUE,log.p = F)-pnorm(0.5,
# #                 RpRps2[2],RpRps2std[2],lower.tail = TRUE,log.p = F)
# # pc32<-pnorm(0.9,RpRps3[2],RpRps3std[2],lower.tail = TRUE,log.p = F)-pnorm(0.5,
# #                 RpRps3[2],RpRps3std[2],lower.tail = TRUE,log.p = F)
# # ### *Probabilidad de estar en zona de colapso* ----
# # pd12<-pnorm(0.5,RpRps1[2],RpRps1std[2],lower.tail = TRUE,log.p = F)
# # pd22<-pnorm(0.5,RpRps2[2],RpRps2std[2],lower.tail = TRUE,log.p = F)
# # pd32<-pnorm(0.5,RpRps3[2],RpRps3std[2],lower.tail = TRUE,log.p = F)
# # 
# # # CBA INICIAL ----
# # n<-3
# # q       <- seq(0.1,0.5,0.1)  # niveles de riesgo (cuantiles)                                
# # nq      <- length(q)                                                                                   
# # CBA_sept     <- matrix(ncol=nq,nrow=n)
# # CBAp_sept    <- rep(0,n)
# # CBApstd_sept <- rep(0,n)
# # 
# # buffer   <- matrix(ncol=nq,nrow=n)
# # descarte <- matrix(ncol=nq,nrow=n)
# # 
# # for(i in 1:n){
# #   std     <- read.table(paste(admb_sept,"1",i,".std",sep=""),header=T,sep="",na="NA",fill=T) 
# #   CBAp_sept[i]    <-subset(std,name=="CBA_c0")$value[1]
# #   CBApstd_sept[i] <-subset(std,name=="CBA_c0")$std[1]
# #   for(j in 1:nq){CBA_sept[i,j]<-qnorm(q[j],CBAp_sept[i],CBApstd_sept[i])}}
# # 
# # for(i in 1:n){for(j in 1:nq){	
# #   buffer[i,j]<-round(1-CBA_sept[i,j]/CBA_sept[i,5],2)}}
# # 
# # # CBA INICIAL MENOS DESCARTE ----
# # n<-3
# # q       <- seq(0.1,0.5,0.1)  # niveles de riesgo (cuantiles)                                
# # nq      <- length(q)                                                                                   
# # CBAd_sept     <- matrix(ncol=nq,nrow=n)
# # CBApd_sept    <- rep(0,n)
# # CBApdstd_sept <- rep(0,n)
# # 
# # 
# # for(i in 1:n){
# #   std     <- read.table(paste(admb_sept,"1",i,".std",sep=""),header=T,sep="",na="NA",fill=T) 
# #   CBApd_sept[i]    <-subset(std,name=="CBA_c0d")$value[1]
# #   CBApdstd_sept[i] <-subset(std,name=="CBA_c0d")$std[1]
# #   for(j in 1:nq){CBAd_sept[i,j]<-qnorm(q[j],CBApd_sept[i],CBApdstd_sept[i])}}
# # 
# # 
# # 
