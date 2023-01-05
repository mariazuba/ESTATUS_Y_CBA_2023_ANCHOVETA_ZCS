# Función DataHito ----

CreaRdata<-function(admb_dat,admb_rep,admb_std,Hito){
  data        <- lisread(admb_dat) 
  names(data) <- str_trim(names(data), side="right")
  dat         <- data
  rep         <- reptoRlist(admb_rep)
  std         <- read.table(admb_std,header=T,sep="",na="NA",fill=T) 
  
  #datos para Rdata ----
  
  years  <- rep$years
  nyears <- length(years)
  
  age     <- seq(0,4,1)                                            
  nage    <- length(age)  
  
  #pesos medios ----
  WmedF    <- dat$Wmed                                             
  WiniF    <- dat$Wini   
  #indices ----
  names_ind<- c('Crucero_verano', 
                'Crucero_otoño',
                'Crucero_huevos', 
                'Desembarques') 
  #observados 
  reclasobs     <-rep$reclasobs
  pelacesobs    <-rep$pelacesobs
  mphobs        <-rep$mphobs
  desembarqueobs<-rep$desembarqueobs
  #predichos
  reclaspred     <-rep$reclaspred 
  pelacespred    <-rep$pelacespred 
  mphpred        <-rep$mphpred
  desembarquepred<-rep$desembarquepred
  #Cvs indices ----
  cvBcV   <-0.30
  cvBcO   <-0.30
  cvdes   <-0.01
  #composiciones edad ----
  #observados
  pfobs<-rep$pf_obs
  pRobs<-rep$pobs_RECLAS
  pPobs<-rep$pobs_PELACES
  #predichos
  pfpred<-rep$pf_pred
  pRpred<-rep$ppred_RECLAS
  pPpred<-rep$ppred_PELACES
  
  #Variables poblacionales ----
  Rt      <- subset(std,name=="Reclutas")$value 
  Rtstd   <- subset(std,name=="Reclutas")$std
  BT      <- subset(std,name=="BT")$value   
  BTstd   <- subset(std,name=="BT")$std
  BD      <- subset(std,name=="SSB")$value   
  BDstd   <- subset(std,name=="SSB")$std
  Ft      <- subset(std,name=="log_Ft")$value   
  Ftstd   <- subset(std,name=="log_Ft")$std
  
  # Puntos biológicos de referencia ----
  Bmed        <- mean(BD,na.rm = T)         
  Fmedian     <- exp(median(Ft,na.rm = T))
  Bo           <- rep$SSBpbr[1]        # Paso 4: Obtenci?n de Bo
  BRMS         <- rep$SSBpbr[3]        # Paso 5: Obtenci?n de Brms = 60%SPRo = 55%Bo
  FRMS         <- rep$Fs[2]
  BLIM         <- Bo*0.275             # Paso 6: Obtenci?n de Blim = 20%Bo 
  FLIM         <- rep$Fs[3]            # Paso 6: Obtenci?n de Flim = 30%SPRo
  SpB          <- BD                   # BD serie hist?rica de evaluaci?n de stock 
  SpBSE        <- BDstd                # desviaci?n estandar BD
  ln_Fyr       <- Ft                   # logaritmo de Ft
  ln_FSE       <- Ftstd                # logaritmo de la desviaci?n standar de Ft
  
  # Indicadores de Estatus ----
  RPR     <- c(subset(std,name=="RPRequ3")$value); 
  RPRstd  <- c(subset(std,name=="RPRequ3")$std)
  FRPR    <- c(subset(std,name=="Frpr")$value); 
  FRPRstd <- c(subset(std,name=="Frpr")$std)
  
  # Probabilidades de estatus ----
  #Para densidad de probabilidad último año----
  rprlast     <-subset(std,name=="RPRequ3")$value[nyears]
  rprlaststd  <-subset(std,name=="RPRequ3")$std[nyears]
  Frprlast    <-subset(std,name=="Frpr")$value[nyears]
  Frprlaststd <-subset(std,name=="Frpr")$std[nyears]
  # biomasa desovante vs BDrms----
  xb1 <-rnorm(1000, mean = rprlast, sd = rprlaststd)
  xb  <-seq(min(xb1),max(xb1),0.005)
  yb  <-dnorm(xb, mean = rprlast, sd =rprlaststd)
  icb <-qnorm(c(0.05,0.95,0.5),rprlast,rprlaststd)
  # mortalidad por pesca vs Frms----
  xf1 <- rnorm(1000, mean = Frprlast, sd = Frprlaststd)
  xf  <-seq(min(xf1),max(xf1),0.005)
  yf  <-dnorm(xf, mean = Frprlast, sd =Frprlaststd)
  icf <-qnorm(c(0.05,0.95,0.5),Frprlast,Frprlaststd)
  #distribución probabilidad----
  xxb<- c(xb[xb>=icb[1]&xb<=icb[2]],rev(xb[xb>=icb[1]&xb<=icb[2]]))
  yyb<- c(yb[xb>=icb[1]&xb<=icb[2]],rep(0,length(yb[xb>=icb[1]&xb<=icb[2]])))
  xxf<- c(xf[xf>=icf[1]&xf<=icf[2]],rev(xf[xf>=icf[1]&xf<=icf[2]]))
  yyf<- c(yf[xf>=icf[1]&xf<=icf[2]],rep(0,length(yf[xf>=icf[1]&xf<=icf[2]])))
  
  ####################################################################################
  # densb_b  <- data.frame(x=xxb, y=yyb , t=rep('a', length(xxb)), r=seq(1,length(xxb),1))
  # densb_f  <- data.frame(x=xxf, y=yyf , t=rep('a', length(xxf)), r=seq(1,length(xxf),1))
  # ####################################################################################
  
  # *Probabilidad de estar bajo BRMS* #Asesoría  #P(BD<BDrms)---- 
  pa <-pnorm(0.9,rprlast,rprlaststd,lower.tail = TRUE,log.p = F)
  # *Probabilidad de estar bajo FRMS* #Asesoría  #P(F>Frms)----
  pb <-1-pnorm(1.1,Frprlast,Frprlaststd,lower.tail = TRUE,log.p = F)
  # *Probabilidad de estar en zona de sobreexplotacion*  #Asesoría  #P(BD<BDrms)---- 
  pc <-pnorm(0.9,rprlast,rprlaststd,lower.tail = TRUE,log.p = F)-pnorm(0.5,
                 rprlast,rprlaststd,lower.tail = TRUE,log.p = F)
  # *Probabilidad de estar en zona de colapso* #Asesoría  #P(BD<BDrms) ----
  pd <-pnorm(0.5,rprlast,rprlaststd,lower.tail = TRUE,log.p = F)
  # *Probailidad de sobrepesca* #Asesoría  #P(F>Frms)----
  pe <-1-pnorm(1.1,Frprlast,Frprlaststd,lower.tail = TRUE,log.p = F)
  
  
  #guarda Rdata ----
  save(years,nyears,age,nage,names_ind,
       WmedF,WiniF, #pesos medios
       reclasobs,pelacesobs,mphobs,desembarqueobs,#indices de abundancia observadas
       reclaspred,pelacespred,mphpred,desembarquepred,#indices de abundancia predicha
       cvBcV,cvBcO,cvdes, # CVs indices de abundancia
       pfobs,pRobs,pPobs,#composiciones de edad observadas
       pfpred,pRpred,pPpred,#composiciones de edad predichas
       Rt,Rtstd,BT,BTstd,BD,BDstd,Ft,Ftstd,#variables poblacionales
       Bmed,Fmedian,Bo,BRMS,FRMS,BLIM,FLIM,SpB,SpBSE,ln_Fyr,ln_FSE, #PBRs
       RPR,RPRstd,FRPR,FRPRstd,#indicadores de estatus
       rprlast,rprlaststd,Frprlast,Frprlaststd, #BD/BDrms y F/Frms último año
       xb1,xb,yb,icb, #probabilidad BD/BDrms último año
       xf1,xf,yf,icf, #probabilidad F/Frms último año
       xxb,yyb,xxf,yyf, #distribución de probabilidad 
       pa,pb,pc,pd,pe, # probabilidades de riesgo
       file=paste(dir.Rdata,'Datos',Hito,'.RData',sep=""))
  
}