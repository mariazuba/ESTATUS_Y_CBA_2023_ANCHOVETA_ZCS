# Función DataHito ----
DataHito<-function(admb_dat,admb_rep,admb_std,Hito){
  
  data        <- lisread(admb_dat) 
  names(data) <- str_trim(names(data), side="right")
  dat         <- data
  rep         <- reptoRlist(admb_rep)
  std         <- read.table(admb_std,header=T,sep="",na="NA",fill=T) 
  
  #datos para Rdata
  years  <- rep$years
  nyears <- length(years)
  
  age     <- seq(0,4,1)                                            
  nage    <- length(age)  
  
  #indices
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
  #Cvs indices
  cvBcV   <-0.30
  cvBcO   <-0.30
  cvdes   <-0.01
  #composiciones edad
  #observados
  pfobs<-rep$pf_obs
  pRobs<-rep$pobs_RECLAS
  pPobs<-rep$pobs_PELACES
  #predichos
  pfpred<-rep$pf_pred
  pRpred<-rep$ppred_RECLAS
  pPpred<-rep$ppred_PELACES
  
  #Variables poblacionales
  Rt      <- subset(std,name=="Reclutas")$value 
  Rtstd   <- subset(std,name=="Reclutas")$std
  BT      <- subset(std,name=="BT")$value   
  BTstd   <- subset(std,name=="BT")$std
  BD      <- subset(std,name=="SSB")$value   
  BDstd   <- subset(std,name=="SSB")$std
  Ft      <- subset(std,name=="log_Ft")$value   
  Ftstd   <- subset(std,name=="log_Ft")$std
  
  #guarda Rdata
  save(years,nyears,age,nage,names_ind,
       reclasobs,pelacesobs,mphobs,desembarqueobs,
       reclaspred,pelacespred,mphpred,desembarquepred,
       cvBcV,cvBcO,cvdes,
       pfobs,pRobs,pPobs,
       pfpred,pRpred,pPpred,
       Rt,Rtstd,BT,BTstd,BD,BDstd,Ft,Ftstd,
       file=paste(dir.Rdata,'Datos',Hito,'.RData',sep=""))
  
}