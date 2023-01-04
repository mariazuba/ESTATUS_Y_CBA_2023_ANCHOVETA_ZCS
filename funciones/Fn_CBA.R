
CBA<-function(dir.0,dir.1,Carpeta,admb,l_opt_proy,l_opRec,l_mf,opt_proy,system){
  
  dir.5<-paste(dir.0,Carpeta,sep="")
  
  dat_admb<-paste(admb,".dat",sep="")
  exe_admb<-paste(admb,".exe",sep="")
  tpl_admb<-paste(admb,".tpl",sep="")

  unlink(dir.5,recursive=T) #borra la carpeta "CBA_sept"
  dir.create(file.path(dir.0,Carpeta))#crea la carpeta "CBA_sept"" nuevamente
  
  setwd(dir.1);file.copy(c(dat_admb,tpl_admb),dir.5) #copia los archivos de la carpeta calendario
  
  setwd(dir.5);
  
  if(system=="mac"){
    system(paste("~/admb-12.2/admb",admb,sep=" "))
    system(paste("./",admb,sep=""))
  }

  if(system=="windows"){
    system(paste("/ADMB/admb",admb,sep=" "))
    system(admb)
  }
  
  
  std_admb<-paste(admb,".std",sep="")
  rep_admb<-paste(admb,".rep",sep="")
  
  opRec<-seq(1,3,1) #//ESCENARIO DE RECLUTAMIENTO PROMEDIO
  mf<-c(1,0.9,1.1)
  
  data_file<-(paste(admb,".dat",sep="")) #// archivos de datos originales
  S<-readLines(data_file,encoding="UTF-8")
  Se<-S
  
  for( i in 1:3){
    for(j in 1:3){
      Se[l_opt_proy]<-opt_proy #opt_proy 
      Se[l_opRec]<-opRec[i] #opRec
      Se[l_mf]<-mf[j]
 
      cat(Se,file=(can<-file(paste(admb,".dat",sep=""),"wb",encoding="UTF-8")),sep="\n")
      close(can)
      
      if(system=="mac"){
        system(paste("./",admb,sep=""))
      }
      
      if(system=="windows"){
        system(admb)
      }
      
      
      Std <- readLines(std_admb,encoding="UTF-8") 
      cat(Std,file=(can<-file(paste(admb,j,i,".std",sep=""),"wb",encoding="UTF-8")),sep="\n");close(can)
      Rep <- readLines(rep_admb,encoding="UTF-8") 
      cat(Rep,file=(can<-file(paste(admb,j,i,".rep",sep=""),"wb",encoding="UTF-8")),sep="\n");close(can)
      
    }}
  cat(S,file=(can<-file(paste(admb,".dat",sep=""),"wb",encoding="UTF-8")),sep="\n")
  close(can)
}

CreaDataProybase<-function(dir.0,carpetaCBA,admb){
  
  
  dira<-paste(dir.0,carpetaCBA,sep="")
  setwd(dira)
  
  reps1a     <- reptoRlist(paste(admb,"11.rep",sep=""))  
  reps2a     <- reptoRlist(paste(admb,"12.rep",sep="")) 
  reps3a     <- reptoRlist(paste(admb,"13.rep",sep="")) 
  
  
  stds1     <- read.table(paste(dir.0,carpetaCBA,admb,"11.std", sep=''),
                          header=T,sep="",na="NA",fill=T) 
  stds2     <- read.table(paste(dir.0,carpetaCBA,admb,"12.std", sep=''),
                          header=T,sep="",na="NA",fill=T) 
  stds3     <- read.table(paste(dir.0,carpetaCBA,admb,"13.std", sep=''),
                          header=T,sep="",na="NA",fill=T) 
  
  # Biomasa desovante
  bds1     <- subset(stds1,name=="BD_p0")$value ; 
  bds1std  <- subset(stds1,name=="BD_p0")$std 
  bds2     <- subset(stds2,name=="BD_p0")$value ; 
  bds2std  <- subset(stds2,name=="BD_p0")$std 
  bds3     <- subset(stds3,name=="BD_p0")$value ; 
  bds3std  <- subset(stds3,name=="BD_p0")$std 
  
  # razon BD/BDrms
  RpRps1     <- subset(stds1,name=="RPR_p0")$value ; 
  RpRps1std  <- subset(stds1,name=="RPR_p0")$std 
  RpRps2     <- subset(stds2,name=="RPR_p0")$value ; 
  RpRps2std  <- subset(stds2,name=="RPR_p0")$std 
  RpRps3     <- subset(stds3,name=="RPR_p0")$value ; 
  RpRps3std  <- subset(stds3,name=="RPR_p0")$std 
  
  # Capturas
  cs1     <- subset(stds1,name=="YTP_p0")$value ; 
  cs1std  <- subset(stds1,name=="YTP_p0")$std 
  cs2     <- subset(stds2,name=="YTP_p0")$value ; 
  cs2std  <- subset(stds2,name=="YTP_p0")$std 
  cs3     <- subset(stds3,name=="YTP_p0")$value ; 
  cs3std  <- subset(stds3,name=="YTP_p0")$std 
  
  save(reps1a,reps2a,reps3a,
       stds1,stds2,stds3,
       bds1,bds1std,
       bds2,bds2std,
       bds3,bds3std,
       RpRps1,RpRps1std,
       RpRps2,RpRps2std,
       RpRps3,RpRps3std,
       cs1,cs1std,
       cs2,cs2std,
       cs3,cs3std,
       file='DataproyBase.RData')
}
