
# ARREGLO DATOS
rm(list=ls(all=T))

library(stringr) # para arreglo de archivo .dat


dir.0       <-getwd() # directorio de trabajo 
dir.1       <-paste(dir.0,"/codigos_admb",sep="") # carpeta de códigos ADMB 
dir.fun     <-paste(dir.0,"/funciones/",sep="") # carpeta de funciones utilizadas en este informe
source(paste(dir.fun,"functions.R",sep="")) # funciones para leer .dat y .rep
source(paste(dir.fun,"Fn_PBRs.R",sep="")) # funciones para leer .dat y .rep


setwd(dir.1)

admb_dat<-list.files(pattern=".dat")
admb_rep<-list.files(pattern=".rep")
admb_std<-list.files(pattern=".std")

# ASESORÍA DE SEPTIEMBRE
data1        <- lisread(admb_dat[3]) 
names(data1) <- str_trim(names(data1), side="right")
dat1         <- data1
rep1         <- reptoRlist(admb_rep[3])
std1         <- read.table(admb_std[3],header=T,sep="",na="NA",fill=T) 

# ASESORÍA DE MARZO
data2        <- lisread(admb_dat[1]) 
names(data2) <- str_trim(names(data2), side="right")
dat2         <- data2
rep2         <- reptoRlist(admb_rep[1])
std2         <- read.table(admb_std[1],header=T,sep="",na="NA",fill=T) 

# ASESORÍA DE JULIO
data3        <- lisread(admb_dat[2]) 
names(data3) <- str_trim(names(data3), side="right")
dat3         <- data3
rep3         <- reptoRlist(admb_rep[2])
std3         <- read.table(admb_std[2],header=T,sep="",na="NA",fill=T) 

# AÑOS BIOLOGICO ANCHOVETA

yearsb<-c("1996/97",
          "1997/98","1998/99","1999/00","2000/01","2001/02","2002/03","2003/04",
          "2004/05","2005/06","2006/07","2007/08","2008/09","2009/10","2010/11",
          "2011/12","2012/13","2013/14","2014/15","2015/16","2016/17","2017/18",
          "2018/19","2019/20",'2020/21','2021/22')


###############################################################################
# VARIABLES POBLACIONALES
###############################################################################
#------------------------------------------------------------------------------
# HITO 1: SEPTIEMBRE
#------------------------------------------------------------------------------
years1<-rep1$years
nyears1<-length(years1)

Rt1      <- c(subset(std1,name=="Reclutas")$value) 
Rt1std   <- c(subset(std1,name=="Reclutas")$std)
BT1      <- c(subset(std1,name=="BT")$value)   
BT1std   <- c(subset(std1,name=="BT")$std)
BD1      <- c(subset(std1,name=="SSB")$value)   
BD1std   <- c(subset(std1,name=="SSB")$std)
Ft1      <- c(subset(std1,name=="log_Ft")$value)   
Ft1std   <- c(subset(std1,name=="log_Ft")$std)

VarPobSep<- data.frame(x=years1, 
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
#------------------------------------------------------------------------------
# HITO 2: MARZO
#------------------------------------------------------------------------------
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
#------------------------------------------------------------------------------
# HITO 3: JULIO
#------------------------------------------------------------------------------
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

###############################################################################
# PUNTOS BIOLÓGICOS DE REFERENCIA
###############################################################################
#------------------------------------------------------------------------------
# HITO 1: SEPTIEMBRE
#------------------------------------------------------------------------------
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

#------------------------------------------------------------------------------
# HITO 2: MARZO
#------------------------------------------------------------------------------
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

#------------------------------------------------------------------------------
# HITO 3: JULIO
#------------------------------------------------------------------------------
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

###############################################################################
# ESTATUS
###############################################################################

#para serie histórica
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

#Para densidad de probabilidad
rprSEPT     <-subset(std1,name=="RPRequ3")$value[nyears1]
rprSEPTstd  <-subset(std1,name=="RPRequ3")$std[nyears1]
FrprSEPT    <-subset(std1,name=="Frpr")$value[nyears1]
FrprSEPTstd <-subset(std1,name=="Frpr")$std[nyears1]

# biomasa desovante vs BDrms
xbs1 <-rnorm(1000, mean = rprSEPT, sd = rprSEPTstd)
xbs  <-seq(min(xbs1),max(xbs1),0.005)
ybs  <-dnorm(xbs, mean = rprSEPT, sd =rprSEPTstd)
icbs <-qnorm(c(0.05,0.95,0.5),rprSEPT,rprSEPTstd)

# mortalidad por pesca vs Frms
xfs1 <- rnorm(1000, mean = FrprSEPT, sd = FrprSEPTstd)
xfs  <-seq(min(xfs1),max(xfs1),0.005)
yfs  <-dnorm(xfs, mean = FrprSEPT, sd =FrprSEPTstd)
icfs <-qnorm(c(0.05,0.95,0.5),FrprSEPT,FrprSEPTstd)

#distribución probabilidad
xxbs<- c(xbs[xbs>=icbs[1]&xbs<=icbs[2]],rev(xbs[xbs>=icbs[1]&xbs<=icbs[2]]))
yybs<- c(ybs[xbs>=icbs[1]&xbs<=icbs[2]],rep(0,length(ybs[xbs>=icbs[1]&xbs<=icbs[2]])))
xxfs<- c(xfs[xfs>=icfs[1]&xfs<=icfs[2]],rev(xfs[xfs>=icfs[1]&xfs<=icfs[2]]))
yyfs<- c(yfs[xfs>=icfs[1]&xfs<=icfs[2]],rep(0,length(yfs[xfs>=icfs[1]&xfs<=icfs[2]])))

densb_bs  <- data.frame(x=xxbs, y=yybs , t=rep('a', length(xxbs)), r=seq(1,length(xxbs),1))
densb_fs  <- data.frame(x=xxfs, y=yyfs , t=rep('a', length(xxfs)), r=seq(1,length(xxfs),1))

# *Probabilidad de estar bajo BRMS* #Asesoría  #P(BD<BDrms) 
pa_sept <-pnorm(0.9,rprSEPT,rprSEPTstd,lower.tail = TRUE,log.p = F)
# *Probabilidad de estar bajo FRMS* #Asesoría  #P(F>Frms)
pb_sept <-1-pnorm(1.1,FrprSEPT,FrprSEPTstd,lower.tail = TRUE,log.p = F)
# *Probabilidad de estar en zona de sobreexplotacion*  #Asesoría  #P(BD<BDrms) 
pc_sept <-pnorm(0.9,rprSEPT,rprSEPTstd,lower.tail = TRUE,log.p = F)-pnorm(0.5,
                   rprSEPT,rprSEPTstd,lower.tail = TRUE,log.p = F)
# *Probabilidad de estar en zona de colapso* #Asesoría  #P(BD<BDrms) 
pd_sept <-pnorm(0.5,rprSEPT,rprSEPTstd,lower.tail = TRUE,log.p = F)
# *Probailidad de sobrepesca* #Asesoría  #P(F>Frms)
pe_sept <-1-pnorm(1.1,FrprSEPT,FrprSEPTstd,lower.tail = TRUE,log.p = F)

