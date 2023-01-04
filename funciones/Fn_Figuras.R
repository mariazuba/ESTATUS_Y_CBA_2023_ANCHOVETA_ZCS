
# FIGURAS ----


source(paste(dir.fun,"Fn_Salidas.R",sep=""))


# 1. Ajuste Índices ----

fig1<-function(base1){
  
BcV <- ggplot(base1 %>% filter(type!='observado', variable=='Crucero_verano'), 
              aes(yrs,value/1000000)) + 
  geom_line(aes(colour=Asesoria,linetype = Asesoria), size=0.8) +
  scale_colour_manual(values=c('black','red')) +
  scale_linetype_manual(values=c("solid",'dashed'))+
  geom_point(data = base1 %>% filter(type=='observado', variable=='Crucero_verano'),
             aes(yrs,value/1000000), shape = 19, colour = 'gray30') +
  geom_errorbar(data = base1 %>% filter(type=='observado', variable=='Crucero_verano'),
                aes(ymin = value*exp(-1.96*cvBcO)*10^-6, ymax = value*exp(1.96*cvBcO)*10^-6), color = 'gray30') +
  scale_x_continuous(breaks = seq(from = 1985, to = 2050, by = 4)) +
  labs(x = '', y = 'Biomasas (millones de t)') +
  theme_bw(base_size=9) + 
  ggtitle('Crucero de verano')+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")

BcP <- ggplot(base1 %>% filter(type!='observado', variable=='Crucero_otoño'), 
              aes(yrs,value/1000000)) + 
  geom_line(aes(colour=Asesoria,linetype = Asesoria), size=0.8) +
  scale_colour_manual(values=c('black','red'),name="Asesoría") +
  scale_linetype_manual(values=c("solid",'dashed'),name="Asesoría")+
  geom_point(data = base1 %>% filter(type=='observado', variable=='Crucero_otoño'),
             aes(yrs,value/1000000), shape = 19, colour = 'gray30') +
  geom_errorbar(data = base1 %>% filter(type=='observado', variable=='Crucero_otoño'),
                aes(ymin = value*exp(-1.96*cvBcV)*10^-6, ymax = value*exp(1.96*cvBcV)*10^-6), color = 'gray30') +
  scale_x_continuous(breaks = seq(from = 1985, to = 2050, by = 4)) +
  labs(x = '', y = 'Biomasas (millones de t)') +
  theme_bw(base_size=9) + 
  ggtitle('Crucero de otoño')+
  theme(plot.title = element_text(hjust = 0.5))

BcH <- ggplot(base1 %>% filter(type!='observado', variable=='Crucero_huevos'), 
              aes(yrs,value/1000)) + 
  geom_line(aes(colour=Asesoria,linetype = Asesoria), size=0.8) +
  scale_colour_manual(values=c('black','red'),name="Asesoría") +
  scale_linetype_manual(values=c("solid",'dashed'),name="Asesoría")+
  geom_point(data = base1 %>% filter(type=='observado', variable=='Crucero_huevos'),
             aes(yrs,value/1000), shape = 19, colour = 'gray30') +
  scale_x_continuous(breaks = seq(from = 1985, to = 2050, by = 4)) +
  labs(x = '', y = 'Biomasas (miles de t)') +
  theme_bw(base_size=9) + 
  ggtitle('Crucero de huevos')+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")


d   <- ggplot(base1 %>% filter(type!='observado', variable=='Desembarques'), 
              aes(yrs,value/1000)) +
  geom_line(aes(colour=Asesoria,linetype = Asesoria), size=0.8) +
  scale_colour_manual(values=c('black','red')) +
  scale_linetype_manual(values=c("solid",'dashed'))+
  geom_point(data = base1 %>% filter(type=='observado', variable=='Desembarques'),
             aes(yrs,value/1000), shape = 19, colour = 'gray30') +
  geom_errorbar(data = base1 %>% filter(type=='observado', variable=='Desembarques'),
                aes(ymin = value*exp(-1.96*cvdes)*10^-3, ymax = value*exp(1.96*cvdes)*10^-3), color = 'gray30') +
  scale_x_continuous(breaks = seq(from = 1985, to = 2022, by = 5)) +
  labs(x = '', y = 'Capturas (miles de t)') +
  theme_bw(base_size=9) + 
  ggtitle('Desembarques') +
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")

(BcV/BcP|BcH/d) + plot_layout(guides="collect")

}

# 2. Residuos índices ----

fig2<-function(DataRes){

r1   <- ggplot(DataRes, aes(yrs,Res)) + 
        geom_bar(stat='identity', 
                 position='dodge') +
  geom_hline(yintercept = 0) + 
  facet_wrap(. ~ variable, ncol = 1) + 
  labs(x= 'Año', y = 'Residuales (escala log)') +
  theme_bw(base_size=9)

r2   <- ggplot(DataRes, aes(Pred,Res)) + 
  geom_point(size = 1.5) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(. ~ variable, ncol = 1) +
  labs(x= 'Predicho (log)', y = 'Residuales') + 
  theme_bw(base_size=8)

r3   <- ggplot(DataRes, aes(Res)) + 
  geom_histogram(fill='white', position  = 'dodge') +
  facet_wrap(. ~ variable, ncol = 1) + 
  labs(x= 'Residuales', y ='Histograma de Residuos (Frecuencia)') +
  theme_bw(base_size=9)

r4   <- ggplot(DataRes, aes(sample = Res)) + 
  stat_qq() + 
  stat_qq_line() + 
  facet_wrap(. ~ variable, ncol = 1) +
  labs(x= 'Sample Quantiles', y ='Theoretical') + 
  theme_bw(base_size=10)

r1.1  <-ggplot(DataRes, aes(yrs,Res)) + 
  geom_smooth()+
  geom_bar(stat='identity', position='dodge') +
  geom_hline(yintercept = 0) + 
  facet_wrap(. ~ variable, ncol = 1) + 
  labs(x= 'Año', y = 'Residuales (escala log)') +
  theme_bw(base_size=9)

r2.2  <-ggplot(DataRes, aes(Pred,Res)) + 
  geom_smooth()+
  geom_point(size = 1.5) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(. ~ variable, ncol = 1) +
  labs(x= 'Predicho (log)', y = 'Residuales') + 
  theme_bw(base_size=8)

r3.3 <- ggplot(DataRes, aes(Res)) + 
  geom_histogram(fill='white', position  = 'dodge') +
  facet_wrap(. ~ variable, ncol = 1) + 
  labs(x= 'Residuales', y ='Histograma de Residuos (Frecuencia)') +
  theme_bw(base_size=9)

r4.4 <- ggplot(DataRes, aes(sample = Res)) + 
  stat_qq() + 
  stat_qq_line() + 
  facet_wrap(. ~ variable, ncol = 1) +
  labs(x= 'Sample Quantiles', y ='Theoretical') + 
  theme_bw(base_size=9)

r1.1 + r2.2 + r4.4

}


# 3. Ajustes Composición de edad ----

# Flota ----
fig3 <-function(matf){
  figf <- ggplot(matf %>% filter(type=='observado')) + 
    geom_bar(aes(x = variable, y = value), stat="identity", fill='gray66', color = 'gray28') + 
    facet_wrap(~yrs, dir = 'v', as.table = TRUE) + 
    labs(x = 'Edad', y = 'Proporción') +
    geom_line(data = matf %>% filter(type=='predicho'),
              aes(x = as.numeric(variable), y = value,colour=Asesoria,linetype =Asesoria)) +
    scale_colour_manual(values=c('black','red'),name="Asesoría") +
    scale_linetype_manual(values=c("solid",'dashed'),name="Asesoría")+
    theme(panel.background = element_rect(fill ="gray99")) + 
    theme(panel.grid=element_line(color=NA)) +
    ggtitle("FLOTA") + theme(plot.title = element_text(size = 12))
  figf
  
}

# crucero de verano ----
fig4 <-function(matr){
  figr <- ggplot(filter(matr, type=='observado')) + 
  geom_bar(aes(x = variable, y = value), stat="identity", fill='gray66', color = 'gray28') + 
  facet_wrap(~yrs, dir = 'v', as.table = TRUE) + 
  labs(x = 'Edad', y = 'Proporción') +
  geom_line(data = matr %>% filter(type != 'observado'), aes(x = as.numeric(variable), y = value, colour=Asesoria,linetype =Asesoria)) +
  scale_colour_manual(values=c('black','red'),name="Asesoría") +
  scale_linetype_manual(values=c("solid",'dashed'),name="Asesoría")+
  theme(panel.background = element_rect(fill ="gray99")) + theme(panel.grid=element_line(color=NA)) +
  ggtitle("CRUCEROS DE VERANO") + theme(plot.title = element_text(size = 12))
figr
}

# crucero de otoño ----
fig5 <-function(matp){
  figp <- ggplot(filter(matp, type=='observado')) + 
    geom_bar(aes(x = variable, y = value), stat="identity", fill='gray66', color = 'gray28') + 
    facet_wrap(~yrs, dir = 'v', as.table = TRUE) + labs(x = 'Edad', y = 'Proporción') +
    geom_line(data = matp %>% filter(type != 'observado'), aes(x = as.numeric(variable), y = value, colour=Asesoria,linetype =Asesoria)) +
    scale_colour_manual(values=c('black','red'),name="Asesoría") +
    scale_linetype_manual(values=c("solid",'dashed'),name="Asesoría")+
    theme(panel.background = element_rect(fill ="gray99")) + theme(panel.grid=element_line(color=NA)) +
    ggtitle("CRUCEROS DE OTOÑO") + theme(plot.title = element_text(size = 12))
  figp
}


# 4. Residuos composición de edades ----
fig6 <-function(rep){
# Flota----
ppredF <-rep$pf_pred 
anos   <-rep$years
obsF   <-rep$pf_obs
preF   <-rep$pf_pred 

resF <-obsF-preF
rng  <-range(resF,na.rm=T)
dd   <-dim(resF)
est  <-matrix(NA,nrow=dd[1],ncol=dd[2])

for(j in 1:dd[1]){for(k in 1:dd[2]){val<-resF[j,k]
if(val>0){est[j,k]<-val/rng[2]}
else{est[j,k]<-val/rng[1]*-1}}}

par(mfrow=c(1,3),mar=c(5.4,6.7,2,1),cex.axis=1,cex.lab=1.1)
fig1<-image(age,anos,t(est),col=0,yaxt="n",xlab="",ylab="")
ee  <-dim(est)
for(n in 1:ee[1]){for(m in 1:ee[2]){vol<-est[n,m]
if(is.na(vol)==FALSE){
  if(vol>0){points(age[m],anos[n],pch=19,cex=2.82*sqrt(vol),col=1)}
  if(vol<0){points(age[m],anos[n],pch=1,cex=2.82*sqrt(vol*-1),col=1)}
}}}

mtext("Flota",side=3,cex=1.2)
mtext("Edades",side=1,line=3.2,cex=1.1);posi<-seq(1,57,by=4)
axis(2,at=anos,labels=anos,las=2)
mtext("Años",side=2,line=4.7,cex=1.1)
mtext("a)",side=3,line=0.25,adj=-0.15,cex=1.5)
box()


#Crucero de verano----
ppredR<-rep$ppred_RECLAS  
anos  <-rep$years[4:length(rep$years)]
obsR  <-rep$pobs_RECLAS[4:length(rep$years),]
preR  <-rep$ppred_RECLAS[4:length(rep$years),]
resR  <-obsR-preR

rng <-range(resR,na.rm=T)
dd  <-dim(resR)
est <-matrix(NA,nrow=dd[1],ncol=dd[2])

for(j in 1:dd[1]){for(k in 1:dd[2]){val<-resR[j,k]
if(val>0){est[j,k]<-val/rng[2]}
else{est[j,k]<-val/rng[1]*-1}}}

par(mar=c(5.4,6.7,2,1),cex.axis=1,cex.lab=1.1)
image(age,anos,t(est),col=0,yaxt="n",xlab="",ylab="")
ee  <-dim(est)
for(n in 1:ee[1]){for(m in 1:ee[2]){vol<-est[n,m]
if(is.na(vol)==FALSE){
  if(vol>0){points(age[m],anos[n],pch=19,cex=2.82*sqrt(vol),col=1)}
  if(vol<0){points(age[m],anos[n],pch=1,cex=2.82*sqrt(vol*-1),col=1)}
}}}
mtext("Cruceros de verano",side=3,cex=1.2)
mtext("Edades",side=1,line=3.2,cex=1.1);posi<-seq(1,57,by=4)
axis(2,at=anos,labels=anos,las=2)
mtext("Años",side=2,line=4.7,cex=1.1)
mtext("b)",side=3,line=0.25,adj=-0.15,cex=1.5)
box()

#Crucero de otoño----
ppredP <-rep$ppred_PELACES 
anos   <-rep$years[11:length(rep$years)]
obsP   <-rep$pobs_PELACES[11:length(rep$years),]
preP   <-rep$ppred_PELACES[11:length(rep$years),]  
resP   <-obsP-preP

rng <-range(resP,na.rm=T)
dd  <-dim(resP)
est <-matrix(NA,nrow=dd[1],ncol=dd[2])

for(j in 1:dd[1]){for(k in 1:dd[2]){val<-resP[j,k]
if(val>0){est[j,k]<-val/rng[2]}
else{est[j,k]<-val/rng[1]*-1}}}

par(mar=c(5.4,6.7,2,1),cex.axis=1,cex.lab=1.1)
image(age,anos,t(est),col=0,yaxt="n",xlab="",ylab="")
ee  <-dim(est)
for(n in 1:ee[1]){for(m in 1:ee[2]){vol<-est[n,m]
if(is.na(vol)==FALSE){
  if(vol>0){points(age[m],anos[n],pch=19,cex=2.82*sqrt(vol),col=1)}
  if(vol<0){points(age[m],anos[n],pch=1,cex=2.82*sqrt(vol*-1),col=1)}
}}}
mtext("Cruceros de otoño",side=3,cex=1.2)
mtext("Edades",side=1,line=3.2,cex=1.1);posi<-seq(1,57,by=4)
axis(2,at=anos,labels=anos,las=2)
mtext("Años",side=2,line=4.7,cex=1.1)
mtext("c)",side=3,line=0.25,adj=-0.15,cex=1.5)
box()
}

# 5. Comparación con asesorías previas ----

fig7 <-function(VarPob){
#Retrospectivo tradicional
Rt <- ggplot(Rtcomp) + 
  geom_ribbon(data=VarPob,aes(ymin=lowerRt, ymax=upperRt, x=x, fill = "IC"), alpha = 0.2)+
  geom_line(aes(y=Rt_sept20,x=x, colour = year_retros[nretros]), size=0.5)+
  geom_line(aes(y=Rt_mar21, x=x, colour = year_retros[nretros-1]), size=0.5)+
  geom_line(aes(y=Rt_jul21, x=x, colour = year_retros[nretros-2]), size=0.5)+
  geom_line(aes(y=Rt_sept21,x=x, colour = year_retros[nretros-3]), size=0.5)+
  geom_line(aes(y=Rt_mar22, x=x, colour = year_retros[nretros-4]), size=0.5)+
  geom_line(aes(y=Rt_jul22, x=x, colour = year_retros[nretros-5]), size=0.5)+
  geom_line(aes(y=Rt_sept22,x=x, colour = year_retros[nretros-6]), size=0.5)+
  labs(x = '', y = 'Reclutamientos ',colour='Asesorías')  +
  scale_x_continuous(breaks = seq(from = 1990, to = 2022, by = 5)) +
  scale_colour_manual("",values=c('grey','purple','orange',"green","blue","red","black"))+
  scale_fill_manual("",values=c("grey30"))+
  theme_bw(base_size=11) +
  ggtitle('')+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")

BD <- ggplot(SSBtcomp) + 
  geom_ribbon(data=VarPob,aes(ymin=lowerBD, ymax=upperBD, x=x, fill = "IC"), alpha = 0.2)+
  geom_line(aes(y=SSBt_sept20,x=x, colour = year_retros[nretros]), size=0.5)+
  geom_line(aes(y=SSBt_mar21, x=x, colour = year_retros[nretros-1]), size=0.5)+
  geom_line(aes(y=SSBt_jul21, x=x, colour = year_retros[nretros-2]), size=0.5)+
  geom_line(aes(y=SSBt_sept21,x=x, colour = year_retros[nretros-3]), size=0.5)+
  geom_line(aes(y=SSBt_mar22, x=x, colour = year_retros[nretros-4]), size=0.5)+
  geom_line(aes(y=SSBt_jul22, x=x, colour = year_retros[nretros-5]), size=0.5)+
  geom_line(aes(y=SSBt_sept22,x=x, colour = year_retros[nretros-6]), size=0.5)+
  labs(x = '', y = 'Biomasa desovante (t)',colour='Asesorías')  +
  scale_x_continuous(breaks = seq(from = 1990, to = 2022, by = 4)) +
  scale_colour_manual("",values=c('grey','purple','orange',"green","blue","red","black"))+
  scale_fill_manual("",values=c("grey30"))+
  theme_bw(base_size=11) +
  ggtitle('')+
  theme(plot.title = element_text(hjust = 0.5))

Ft <- ggplot(Ftcomp) + 
  geom_ribbon(data=VarPob,aes(ymin=lowerFt, ymax=upperFt, x=x, fill = "IC"), alpha = 0.2)+
  geom_line(aes(y=Ft_sept20,x=x, colour = year_retros[nretros]), size=0.5)+
  geom_line(aes(y=Ft_mar21, x=x, colour = year_retros[nretros-1]), size=0.5)+
  geom_line(aes(y=Ft_jul21, x=x, colour = year_retros[nretros-2]), size=0.5)+
  geom_line(aes(y=Ft_sept21,x=x, colour = year_retros[nretros-3]), size=0.5)+
  geom_line(aes(y=Ft_mar22, x=x, colour = year_retros[nretros-4]), size=0.5)+
  geom_line(aes(y=Ft_jul22, x=x, colour = year_retros[nretros-5]), size=0.5)+
  geom_line(aes(y=Ft_sept22,x=x, colour = year_retros[nretros-6]), size=0.5)+
  labs(x = '', y = 'Mortalidad por pesca (1/año)',colour='Asesorías')  +
  scale_x_continuous(breaks = seq(from = 1990, to = 2022, by = 4)) +
  scale_colour_manual("",values=c('grey','purple','orange',"green","blue","red","black"))+
  scale_fill_manual("",values=c("grey30"))+
  theme_bw(base_size=11) +
  ggtitle('')+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")

Rt/BD/Ft 

}

# 6. Análisis retrospectivo relativo ----

fig8 <-function(year_retros,nretros){
  
}

# 7. Perfil de verosimilitud ----

fig9 <-function(asesoria){

  fig<-ggplot() +
    geom_line(aes(x = TLk2$Ro, y = TLk2$Bio_Reclas, colour=names[2])) +
    geom_line(aes(x = TLk2$Ro, y = TLk2$Bio_Pelaces, colour=names[3])) +
    geom_line(aes(x = TLk2$Ro, y = TLk2$Desembarques,colour=names[4])) +
    geom_line(aes(x = TLk2$Ro, y = TLk2$Bio_Mph,colour=names[5])) +
    geom_line(aes(x = TLk2$Ro, y = TLk2$C.Edad_Flota,colour=names[6])) +
    geom_line(aes(x = TLk2$Ro, y = TLk2$C.Edad_Recl,colour=names[7])) +
    geom_line(aes(x = TLk2$Ro, y = TLk2$C.Edad_Pel,colour=names[8])) +
    geom_line(aes(x = TLk2$Ro, y = TLk2$Total,colour=names[17])) +
    geom_hline(yintercept = 2,colour='black',lty=2) +
    scale_colour_manual("",values=c('cyan','magenta','gray',"orange","green","blue","red","black"))+
    coord_cartesian(xlim = c(14000, 50000), ylim = c(0, 3))+
    xlab("Ro") + 
    ylab("L-min(L)") + 
    ggtitle(asesoria)+
    theme_bw(base_size=10) +
    theme(plot.title = element_text(hjust = 0.5),legend.position="left")
  
  #fig<-ggplot(data = datares, aes(x = reorder(names_res,residuos), y = residuos)) +
  #      ylim(-2, 3)+
  #      geom_bar(stat = "identity",fill="azure3") +
  #      coord_flip() + # Barras horizontales
  #      xlab("") + 
  #      ylab("Diferencia porcentual de Ro") + 
  #      theme_bw(base_size=10) +
  #     ggtitle("Anchoveta Centro-sur")+
  #      theme(plot.title = element_text(hjust = 0.5))
  
  fig
}

# 8. Variables poblacionales ----

fig10<-function(meanRt,meanBT,meanBD,medianFt){
  
  Rt <- ggplot() + 
    geom_line(data=VarPobMar,aes(y=Rt, x=x, colour = "Hito 2",linetype ="Hito 2"), size=0.5)+
    geom_line(data=VarPobSep,aes(y=Rt, x=x, colour = "Hito 1",linetype ="Hito 1"), size=0.5)+
    geom_ribbon(data=VarPobMar,aes(ymin=lowerRt, ymax=upperRt, x=x, fill = "IC"), alpha = 0.2)+
    geom_ribbon(data=VarPobSep,aes(ymin=lowerRt, ymax=upperRt, x=x, fill = "IC"), alpha = 0.2)+
    geom_hline(yintercept = meanRt,colour='black',lty=2) +
    annotate("text", x=2012, y=meanRt+5000,label=expression("R"[promedio])) +
    labs(x = '', y = 'Reclutamientos',colour='Asesorías')  +
    scale_x_continuous(breaks = seq(from = 1960, to = 2022, by = 4)) +
    scale_colour_manual("",values=c('Hito 1'='black','Hito 2'='red'))+
    scale_linetype_manual('',values=c('Hito 1'="solid",'Hito 2'='dashed'))+
    scale_fill_manual("",values=c("grey50","grey60"))+
    theme_bw(base_size=11) +
    ggtitle('')+
    theme(plot.title = element_text(hjust = 0.5),legend.position="top")
  
  BT <- ggplot() + 
    geom_line(data=VarPobMar,aes(y=BT, x=x, colour = "Hito 2",linetype ="Hito 2"), size=0.5)+
    geom_line(data=VarPobSep,aes(y=BT, x=x, colour = "Hito 1",linetype ="Hito 1"), size=0.5)+
    geom_ribbon(data=VarPobMar,aes(ymin=lowerBT, ymax=upperBT, x=x, fill = "IC"), alpha = 0.2)+
    geom_ribbon(data=VarPobSep,aes(ymin=lowerBT, ymax=upperBT, x=x, fill = "IC"), alpha = 0.2)+
    geom_hline(yintercept = meanBT,colour='black',lty=2) +
    annotate("text", x=2012, y=meanBT+25000,label=expression("BT"[promedio])) +
    labs(x = '', y = 'Biomasa total (t)',colour='Asesorías')  +
    scale_x_continuous(breaks = seq(from = 1960, to = 2022, by = 4)) +
    scale_colour_manual("",values=c('Hito 1'='black','Hito 2'='red'))+
    scale_linetype_manual(values=c('Hito 1'="solid",'Hito 2'='dashed'))+
    scale_fill_manual("",values=c("grey50","grey60"))+
    theme_bw(base_size=11) +
    ggtitle('')+
    theme(plot.title = element_text(hjust = 0.5),legend.position="none")
  
  BD <- ggplot() + 
    geom_line(data=VarPobMar,aes(y=BD, x=x, colour = "Hito 2",linetype ="Hito 2"), size=0.5)+
    geom_line(data=VarPobSep,aes(y=BD, x=x, colour = "Hito 1",linetype ="Hito 1"), size=0.5)+
    geom_ribbon(data=VarPobMar,aes(ymin=lowerBD, ymax=upperBD, x=x, fill = "IC"), alpha = 0.2)+
    geom_ribbon(data=VarPobSep,aes(ymin=lowerBD, ymax=upperBD, x=x, fill = "IC"), alpha = 0.2)+
    geom_hline(yintercept = meanBD,colour='black',lty=2) +
    annotate("text", x=2012, y=meanBD+25000,label=expression("BD"[promedio])) +
    labs(x = '', y = 'Biomasa desovante (t)',colour='Asesorías')  +
    scale_x_continuous(breaks = seq(from = 1960, to = 2022, by = 4)) +
    scale_colour_manual("",values=c('Hito 1'='black','Hito 2'='red'))+
    scale_linetype_manual(values=c('Hito 1'="solid",'Hito 2'='dashed'))+
    scale_fill_manual("",values=c("grey50","grey60"))+
    theme_bw(base_size=11) +
    ggtitle('')+
    theme(plot.title = element_text(hjust = 0.5),legend.position="none")
  
  Ft <- ggplot() +
    geom_line(data=VarPobMar,aes(y=Ft, x=x, colour = "Hito 2",linetype ="Hito 2"), size=0.5)+
    geom_line(data=VarPobSep,aes(y=Ft, x=x, colour = "Hito 1",linetype ="Hito 1"), size=0.5)+
    geom_ribbon(data=VarPobMar,aes(ymin=lowerFt, ymax=upperFt, x=x, fill = "IC"), alpha = 0.2)+
    geom_ribbon(data=VarPobSep,aes(ymin=lowerFt, ymax=upperFt, x=x, fill = "IC"), alpha = 0.2)+
    geom_hline(yintercept = medianFt,colour='black',lty=2) +
    annotate("text", x=2011, y=medianFt+0.15,label=expression("F"[mediana])) +
    labs(x = '', y = 'Mortalidad por pesca (F)',colour='Asesorías')  +
    scale_x_continuous(breaks = seq(from = 1960, to = 2022, by = 4)) +
    scale_colour_manual("",values=c('Hito 1'='black','Hito 2'='red'))+
    scale_linetype_manual(values=c('Hito 1'="solid",'Hito 2'='dashed'))+
    scale_fill_manual("",values=c("grey50","grey60"))+
    theme_bw(base_size=11) +
    ggtitle('')+
    theme(plot.title = element_text(hjust = 0.5),legend.position="none")
  
  Rt/BT | (BD/Ft)
}

# 9. Selectividades ----

fig11<-function(sel_Flota,sel_CruV,sel_CruO){
  
  age     <- seq(0,4,1)                                       
  nage    <- length(age)   
  
  g1 <- ggplot () +
    lims(y=c(0,1))+
    #lineas
    geom_line(aes(x=age,y=sel_Flota))+
    geom_line(aes(x=age,y=sel_CruV))+
    geom_line(aes(x=age,y=sel_CruO),linetype="dashed")+
    #puntos
    geom_point(aes(x=age,y=sel_Flota,shape="FLota"),size=2.5) +
    geom_point(aes(x=age,y=sel_CruV,shape="Cruceros de Verano"),size=2.5) +
    geom_point(aes(x=age,y=sel_CruO,shape="Cruceros de Otoño"),size=2.5) +
    #parámetros
    labs(x = 'Edad (años)', y = 'Patrón de explotación',shape="Selectividades") +
    ggtitle("")+
    theme_bw(base_size=11) + 
    theme(plot.title = element_text(hjust = 0.5),legend.justification=c(1.1,0), legend.position=c(1,0.1))
  g1
  
}

# Puntos biológicos de referencia en sobre  series BD y F

fig12<-function(lbrms,lblim,lbo,lbmed,lfrms,lfmedian){
  
  BD <- ggplot() + 
    geom_line(data=VarPobSep,aes(y=BD, x=x, colour = "Hito 1",linetype ="Hito 1"),  size=0.5)+
    geom_line(data=VarPobMar,aes(y=BD, x=x, colour = "Hito 2",linetype ="Hito 2"),  size=0.5)+
    scale_colour_manual("",values=c('Hito 1'='black','Hito 2'='red'))+
    scale_linetype_manual("",values=c('Hito 1'="solid",'Hito 2'='dashed'))+
    geom_ribbon(data=VarPobSep,aes(ymin=lowerBD, ymax=upperBD, x=x, fill = "IC"), alpha = 0.2)+
    geom_ribbon(data=VarPobMar,aes(ymin=lowerBD, ymax=upperBD, x=x, fill = "IC"), alpha = 0.2)+
    geom_hline(yintercept = c(lbrms,lblim,lbo,lbmed),colour=c('green3','red','blue','black'))+
    annotate("text", x=c(rep(2012,3),2005), y=c(lbrms,lblim,lbo,lbmed)+30000,
             label=c(expression("BD"[RMS]),expression("BD"[LIM]),expression("BD"[0]),expression("BD"[promedio]))) +
    labs(x = '', y = 'Biomasa desovante (t)',colour='Asesorías')  +
    scale_x_continuous(breaks = seq(from = 1960, to = 2022, by = 4)) +
    scale_fill_manual("",values=c('gray50','gray60'))+
    theme_bw(base_size=11) +
    ggtitle('')+
    theme(plot.title = element_text(hjust = 0.5),legend.position="top")
  
  Ft <- ggplot() + 
    geom_line(data=VarPobSep,aes(y=Ft, x=x, colour = "Hito 1",linetype ="Hito 1"), size=0.5)+
    geom_line(data=VarPobMar,aes(y=Ft, x=x, colour = "Hito 2",linetype ="Hito 2"), size=0.5)+
    scale_colour_manual("",values=c("Hito 1"='black','Hito 2'='red'))+
    scale_linetype_manual("",values=c('Hito 1'='solid', "Hito 2"='dashed'))+
    geom_ribbon(data=VarPobSep,aes(ymin=lowerFt, ymax=upperFt, x=x, fill = "IC"), alpha = 0.2)+
    geom_ribbon(data=VarPobMar,aes(ymin=lowerFt, ymax=upperFt, x=x, fill = "IC"), alpha = 0.2)+
    geom_hline(yintercept = c(lfrms,lfmedian),colour=c('green3','black')) +
    annotate("text", x=c(2011,2001), y=c(lfrms,lfmedian)+0.15,label=c(expression("F"[RMS]),expression("F"[mh]))) +
    labs(x = '', y = 'Mortalidad por pesca (F)',colour='Asesorías')  +
    scale_x_continuous(breaks = seq(from = 1960, to = 2022, by = 4)) +
    scale_fill_manual("",values=c('gray50','gray60'))+
    theme_bw(base_size=11) +
    ggtitle('')+
    theme(plot.title = element_text(hjust = 0.5),legend.position="none")
  
  BD + Ft
}

# 10. Puntos biológicos de referencia ----

fig13<-function(sel_Flota,madurez,Fspr,BDspr,FRMS){
  
  age<-seq(0,4,1)

  g1 <- ggplot () +
    lims(y=c(0,1))+
    #lineas
    geom_line(aes(x=age,y=sel_Flota))+
    geom_line(aes(x=age,y=madurez),linetype="dashed")+
    #puntos
    geom_point(aes(x=age,y=sel_Flota,shape="Selectividad de la flota"),size=2.5) +
    geom_point(aes(x=age,y=madurez,shape="Madurez sexual"),size=2.5) +
    #parámetros
    labs(x = 'Edad (años)', y = 'Madurez y selectividad',shape="") +
    ggtitle("")+
    theme_bw(base_size=11) + 
    theme(plot.title = element_text(hjust = 0.5),legend.justification=c(1.1,0), legend.position=c(1,0.1))
  
  
  g2 <- ggplot () +
    geom_line(aes(x=Fspr,y=BDspr))+
    geom_hline(yintercept = 0.6,colour=c('gray35'),linetype="dashed") +
    geom_vline(xintercept = FRMS,colour=c('gray35'),linetype="dashed") +
    annotate("text", x=2, y=0.6+0.02,label=c(expression("F"[RMS]))) +
    labs(x = 'Mortalidad por pesca (F)', y = '%BDPR',shape="") +
    ggtitle("")+
    theme_bw(base_size=11) + 
    theme(plot.title = element_text(hjust = 0.5),legend.justification=c(1.1,0), legend.position=c(1,0.1))
  g1 + g2
  
}


# 11. Indicadores de estatus relativos a BDrms y Frms----

fig14<-function(h1,h2,colin,typelin,colfill,xdensb,ydensb,xdensf,ydensf,labeldensb,labeldensf){
  
  BD_BDrms <- ggplot() + 
    geom_line(data=EstatusSep,aes(y=Rpr, x=x, colour = h1,linetype =h1), size=0.5)+
    {if(H2)geom_line(data=EstatusMar,aes(y=Rpr, x=x, colour = h2,linetype =h2), size=0.5)}+
    {if(H3)geom_line(data=EstatusJul,aes(y=Rpr, x=x, colour = h3,linetype =h3), size=0.5)}+
    scale_colour_manual("",values=colin)+
    scale_linetype_manual("",values=typelin)+
    geom_ribbon(data=EstatusSep,aes(ymin=lowerRpr, ymax=upperRpr, x=x, fill = "IC"), alpha = 0.2)+
    {if(H2)geom_ribbon(data=EstatusMar,aes(ymin=lowerRpr, ymax=upperRpr, x=x, fill = "IC"), alpha = 0.2)}+
    {if(H3)geom_ribbon(data=EstatusJul,aes(ymin=lowerRpr, ymax=upperRpr, x=x, fill = "IC"), alpha = 0.2)}+
    geom_hline(yintercept = c(1,0.5),colour=c('green3','red'))+
    annotate("text", x=c(2012,2012), y=c(1,0.5)+0.06,
             label=c(expression("BD"[RMS]),expression("BD"[LIM]))) +
    labs(x = '', y = expression("BD/BD"[RMS]),colour='Asesorías',tag="a)")  +
    scale_x_continuous(breaks = seq(from = 1960, to = 2062, by = 4)) +
    scale_fill_manual("",values=colfill)+
    theme_bw(base_size=11) +
    ggtitle('')+
    theme(plot.title = element_text(hjust = 0.5),legend.position="top")
  
  F_Frms <- ggplot() + 
    geom_line(data=EstatusSep,aes(y=Frpr, x=x, colour = h1,linetype =h1), size=0.5)+
    {if(H2)geom_line(data=EstatusMar,aes(y=Frpr, x=x, colour = h2,linetype =h2), size=0.5)}+
    {if(H3)geom_line(data=EstatusJul,aes(y=Frpr, x=x, colour = h3,linetype =h3), size=0.5)}+
    geom_ribbon(data=EstatusSep,aes(ymin=lowerFrpr, ymax=upperFrpr, x=x, fill = "IC"), alpha = 0.2)+
    {if(H2)geom_ribbon(data=EstatusMar,aes(ymin=lowerFrpr, ymax=upperFrpr, x=x, fill = "IC"), alpha = 0.2)}+
    {if(H3)geom_ribbon(data=EstatusJul,aes(ymin=lowerFrpr, ymax=upperFrpr, x=x, fill = "IC"), alpha = 0.2)}+
    geom_hline(yintercept = 1,colour=c('green3')) +
    annotate("text", x=2012, y=1+0.25,label=c(expression("F"[RMS]))) +
    labs(x = '', y = expression("F/F"[RMS]),colour='Asesorías',tag="c)")  +
    scale_x_continuous(breaks = seq(from = 1960, to = 2062, by = 4)) +
    scale_colour_manual("",values=colin)+
    scale_linetype_manual("",values=typelin)+
    scale_fill_manual("",values=colfill)+
    theme_bw(base_size=11) +
    ggtitle('')+
    theme(plot.title = element_text(hjust = 0.5),legend.position="none")
  
  fig_desnb<- ggplot() + lims(y=c(0,3)) +
    {if(H3)geom_polygon(data=densb_bj,aes(x=x, y=y, group=t,alpha=0.9),fill=colfill[3])}+                
    {if(H2)geom_polygon(data=densb_bm,aes(x=x, y=y, group=t,alpha=0.9),fill=colfill[2])}+
    geom_polygon(data=densb_bs,aes(x=x, y=y, group=t,alpha=0.9),fill=colfill[1])+
    {if(H3)geom_line(aes(xbj,ybj), size=0.3,color=colin[3],linetype =typelin[3])}+
    {if(H2)geom_line(aes(xbm,ybm), size=0.3,color=colin[2],linetype =typelin[2])}+
    geom_line(aes(xbs,ybs), size=0.3,color=colin[1],linetype =typelin[1])+
    annotate("text", xdensb, ydensb,colour = colin, size = 2.5,
             label=labeldensb) +
    labs(x = expression("BD"[last]*"/BD"[RMS]), y = 'Densidad de probabilidad',tag="b)")  +
    theme_bw(base_size=11) + 
    theme(plot.title = element_text(hjust = 0.5),legend.position="none")
  
  fig_desnf<- ggplot() + lims(y=c(0,3.5))+
    {if(H3)geom_polygon(data=densb_fj,aes(x=x, y=y, group=t,alpha=0.9),fill=colfill[3])}+
    {if(H2)geom_polygon(data=densb_fm,aes(x=x, y=y, group=t,alpha=0.9),fill=colfill[2])}+
    geom_polygon(data=densb_fs,aes(x=x, y=y, group=t,alpha=0.9),fill=colfill[1])+
    {if(H3)geom_line(aes(xfj,yfj), size=0.3,color=colin[3],linetype =typelin[3])}+
    {if(H2)geom_line(aes(xfm,yfm), size=0.3,color=colin[2],linetype =typelin[2])}+
    geom_line(aes(xfs,yfs), size=0.3,color=colin[1],linetype =typelin[1])+
    annotate("text", xdensf, ydensf,colour = colin, size = 2.5,
             label=labeldensf) +
    labs(x = expression("F"[last]*"/F"[RMS]), y = 'Densidad de probabilidad',tag="d)")  +
    theme_bw(base_size=11) + 
    theme(plot.title = element_text(hjust = 0.5),legend.position="none")
  
  {(BD_BDrms / F_Frms)   | (fig_desnb/fig_desnf)} +  plot_layout(ncol=2,widths=c(2,1))
  
}

# 12. Estatus ----
source(paste(dir.fun,"Fn_DiagramaFase2.R",sep=""))

# diagrama fase Hito 1 ----
fig15<-function(name1,years1,nyears1,yearpoints){
  
  DiagramaFase2(name1,
                years1[1:nyears1-1],
                SpB1[1:nyears1-1],
                SpBSE1[1:nyears1-1],
                ln_Fyr1[1:nyears1-1],
                ln_FSE1[1:nyears1-1],
                SpB1[nyears1],
                SpBSE1[nyears1],
                ln_Fyr1[nyears1],
                ln_FSE1[nyears1],
                FRMS1,
                BRMS1,
                BLIM1,
                FLIM1,
                color=F,
                dir.1,
                etiqueta=F,
                preliminar=F,
                completo=T)
  
  text(c(SpB1[1]/BRMS1,
         SpB1[nyears1]/BRMS1,
         SpB1[nyears1-1]/BRMS1),
       c(exp(ln_Fyr1[1])/FRMS1-0.05,
         exp(ln_Fyr1[nyears1])/FRMS1-0.05,
         exp(ln_Fyr1[nyears1-1])/FRMS1+0.05),
       yearpoints,cex=0.9)
  
}

# diagrama fase Hito 2 ----
fig16<-function(name2,years2,nyears2,yearpoints){
  
  DiagramaFase2(name2,
                years2[1:nyears2-1],
                SpB2[1:nyears2-1],
                SpBSE2[1:nyears2-1],
                ln_Fyr2[1:nyears2-1],
                ln_FSE2[1:nyears2-1],
                SpB2[nyears2],
                SpBSE2[nyears2],
                ln_Fyr2[nyears2],
                ln_FSE2[nyears2],
                FRMS2,
                BRMS2,
                BLIM2,
                FLIM2,
                color=F,
                dir.1,
                etiqueta=F,
                preliminar=T,
                completo=F)
  
  text(c(SpB2[1]/BRMS2,
         SpB2[nyears2]/BRMS2,
         SpB2[nyears2-1]/BRMS2),
       c(exp(ln_Fyr2[1])/FRMS2-0.05,
         exp(ln_Fyr2[nyears2])/FRMS2-0.05,
         exp(ln_Fyr2[nyears2-1])/FRMS2+0.05), 
       yearpoints,cex=1.2)
  
  
}


# diagrama fase Hito 3 ----
fig17<-function(name3,years3,nyears3,yearpoints){
  
  DiagramaFase2(name3,
                years3[1:nyears3-1],
                SpB3[1:nyears3-1],
                SpBSE3[1:nyears3-1],
                ln_Fyr3[1:nyears3-1],
                ln_FSE3[1:nyears3-1],
                SpB3[nyears3],
                SpBSE3[nyears3],
                ln_Fyr3[nyears3],
                ln_FSE3[nyears3],
                FRMS3,
                BRMS3,
                BLIM3,
                FLIM3,
                color=F,
                dir.1,
                etiqueta=F,
                preliminar=F,
                completo=F)
  
  text(c(SpB3[1]/BRMS3,
         SpB3[nyears3]/BRMS3,
         SpB3[nyears3-1]/BRMS3),
       c(exp(ln_Fyr3[1])/FRMS3-0.05,
         exp(ln_Fyr3[nyears3])/FRMS3-0.05,
         exp(ln_Fyr3[nyears3-1])/FRMS3+0.05), 
       yearpoints,cex=1.2)
  
}

# 13. Proyección ><> ><> ><> ><>  ----

fig18<-function(DataProy,escRecl,col_escRecl){
  
 ggplot(data=DataProy,aes(y=value, x=years, colour = variable)) + 
   annotate("rect", 
            xmin = yearProy[1]-1, xmax = yearProy[2]+1,
            ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "pink") + 
   geom_vline(xintercept = yearProy[1]-1,linetype ="dashed") +
   geom_line()+
   geom_point()+
   facet_wrap(vars(fct_recode(indicador,
                              "Reclutamientos"="Rt",
                              "Biomasa desovante (miles de t)"="BD",
                              "BD/BDrms" ="BD_BDrms",
                              "Capturas (miles de t)"="Ct")%>% 
                     # Change factor level order
                     fct_relevel("Reclutamientos")),
              dir = 'h', as.table = TRUE,scales='free_y',ncol=2) +  
   scale_x_continuous(breaks = seq(from = 1960, to = 2062, by = 4)) +
   scale_colour_manual("",values=col_escRecl,
                       labels = escRecl,
                       name='Escenarios de\n Reclutamientos')+
   labs(x = '', y = '') +
   theme_bw(base_size=11) + 
   theme(plot.title = element_text(hjust = 0.5),legend.position="right")
 
}
