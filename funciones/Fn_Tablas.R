
# Tabla índices de abundancia ----
tb1<-function(archivo.Rdata){
  load(paste(dir.Rdata,archivo.Rdata,sep=""))
  
  dataInd<-data.frame(years,reclasobs,pelacesobs,mphobs)
  
  kbl(dataInd, booktabs = T,format = "latex",position="h!",escape = F,align="c",
      format.args=list(big.mark = '.'),
      col.names = linebreak(c("Año\ncalendario ",
                              "Biomasa crucero\nde verano\n(toneladas)",
                              "Biomasa crucero\nde otoño\n(toneladas)",
                              "Biomasa desovante\nMPDH\n(toneladas)"),align="c"),
      caption = "\\label{t1} Estimaciones de biomasas utilizadas en la evaluación de stock de anchoveta provenientes de los cruceros de Verano (RECLAS), Otoño (PELACES) y crucero de huevos (MPDH).") %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)
}

# tabla capturas ----
tb2<-function(bioyear,desembarque,porcDesc_actualizado,CapturaDescartada,CapturaTotal){
  
  dataDes_y_descar<-data.frame("Año"=bioyear,
                               "Desembarques"=round(desembarque,0),
                               "Pdescarte"=porcDesc_actualizado,
                               "Capturadesc"=round(CapturaDescartada,0),
                               "Capturatotal"=round(CapturaTotal,0))
  
  
  kbl(dataDes_y_descar, booktabs = T,format = "latex",position="h!",align="c",
      format.args=list(big.mark = '.'),escape=F,
      col.names = linebreak(c("Año\nbiológico ",
                              "Desembarques\n(toneladas)",
                              "Porcentaje\nDescarte",
                              "Captura\ndescartada\n(toneladas)",
                              "Captura\ntotal\n(toneladas)"),align="c"),   
      caption = "\\label{t2} Desembarques en toneladas, porcentaje de descarte supuesto,
captura descartada (toneladas) y captura total (toneladas) estimadas en año biológico para anchoveta de las regiones de Valparaíso a Los Lagos.") %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)
}

tb3<-function(VarPob_H1,VarPob_H2,yearsb){
  
  VarPobl1<- cbind(yearsb,
                   format(round(c(VarPob_H1[VarPob_H1$indicador=='BD',]$value,NA),0),nsmall=0, big.mark="."),
                   format(round(VarPob_H2[VarPob_H2$indicador=='BD',]$value,0),nsmall=0, big.mark="."),
                   format(round(c(VarPob_H1[VarPob_H1$indicador=='BT',]$value,NA),0),nsmall=0, big.mark="."),
                   format(round(VarPob_H2[VarPob_H2$indicador=='BT',]$value,0),nsmall=0, big.mark="."),
                   format(round(c(VarPob_H1[VarPob_H1$indicador=='Rt',]$value,NA),0),nsmall=0, big.mark="."),
                   format(round(VarPob_H2[VarPob_H2$indicador=='Rt',]$value,0),nsmall=0, big.mark="."),
                   formatC(round(c(VarPob_H1[VarPob_H1$indicador=='Ft',]$value,NA),3),decimal.mark = ",",digits = 3),
                   formatC(round(VarPob_H2[VarPob_H2$indicador=='Ft',]$value,3),decimal.mark = ",",digits = 3))
  #-----------
  # TABLA
  #-----------
  VarPobl1 %>%
  kbl(booktabs = T,format = "latex",position="h!",escape = F,align="c",
      col.names = linebreak(c("",
                              "Hito 1","Hito 2","Hito 1","Hito 2","Hito 1","Hito 2","Hito 1","Hito 2"),align="c"),
  caption = "\\label{Tab17}Estimaciones medias de las biomasa desovante (t) , biomasa total (t), reclutamientos (\\#) y  mortalidad por pesca (año-1)  estimadas en la asesoría de septiembre 2022 de anchoveta de las regiones de Valparaíso a Los Lagos.") %>%
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=8)%>%
    add_header_above(c('Año\nbiológico'=1,
                        "Biomasa\ndesovante"=2,
                        "Biomasa total"=2,
                        "Reclutamientos"=2,
                        "Mortalidad\npor pesca"=2))

}