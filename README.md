#ESTATUS_Y_CBA_2023_ANCHOVETA_ZCS


Este repositorio contiene los archivos utilizados para el cálculo de la CBA inicial 2023 de anchoveta de las Regiones de Valparaíso a Los Lagos.


## CARPETAS

-  **Datos_julio2022** contiene los archivos excell enviados por seguimiento, descarte, edad y crecimiento y evaluaciones directa. Contiene los desembarques 2021 y 2022, composiciones de tallas y edades de la flota 2021/2022, pesos medios e iniciales, biomasas cruceros de verano y otoño.

-  **codigos_admb** contiene los archivos ADMB (.dat, .tpl) utilizados para las asesorías de marzo 2022, julio 2022 y septiembre 2022 (MAE322, MAE722 y MAE922).

- **funciones** contiene las funciones utilizadas para generar los escenarios de análisis retrospectivos, perfiles de verosimilitud, CBAs, entre otros.


-  **CBA_sep** contienen los escenarios de CBA 2023 calculadas bajo tres escenarios de reclutamientos en la asesoría de  septiembre 2022.

- **Retrospectivo_sept** contiene los escenarios utilizados para el análisis retrospectivo descontando 5 años a la serie, realizado para la asesoría de septiembre 2022.

- **Verosimilitud_sept** contiene los escenarios utilizados para generar los perfiles de verosimilitud fijando un rango de valores de Ro. Realizado para la asesoría de septiembre 2022.


- **rep_AsesoriasPrevias** contiene los reportes de asesorías previas para realizar la comparación con las salidas del modelo actual.


- **Figuras** y **Tablas** contienen las Figuras y Tablas generadas por el código Rmd llamado *FigyTab_PrimerInforme_Anch*.

- **SUBDECON_483-260** contiene PRIMER INFORME, base de datos y FAI listos para ser enviados a revisión.


## ARCHIVOS

### Archivos para referencias

- **apa_1** archivo .csl permite generar las referencias en formato APA 7ma edición

- **Referencias_noviembre2021** archivo BibTeX que guarda las referencias que serán utilizada en tercer informe

### Archivos para portadas

- **PORTADA_INICIAL** archivo en formato LATEX genera la portada en formato PDF que se pega al inicio del documento 

- **PORTADA_FINAL** archivo en formato LATEX genera la portada en formato PDF que se pega al final del documento 

### Archivos Rmarkdown


- **FigyTab_PrimerInforme_Anch** archivo que corre el código actualizado MAE922, corre los escenarios para análisis retrospectivo, verosimilitud y CBA. genera las Figuras y tablas utilizadas en archivo llamado *PRIMER_INFORME_ANCH_SEPT2022*. 

- **PRIMER_INFORME_ANCH_SEPT2022** genera el PRIMER INFORME DE ESTATUS 2021/2022 Y CBA 2023 correspondiente a la asesoría de septiembre 2022.






