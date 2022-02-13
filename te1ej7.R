rm(list = ls(all.names = TRUE))
gc()
library(latex2exp)
setwd('C:/Users/aldai/OneDrive/Documentos/R')


Datos= read.csv('Stereo.csv')
str(Datos)


###Alternativa interactiva.
require(ggplot2)
require(ggiraph)
require(ggiraphExtra)
ggPoints(aes(x=COST,y=SOUND),smooth=TRUE, data=Datos,interactive=F, method = "lm")

#EstimaciÃ³n usando la funciÃ³n lm
fit=lm(SOUND~COST, data=Datos)
#Principales resultados del modelo
summary(fit)

#---------------------------HOMOCEDASTICIDAD ------------------------------------------
### Pruebas de hipótesis
### H0: varianza no depende de forma lineal en x vs  Ha: varianza depende de forma lineal en x
### Se busca no rechazar, es decir,
### que sea plausible asumir que la varianza no depende de forma lineal de x
### i.e. p-value mayor a significancia.
#Usa residuales studentilizados
library(lmtest)
lmtest::bptest(fit)

#Usa residuales estandarizados
library(car)
car::ncvTest(fit)
car::ncvTest(fit,~COST)

#%%%%%%%%%%%%%---- P-VALUE 0.08808 NO SE RECHAZA HO----%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#NO SE  ENCUENTRA EVIDENCIA EN CONTRA DE LA HOMOCEDASTICIDAD

#R gráfica propia para verificar homocedasticidad
par(mfrow=c(1,2)) 
par(mar=c(4, 5, 1, 1))
plot(fit, 3)
plot(Datos$yhat, sqrt(abs(Datos$errorSt)), xlab = TeX("$\\widehat{y}$"), ylab=TeX("$\\sqrt{|e_{s}|}$") )

library(broom)
Datosfit=augment(fit)
head(Datosfit)
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,2)) 
plot(Datosfit$.fitted, Datosfit$.std.resid, xlab = TeX("$\\widehat{y}$"), ylab=TeX("$e_{s}$")   )



#---------------------------------LINEALIDAD-----------------------------------

#R tiene una gráfica propia para verificar linealidad
par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fit, 1)



# Herramienta para argumentar la no necesidad de transformación de X
# a partir de la familia de transformaciones Box-Tidwell
boxTidwell(SOUND~COST, data=Datos)

#Test Ho: lambda=1 vs Ha:lambda != 1.
#Si no se rechaza, entonces es plausible considerar el modelo con X directamente
#Si se rechaza, entonces conviene usar el estimador de lambda para transformar X
#u otra transformación

#%%%%%%%%%%%%%%%%%%------ P-VALUE 2.2e-16,  SÍ SE RECHAZA H_o -----%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%% SE ENCONTRÓ EVIDENCIA EN CONTRA DE LA LINEALIDAD, ES NECESARIO HACER ALGUNA TRANSFORMACIÓN %%%%%%%%%%%%%



#--------------------------------NORMALIDAD-----------------------------------------

#n no es tan grande, veamos qué observamos si aplicamos lo de n grande
#gráfica QQplot directa de R
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,1))
plot(fit, 2)
#Se usan los residuales estandarizados y los cuantiles de una normal


#Pruebas de normalidad

shapiro.test(Datosfit$.std.resid)
#%%%%%%%%%%%%%%%%%%%----- P VALUE 0.06745, NO SE RECHAZA H_o----%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%% NO SE ENCUENTRA EVIDENCIA EN CONTRA DE LA NORMALIDAD %%%%%%%%%%%%

library(nortest)
nortest::lillie.test(Datosfit$.std.resid)
#%%%%%%%%%%%%%%%%%%%%%---- P VALUE 0.331, NO SE RECHAZA H_o----%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%% NO SE ENCUENTRA EVIDENCIA EN CONTRA DE LA NORMALIDAD %%%%%%%%%%%


#-------------------- COVARIANZA CERO - INDEPENDENCIA ------------------------------- 

#gráfica sobre el índice de los datos
par(mar=c(4, 5, 3, 1))
par(mfrow=c(1,3))
plot(1:length(Datosfit$.std.resid), Datosfit$.std.resid, xlab = TeX("$i$"), ylab=TeX("$e_s$")   )

#autocorrelograma de los errores
acf(Datosfit$.std.resid)


#Prueba de rachas
library(lawstat)
lawstat::runs.test(Datosfit$.std.resid, plot.it = TRUE)
#%%%%%%%%%%%%%%%%%%%%%---- P VALUE 6.512e-07, SE RECHAZA H_o----%%%%%%%%%%%%%%%%%%%
library(randtests)
randtests::runs.test(Datosfit$.std.resid)
#%%%%%%%%%%%%%%%%%%%%%---- P VALUE 1.054e-06, SE RECHAZA H_o----%%%%%%%%%%%%%%%%%%%
#HAY EVEDENCIA EN CONTRA DE LA CORRELACION DE LOS DATO, PERO ES DEBIDA A LA NO LINEALIDAD

#---------------------------------------- II --------------------------------------
#  b) transformacion BoxTidwell  
Datos$Xprima=Datos$COST^(-1/3)
fit2=lm(SOUND~Xprima, data=Datos)

#Linealidad, gráfica de R

par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fit2, 1)

boxTidwell(SOUND~Xprima, data=Datos)
#Ya no se rechaza H0
ggPoints(aes(x=Xprima,y=SOUND),smooth=TRUE, data=Datos,interactive=F, method = "lm")


#Homocedasticidad
par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fit2, 3)


lmtest::bptest(fit2)
#%%%%%%%%%%%%%---- P-VALUE 0.002339, SE RECHAZA H_0----%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#SE  ENCUENTRA EVIDENCIA EN CONTRA DE LA HOMOCEDASTICIDAD

#Usando residuales estandarizados
car::ncvTest(fit2)
#%%%%%%%%%%%%%---- P-VALUE 0.002029, SE RECHAZA H_0----%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



# a) transformcion Box - Cox

summary(powerTransform(fit2))
#usamos  1.3
Datos$yBC=bcPower(Datos$SOUND, 1.3)

par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,1)) 
plot(Datos$Xprima, Datos$yBC, xlab = TeX("$Cost*$"), ylab=TeX("$(y^{2}-1)/2$") )


fit3=lm(yBC~Xprima, data=Datos)
summary(fit3)

par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fit3, 3)

lmtest::bptest(fit3)
car::ncvTest(fit3)

#%%%%%%%%%%%%%---- P-VALUE 0.019408 NO SE RECHAZA HO----%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#NO SE  ENCUENTRA EVIDENCIA EN CONTRA DE LA HOMOCEDASTICIDAD
#--------------------------------NORMALIDAD-----------------------------------------

#n no es tan grande, veamos qué observamos si aplicamos lo de n grande
#gráfica QQplot directa de R
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,1))
plot(fit3, 2)
#Se usan los residuales estandarizados y los cuantiles de una normal


#Pruebas de normalidad
#R  función para crear errores de forma automatizada
Datosfit2=augment(fit3)
head(Datosfit2)
shapiro.test(Datosfit2$.std.resid)
#%%%%%%%%%%%%%%%%%%%----- P VALUE 0.9921, NO SE RECHAZA H_o----%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%% NO SE ENCUENTRA EVIDENCIA EN CONTRA DE LA NORMALIDAD %%%%%%%%%%%%


nortest::lillie.test(Datosfit2$.std.resid)
#%%%%%%%%%%%%%%%%%%%%%---- P VALUE 0.8406, NO SE RECHAZA H_o----%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%% NO SE ENCUENTRA EVIDENCIA EN CONTRA DE LA NORMALIDAD %%%%%%%%%%%


#-------------------- COVARIANZA CERO - INDEPENDENCIA ------------------------------- 

#gráfica sobre el índice de los datos
par(mar=c(4, 5, 3, 1))
par(mfrow=c(1,3))
plot(1:length(Datosfit2$.std.resid), Datosfit2$.std.resid, xlab = TeX("$i$"), ylab=TeX("$e_s$")   )

#autocorrelograma de los errores
acf(Datosfit2$.std.resid)


#Prueba de rachas
lawstat::runs.test(Datosfit2$.std.resid, plot.it = TRUE)
#%%%%%%%%%%%%%%%%%%%%%---- P VALUE 0.6484, SE RECHAZA H_o----%%%%%%%%%%%%%%%%%%%

randtests::runs.test(Datosfit2$.std.resid)
#%%%%%%%%%%%%%%%%%%%%%---- P VALUE 0.5418, SE RECHAZA H_a----%%%%%%%%%%%%%%%%%%%
# NO HAY EVEDENCIA EN CONTRA DE LA CORRELACION DE LOS DATOs


