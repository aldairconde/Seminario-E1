rm(list = ls(all.names = TRUE))
gc()

setwd('C:/Users/aldai/OneDrive/Documentos/R')
datos <- read.csv("Ex5.csv")
str(datos)

datos$Trat=factor(datos$Trat)

library(ggplot2)
ggplot(datos, aes(Edad, Ant, color = Trat)) +
  geom_point()

############ a) AN�LISIS DESCRIPTIVO ############

library(GGally)
ggpairs(datos)

# Los datos nos proporciona una variable
# categ�ricas (Trat) con dos niveles ("Control","Med")
# y una variables continuas (Edad). La correlaci�n de 
# la variable de respuesta con la continua es considerablemente
# alta, lo cual podemos notar en el gr�fico de dispersi�n.


# La distribuci�n de la variable respuesta
# respecto a las categ�ricas, se puede observar que 
# el comportamiento es bastante similar en ambas categor�as.
# Las medianas son distintas y se encuentran
# al rededor de los 17 y el rango tambi�n es diferente


############ II) ajuste de modelo ############
fit <- lm(Ant ~ Edad * Trat, data = datos)
summary(fit)

ggplot(datos, aes(Edad, Ant, color = Trat)) +
  geom_point() +
  geom_abline(intercept = (fit$coefficients[1] + fit$coefficients[3]) ,
              slope = fit$coefficients[2] + fit$coefficients[4],color=5) +
  geom_abline(intercept = (fit$coefficients[1] ) ,
              slope = fit$coefficients[2],color=2)
#Se rechaza $H_o$ en la prueba asociada a la tabla ANOVA, por lo tanto el modelo tiene sentido.
#El modelo con interacci�n es
#$$Ant = \beta_0 + \beta_1 Edad + \beta_3 Trat + \beta_4 Edad*Trat$$

############ III) promedio de anticuerpos ############
# Recordemos que el modelo es
# $$Ant = \beta_0 + \beta_1 Edad + \beta_3 Trat + \beta_4 Edad*Trat$$
#Para el grupo de control
#$Trat=0$, entonces
#E(Ant;Trat=control, Edad)= \beta_0 + \beta_1 Edad

#Para el de tratamiento medico
#$Trat=1$, entonces
#E(Ant;Tat=Med, Edad)=  \beta_0 + \beta_1 Edad +\beta_2 + \beta_3 Edad = (\beta_0 + \beta_2) + (\beta_1 + \beta_3) Edad

######## IV) Edad afecta igual a la generaci�n de anticuerpos en el grupo control que en el grupo que recibe el medicamento #####

# El cambio en el valor promedio de los anticuerpos que se 
# obtiene al aumentar en una unidad la variable Edad en el
# tratamineto medico es

#$E(Ant;Trat=MEd, Edad + 1) ??? E(Ant;Trat=MEd, Edad) = (\beta_0 + \beta_2) + (\beta_1 + \beta_4) (Edad+1)???
#(\beta_0 + \beta_2) + (\beta_1 + \beta_4) (Edad)=\beta_1 + \beta_4$
# es decir, $\beta_1 + \beta_3$ es el promedio del cambio en la generaci�n de 
# anticuerpos al aumantar la edad que tiene el paciente en el grupo
# al que se le aplic� tratamiento m�dico.

#para el tratamiento de control
#$E(Ant;Trat=Control, Edad + 1) ??? E(Ant;Trat=Control, Edad) = \beta_0  + \beta_1 (Edad+1)???
#\beta_0  + \beta_1 (Edad)=\beta_1 $
# Entonces, $\beta_1$ es el promedio del cambio en la generaci�n de 
# anticuerpos al cambiar la edad en un a�o al paciente en un grupo
# de control

# entonces para saber si la edad afecta de la misma forma
# la generaci�n de anticuerpos en el grupo control
# que en el grupo que recibe el medicamento, la hip�tesis nula propuesta
# es $H_o:\ \beta_1=\beta_1 + \beta_3$, que equivale a
# $$H_0: \beta_3=0 \quad \ \text{vs} \quad \  \beta_4=\neq$$


#$$Ant = \beta_0 + \beta_1 Edad + \beta_2 Trat + \beta_3 Edad*Trat$$

library(multcomp)
K=matrix(c(0,0,0,1), ncol=4, nrow=1, byrow=TRUE)
m=c(0)
summary(glht(fit, linfct=K, rhs=m), test=Ftest())

# Se rechaza $H_0$, encontramos evidencia estad�stica
# que indica que el coeficiente
# asociados a las interaccion entre las variable edad y tratamiento 
# ($\beta_3$) es significativo. Es decir, encontramos evidencia 
# que nos dice que la edad afecta de distinta forma
# la generaci�n de anticuerpos en el grupo control
# que en el grupo que recibe el medicamento.

####### v) Ajuste del modelo incluyendo la interpretaci�n de cada uno de los coeficientes.
# El modelo ajustado es 

#$$Ant = 25.43075 -0.3091356 Edad + 0.9351099 Trat + 0.1606855 Edad*Trat$$


#el coeficiente $\beta_0=25.43075$ nos dice la cantidad de
# anticuerpos que tiene una persona de cero a�os dado que esta en el grupo de control.
# Como ya se hab�a mencionado anteriormente, el coeficiente $\beta_1=-0.3091356$ 
# es el promedio del cambio en la generaci�n de 
# anticuerpos al aumentarle la edad en un a�o al paciente en un grupo
# de control.

# El coeficiente $\beta_2=0.9351099$ 
# Al ver el summary vemos la posibilidad de que el coeficiente $\beta_2$=tratMed
# sea 0 porque tiene una significancia baja. 

library(multcomp)
K=matrix(c(0,0,1,0), ncol=4, nrow=1, byrow=TRUE)
m=c(0)
summary(glht(fit, linfct=K, rhs=m), test=Ftest())

# Rechaza la prueba F, por lo tanto hemos encontrado evidencia
# que nos permite tomar el coeficiente $\beta_2=0$, por lo tanto
# no tiene sentido interpretarlo.

# El coeficiente $\beta_3$ se puede intermpretar como cu�nto cambia el promedio
# de anticuerpos en un a�o al cambiar de un grupo a otro y como vimos
# en el inciso anterior es distinto de cero.

fitred <- lm(Ant ~ Edad  + I(Edad*(Trat=="Med")), data = datos) 
summary(fitred)

#El modelo reducido es $\beta_2*=\beta_3$
# Ant= $\beta_0$+ $\beta_1$Edad+$\beta_2*$Trat*Med
# Todos los coeficinetes nos aportan al modelo

############# vi) Argumente en contra o a favor de la afirmaci�n:

edad <- seq(from = 16, to = 60, by = .5)
length(edad)


#Para una banda para la recta del grupo de control
#E(Y;Trat=control, Edad)= b0 + b1 Edad
Kc <- cbind(1, edad, 0)
(Kc)
#Para una banda para la recta del tratamiento m�dico
#E(Y;Tat=med, Edad)=  b0 + b1 Edad + b2 Edad = (b0 + b2) + (b1 + b4) Edad
Km <- cbind(1, edad, edad)
(Km)

K=rbind(Kc, Km)

fitE <- glht(fitred, linfct = K)
fitci <- confint(fitE, level = 0.90)

plot(datos$Edad, datos$Ant, pch=19, col = c("red", "green")[datos$Trat])
legend("bottomleft", c("Tratamiento Con", "Tratamiento med"),
       col = c("red", "green"), pch = 19, inset = 0.01)
lines(edad, coef(fitE)[1:89], col="red")
lines(edad, fitci$confint[1:89,"upr"])
lines(edad, fitci$confint[1:89,"lwr"])

lines(edad, coef(fitE)[90:178], col="green")
lines(edad, fitci$confint[90:178,"upr"])
lines(edad, fitci$confint[90:178,"lwr"])

# Con ayuda de la prueba de hip�tesis simultanea, con un nivel del 90\%,
# podemos afirmar que el medicamento aumenta el n�mero de anticuerpos, 
# pues entre las edades de 16 y 60 a�os la banda de confianza del tratamiento se
# encuentra por encima de la banda de confianza del grupo de control.



