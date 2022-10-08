setwd("C:/Users/mmrcr/Dropbox/Doc/Borrador")
seeds<-read.table("seeds.txt", header=T)
seeds
names(seeds)

# Modelos con una variable dependiente continua y varias variables independientes continuas:
# Se trata de datos de peso corporal de roedores del desierto en función de cuatro variables explicativas o predictoras
#1 cantidad de lluvia
#2 abundancia de depredadores
#3 cobertura de vegetación perenne
#4 producción de semillas. Se quiere encontrar el modelo mínimo adecuado.	 



# Primero exploramos los datos con gráficas de la variables una a una usando el comando pairs

pairs(seeds, panel=panel.smooth)

# Parece que hay cierta relación de proporcionalidad entre la cantidad de lluvia y el peso de los roedores
# también entre el peso y la producciónde semillas. Pero nótese que hay correlación entre las semillas y la lluvia. 

#Exploremos las posibles interacciones: 
#quizá el efecto de la cobertura vegetal depende de que tanta lluvia caiga, 
#es decir que puede ser importante que haya donde refugiarse cuando llueve mucho!
#una manera facil para no acomodar las variables el 
library(car)
coplot(data=seeds,rodent~cover|rain)

#coplot, manteniendo fijo la variable lluvia |
#Puede ser también que la cobertura sea importante cuando hay muchos depredadores, hay que tener donde esconderse!

coplot(data=seeds,rodent~predators|cover)

#Veámoslo de otra manera 
install.packages("tree")
library(tree)
model<-tree(rodent~ rain + seed +cover + predators,data=seeds)
plot(model)
text(model)


# Una regla empírica recomienda no tener más de n/3 parámetros en el modelo inicial
#  n es el número de datos o filas (16)
# De acuerdo a esta regla  incluiremos solo interacción cover*rain, para poder incluir todas las variables de interés

# El modelo máximo será

attach(seeds)
modrod1<-lm(rodent~rain+predators+cover+seed+rain:cover)
summary(modrod1) 
#para ver que variables son significativas y cuales no por ejemplo lluvia	
# Examinando el valor de "t" y su probabilidad asociada, decidimos eliminar primero a la interaccion de lluvia.cobertura
# Para ello usamos update

sininter<-update(modrod1,~.-rain:cover)
anova(modrod1,sininter)
AIC(modrod1,sininter)
summary(modrod1)

# Los tres criterios: la t no significativa para el coeficiente, la tabla de ANOVA y el AIC
# sugieren que sí conviene retirar la interacción
# Ahora removamos los depredadores del modelo sininter

sindepred<-update(sininter,~.-predators)
AIC(modrod1,sininter,sindepred)

# Se justifica remover tanto la interaccion como depredadores.
# Ahora removemos la cobertura tanto del modelo máximo como del actual

sincobertura1<-update(sininter,~.-cover)
sincobertura2<-update(sindepred,~.-cover)
anova(sininter,sindepred,sincobertura2)
AIC(sininter,sindepred,sincobertura1,sincobertura2)


#para utilizar el AIC tienen que utilizarse modelos anidados es decir inicias con el modelo completo
#con todas las variables o parametros y lo comparas vs los modelos derivados o a los que se les fue
#quitando algunas variables:
#siniter
#sindepred
#sincobertura1
#sincobertura2
# Se justifica remover tanto la interaccion como depredadores y la cobertura.
# Ahora removemos la producción de semillas tanto del modelo máximo como del actual

sinsemilla1<-update(sininter,~.-seed)
sinsemilla2<-update(sincobertura2,~.-seed)
anova(modrod1,sinsemilla1,sincobertura2,sinsemilla2)
AIC(modrod1,sininter,sincobertura2,sinsemilla1,sinsemilla2)


# El quitar semilla no disminuye el AIC, por lo tanto se deja en el modelo
# Inspeccionemos el modelo con lluvia y semillas dentro

summary(sincobertura2)

# Como el AIC tiende a ser generoso en dejar términos, además como la significancia es 
# marginal para semillas y además la produción de semillas y la lluvia están algo correlacionadas
# decidimos sacar semillas

# El modelo mínimo adecuado es entonces el sinsemilla2

summary(sinsemilla2)

# Y el modelo es: ?????











# Peso de roedores = -1.26566 +1.74lluvia

#Ahora, el intercepto no parece ser significativo, es decir que probablemente la línea 
#pasa por 0,0. Existen problemas importantes con los ajustes de regresión sin intercepto. ver Quinn y Keough p.98
#Hay autores que consideran justificado quitar el intercepto cuando éste no resulta siginficativo en
#una prueba de t. El problema es que el ajuste a los datos reales ya no es igual de bueno y además tenemos
#que asumir que la relación entre variables es lineal fuera del intervalo de datos que tenemos. Esto NO siempre se 
#justifica.
#Se hace de la siguiente manera

sinint<-lm(formula = rodent ~0+rain)
summary(sinint)

anova(sinint,sinsemilla2)


#Noten que ahora r cuadrada es mucho mas alta y que no existe diferencia siginifcativa entre modelos en la anova. Sin embargo,
#la partición de la suma de cuadrados total, residual y de regresión no se hace de la misma manera cuando no hay intercepto y por 
#ello las r cuadradas no son comparables. OJO! no podemos decir si el ajuste de una es mejor que la otra. 


# Volviendo a los modelos con intercepto. Noten lo que pasa si ajustáramos un modelo con solo semillas como predictoras

summary(lm(rodent~seed))

# Parece altamente significativa. De aquí la importancia del orden en el que se metan o saquen
# las variables cuando están correlacionadas.
#C7.3
#R7.4
# Ahora examinemos los residuales

gramulti<-par(mfrow=c(2,2))
gramulti
plot(sinsemilla2)

#¿cuales son los problemas?
par(mfrow=c(1,1))
plot(rain,rodent,pch=16)
abline(lm(rodent~rain))



logy<-log(rodent)
log<-lm(logy~rain)

summary(log)
xv<-seq(0,16,0.1)
yv<-exp(predict(log,list(rain=xv)))
plot(rain,rodent,pch=16)
lines(xv,yv)

div<-par(mfrow=c(2,2))
plot(log)

#C7.4
#R7.5

# Finalmente veamos que pasa al usar step

automat<-step(modrod1)

summary(automat)

# FIN
