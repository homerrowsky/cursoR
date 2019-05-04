# Sesion 13
# Continuación de pruebas estadísticas


ET1 <- c(44, 48,36, 32, 51, 45, 54, 56, 142, 148)
ET2 <- c(32, 40, 44, 44, 34, 30, 26, 21, 55, 37)

# Prueba de Wilcox (conocida después como Prueba U de Mann Whitney)
# La hipotesis nula del contraste es que las dos muestras,  detamaño n1 y n2,
# repectivamente, proceden de poblaciones coninuas  idénticas:
# La hipotesis alternativa puede ser unilateral o bilateral y unicamente
# supone que la tendencia central de una poblacion difiere de la otra,
# pero no una diferencia de forma o de dispersión.
# Por esta razón esta prueba es el equivalente no paramétrico de la prueba t
# para la diferencia de dos medias cuando las muestra son independientes
# pero no puede suponerse la normalidad de las poblaciones de origen.
wilcox.test(ET1, ET2, paired = FALSE)

# Diagrama de cajas y bigotes
boxplot(ET1, ET2)

# Prueba t de muestras dependientes o relacionadas
# Use la misma función pero agregue la siguiente instrucción
# paired = TRUE

# Peso antes de tratamiento
antes <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)

# Peso después del tratamiento
despues <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)

t.test(antes, despues, paired = TRUE)


# Prueba de los rangos con signo de Wilcoxon
# Es la prueba análoga a la t de Student para muestras relacionadas
wilcox.test(antes, despues, paired = TRUE)

# Prueba de ANOVA
# Analisis de Varianza

#trat <- c(1,1,1,2,2,2,3,3,3,4,4,4)
#y <- c(7.66, 6.98, 7.80, 5.26, 5.44, 5.80, 7.41, 7.33, 7.04, 3.51, 2.91, 3.66)
# Usar datos de chickwts
trat <- chickwts$feed
y <- chickwts$weight
trat <- as.factor(trat)


# ANOVA es un modelo de regresión lineal , por eso se usa lm
m1 <- lm(y ~ trat)
anova(m1) # ANOVA usual


# análisis de residuales
# 1) Prueba estadística de normalidad
# Almacenar residuales en un vector
residuales <- m1$res
shapiro.test(residuales)

library(e1071)
# 2) Criterio con las medidas de forma
# Coeficiente de asimetria: -0.5 a 0.5
skewness(residuales)
# Curtosis: -2 a 2
kurtosis(residuales)

# histograma de los residuales
hist(residuales, col="tomato")

# 3) Criterio gráfico QQ Normal
qqnorm(residuales)
qqline(residuals(m1))


# Análsis de HOMOCEDASTICIDAD
# residuales vs ajustados
plot(m1$fit, residuales, xlab = "Estimados", ylab = "Residuales",
     main = "Gráfica de residuales vs estimados")

# la misma grafica pero con un poco de ruido
plot(jitter(m1$fit), residuales, xlab = "Estimados", ylab = "Residuales",
     main = "Gráfica CON RUIDO de residuales vs estimados")

# Comparaciones múltiples TUKEY
# es una prueba de comparaciones múltiples
# ayuda a discernir que grupo es el distinto
TukeyHSD(aov(y ~ trat))
# Cómo presentamos los resultados
ybar <- tapply(y, trat, mean)
ybarord <- sort(ybar)
ybarord

boxplot(y ~ trat)
