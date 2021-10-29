getwd()
setwd("C:/Users/Diego Aguilar/Desktop/BEDU/SecurityThreats")
securityThreats<-read.csv("cve.csv")
dim(securityThreats)
class(securityThreats)
# Se guarda en un dataframe las amenazas de seguridad que son mayores a la calificación 5, 
# que se pueden considerar como las vulnerabilidades que pueden generar un alto impacto y pérdida a las empresas
majorST <- securityThreats[securityThreats$cvss > 5,]
dim(majorST)
tail(majorST)
#El nombre de las vulnerabilidades como el nombre de las columnas
tSecurity <- as.data.frame(t(majorST))
colnames(tSecurity) <- tSecurity[6,]
row.names(tSecurity)
# Se conoce cual es el nombre de la vulnerabilidad que representa el mayor riesgo
which.max(tSecurity["cvss",])
#--------------------------------------------2da parte del analisis
#Generé una serie de números random para poder analizar el riesgo que pueden representar estos casos de vulnerabilidades
#Habiendo creado el escenario pude modelar un caso hipotetico en el que obtuve la media, mediana y moda del caso, así como
#Los deciles,intercuartílico, la desviación estándar y varianza muestral de las mediciones en x
set.seed(1)
sample(10)
x <- round(rnorm(10, 5, 2.5), 1)
library(DescTools)
mean(x); median(x); Mode(x)
quantile(x, probs = seq(0.1, 0.9, 0.1))
IQR(x); sd(x); var(x)
#Cuál es la probabilidad de que un producto de software de 25 obtenga una calificación mayor a 5, que puede representar una
#Pérdida significativa dentro de la empresa
dbinom(1,25,0.06)
#Se realizó el histograma con la frecuencia sobre la calificación de las vulnerabilidades que se están analizando siendo 7.5 la calificación con más frecuencia
library(ggplot2)
hist(majorST$cvss, 
     breaks = 10,
     main = " Histograma de CVSS",
     ylab = "Frecuencia",
     xlab = "Calificación de riesgo", 
     col = "blue")

#Vulnerabilidad más alta
(vuln_alta <- which.max(securityThreats$cvss))
paste("La vulnerabilidad más alta es:", securityThreats$cwe_name[vuln_alta],"con un cvss de:", round(securityThreats$cvss[vuln_alta],2))
#Vulnerabilidad más baja
(vuln_baja <- which.min(securityThreats$cvss))
paste("La vulnerabilidad más baja es:", securityThreats$cwe_name[vuln_baja],"con un cvss de:",round(securityThreats$cvss[vuln_baja],2))
#La vulnerabilidad promedio
(vuln_m <- mean(na.omit(securityThreats$cvss)))
paste("La vulnerabilidad promedio es:", round(vuln_m,2))
#Distribución normal

# Considere una variable aleatoria normal con la media obtenida 6.02 y desviación estándar 2.73.
# 2. Encuentre la probabilidad de que la v.a. sea mayor o igual a 5 que es la media de la calificación del CVSS
# 3. Encuentre el cuantil de orden 0.56 / Para esto dividí 5 entre 10, ya que va de 1 a 5 y de 1 a 10 la calificación de CVSS
# 4. Histograma de frecuencias relativas para la muestra
library(ggplot2)
x <- seq(2, 5, 0.1) # 1.
y <- dnorm(x = x, mean = 6.02, sd = 2.736177)
data <- data.frame(x, y)
tail(data)
pnorm(5, mean = 6.02, sd = 2.736177, lower.tail = FALSE) # 2. 
qnorm(0.56, mean = 6.02, sd = 2.736177) # 3.

set.seed(20) # 4.
ma <- rnorm(1000, mean = 6.02, sd = 2.736177) 
madf <- as.data.frame(ma)
tail(madf)

p <- ggplot(madf, aes(ma)) + 
  geom_histogram(colour = 'red', 
                 fill = 'yellow',
                 alpha = 0.8, # Intensidad del color fill
                 binwidth = 3.5) + 
  geom_density(aes(y = 3.5*..count..))+
  geom_vline(xintercept = mean(ma), 
             linetype="dashed", color = "black") + 
  ggtitle('Histograma para la muestra de vulnerabilidades') + 
  labs(x = 'Calificación de riesgo', y = 'Frecuencia')+
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
p

