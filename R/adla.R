#'Anaisis de discriminantes lineales
#'
#'Realiza una clasficación en base al analisis de discriminantes lineales
#'
#'@param y (vector) datos de respuesta.
#'@param x (vector) datos de la variable explicativa.
#'@return una lista con los valores de lad media, la varianza, el valor de pi,los valores de los vectores que contiene los elementos anteriores y los valores de etiqueta que son asignados en base a la condición que se le otorge
#'@export
#'
#'@examples
#'\dontrun{
#'#directorio de trabajo
#'datos_adl <- read.csv("C:\\Users\\Alessandro\\OneDrive\\Documentos\\datos ejemplo adl.csv")
#'
#'#------------------------------------------------------------------------------------------
#'Ejemplo 1
#'print(datos_adl)
#cargo la libreria
#'library(ADLals)
#'ADL(y=datos_adl$salud,x=datos_adl$fiebre)
#'#------------------------------------------------------------------------------------------
#'#Ejemplo 2
#'y <- c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1)
#'x <- c(39.2, 40.0, 41.2, 42.6, 40.0, 39.8, 41.8, 40.4, 41.8, 42.0, 38.2, 37.0, 37.8, 37.2, 38.0, 37.6, 37.8, 38.2, 38.0, 37.4)
#'ADL(y=y,x=x)
#'}
ADL <- function(y, x) {
  # Cálculo de m
  m0 <- mean(x[y == 0])
  print(m0)
  m1 <- mean(x[y == 1])
  print(m1)

  # Cálculo de la varianza
  cv0 <- var(x[y == 0])
  print(cv0)
  cv1 <- var(x[y == 1])
  print(cv1)
  print(cv1)
  # Cálculo de pi
  ty <- table(y)

  pi0 <- ty[1] / sum(ty)
  print(pi0)
  pi1 <- ty[2] / sum(ty)
  print(pi1)

  adl0 <- function(y, x) {
    x0 <- x[y == 0]
    m0 <- mean(x0)
    cv0 <- var(x0)
    pi0 <- sum(y == 0) / length(y)
    resultado0 <- x * ((m0 / cv0^2) - (m0^2 / cv0^2) / 2 + log(pi0))
    return(resultado0)
  }

  resultado_adl0 <- adl0(y, x)
  print(resultado_adl0)

  adl1 <- function(y, x) {
    x1 <- x[y == 1]
    m1 <- mean(x1)
    cv1 <- var(x1)
    pi1 <- sum(y == 1) / length(y)
    resultado1 <- x * ((m1 / cv1^2) - (m1^2 / cv1^2) / 2 + log(pi1))
    return(resultado1)
  }

  resultado_adl1 <- adl1(y, x)
  print(resultado_adl1)
  etiqueta <- ifelse(x > 39.5, "enfermo", "sano")
  print(etiqueta)
}

