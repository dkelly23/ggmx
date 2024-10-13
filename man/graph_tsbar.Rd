\name{graph_tsbar}
\alias{graph_tsbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Gráfica de Barras para Series de Tiempo
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Gráfica de barras para series de tiempo con el formato correspondiente a la
Administración Pública Federal de México en el sexenio 2024-2030.
}
\usage{
graph_tsbar <- function(
    x,
    y,
    export=FALSE,
    path=NA,
    formato=NA,
    size=c(8,4.5),
    cxlims = c(as.Date(min(x)), as.Date(max(x))),
    intervalo,
    color,
    date.format=NULL,
    x.lab=format(seq.Date(as.Date(cxlims[1]), as.Date(cxlims[2]), by=intervalo), date.format),
    y.lab=NULL,
    grid=FALSE,
    margins=c(6,6,0,0),
    lab.font="bold",
    xlab.font=lab.font,
    ylab.font=lab.font,
    family="MontserratRoman-Bold",
    ymain=NA,
    xmain=NA,
    main.cex=1.2,
    xmain.cex=main.cex,
    ymain.cex=main.cex,
    dist=2.5,
    xdist=dist,
    ydist=dist+1,
    main.font="bold",
    ymain.font=main.font,
    xmain.font=main.font,
    y.ticks=NULL,
    lwd.axis=2,
    etiquetas=FALSE,
    etiquetas.cex=0.8,
    ylim=c(min(0, min(y)*1.1), max(y)*1.1),
    division=FALSE,
    fechas.div=NA,
    evento=FALSE,
    fechas.ev=fechas.div,
    axis.col="gray25",
    sep=0.05*min(y),
    ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{x}{Vector de fechas a graficar en el eje horizontal.}
 \item{y}{Vector de valores numéricos.}
 \item{export}{Lógico que indica si la gráfica se exporta.}
 \item{path}{Ruta dónde se exporta el archivo. Debe proporcionarse si \code{export=TRUE}.}
 \item{formato}{Formato del archivo a exportar, puede ser \code{png} o \code{pdf}.}
 \item{size}{Un vector numérico de longitud 2 que define el tamaño en pulgadas del gráfico con el formato: \code{c(ancho, alto)}.}
 \item{cxlims}{Límites del eje \code{x}, definidos como un vector de fechas \code{c(min, max)}.}
 \item{intervalo}{Frecuencia de los datos. Puede ser \code{"week", "month", "year" o "day"}.}
 \item{color}{Un vector de colores para las barras del gráfico.}
 \item{date.format}{Formato de las etiquetas del eje \code{x}. Por defecto, es \code{\%d-\%b-\%Y}.}
 \item{x.lab}{Etiquetas del eje \code{x}. Por defacto es una secuencia de fechas definida por
 \code{cxlims}, de acuerdo con \code{intervalo}.}
 \item{y.lab}{Etiquetas del eje \code{y}. Por defecto, es \code{NULL}.}
 \item{grid}{Variable lógica que indica si se debe graficar una cuadrícula detrás de la
 gráfica. Por defecto es \code{FALSE}.}
 \item{margins}{Dimensiones de los márgenes de la gráfica, especificados como un vector de longitud 4 con el formato: \code{c(superior, izquierdo, inferior, derecho)}. Por defecto es
 \code{c(6,6,0,0)}.}
 \item{lab.font}{Tipo de letra para las etiquetas de los ejes \code{x} y \code{y}. Puede ser \code{"bold", "regular", "italic", "italic-bold"}.}
 \item{xlab.font}{Tipo de letra para las etiquetas del eje \code{x}. Por defecto, es igual a \code{lab.font}.}
 \item{ylab.font}{Tipo de letra para las etiquetas del eje \code{y}. Por defecto, es igual a \code{lab.font}.}
 \item{family}{Familia tipográfica para el texto. Por defecto, es \code{Montserrat}.}
 \item{ymain}{Título del eje \code{y}. Por defecto, es \code{NA}.}
 \item{xmain}{Título del eje \code{x}. Por defecto, es \code{NA}.}
 \item{main.cex}{Tamaño de los títulos de los ejes \code{x} e \code{y}. Por defecto, es \code{1.2}.}
 \item{xmain.cex}{Tamaño del título del eje \code{x}. Por defecto, igual a \code{main.cex}.}
 \item{ymain.cex}{Tamaño del título del eje \code{y}. Por defecto, igual a \code{main.cex}.}
 \item{dist}{Distancia de los títulos desde el eje \code{x} y \code{y}. Por defecto, es \code{2.5}.}
 \item{xdist}{Distancia del título al eje \code{x}. Por defecto, es igual a \code{dist}.}
 \item{ydist}{Distancia del título al eje \code{y}. Por defecto, es \code{dist + 1}.}
 \item{main.font}{Tipo de letra para los títulos de los ejes \code{x} e \code{y}. Debe ser uno de \code{"bold", "regular", "italic", "italic-bold"}.}
 \item{ymain.font}{Tipo de letra para el título del eje \code{y}. Por defecto, igual a \code{main.font}.}
 \item{xmain.font}{Tipo de letra para el título del eje \code{x}. Por defecto, igual a \code{main.font}.}
 \item{y.ticks}{Posición de los breaks en el eje \code{y}. Por defecto, es igual a \code{NULL}.}
 \item{lwd.axis}{Ancho de las líneas de los ejes. Por defecto, es igual a \code{2}.}
 \item{etiquetas}{Variable lógica que indica si se añaden etiquetas encima de las barras. Por defecto, es igual \code{FALSE}.}
 \item{etiquetas.cex}{Tamaño de las etiquetas con el valor de las barras. Por defecto, es \code{0.8}.}
 \item{ylim}{Límites del eje \code{y}, ajustados para que el rango mínimo sea al menos \code{0}.}
 \item{division}{Variable lógica que indica si hay una división en las barras. Por defecto, es igual a \code{FALSE}.}
 \item{fechas.div}{Fechas en las que se realizan las divisiones. Por defecto, es \code{NA}.}
 \item{evento}{Variable lógica que indica si hay un evento. Por defecto, es \code{FALSE}.}
 \item{fechas.ev}{Fechas de los eventos. Por defecto, es igual a \code{fechas.div}.}
 \item{axis.col}{Color del eje. Por defecto, es igual a \code{"gray25"}.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Un gráfico generado en la salida gráfica o un archivo exportado, dependiendo del argumento \code{export}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Daniel Kelly \\
djsanchez@colmex.mx \\
github.com/dkelly23
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
requiere(tidyverse)

## Sample Data:
prueba_year <- bind_cols(
  x=seq(as.Date("1994-01-01"), as.Date("2024-12-31"), by="year"),
  y=round(rnorm(length(seq(as.Date("1994-01-01"), as.Date("2024-12-31"), by="year")), mean=100, sd=10),0)
) %>%
  mutate(c=runif(length(seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by="year")), 0, 3),
         categoria=case_when(
           c >= 0 & c<1 ~ "A",
           c >= 1 & c<2 ~ "B",
           c >= 2 & c<=3 ~ "C"
         )
  ) %>%
  select(-c)

## Gráfica de Barras dividida en Segmentos
graph_tsbar(
  x=prueba_year$x,
  y=prueba_year$y,
  intervalo="year",
  color = c("dodgerblue", "firebrick4", "goldenrod"),
  etiquetas=TRUE,
  las=2,
  division=TRUE,
  fechas.div = as.Date(c("2002-01-01", "2008-01-01")),
  evento=TRUE,
  etiquetas.cex = 0.7,
  grid=FALSE,
  ymain="Fecha",
  xmain="Prueba",
  ylim=c(0,140),
  margins=c(5,4,0,0)
)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
