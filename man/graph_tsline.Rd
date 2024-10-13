\name{graph_tsline}
\alias{graph_tsline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gráfica de Linea para Series de Tiempo
}
\description{
Gráfica de lineas para series de tiempo con el formato correspondiente a la
Administración Pública Federal de México en el sexenio 2024-2030.
}
\usage{
graph_tsline <- function(
    x,
    y,
    intervalo,
    color,
    export = FALSE,
    path = NA,
    formato = NA,
    size = c(8, 4.5),
    cxlims = c(as.Date(min(x)), as.Date(max(x))),
    date.format = "\%d-\%b-\%Y",
    x.lab = format(seq.Date(as.Date(cxlims[1]), as.Date(cxlims[2]), by = intervalo), date.format),
    y.lab = NULL,
    grid = FALSE,
    margins = c(6, 6, 0, 0),
    lab.font = "bold",
    xlab.font = lab.font,
    ylab.font = lab.font,
    family = "MontserratRoman-Bold",
    ymain = NA,
    xmain = NA,
    main.cex = 1.2,
    xmain.cex = main.cex,
    ymain.cex = main.cex,
    dist = 2.5,
    xdist = dist,
    ydist = dist + 1,
    main.font = "bold",
    ymain.font = main.font,
    xmain.font = main.font,
    y.ticks = NULL,
    lwd.axis = 2,
    etiquetas = FALSE,
    etiquetas.cex = 0.8,
    ylim = c(min(0, min(y) * 1.1), max(y) * 1.1),
    division = FALSE,
    fechas.div = NA,
    evento = FALSE,
    fechas.ev = fechas.div,
    axis.col = "gray25",
    points = TRUE,
    lwd = 2,
    lty = 1,
    pch = 16,
    points.cex = 1,
    sep = 0.05 * min(y),
    posc = NULL,
    cex.lab = 0.8,
    cex.ylab = cex.lab,
    cex.xlab = cex.lab,
    ...
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{Objeto de fechas. Debe estar en un formato que R reconozca como fechas, como un vector de clase Date.}
\item{y}{Objeto de niveles (valores numéricos) que se graficarán.}
\item{export}{Booleano que indica si se exportará el gráfico (TRUE/FALSE). Por defecto, es FALSE.}
\item{path}{Dirección del archivo a exportar. Especifica la ruta y el nombre del archivo.}
\item{formato}{Formato del archivo a exportar, puede ser "pdf" o "png".}
\item{size}{Tamaño en pulgadas del gráfico, especificado como un vector de longitud 2 (ancho, alto).}
\item{cxlims}{Límites del eje x, definidos como un vector de fechas (mínimo, máximo).}
\item{intervalo}{Frecuencia de los datos, puede ser "week", "month", "year" o "day".}
\item{color}{Vector de colores para las líneas o elementos gráficos.}
\item{date.format}{Formato de las etiquetas del eje x. Por defecto, es "d-b-Y".}
\item{x.lab}{Etiquetas del eje x, generadas en base al intervalo definido.}
\item{y.lab}{Etiquetas del eje y. Por defecto, es NULL.}
\item{grid}{Booleano que indica si se añade una cuadrícula detrás de la gráfica (TRUE/FALSE).}
\item{margins}{Márgenes de la gráfica, especificados como un vector de longitud 4 (superior, derecho, inferior, izquierdo).}
\item{lab.font}{Tipo de letra para las etiquetas de los ejes x e y ("bold", "regular", "italic", "italic-bold").}
\item{xlab.font}{Tipo de letra para las etiquetas del eje x. Por defecto, es igual a lab.font.}
\item{ylab.font}{Tipo de letra para las etiquetas del eje y. Por defecto, es igual a lab.font.}
\item{family}{Familia de texto utilizada en el gráfico. Por defecto, es "Montserrat".}
\item{ymain}{Título del eje y. Por defecto, es NA.}
\item{xmain}{Título del eje x. Por defecto, es NA.}
\item{main.cex}{Tamaño de los títulos de los ejes x e y. Por defecto, es 1.2.}
\item{xmain.cex}{Tamaño del título del eje x. Por defecto, igual a main.cex.}
\item{ymain.cex}{Tamaño del título del eje y. Por defecto, igual a main.cex.}
\item{dist}{Distancia de los títulos desde el eje x e y. Por defecto, es 2.5.}
\item{xdist}{Distancia del título al eje x. Por defecto, igual a dist.}
\item{ydist}{Distancia del título al eje y. Por defecto, es dist + 1.}
\item{main.font}{Tipo de letra para los títulos de los ejes x e y ("bold", "regular", "italic", "italic-bold").}
\item{ymain.font}{Tipo de letra para el título del eje y. Por defecto, igual a main.font.}
\item{xmain.font}{Tipo de letra para el título del eje x. Por defecto, igual a main.font.}
\item{y.ticks}{Posición de los puntos de interrupción en el eje y. Por defecto, es NULL.}
\item{lwd.axis}{Ancho de línea del eje. Por defecto, es 2.}
\item{etiquetas}{Booleano que indica si se añaden etiquetas encima de las barras (TRUE/FALSE). Por defecto, es FALSE.}
\item{etiquetas.cex}{Tamaño de las etiquetas encima de las barras. Por defecto, es 0.8.}
\item{ylim}{Límites del eje y, ajustados para que el rango mínimo sea al menos 0.}
\item{division}{Booleano que indica si se divide en las barras (TRUE/FALSE). Por defecto, es FALSE.}
\item{fechas.div}{Fechas en las que se marcan las divisiones. Por defecto, es NA.}
\item{evento}{Booleano que indica si hay un evento a analizar (TRUE/FALSE). Por defecto, es FALSE.}
\item{fechas.ev}{Fechas de los eventos, que por defecto son las fechas de división.}
\item{axis.col}{Color del eje. Por defecto, es "gray25".}
\item{points}{Booleano que indica si se grafican puntos en la gráfica (TRUE/FALSE). Por defecto, es TRUE.}
\item{lwd}{Ancho de las líneas de la gráfica. Por defecto, es 2.}
\item{lty}{Tipo de línea utilizada en la gráfica. Por defecto, es 1.}
\item{pch}{Forma de los puntos en la gráfica. Por defecto, es 16.}
\item{points.cex}{Tamaño de los puntos en la gráfica. Por defecto, es 1.}
\item{sep}{Separación entre la línea y la etiqueta que indica su valor. Debe proporcionarse
si \code{etiquetas=TRUE}.}
\item{posc}{}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Un gráfico generado en la salida gráfica o un archivo exportado, dependiendo del argumento export.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Daniel Kelly
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
require(tidiverse)

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
graph_tsline(
  x=prueba_year$x,
  y=prueba_year$y,
  intervalo="year",
  color = c("dodgerblue", "firebrick4", "goldenrod"),
  division = TRUE,
  fechas.div = c(as.Date("2001-01-01"), as.Date("2020-01-01")),
  etiquetas=TRUE,
  las=2,
  points = TRUE,
  lwd=3,
  evento=TRUE
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
