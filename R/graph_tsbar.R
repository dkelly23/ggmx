require(tidyverse)
require(sysfonts)

graph_tsbar <- function(
    x, # Objeto de fechas
    y, # Objeto de niveles
    export=FALSE, # Si se exporta o no
    path=NA, # Dirección del archivo a exportar
    formato=NA, # Formato del archivo a exportar (pdf o png)
    size=c(8,4.5), # Tamaño en pulgadas
    cxlims = c(as.Date(min(x)), as.Date(max(x))), # Límites del eje x
    intervalo, # Frecuencia de los datos (week, month, year, day)
    color, # Vector de colores
    date.format="%d-%b-%Y", # Formato de las etiquetas del eje x
    x.lab=format(seq.Date(as.Date(cxlims[1]), as.Date(cxlims[2]), by=intervalo), date.format), # Etiquetas del eje x
    y.lab=NULL, # Etiquetas del eje y
    grid=FALSE, # Opción para añadir una grid detrás de la gráfica
    margins=c(6,6,0,0), # Margenes de la gráfica
    lab.font="bold", # Tipo de letra para las labels de x y y (bold, regular, italic, italic-bold)
    xlab.font=lab.font, # Tipo de letra para las labels de x
    ylab.font=lab.font, # Tipo de letra para las labels de y
    family="MontserratRoman-Bold", # Familia de texto
    ymain=NA, # Título del eje y
    xmain=NA, # Título del eje x
    main.cex=1.2, # Tamaño de los títulos de los ejes x y y
    xmain.cex=main.cex, # Tamaño de los títulos del eje x
    ymain.cex=main.cex, # Tamaño de los títulos del eje y
    dist=2.5, # Distancia de los títulos desde el eje x y y
    xdist=dist, # Distancia del título al eje x
    ydist=dist+1, # Distancia del título al eje y
    main.font="bold", # Tipo de letra para los titulos de x y y (bold, regular, italic, italic-bold)
    ymain.font=main.font, # Tipo de letra para el título de y
    xmain.font=main.font, # Tipo de letra para el título de x
    y.ticks=NULL, # Posición de los breaks en el eje y
    lwd.axis=2, # Ancho de línea del eje
    etiquetas=FALSE, # Etiquetas encima de las barras
    etiquetas.cex=0.8, # Tamaño de las etiquetas encima de las barras
    ylim=c(min(0, min(y)*1.1), max(y)*1.1), # Límites del eje y
    division=FALSE, # División en las barras
    fechas.div=NA, # Fechas en dónde marcar la división
    evento=FALSE, # Evento a analizar
    fechas.ev=fechas.div, # Fecha de los eventos, por defecto, la fecha de la división
    axis.col="gray25", # Color del eje
    sep=0.05*min(y), # Separación entre la observación y la etiqueta.
    cex.lab=0.8, # Tamaño de las etiquetas de los ejes.
    cex.ylab=cex.lab, # Tamaño de las etiquetas del eje y.
    cex.xlab=cex.lab, # Tamañod de las etiquetas del eje x.
    ...)

{

  if (export==TRUE) {
    if (!formato %in% c("pdf", "png")) {
      stop("El formato de exportación debe ser 'pdf' o 'png'.")
    }
    if (!is.numeric(size[1])) {
      stop("El ancho debe ser un caracter numérico positivo.")
    }
    if(!is.numeric(size[2])){
      stop("El alto debe ser un caracter numérico positivo.")
    }
    if(is.na(path) || path == "") {
      stop("Se debe especificar la dirección del archivo.")
    }
    if(as.numeric(file.access(dirname(paste0(getwd(), "/", path))))!=0) {
      stop("El directorio proporcionado es inaccesible.")
    }
    if(substr(path, nchar(path)-2, nchar(path))!=formato) {
      stop("El tipo de archivo y el directorio son incompatibles.")
    }
  }

  if (!intervalo %in% c("week", "month", "year", "day")) {
    stop("El intervalo para graficar no está definido, debe ser
         'week', 'month', 'year', 'day'.")
  }

  fonts <- c("lab.font", "xlab.font", "ylab.font",
             "main.font", "xmain.font", "ymain.font")
  for (a in fonts) {
    font_val <- get(a)
    if (!font_val %in% c("regular", "bold", "italic", "italic-bold")) {
      stop("El tipo de fuente para las etiquetas '", a, "' debe ser 'regular', 'bold', 'italic' o 'italic-bold'.")
    }
  }

  if (!is.Date(cxlims)) {
    stop("'cxlims' debe ser un objeto de fechas.")
  }

  color_defined <- function(color) {
    tryCatch({
      col2rgb(color)
      TRUE
    }, error = function(e) {
      FALSE
    })
  }

  for (c in 1:length(color)) {
    if (color_defined(color[c])==FALSE) {
      stop(paste0("El elemento ", c, " de 'color' no es un color válido."))
    }
  }

  xlab.font <- case_when(
    xlab.font=="regular" ~ 1,
    xlab.font=="bold" ~ 2,
    xlab.font=="italic" ~ 3,
    xlab.font=="italic-bold" ~ 4
  )

  ylab.font <- case_when(
    ylab.font=="regular" ~ 1,
    ylab.font=="bold" ~ 2,
    ylab.font=="italic" ~ 3,
    ylab.font=="italic-bold" ~ 4
  )


  if (export==TRUE) {
    if (formato=="pdf") {
      pdf(paste0(getwd(), "/", path), width=size[1], height=size[2])
    }
    if (formato=="png") {
      png(paste0(getwd(), "/", path), width=size[1], height=size[2], units="in", res=254)
    }
  }

  if (!is.numeric(margins)) {
    stop("'margins' debe ser un objeto numérico.")
  }
  if (length(margins)!=4) {
    stop("'margins' debe contener 4 elementos.")
  }

  par(mar=margins, mfrow=c(1,1), family=family)

  if (!is.Date(x)) {
    stop("El vector 'x' debe ser un objeto de fechas.")
  }
  if(!is.numeric(y)) {
    stop("El vector 'y' debe ser un objeto numérico.")
  }

  plot(1, type="n", xlab="", ylab="", xlim=cxlims, ylim=ylim, main='', axes=FALSE, ...)

  if (!is.null(x.lab)) {
    if (length(x.lab) != length(seq(cxlims[1], cxlims[2], by=intervalo))) {
      stop("El vector de 'labels' para el eje x debe tener la misma longitud
               que la secuencia de fechas.")
    }
  }

  xticks <- axis.Date(1, x=x, at=seq.Date(as.Date(cxlims[1]), as.Date(cxlims[2]), by=intervalo), labels=x.lab,
                      lwd=lwd.axis, tck=0, col=axis.col, col.axis=axis.col, font=xlab.font,  cex.axis=cex.xlab, ...)

  yticks <- axis(2, at=y.ticks, labels=y.lab, lwd=lwd.axis, lwd.tick=2, tck=0.02, col = axis.col, col.axis=axis.col,
                 font=ylab.font, cex.axis=cex.ylab, ...)

  if (grid==TRUE) {
    abline( h=yticks , lty=3 , lw=0.7)
    abline( v=xticks , lty=3 , lw=0.7)
  }

  ancho <- case_when(
    intervalo=="day" ~ 1-1/7,
    intervalo=="week" ~ round(7-7/7,0),
    intervalo=="month" ~ round(30-30/7,0),
    intervalo=="year" ~ round(365-365/7,0)
  )

  if (division==FALSE) {
    if (length(color>1)) {
      warning("El objeto 'color' debe tener longitud unitaria.")
    }
    rect(x, 0, x+ancho, y, col=color[1], border=NA)
  }

  if (division==TRUE) {
    if(!is.Date(fechas.div)) {
      stop("'fechas.div' debe ser un objeto de fechas.")
    }
    if (length(color)!=length(fechas.div)+1) {
      stop("Se debe proporcionar un color para cada división en la gráfica.")
    }
    if (all(!fechas.div %in% x)){
      stop("Las divisiones deben corresponder a fechas presentes en 'y'.")
    }

    if (length(fechas.div)==1) {
      x1 <- x[1:(which(x==fechas.div)-1)]
      x2 <- x[which(x==fechas.div):length(x)]

      rect(x1, 0, x1+ancho, y[1:(which(x==fechas.div)-1)], col=color[1], border=NA)
      rect(x2, 0, x2+ancho, y[which(x==fechas.div):length(x)], col=color[2], border=NA)

    } else if (length(fechas.div)>1) {

      x_divs <- character()
      dates <- c(min(x), fechas.div)
      for (i in 1:length(dates)) {
        x_divs <- c(x_divs, paste0("x",i))
        if (i!=length(dates)) {
          assign(paste0("x",i), x[which(x==dates[i]):(which(x==dates[i+1])-1)])
        } else if (i==length(dates)) {
          assign(paste0("x",i), x[which(x==dates[i]):length(x)])
        }
      }
      for (u in 1:length(x_divs)) {
        if (u!=length(x_divs)) {
          rect(get(x_divs[u]), 0, get(x_divs[u])+ancho, y[which(x==dates[u]):(which(x==dates[u+1])-1)], border=NA, col=color[u])
        } else if (u==length(x_divs)) {
          rect(get(x_divs[u]), 0, get(x_divs[u])+ancho, y[which(x==dates[u]):length(x)], border=NA, col=color[u])
        }
      }
    }
  }

  if (etiquetas==TRUE){
    text(x+ancho/2, y+sep, labels=round(y,0), cex=etiquetas.cex, col="black", font=2)
  }

  separacion <- case_when(
    intervalo=="day" ~ 1/14,
    intervalo=="week" ~ 0.5,
    intervalo=="month" ~ 2,
    intervalo=="year" ~ 26
  )

  if (evento==TRUE){
    if(!is.Date(fechas.ev)) {
      stop("'fechas.ev' debe ser un objeto de fechas.")
    }
    for (i in fechas.ev) {
      abline(v=i-separacion, col=axis.col, lty=2, lwd=2)
    }
  }

  box(lwd=lwd.axis, col=axis.col)

  if (export==TRUE) {
    dev.off()
  }

  xmain.font <- case_when(
    xmain.font=="regular" ~ 1,
    xmain.font=="bold" ~ 2,
    xmain.font=="italic" ~ 3,
    xmain.font=="italic-bold" ~ 4
  )

  ymain.font <- case_when(
    ymain.font=="regular" ~ 1,
    ymain.font=="bold" ~ 2,
    ymain.font=="italic" ~ 3,
    ymain.font=="italic-bold" ~ 4
  )

  if (!is.na(xmain) && xmain!='') {
    mtext(xmain, side=1, line=xdist, font=xmain.font, cex=xmain.cex)
  }

  if (!is.na(ymain) && ymain!='') {
    mtext(ymain, side=2, line=ydist, font=ymain.font, cex=ymain.cex)
  }

}
