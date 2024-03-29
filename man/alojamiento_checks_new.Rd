% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alojamiento_checks_new.R
\name{alojamiento_checks_new}
\alias{alojamiento_checks_new}
\title{Alojamiento Checks}
\usage{
alojamiento_checks_new(base_datos)
}
\arguments{
\item{base_datos}{La base de datos despues de haber realizado el cleaning}
}
\value{
Encuestas_piloto

Intencion_moverse

Tipo_alojamiento_rancho

Tipo_alojamiento_temp

Tipo_alojamiento_pagadiario

Tipo_alojamiento_hotel

Num_hogar

Facilidades

Facilidadaes_otr

Num_cuartos

precioslocos_inquilinato

precioslocos_cuarto

precioslocos_pagadiario

precioslocos_casa

precioslocos_habitacion

precioslocos_albergue

precioslocos_asentamiento

Pago_alojamiento

Agua_valor

Gas_valor

EnergiaLuz_valor

Internet_valor

Telefono_valor

Disminuye_arriendo

Aumen ta_arriendo
}
\description{
Sirve para identificar los datos atipicos de precios y valores atipicos de la base de datos
}
\examples{
base_cleaning <- cleaning_alojamiento(base_datos_reshape)

base_outliers <- checks_alojamiento(base_cleaning)
base_outliers <- checks_alojamiento(base_datos = base_cleaning)
}
