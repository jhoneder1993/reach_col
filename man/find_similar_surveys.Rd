% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_similar_surveys.R
\name{find_similar_surveys}
\alias{find_similar_surveys}
\title{Find Similar Surveys}
\usage{
find_similar_surveys(raw.data, tool.survey, uuid = "_uuid")
}
\arguments{
\item{raw.data}{Un dataframe}

\item{tool.survey}{el survey de la herramienta kobo}

\item{uuid}{como aparece el uuid}
}
\value{
La función devuelve un marco de datos con el mismo número de filas (todas las encuestas) y algunas columnas adicionales que indican el ID de la encuesta más similar y cuántas columnas son diferentes.
}
\description{
compara cada encuesta con cada una de las otras encuestas del conjunto de datos y encuentra la más similar, es decir, la que tiene el menor número de respuestas diferentes.
}
\examples{
survey <- read_excel("Input/Kobo_ABA_GN2_JM_V22.xlsx", sheet = "survey")
data <- read_excel("Input/encuesta.xlsx")
similares <- find_similar_surveys(data, survey, "uuid")
}
