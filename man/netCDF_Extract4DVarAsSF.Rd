% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/netCDF_Extract4DVarAsSF.R
\name{netCDF_Extract4DVarAsSF}
\alias{netCDF_Extract4DVarAsSF}
\title{Create a \code{sf} dataframe representing a single time step and depth layer from a ROMS 4DVar in a netcdf file}
\usage{
netCDF_Extract4DVarAsSF(
  netcdf,
  roms_grid,
  variable,
  timeslice = 1,
  layer = 1,
  verbose = FALSE
)
}
\arguments{
\item{netcdf}{- path to netcdf file with ROMS model output, or a ncdf4 object connected to a netcdf file}

\item{roms_grid}{- roms grid as an sf dataframe object}

\item{variable}{- variable name to extract}

\item{timeslice}{- time slice from file to extract}

\item{layer}{- depth layer to extract [positive numbers start from bottom; non-negative numbers start from surface]}

\item{verbose}{- flag (T/F) to print info}
}
\value{
A \code{sf} dataframe object representing the layer
}
\description{
Function to create a \code{sf} dataframe representing a single time step and depth layer from a ROMS 4DVar in a netcdf file.
}
\details{
Creates an sf dataframe representing a single time step and depth layer from a ROMS 4DVar in a netcdf file.
}
