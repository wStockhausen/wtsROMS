#--files
top<-"~/Work/Projects/ROMS"
#----CGOA
topCGOA<-file.path(top,"CGOA")
netcdf_grid_CGOA<-file.path(topCGOA,"CGOA_grid_5.nc");
netcdf_model_CGOA<-file.path(topCGOA,"cgoa_avg_0002.nc");
#----Bering10k
topEBS<-file.path(top,"Bering10K")
netcdf_grid_EBS<-file.path(topEBS,"Bering_grid_10k.nc");
netcdf_model_EBS<-file.path(topEBS,"eringSea/AVG_1972_00001.nc");
#--NEP4 EBS
topNEP4<-file.path(top,"NEP4")
netcdf_grid_NEP4<-file.path(topNEP4,"NEP4_grid_EBS.nc");
netcdf_model_NEP4<-file.path(topNEP4,"pollock_nep4_avg_0770.nc");

#--Create grids, as necessary
if (FALSE){#--CGOA model
  roms_grid<-wtsROMS::netCDF_CreateGrid(netcdf_grid_CGOA,"rho","latlon");
  save(roms_grid,file="inst/extdata/grids/CGOA.RData");
  sum(is.na(roms_grid$Z)); sum(!is.na(roms_grid$Z));
  rm(roms_grid);
}
if (FALSE){#--Bering10k model
  roms_grid<-wtsROMS::netCDF_CreateGrid(netcdf_grid_EBS,"rho","latlon");
  save(roms_grid,file="inst/extdata/grids/Bering10k.RData");
  sum(is.na(roms_grid$Z)); sum(!is.na(roms_grid$Z));
  rm(roms_grid);
}
if (FALSE){#--NEP4 EBS model
  roms_grid<-wtsROMS::netCDF_CreateGrid(netcdf_grid_NEP4,"rho","latlon");
  save(roms_grid,file="inst/extdata/grids/NEP4.RData");
  sum(is.na(roms_grid$Z)); sum(!is.na(roms_grid$Z));
  rm(roms_grid);
}

