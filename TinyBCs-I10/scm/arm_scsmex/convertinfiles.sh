#!/usr/local/bin/bash

ln -s datmodyn_internal_rst_XY1x1-C_* datmodyn_internal_rst
#ln -s fraci_simple1_1990_2005_XY1x1-C_*.dat fraci.data
ln -s fraci*.dat fraci.data
#ln -s fvcore_internal_rst.b19830214_21z_XY1x1-C_* fvcore_internal_rst
ln -s fvcore_internal_rst.* fvcore_internal_rst
#ln -s moist_internal_rst.0214_21z_XY1x1-C_* moist_internal_rst
ln -s moist_internal_rst* moist_internal_rst
ln -s SEAWIFS_KPAR_mon_clim_XY1x1-C_*.dat SEAWIFS_KPAR_mon_clim.data
ln -s sst_simple1_1990_2005_XY1x1-C_*.dat sst.data
ln -s sstsi_simple1_1990_2005_XY1x1-C_*.dat sstsi.data
ln -s tile.data_simple1_XY1x1-C_* tile.data
ln -s topo_GWD_var_144x91_DC.data_XY1x1-C_* topo_gwdvar.data
ln -s topo_TRB_var_144x91_DC.data_XY1x1-C_* topo_trbvar.data
#ln -s Landfiles/catch_internal_rst.* catch_internal_rst
#ln -s Landfiles/laigrn.data.TILE_* laigrn.data
#ln -s Landfiles/nirdf.dat.TILE_* nirdf.dat
#ln -s Landfiles/vegdyn_internal_rst.* vegdyn.data
#ln -s Landfiles/visdf.dat.TILE_* visdf.dat
ln -s Landfiles/catch_internal_rst* catch_internal_rst
ln -s Landfiles/laigrn.data* laigrn.data
ln -s Landfiles/nirdf.dat* nirdf.dat
ln -s Landfiles/vegdyn* vegdyn.data
ln -s Landfiles/visdf.dat* visdf.dat

ln -s AGCM.rc* AGCM.rc
ln -s CAP.rc* CAP.rc
ln -s cap_restart* cap_restart
ln -s HISTORY.rc .

