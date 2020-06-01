

system("mpiexec -n 8 pitremove -z dem_1m.tif -fel dem_1m_fel.tif")
# fel=raster("dem_1m_fel.tif")
# plot(fel)

# D8 flow directions
system("mpiexec -n 8 D8Flowdir -p dem_1m_p.tif -sd8 dem_1m_sd8.tif -fel dem_1m_fel.tif",
       show.output.on.console=T,invisible=F)
# p=raster("dem_1m_p.tif")
# plot(p)
# sd8=raster("dem_1m_sd8.tif")
# plot(sd8)


# Contributing area
system("mpiexec -n 8 AreaD8 -p dem_1m_p.tif -ad8 dem_1m_ad8.tif -nc")
# ad8=raster("dem_1m_ad8.tif")
# plot(ad8)
# zoom(ad8)



# Threshold
system("mpiexec -n 8 Threshold -ssa dem_1m_ad8.tif -src dem_1m_src.tif -thresh 50000")


# Stream Reach and Watershed
system("mpiexec -n 8 Streamnet -fel dem_1m_fel.tif -p dem_1m_p.tif -ad8 dem_1m_ad8.tif -src dem_1m_src.tif -ord dem_1m_ord.tif -tree dem_1m_tree.txt -coord dem_1m_coord.txt -net dem_1m_net.shp -w dem_1m_w.tif")
# plot(raster("dem_1m_ord.tif"))
# zoom(raster("dem_1m_ord.tif"))
# plot(raster("dem_1m_w.tif"))

# DInf flow directions
system("mpiexec -n 8 DinfFlowdir -ang dem_1m_ang.tif -slp dem_1m_slp.tif -fel dem_1m_fel.tif",
       show.output.on.console=T,invisible=T)
# ang=raster("dem_1m_ang.tif")
# plot(ang)
# slp=raster("dem_1m_slp.tif")
# plot(slp)

# Dinf contributing area
system("mpiexec -n 8 AreaDinf -ang dem_1m_ang.tif -sca dem_1m_sca.tif -nc")
# sca=raster("dem_1m_sca.tif")
# plot(log(sca))
# zoom(log(sca))

# Distance Down
system("mpiexec -n 8 DinfDistDown -ang dem_1m_ang.tif -fel dem_1m_fel.tif -src dem_1m_src.tif -m ave v -nc -dd dem_1m_dd.tif",
       show.output.on.console=T,invisible=F)
