

system("mpiexec -n 8 pitremove -z output_be.tif -fel yosemite_be_fel.tif")
# fel=raster("yosemite_be_fel.tif")
# plot(fel)

# D8 flow directions
system("mpiexec -n 8 D8Flowdir -p yosemite_be_p.tif -sd8 yosemite_be_sd8.tif -fel yosemite_be_fel.tif",
       show.output.on.console=T,invisible=F)
# p=raster("yosemite_be_p.tif")
# plot(p)
# sd8=raster("yosemite_be_sd8.tif")
# plot(sd8)


# Contributing area
system("mpiexec -n 8 AreaD8 -p yosemite_be_p.tif -ad8 yosemite_be_ad8.tif -nc")
# ad8=raster("yosemite_be_ad8.tif")
# plot(ad8)
# zoom(ad8)

# Threshold
system("mpiexec -n 8 Threshold -ssa yosemite_be_ad8.tif -src yosemite_be_src_6mil.tif -thresh 20000")


# Stream Reach and Watershed
system("mpiexec -n 8 Streamnet -fel yosemite_be_fel.tif -p yosemite_be_p.tif -ad8 yosemite_be_ad8.tif -src yosemite_be_src_6mil.tif -ord yosemite_be_ord.tif -tree yosemite_be_tree.txt -coord yosemite_be_coord.txt -net yosemite_be_net.shp -w yosemite_be_w.tif")
# plot(raster("yosemite_be_ord.tif"))
# zoom(raster("yosemite_be_ord.tif"))
# plot(raster("yosemite_be_w.tif"))

# DInf flow directions
system("mpiexec -n 8 DinfFlowdir -ang yosemite_be_ang.tif -slp yosemite_be_slp.tif -fel yosemite_be_fel.tif",
       show.output.on.console=T,invisible=T)
# ang=raster("yosemite_be_ang.tif")
# plot(ang)
# slp=raster("yosemite_be_slp.tif")
# plot(slp)

# Dinf contributing area
system("mpiexec -n 8 AreaDinf -ang yosemite_be_ang.tif -sca yosemite_be_sca.tif -nc")
# sca=raster("yosemite_be_sca.tif")
# plot(log(sca))
# zoom(log(sca))

# Distance Down
system("mpiexec -n 8 DinfDistDown -ang yosemite_be_ang.tif -fel yosemite_be_fel.tif -src yosemite_be_src.tif -m ave v -nc -dd yosemite_be_dd.tif",
       show.output.on.console=T,invisible=F)
