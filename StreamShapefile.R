

system("mpiexec -n 8 pitremove -z raster3.tif -fel raster3_fel.tif")
# fel=raster3("raster3_fel.tif")
# plot(fel)

# D8 flow directions
system("mpiexec -n 8 D8Flowdir -p raster3_p.tif -sd8 raster3_sd8.tif -fel raster3_fel.tif",
       show.output.on.console=T,invisible=F)
# p=raster3("raster3_p.tif")
# plot(p)
# sd8=raster3("raster3_sd8.tif")
# plot(sd8)


# Contributing area
system("mpiexec -n 8 AreaD8 -p raster3_p.tif -ad8 raster3_ad8.tif -nc")
# ad8=raster3("raster3_ad8.tif")
# plot(ad8)
# zoom(ad8)



# Threshold
system("mpiexec -n 8 Threshold -ssa raster3_ad8.tif -src raster3_src.tif -thresh 50000")


# Stream Reach and Watershed
system("mpiexec -n 8 Streamnet -fel raster3_fel.tif -p raster3_p.tif -ad8 raster3_ad8.tif -src raster3_src.tif -ord raster3_ord.tif -tree raster3_tree.txt -coord raster3_coord.txt -net raster3_net.shp -w raster3_w.tif")
# plot(raster3("raster3_ord.tif"))
# zoom(raster3("raster3_ord.tif"))
# plot(raster3("raster3_w.tif"))

# DInf flow directions
system("mpiexec -n 8 DinfFlowdir -ang raster3_ang.tif -slp raster3_slp.tif -fel raster3_fel.tif",
       show.output.on.console=T,invisible=T)
# ang=raster3("raster3_ang.tif")
# plot(ang)
# slp=raster3("raster3_slp.tif")
# plot(slp)

# Dinf contributing area
system("mpiexec -n 8 AreaDinf -ang raster3_ang.tif -sca raster3_sca.tif -nc")
# sca=raster3("raster3_sca.tif")
# plot(log(sca))
# zoom(log(sca))

# Distance Down
system("mpiexec -n 8 DinfDistDown -ang raster3_ang.tif -fel raster3_fel.tif -src raster3_src.tif -m ave v -nc -dd raster3_dd.tif",
       show.output.on.console=T,invisible=F)


# Dinf Reverse Accumulation
temp <- raster3::raster3("raster3.tif")
temp2 <- as.numeric(temp>0)
raster3::writeraster3(temp2,"raster3_wg1.tif")

system("mpiexec -n 8 DinfRevAccum -ang raster3_ang.tif -wg raster3_wg1.tif -racc raster3_racc.tif -dmax raster3_dmax.tif",
       show.output.on.console=T,invisible=F)
