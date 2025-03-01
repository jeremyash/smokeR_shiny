# create points to be used in legend for smokeR
png("smokeR/perm_mon.png", width = 900, height = 900, res = 10500, bg = "transparent")
par(mar = c(0, 0, 0, 0))
plot.new()
points(.5, .5, pch = 21, col = "black", bg = "#00E400")
dev.off()


png("smokeR/temp_mon.png", width = 900, height = 900, res = 8050, bg = "transparent")
par(mar = c(0, 0, 0, 0))
plot.new()
points(.5, .4, pch = 24, col = "black", bg = "#00E400")
dev.off()

