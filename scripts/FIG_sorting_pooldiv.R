# script to plot functional species sorting vs. pool functional diversity

Radj = readRDS("output/Radj_mntd_plotcorrected.RDS")
# plot against total FDis
fd <- readRDS("output/pooldiversity.RDS")[[2]]
fd <- fd[c("Alx", "Clf", "MdtLB", "MdtOC", "Adr", "Phl", "Snd", "Mlk", "Mln"),]

# palette from metacommunity/sp pool map
palette <- c("skyblue1", "#FD8E38", "#E942A6", "#FEDA28", "#21A723",
             "#BC43F1", "#28BD92", "#1C38F7", "#D73535")
shapes <- c(17, 17, 17, 16, 17, 16, 17, 16, 16)
names(shapes) <-  c("Alx", "Clf", "MdtLB", "MdtOC", "Adr", "Phl", "Snd", "Mlk", "Mln")
names(palette) <- c("Alx", "Clf", "MdtLB", "MdtOC", "Adr", "Phl", "Snd", "Mlk", "Mln")

# plot sorting against single trait FDis

  plot(fd, 100*Radj[6,],
       pch = shapes, col = palette, cex = 2, cex.lab = 1.2,
       ylim = c(0, 40),
       xlab = "Pool FDis",
       ylab = "[c] Species sorting (%)")

# correlation tests
cor.test(fd, 100*Radj[6,], alternative = "less")
