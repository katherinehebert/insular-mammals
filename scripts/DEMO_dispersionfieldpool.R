# demonstration of dispersion field
require(sf)
require(tidyverse)

# import worldwide grid with only the cells holding >= 1 species in common with metacommunity
g1 = read_sf("~/Documents/M. Sc./Manuscript/R/Species pools/Grid shapefiles/Per archipelago/Full metacommunity/Alx _fullmeta_grid.shp")
p1 = ggplot() + 
      geom_sf(data = g1, aes(fill = commonsp), lwd = 0) + 
      ggtitle("1. Cells with >= 1 species in common with metacommunity") +
      labs(fill = "Species in common") +
      theme_classic()
p1

# remove widespread species like Graves & Gotelli (1983)
# widespread: species who occupy > 200 1x1 grid cells
# restricted: species who occupy < 200 1x1 grid cells
g2 = read_sf("~/Documents/M. Sc./Manuscript/R/Species pools/Grid shapefiles/Per archipelago/Restricted species/Alx_restricted_grid.shp")
p2 = ggplot() + 
  geom_sf(data = g1, aes(fill = commonsp), lwd = 0) + 
  geom_sf(data = g2, fill = "yellow", lwd = 0) + 
  ggtitle("2. Cells after removing widespread* species",
          subtitle = "*widespread: species occupying > 200 1x1 grid cells") +
  labs(fill = "Species in common") +
  theme_classic()
p2

# keep 25% of the closest cells (in climate space)
g3 = read_sf("~/Documents/M. Sc./Manuscript/R/Species pools/Grid shapefiles/Per archipelago/Restricted species/Bioclim means/Restricted and Env25p/Alx_Restricted_Env25p.shp")
p3 = ggplot() + 
  geom_sf(data = g1, aes(fill = commonsp), lwd = 0) + 
  geom_sf(data = g2, fill = "yellow", lwd = 0) + 
  geom_sf(data = g3, fill = "red", lwd = 0) +
  ggtitle("3. Cells after keeping 25% most environmentally similar cells",
          subtitle = "i.e. cells with closest mean climate to mean metacommunity climate") +
  labs(fill = "Species in common") +
  theme_classic()
p3

# Climate space 

# import mean bioclimatic values per retained grid cell
c1 = read_sf("~/Documents/M. Sc./Manuscript/R/Species pools/Grid shapefiles/Per archipelago/Restricted species/Bioclim means/Alx_MeanBioclim.shp")
# amt
c1a = ggplot() + 
  geom_sf(data = g1, fill = "lightgrey", lwd = 0) +
  geom_sf(data = c1, aes(fill = bio1_temp), lwd = 0) + 
  labs(fill = "Annual mean temperature (°C)") +
  scale_fill_viridis_c() + ylim(c(10,90)) +
  theme_classic()
c1a
#varT
c1b = ggplot() + 
  geom_sf(data = g1, fill = "lightgrey", lwd = 0) +
  geom_sf(data = c1, aes(fill = bio4_varT), lwd = 0) + 
  labs(fill = "Temperature seasonality (°C)") +
  scale_fill_viridis_c() + ylim(c(10,90)) +
  theme_classic()
c1b
#prec
c1c = ggplot() + 
  geom_sf(data = g1, fill = "lightgrey", lwd = 0) +
  geom_sf(data = c1, aes(fill = bio12_prec), lwd = 0) + 
  labs(fill = "Annual precipitation (mm)") +
  scale_fill_viridis_c() + ylim(c(10,90)) +
  theme_classic()
c1c
#varP
c1d = ggplot() + 
  geom_sf(data = g1, fill = "lightgrey", lwd = 0) +
  geom_sf(data = c1, aes(fill = bio15_varP), lwd = 0) + 
  labs(fill = "Precipitation seasonality (mm)") +
  scale_fill_viridis_c() + ylim(c(10,90)) +
  theme_classic()
c1d
# arrange on one page
ggpubr::ggarrange(c1a, c1b, c1c, c1d,
                 labels = c("A", "B", "C", "D"),
                 nrow = 4, ncol = 1)
