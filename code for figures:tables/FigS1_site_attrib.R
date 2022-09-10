# Making histograms of site attributes
library(ggplot2)
library(cowplot)
library(plyr)
theme_set(theme_cowplot())

site <- read.csv("site_attributes.csv")

mu_slope <- ddply(site, "SITE", summarise, grp.mean = mean(SLOPE))
slope_plot <- ggplot(site, aes(x = site$SLOPE, color = SITE)) + 
  geom_histogram(fill = "white", alpha = 0.5, position = "identity",
                 binwidth = 1) + 
  geom_vline(data =mu_slope, aes(xintercept = grp.mean, color = SITE), 
             linetype = "dashed" ) + 
  scale_color_manual(name = "Site",
                     values = c("#000000", "#bdbdbd"), 
                     labels = c("Lowland", "Upland")) +
  labs(x = "Slope (Degrees)", y = "Frequency", title = "Slope") + 
  theme(legend.position = "none")

mu_elev <- ddply(site, "SITE", summarise, grp.mean = mean(ELEVATION))
elev_plot <- ggplot(site, aes(x = site$ELEVATION, color = SITE)) + 
  geom_histogram(fill = "white", alpha = 0.5, position = "identity",
                 binwidth = 25) + 
  geom_vline(data =mu_elev, aes(xintercept = grp.mean, color = SITE), 
             linetype = "dashed" ) + 
  scale_color_manual(name = "Site",
                     values = c("#000000", "#bdbdbd"), 
                     labels = c("Lowland", "Upland")) +
  labs(x = "Elevation (m)", y = " ", title = "Elevation") + 
  theme(legend.position = "none")

mu_solar <- ddply(site, "SITE", summarise, grp.mean = mean(SOLAR))
sol_plot <- ggplot(site, aes(x = site$SOLAR, color = SITE)) + 
  geom_histogram(fill = "white", alpha = 0.5, position = "identity",
                 binwidth = 2500) + 
  geom_vline(data =mu_solar, aes(xintercept = grp.mean, color = SITE), 
             linetype = "dashed" ) + 
  scale_color_manual(name = "Site",
                     values = c("#000000", "#bdbdbd"), 
                     labels = c("Lowland", "Upland")) +
  labs(x = "Solar Radiation (W/m2)", y = " ", title = "Solar Radiation")

site_attrib_plot <- plot_grid(slope_plot, elev_plot, sol_plot, 
          nrow = 1, rel_widths = c(1,1,1.5),
          labels = c("A.", "B.", "C."))
site_attrib_plot
save_plot("site_attrib.png", site_attrib_plot, nrow = 1, ncol = 3,
          base_aspect_ratio = 1.5)
