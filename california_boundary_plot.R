
CaliforniaBoundaryPlot <- function(coords,
                                   ori_data_file,
                                   syn_data_files,
                                   ori_data_path,
                                   syn_data_path,
                                   my_mfrow = c(3, 3),
                                   my_mar = c(4, 3, 1, 0.4),
                                   my_mgp = c(2, 0.75, 0),
                                   methods_names,
                                   n_points = NULL,
                                   leg_cex = 1,
                                   leg_pos = "topright",
                                   lab_at = -124,
                                   lab_cex = 1,
                                   point_cex_real = 0.5,
                                   point_cex_syn = 0.5) {
  idx <- which(coords[, "L2"] == 1)
  odat <- read.csv(paste0(ori_data_path, ori_data_file))
  fm_real <- lm(Latitude ~ Longitude, data = odat)
  n_methods <- length(syn_data_files)
  
  n <- nrow(odat)
  if (!is.null(n_points)) {
    idx_p <- sample(n, n_points, replace = FALSE)
  }
  else {
    idx_p <- seq(n)
  }
  
  par(mfrow = my_mfrow, mar = my_mar, mgp = my_mgp)
  for (i in seq(n_methods)) {
    sdat <- read.csv(paste0(syn_data_path, syn_data_files[i]))
    fm_syn <- lm(Latitude ~ Longitude, data = sdat)
    plot(coords[idx, "X"], coords[idx, "Y"], type = "l",
         xlab = "Longitude", ylab = "Latitude", lwd = 2,
         main = methods_names[i])
    points(odat$Longitude[idx_p], odat$Latitude[idx_p], 
           cex = point_cex_real, col = rgb(0, 0, 1, 0.25))
    points(sdat$Longitude[idx_p], sdat$Latitude[idx_p], 
           cex = point_cex_syn, col = rgb(1, 0, 0, 0.25))
    legend(leg_pos, c("synth", "real"), text.col = c("red", "blue"), 
           bty = "n", cex = leg_cex)
    mtext(side = 3, paste0("(", letters[i], ")"), at = lab_at, cex = lab_cex)
    abline(a = fm_real$coefficients[1], b = fm_real$coefficients[2], 
           col = "blue", lwd = 3)
    abline(a = fm_syn$coefficients[1], b = fm_syn$coefficients[2], 
           col = "red", lwd = 2)
  }
}



methods_names <- c("TabSDS",
                   "ddpm",
                   "arf",
                   "tvae",
                   "ctgan",
                   "bayesnet")

ori_data_file <- "train_set.csv"
syn_data_files <- c("syn_tab_sjppds_200_0.5.csv",
                    "syn_ddpm.csv",
                    "syn_arf.csv",
                    "syn_tvae.csv",
                    "syn_ctgan.csv",
                    "syn_bayesnet.csv")
ori_data_path <- "~/outputs/california_housing_original/simulated_datasets/"
syn_data_path <- "~/outputs/california_housing_original/simulated_datasets/"

manus_path <- ""


###########################################################
## California boundary plots
###########################################################

## get boundary coordinates for California state

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Get US states' geometries
states <- ne_states(country = "United States of America", returnclass = "sf")

# Filter California
california <- states[states$name == "California", ]

# Extract coordinates
coords <- st_coordinates(california)

my_mfrow <- c(2, 3)
my_mgp <- c(2, 0.75, 0)

#pdf(file = paste0(manus_path, "ca_boundary_plot.pdf"), width = 6, height = 4.5)
set.seed(12345)
CaliforniaBoundaryPlot(coords,
                       ori_data_file,
                       syn_data_files,
                       ori_data_path,
                       syn_data_path,
                       my_mfrow = my_mfrow,
                       my_mar = c(3, 3, 1, 0.2),
                       my_mgp = my_mgp,
                       methods_names = methods_names,
                       point_cex_real = 1,
                       point_cex_syn = 1,
                       n_points = 1000)
#dev.off()

