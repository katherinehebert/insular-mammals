# Functions to make plots for lmm building and validation

# Map residual values in space
resid_map <- function(model, data = perisland){
  df <- data.frame(
    res = resid(model),
    x = data$Long,
    y = data$Lat,
    group = data$group.x
  )
  limit <- max(abs(df$res)) * c(-1, 1)
  resid_map <- ggplot(df) +
    geom_point(aes(x=x, y=y, col = res)) +
    facet_wrap(~group, scales = "free") +
    scale_color_distiller(palette = "RdYlBu", limit = limit) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
    scale_x_continuous(labels = scales::number_format(accuracy = 0.1))+
    theme(axis.text = element_text(size = 10),
          legend.position = "bottom") +
    labs(x = "Latitude (scaled)", y = "Longitude (scaled)", col = "Residual value") 
  return(resid_map)
}

# Plot residuals vs. fitted values
resid_fit <- function(model){
  df <- data.frame(res = resid(model), fit = fitted(model))
  resid_fit <- ggplot(df) + 
    geom_point(aes(x = fit, y = res)) + 
    geom_hline(yintercept = 0, lty = 2) +
    labs(x = "Fitted values", y = "Residuals")
  return(resid_fit)
}

# check normality
resid_normal <- function(model){
  resid_normal <- ggplot(data.frame("resids" = resid(model))) + 
    geom_histogram(aes(x = resids), bins = 10) +
    labs(x = "Residuals", y = "Frequency")
  return(resid_normal)
}

# calculate spatial autocorrelation
morans_i <- function(perisland, model){
  coor.dist <- as.matrix(dist(cbind(perisland$Long, perisland$Lat)))
  coor.dist.inv <- 1/coor.dist
  diag(coor.dist.inv) <- 0
  res <- resid(model)
  ape::Moran.I(res, coor.dist.inv) # no spatial autocorrelation
}

# function to plot coefficients from linear mixed model
lmm_coeff_plot <- function(lmm0_out, pooldiv_label, plot_title, ylimits = c(-.5, .5)){
  # remove rows that shouldn't be plotted 
  lmm0_coeffplot = lmm0_out[-grep("(Intercept)", lmm0_out$term),]
  lmm0_coeffplot = lmm0_coeffplot[-grep("sd", lmm0_coeffplot$term),]
  # create significance factor variable
  lmm0_coeffplot$sig = "p > 0.05"
  lmm0_coeffplot$sig[which(lmm0_coeffplot$p.value <= 0.05)] = "p ≤ 0.05"
  lmm0_coeffplot <- na.omit(lmm0_coeffplot)
  
  lmm0_coeffplot$term <- gsub("pool", pooldiv_label, lmm0_coeffplot$term)
  lmm0_coeffplot$term <- gsub("isol_PC1", "isolPC1", lmm0_coeffplot$term)
  lmm0_coeffplot$term <- gsub("sp_rich", "TR", lmm0_coeffplot$term)
  lmm0_coeffplot$term <- gsub("sesmpd", "sesMPD", lmm0_coeffplot$term)
  lmm0_coeffplot$term <- factor(lmm0_coeffplot$term,
                                levels = rev(c("topoPC1", "envPC1", "envPC2", "isolPC1", "PC1", "PC2", "PC3",
                                               "Area", "Elev", "Prec", "varP", "SLMP",
                                               "GMMC1", "TR", "sesMPD")))
  lmm0_coeffplot$sig <- factor(lmm0_coeffplot$sig, levels = c("p > 0.05", "p ≤ 0.05"))
  
  sigcolor <- c("p ≤ 0.05" = "#0072B2", "p > 0.05" = "grey40")
  
  # plot coefficients
  p <- ggplot(lmm0_coeffplot, aes(x = term, y = estimate, col = sig)) + # without intercept
    geom_hline(aes(yintercept = 0), lwd = .4, lty = 2, col = "grey") +
    geom_pointrange(aes(ymin=estimate-1.96*std.error,
                        ymax=estimate+1.96*std.error)) +
    scale_color_manual(values = sigcolor) +
    labs(y = "Coefficient estimate", 
         title = plot_title) +
    ylim(ylimits) + 
    coord_flip() +
    theme(axis.title.y = element_blank(),
          legend.position = c(0.87, 0.92),
          legend.text = element_text(size = 11),
          legend.background = element_blank(),
          legend.title = element_blank())
  return(p)
}
