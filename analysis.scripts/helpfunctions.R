simp.plot <- function (x, min.vi = 0.5, main = NULL, cex.axis = 1, cex.names = 1, 
                       cex.main = 1, maxval = NULL, minval = NULL, col.ramp = NULL, 
                       ...) 
{
  library(plotrix)
  
  colnames(x) <- c("estimates", "SE", "vi")
  data <- x
  
  
  
  data <- x[x$vi >= min.vi, ]
  par(mar = c(2, 2, 2, 6))
  if (is.null(col.ramp)) 
    col.ramp <- heat.colors(100)[100:1]
  if (is.null(maxval)) {
    maxval <- max(as.numeric(data$estimates) + as.numeric(data$SE))
    if (maxval < 0) 
      maxval <- 0
  }
  if (is.null(minval)) {
    minval <- min(as.numeric(data$estimates) - as.numeric(data$SE))
    if (minval > 0) 
      minval <- 0
  }
  if (is.null(main)) 
    main <- "Model Weighted Averages and Unconditional SE"
  mp <- barplot(as.numeric(data$estimates), names.arg = row.names(data), 
                col = col.ramp[round(100 * as.numeric(data$vi))], ylim = c(minval - 
                                                                             0.4 * abs(minval), maxval + 0.4 * abs(maxval)), main = main, 
                cex.axis = cex.axis, cex.names = cex.names, cex.main = cex.main)
  segments(mp, as.numeric(data$estimates) - as.numeric(data$SE), 
           mp, as.numeric(data$estimates) + as.numeric(data$SE), 
           lwd = 2)
  segments(mp - 0.1, as.numeric(data$estimates) - as.numeric(data$SE), 
           mp + 0.1, as.numeric(data$estimates) - as.numeric(data$SE), 
           lwd = 2)
  segments(mp - 0.1, as.numeric(data$estimates) + as.numeric(data$SE), 
           mp + 0.1, as.numeric(data$estimates) + as.numeric(data$SE), 
           lwd = 2)
  locs <- par("usr")
  color.legend(locs[2], locs[4] - ((locs[4] - locs[3]) * 0.5), 
               locs[2] + (locs[2] * 0.05), locs[4], legend = c("0.00", 
                                                               "0.25", "0.50", "0.75", "1"), 
               rect.col = col.ramp, cex = 0.6, align = "rb", gradient = "y")
  par(xpd = TRUE)
  text(x = (((locs[2]) + (locs[2] + (locs[2] * 0.05)))/2), 
       y = locs[4], labels = "Variable\nImportance", cex = 0.5, 
       pos = 3)
}
