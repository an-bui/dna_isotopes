getColorsa<- function(groups, levels){
  brn1 <- c("#2D1A03", "#714108", "#D87D0E")
  brn2 <- c("#543005", "#985709", "#F2911D")
  brn3 <- c("#8C510A", "#D77C0F", "#F3AF5D")
  brn4 <- c("#BF812D", "#D8A157", "#E4BE8B")
  brn5 <- c("#DFC27D", "#EBD8AC", "#F4EAD3")
  teal1 <- c("#80CDC1",  "#A5DBD3",  "#CFECE8")
  teal2 <- c("#35978F", "#4ABFB6", "#97DAD5")
  teal3 <- c("#01665E", "#02A296", "#0AFDE9")
  teal4 <- c("#003C30", "#007A62", "#00DFB3")
  teal5 <- c("#001511", "#00614E", "#008C71")
  colorList<- list(brn1, brn2, brn3, brn4, brn5,
                   teal1, teal2, teal3, teal4, teal5)
  #create vector to hold ordered colors
  fill<- character()
  for(i in 1:groups){
    fill<- c(fill, colorList[[i]][1:levels])
  }
  return(fill)
}


# Plot Kin ----------------------------------------------------------------

#' Plotting function for rKIN polygons
#'
#' Using ggplot2 methods, simultaneously plot all of the groups and levels of niche space
#'
#' @param estObj list object created from estKIN, estMCP or estEllipse functions
#' @param scaler numeric value indicating number of isotopic units to expand the x and y axes of the plot. Default is 1.
#' @param alpha numeric value between 0 and 1, representing the amount of transparency of each polygon. 0 is transparent, 1 is opaque.
#' @param title character string for a plot title.
#' @param xlab character or expression string for the x-axis label.
#' @param ylab character or expression string for the y-axis label.
#' @return A plot of all groups and levels.
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming
#' @export
#' @import maptools
#' @import rgeos
#' @import ggplot2
#' @examples
#' library(rKIN)
#' data("rodents")
#' #estimate niche overlap between 2 species using kernel UD
#' test.kin<- estKIN(data=rodents, x="Ave_C", y="Ave_N", group="Species",
#'                    levels=c(50, 75, 95), scaler=2)
#' #determine polygon overlap for all polygons
#' plotKIN(test.kin, scaler = 1, title = "Kernel Estimates",
#'          xlab = expression({delta}^13*C~ ('\u2030')),
#'          ylab = expression({delta}^15*N~ ('\u2030')))

plotKIN<- function(estObj, scaler = 1, alpha = 0.3, title = "", xlab = "x", ylab = "y"){
  requireNamespace("maptools")
  #requireNamespace("rgeos")
  #library(maptools)
  #//////////////////////////
  # NEED TO CHECK FOR PROPER OBJECT TYPES
  if(!inherits(estObj$estObj, "estObj"))
    stop("estObj must be of class estObj created from estEllipse, estKIN, or estMCP functions!")
  if(!inherits(scaler, "numeric"))
    stop("scaler must be numeric!")
  if(!inherits(alpha, "numeric"))
    stop("alpha must be numeric!")
  if(alpha > 1 | alpha < 0)
    stop("alpha must be a numeric value between 0 and 1!")
  if(!inherits(title, "character"))
    stop("title must be a character!")
  if( !inherits(xlab, "character"))
    if(!inherits(xlab, "expression"))
      stop("xlab must be a character or an expression!")
  if(!inherits(ylab, "character"))
    if(!inherits(ylab, "expression"))
      stop("ylab must be a character or an expression!")
  
  # Get the ConfInt to be sorted descending for ggplot stuff
  ord<- unique(estObj$estObj[[1]]@data$ConfInt)[order(unique(estObj$estObj[[1]]@data$ConfInt), decreasing = TRUE)]
  #get the min/max extent of all SPDF
  xs<- numeric()
  ys<- numeric()
  df<- list()
  
  #Loop through the polygons
  for(i in 1:length(estObj$estObj)){
    xs<- c(xs, sp::bbox(estObj$estObj[[i]])[1, ])
    ys<- c(ys, sp::bbox(estObj$estObj[[i]])[2, ])
    # Create new column to set the drawing order in ggplot, largest CI first
    for(j in 1:length(ord)){
      estObj$estObj[[i]]@data$PlotOrder[estObj$estObj[[i]]@data$ConfInt==ord[j]]<- j
    }# close j loop
    gdf<- ggplot2::fortify(estObj$estObj[[i]], region = "PlotOrder")
    gdf<- merge(gdf, estObj$estObj[[i]]@data, by.x = "id", by.y = "PlotOrder")
    gdf$Group_ConfInt<- paste(gdf$Group, gdf$ConfInt, sep = "_")
    df<- c(df, list(gdf))
  }# close i loop
  #loop through the input points
  pts<- list()
  for(i in 1:length(estObj$estInput)){
    #place all points into data.frame list for plotting
    #pf<- ggplot2::fortify(estObj$estInput[[i]], region = "Group")
    
    pts<- c(pts, list(estObj$estInput[[i]]@data))
    #store all coordinates for later use
    xs<- c(xs, estObj$estInput[[i]]@data[ , 3])
    ys<- c(ys, estObj$estInput[[i]]@data[ , 4])
  }# close i loop
  
  gdf <- gdf %>%
    mutate(Group = factor(Group, levels = c("Castor", "Fern", 
                                            "Paradise", "Holei",
                                            "Kaula", "Dudley",
                                            "Leslie", "Lost",
                                            "Sand", "Eastern")))
  
  # make a plot using ggplot2
  kin.plot<- ggplot2::ggplot() +
    lapply(df, function(x) ggplot2::geom_polygon(data = x, alpha = alpha, ggplot2::aes_string(x = "long", y = "lat", fill = "Group_ConfInt", group = "group"))) +
    ggplot2::scale_fill_manual(values=getColorsa(length(df), length(ord))) +
    #ggplot2::geom_point(data = pts, aes_string(x = names(pts)[3], y = names(pts)[4], colour = "Group", shape = "Group")) +
    lapply(pts, function(x) ggplot2::geom_point(data = x, ggplot2::aes_string(x = names(x)[3], y = names(x)[4], colour = "Group"))) +
    ggplot2::scale_color_manual(values = getColorsa(length(df), 1)) +
    ggplot2::coord_fixed(ratio = ((max(xs) + scaler) - (min(xs) - scaler))/((max(ys) + scaler) - (min(ys) - scaler)),
                         xlim = c((min(xs) - scaler), (max(xs) + scaler)),
                         ylim = c((min(ys) - scaler), (max(ys) + scaler))) +
    ggplot2::scale_x_continuous(breaks = seq(from = round((min(xs) - scaler)), to = round((max(xs) + scaler)), by = scaler)) +
    ggplot2::scale_y_continuous(breaks = seq(from = round((min(ys) - scaler)), to = round((max(ys) + scaler)), by = scaler)) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = alpha))) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border= ggplot2::element_rect(fill = NA, color = "black"),
                   plot.title = element_text(hjust = 0.5))
  # return the plot
  return(kin.plot)
  
}# close function


