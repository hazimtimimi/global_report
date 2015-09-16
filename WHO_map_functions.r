# Tom Hiatt
# 8 May 2012
# Functions for creating WHO maps
# (later adapted by Philippe at https://github.com/glaziou/whomap)
#
# Requires WHO shapefiles in ~/Data/gparts.Rdata   (~ = scripts_folder)


# ----------------------------------------------------------
# A slide-worthy WHO map
# ----------------------------------------------------------

WHOmap.slide <- function(data, map.title="", legend.title="", low.color='#BDD7E7',  high.color='#08519C', na.label='No data') {

  # tests to make sure inputs are right
  if(nchar(legend.title)>25) warning("You might want to try and trim your legend title a bit.")
  if(nchar(legend.title)>10 & any(grep("\\n", legend.title))==FALSE) warning("You might want to wrap that legend title a couple of times with '\\n'.")
  if(nchar(map.title)>50 & any(grep("\\n", map.title))==FALSE) warning("You might want to try and trim your title a bit or wrap it with '\\n'.")
  if(max(nchar(levels(data[["cat"]]))) > 10) warning("Check if your categories are running into Australia. (I hate it when that happens.)")

  if(any(names(data) %in% c("iso3", "cat"))==FALSE) stop("I need the data with just two columns labeled 'iso3' and 'cat' (for category). The cut() function works well for continuous data.")
  if(!is.factor(data[["cat"]])) stop("I need you to make that cat column into a factor. (And order it in the order you want the legend to appear.)")

  #   OK let's get started!

  # Get and format shapefiles

  require(ggplot2)
  require(maptools)
  require(gpclib)
  require(grid)
  require(scales)

  # ----------------------------------------------------
  # Prepare map
  # ----------------------------------------------------

  load(file.path(scripts_folder,"/Data/gparts.Rdata"))


  # Generic map parts

  pol1 <- geom_polygon(data=gworld, aes(group = group), colour = "grey50", fill = NA)   # A layer to map all countries (so none are missing.)
  pol2 <- geom_polygon(data = subset(gpoly, id=="Lakes"), aes(group = group), fill = I("white"), colour = "grey50") 	# Adds the polygon layer
  pol3 <- geom_polygon(data = subset(gpoly, id=="Jammu and Kashmir"), aes(group = group), fill = I("grey75"), colour = "grey50")  # To color Jammu Kashmir
  pol4 <- geom_polygon(data = subset(gpoly, id=="Abyei"), aes(group = group), fill = I("grey75"), colour = "grey50", linetype="dotted")  # To color Abyei
  pol5 <-	geom_polygon(data = gworld[gworld$id=='ESH',], aes(group = group), fill = I("grey75"), colour = "grey50")  # To color Western Sahara
  lin1 <- geom_path(data = subset(gline, id %in% 2), aes(group = group), colour = "grey50") 					# solid black lines
  lin2 <- geom_path(data = subset(gline, id %in% c(0,3,6,7)), aes(group = group), colour = "white", linetype = "dashed") 	# dashed white and black lines
  lin3 <- geom_path(data = subset(gline, id %in% c(1,4,5)), aes(group = group), colour = "grey50", linetype = "dashed") 	# dashed black lines
  lin4 <- geom_path(data = subset(gline, id %in% c(8)), aes(group = group), colour = "white", linetype = "dotted")   # dotted white lines (8 and 9 are the same!)
  thm1 <- scale_y_continuous('', breaks = NULL)
  thm2 <- scale_x_continuous('', breaks = NULL)
  thm3 <- theme_bw()

  #   Get colors
  x <- seq(0, 1, length=length(levels(data[["cat"]])))

  colors <- c(seq_gradient_pal(low.color, high.color)(x), 'grey90', 'grey75')


  #   Merge data
  toplot <- merge(gworld, data, by.x = "id", by.y = "iso3", all.x=TRUE)
  toplot <- toplot[order(toplot$order), ]
  levels(toplot$cat) <- c(levels(toplot$cat), na.label, 'Not applicable')
  toplot[is.na(toplot$cat),"cat"] <- na.label
  toplot[toplot$id=="ESH","cat"] <- 'Not applicable'

  # ----------------------------------------------------
  # Plot map
  # ----------------------------------------------------


  windows (12,8)

  plot <-  ggplot(toplot, aes(long, lat)) +
    geom_polygon(aes(group=group, fill=cat)) +
    pol1+pol2+pol3+pol4+pol5+lin1+lin2+lin3+lin4+thm1+thm2+thm3+
    geom_polygon(aes(group=group, fill=cat), toplot[toplot$id %in% c('SWZ', 'LSO'),]) +
    scale_fill_manual(legend.title, values=colors) +
    coord_cartesian(xlim = c(-180, 180)) +
    opts(title = paste(map.title, "\n"), aspect.ratio = 2.2/4, plot.title=theme_text(size=30),
         legend.key.size = unit(0.75, "cm"), legend.text=theme_text(size=14),
         legend.position=c(0.65, 0.45), legend.justification= c(0,1),
         legend.title=theme_text(size=17, hjust=0), panel.border=theme_blank())
  print(plot)
  return(plot)

  cat("Here you go. You can save it with ggsave if you'd like.")

}

# ----------------------------------------------------------
# A print-worthy WHO map
# ----------------------------------------------------------

WHOmap.print <- function(data, map.title="", legend.title="", colors=NULL, low.color='#BDD7E7',  high.color='#08519C', shapefiles.path=NULL, na.label='No data', copyright=TRUE, show=TRUE, line.color="grey50", zoom='Global') {

  # tests to make sure inputs are right
  if(nchar(legend.title)>45) warning("You might want to try and trim your legend title a bit.")
  if(nchar(legend.title)>25 & any(grep("\\n", legend.title))==FALSE) warning("You might want to wrap that legend title a couple of times with '\\n'.")
  if(nchar(map.title)>100 & any(grep("\\n", map.title))==FALSE) warning("You might want to try and trim your title a bit or wrap it with '\\n'.")
  if(max(nchar(levels(data[["cat"]]))) > 20 & !any(grep('\n', levels(data[["cat"]])))) warning("Check if your categories are running into Australia. (I hate it when that happens.)")

  if(any(names(data) %in% c("iso3", "cat"))==FALSE) stop("I need the data with just two columns labeled 'iso3' and 'cat' (for category).")
  if(!is.factor(data[["cat"]])) stop("I need you to make that cat column into a factor. (And order it in the order you want the legend to appear.) The cut() function works well for continuous data.")
  if(length(levels(data[["cat"]])) > 5 & copyright==TRUE) warning("You probably noticed your legend is dancing with the copyright. I can do up to 5 categories excluding NA.")

  #   OK let's get started!

  # Get and format shapefiles

  require(ggplot2)
  require(maptools)
  require(grid)
  require(scales)

  # ----------------------------------------------------
  # Prepare map
  # ----------------------------------------------------

  load(file.path(scripts_folder,"/Data/gparts.Rdata"))

  # Add in missing circles for ASM, PYF, MNP, WLF
  asm <- subset(gworld, id=="WSM") ; asm$id <- "ASM" ; asm$group <- "ASM.1" ; asm$long <- asm$long + 2 ; asm$lat <- asm$lat - 0.5
  pyf <- subset(gworld, id=="COK") ; pyf$id <- "PYF" ; pyf$group <- "PYF.1" ; pyf$long <- pyf$long + 10 ; pyf$lat <- pyf$lat + 1
  mnp <- subset(gworld, id=="GUM") ; mnp$id <- "MNP" ; mnp$group <- "MNP.1" ; mnp$long <- mnp$long + 0.5 ; mnp$lat <- mnp$lat + 2
  wlf <- subset(gworld, id=="WSM") ; wlf$id <- "WLF" ; wlf$group <- "WLF.1" ; wlf$long <- wlf$long - 5 ; wlf$lat <- wlf$lat - 0.2

  gworld <- rbind(gworld, asm, pyf, mnp, wlf)

  # Bring WPRO islands over to the other side if needed.
  if(zoom=='WPR') {
    lefties <- c("COK", "NIU", "TON", "WSM", "TKL", "ASM", "PYF", "WLF")
    gworld[gworld$id %in% lefties, "long"] <- gworld[gworld$id %in% lefties, "long"] + 360
  }
  
  # Color Svalbard and Jan Mayen the same as Norway
  gworld[gworld$group=="SJM.1", "piece"] <- "2"
  gworld[gworld$group=="SJM.2", "piece"] <- "3"
  gworld[gworld$group=="SJM.3", "piece"] <- "4"
 
  gworld[gworld$id=="SJM", "id"] <- "NOR"
  
  levels(gworld$group) <- c(levels(gworld$group), "NOR.2", "NOR.3", "NOR.4")
  gworld[gworld$group=="SJM.1", "group"] <- "NOR.2"
  gworld[gworld$group=="SJM.2", "group"] <- "NOR.3"
  gworld[gworld$group=="SJM.3", "group"] <- "NOR.4"
  
  
  # Generic map parts

#   drop lines that would be whited out.
  dashiso3s <- c("EGY", "ISR", "KOR", "PRK", "PSE", "SDN", "SSD")
  gworldndash <- subset(gworld, !id %in% dashiso3s)
  gworlddash <- subset(gworld, id %in% dashiso3s)
  gworlddash$group2 <- as.character(gworlddash$group)

  SSD <- subset(gworlddash, !(long > 25 & lat < 13 & long < 34 & lat > 9) & id=='SSD')
  SSD[24:27,'group2'] <- 'SSD.1.2'
  SDN <- subset(gworlddash, !(long > 25 & lat < 13 & long < 34 & lat > 9) & !(long > 33.2 & lat < 23 & long < 35 & lat > 21.5) & id=='SDN')
  SDN[14:31,'group2'] <- 'SDN.1.2'
  SDN[32:33,'group2'] <- 'SDN.1.3'
  EGY <- subset(gworlddash, !(long > 33.2 & lat < 23 & long < 35 & lat > 21.5) & id=='EGY')
  EGY[13:15,'group2'] <- 'EGY.1.2'
  ISR <- subset(gworlddash, !(long > 34.8 & lat < 32.6 & long < 35.4 & lat > 31.3) & !(long > 34.5 & lat < 31.55 & long < 34.6 & lat > 31.5) & id=='ISR')
  ISR[7:15,'group2'] <- 'ISR.1.2'
  ISR[16:18,'group2'] <- 'ISR.1.3'
  PSE <- subset(gworlddash, id=='PSE')
  PSE <- PSE[16:17,]
  KOR <- subset(gworlddash, !(long > 127 & lat < 38.5 & long < 127.5 & lat > 38) & id=='KOR')
  KOR <- KOR[1:10,]
  PRK <- subset(gworlddash, !(long > 127 & lat < 38.5 & long < 127.5 & lat > 38) & id=='PRK')
  PRK[5:12,'group2'] <- 'PRK.1.2'

  gworlddash2 <- rbind(SSD, SDN, EGY, ISR, PSE, KOR, PRK)

  # Create solid lines for Jammu Kashmir
  jk1 <- subset(gpoly, id=="Jammu and Kashmir")
  jk1$group2 <- as.character(jk1$group)
  jk1[1:2,'group2'] <- 'Jammu and Kashmir.2'
  jk1[8:16,'group2'] <- 'Jammu and Kashmir.3'
  jk1[21:22,'group2'] <- 'Jammu and Kashmir.4'
  jk2 <- subset(jk1, group2!='Jammu and Kashmir.1')

  pol1 <- geom_polygon(data=gworldndash, aes(group = group), colour = line.color, fill = NA)   # A layer to map all countries (so none are missing.)
  lin0 <- geom_path(data = gworlddash2, aes(group = group2), colour = line.color)
  pol2 <- geom_polygon(data = subset(gpoly, id=="Lakes"), aes(group = group), fill = I("white"), colour = line.color) 	# Adds the polygon layer
  pol3 <- geom_polygon(data = subset(gpoly, id=="Jammu and Kashmir"), aes(group = group), fill = I("grey75"), colour = NA)  # To color Jammu Kashmir
  pol4 <- geom_polygon(data = subset(gpoly, id=="Abyei"), aes(group = group), fill = I("grey75"), colour = line.color, linetype="dotted")  # To color Abyei
  pol5 <-	geom_polygon(data = gworld[gworld$id=='ESH',], aes(group = group), fill = I("grey75"), colour = line.color)  # To color Western Sahara
  lin1 <- geom_path(data = subset(gline, id %in% 2), aes(group = group), colour = line.color) 					# solid black lines
  lin2 <- geom_path(data = subset(gline, id %in% c(0,3,6,7)), aes(group = group), colour = line.color, linetype = "dashed") 	# dashed white and black lines (now modified to be dashed lines over color of country)
  lin3 <- geom_path(data = subset(gline, id %in% c(1,4,5)), aes(group = group), colour = line.color, linetype = "dashed") 	# dashed black lines
#   lin4 <- geom_path(data = subset(gline, id %in% c(8)), aes(group = group), colour = "white", linetype = "dotted")   # dotted white lines (8 and 9 are the same!) I'm replacing this with a new line 4...
  lin4 <- geom_path(data = jk2, aes(group = group2), colour = line.color)
  thm1 <- scale_y_continuous('', breaks = NULL)
  thm2 <- scale_x_continuous('', breaks = NULL)
  thm3 <- theme_bw()

#   Copyright text
  cright <- ifelse(copyright==FALSE, "", "\uA9 World Health Organization 2015. All rights reserved.
  The designations employed and the presentation of the material in this publication do not
  imply the expression of any opinion whatsoever on the part of the World Health Organization
  concerning the legal status of any country, territory, city or area or of its authorities,
  or concerning the delimitation of its frontiers or boundaries. Dotted and dashed lines on
  maps represent approximate borderlines for which there may not yet be full agreement.")

  #   Get colors

  if(!is.null(colors) & length(levels(data[["cat"]]))!=length(colors)) stop('Your cats and colors don\'t match.')
  if(is.null(colors)){
    x <- seq(0, 1, length=length(levels(data[["cat"]])))
    x1 <- seq_gradient_pal(low.color, high.color)(x)
  } else x1 <- colors

  colors2 <- c(x1, 'grey90', 'grey75')

  # Get dimensions
  if(zoom=='Global'){
    leg.pos <- c(0.73, 0.41)
    zoomx <- c(-180, 180)
    zoomy <- c(min(gworld$lat), max(gworld$lat))
    a.ratio = 2.2/4
  }   else
  if(zoom=='WPR'){
    leg.pos <- c(0.83, 0.95)
    zoomx <- c(70, 212)
    zoomy <- c(-50, 55)
    a.ratio = 3.5/4
     } else stop(paste(zoom, "is not on my list of zoom level options."))

  #   Merge data
  toplot <- merge(gworld, data, by.x = "id", by.y = "iso3", all.x=TRUE)
  toplot <- toplot[order(toplot$order), ]
  levels(toplot$cat) <- c(levels(toplot$cat), na.label, 'Not applicable')
  toplot[is.na(toplot$cat),"cat"] <- na.label
  toplot[toplot$id=="ESH","cat"] <- 'Not applicable'

  # ----------------------------------------------------
  # Plot map
  # ----------------------------------------------------

  plot <-  ggplot(toplot, aes(long, lat)) +
    geom_polygon(aes(group=group, fill=cat), colour=NA) +
    pol1+pol2+pol3+pol4+pol5+lin0+lin1+lin2+lin3+lin4+thm1+thm2+thm3+
    geom_polygon(aes(group=group, fill=cat), toplot[toplot$id %in% c('SWZ', 'LSO'),]) +
    scale_fill_manual(legend.title, values=colors2) +
    coord_cartesian(xlim = zoomx, ylim=zoomy) + labs(title = map.title) +

    theme(aspect.ratio = a.ratio, plot.title=element_text(size=16, hjust=0),
         legend.key.size = unit(0.50, "cm"), legend.text=element_text(size=8),
         legend.position=leg.pos, legend.justification= c(0.5,1),
         legend.title=element_text(size=10, hjust=0), rect=element_blank()) +
           annotate("text", 70, -54, label=cright, size=2, hjust=0)
           if(show==TRUE) {
             windows (12,8)
             print(plot)
           }

  return(plot)

  cat("Here you go. You can save it with ggsave if you'd like.")

}