

# For saving figures ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


### HT NOTE FOR 2017

### FOr CMYK ouput see https://stackoverflow.com/a/17168202  or  https://stackoverflow.com/a/21958166 or http://onetipperday.sterding.com/2012/10/draw-figures-in-cmyk-mode-in-r.html and also https://en.99designs.ch/blog/tips/correct-file-formats-rgb-and-cmyk/

### bascially need to open a device (ps, eps, tiff or pdf) with colourmode set to CMYK. See ?Devices. Best use PDF me thinks.

### Also set dpi = 300 where possible for bitmapped images!  See ?Devices



figsave <- function(obj, data, name, width=11, height=7){


  # save PDF in CMYK colour mode for designer
  ggsave(filename=paste0(figures_folder, "/Figs/", name, Sys.Date(), ".pdf"),
         plot=obj,
         width=width,
         height=height,
         colormodel="cmyk")

  # # save EPS in CMYK colour mode for designer (testing)
  # ggsave(filename=paste0(figures_folder, "/Figs/", name, Sys.Date(), "_cmyk.eps"),
  #        plot=obj,
  #        device="eps",
  #        width=width,
  #        height=height,
  #        colormodel="cmyk")

  # save PNG for reviewer
  # ggsave(filename=paste0(figures_folder, "/Review/", name, ".png"),
  #        plot=obj,
  #        width=width,
  #        height=height)

  # save data for designer
  write.csv(data,
            file=paste(figures_folder, "/FigData/", name, Sys.Date(), ".csv", sep=""),
            row.names=FALSE,
            na="")

}


# For adding an x-axis to orphaned plots ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

facetAdjust <- function(x, pos = c("up", "down"))
{
  pos <- match.arg(pos)
  p <- ggplot_build(x)
  gtable <- ggplot_gtable(p); dev.off()
  dims <- apply(p$panel$layout[2:3], 2, max)
  nrow <- dims[1]
  ncol <- dims[2]
  panels <- sum(grepl("panel", names(gtable$grobs)))
  space <- ncol * nrow
  n <- space - panels
  if(panels != space){
    idx <- (space - ncol - n + 1):(space - ncol)
    gtable$grobs[paste0("axis_b",idx)] <- list(gtable$grobs[[paste0("axis_b",panels)]])
    if(pos == "down"){
      rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], "]"),
                   gtable$layout$name)
      lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
      gtable$layout[rows, c("t","b")] <- gtable$layout[lastAxis, c("t")]
    }
  }
  class(gtable) <- c("facetAdjust", "gtable", "ggplot"); gtable
}

# The function for printing which differs only by few lines from ggplot2:::print.ggplot:
print.facetAdjust <- function(x, newpage = is.null(vp), vp = NULL) {
  if(newpage)
    grid.newpage()
  if(is.null(vp)){
    grid.draw(x)
  } else {
    if (is.character(vp))
      seekViewport(vp)
    else pushViewport(vp)
    grid.draw(x)
    upViewport()
  }
  invisible(x)
}
