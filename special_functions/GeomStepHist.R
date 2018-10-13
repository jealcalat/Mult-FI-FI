
GeomStepHist <- ggproto("GeomStepHist", GeomPath,
                        required_aes = c("x"),
                        
                        draw_panel = function(data, panel_scales, coord, direction) {
                          data <- as.data.frame(data)[order(data$x), ]
                          
                          n <- nrow(data)
                          i <- rep(1:n, each=2)
                          newdata <- rbind(
                            transform(data[1, ], x=x - width/2, y=0),
                            transform(data[i, ], x=c(rbind(data$x-data$width/2, data$x+data$width/2))),
                            transform(data[n, ], x=x + width/2, y=0)
                          )
                          rownames(newdata) <- NULL
                          
                          GeomPath$draw_panel(newdata, panel_scales, coord)
                        }
)


geom_step_hist <- function(mapping = NULL, data = NULL, stat = "bin",
                           direction = "hv", position = "stack", na.rm = FALSE, 
                           show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStepHist,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      direction = direction,
      na.rm = na.rm,
      ...
    )
  )
}