with.caption <- function(plot, caption, gp=gpar(fontsize=8)) {
  wrap <- paste(strwrap(caption, width=80), collapse="\n")
  
  plotGrob <- editGrob(ggplotGrob(plot), name="plot")
  text <- textGrob(wrap, hjust=0, vjust=1, x=0, y=1, name="caption")

  fr <- frameGrob(name="frame")
  subframe <- frameGrob(name="subframe")
  subframe <- packGrob(subframe, text, height=grobHeight(text) + unit(4, "mm"));
  fr <- packGrob(fr, ggplotGrob(plot), side="top", width=1, force.width=TRUE)
  fr <- packGrob(fr, subframe, side="bottom")
}
