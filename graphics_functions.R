with.caption <- function(plot, caption, gp=gpar(fontsize=8)) {
  wrap <- paste(strwrap(caption, width=80), collapse="\n")
  text <- textGrob(wrap, just="left", x=0)
  fr <- frameGrob(name="fr")
  fr <- packGrob(fr, text, side="bottom")
  fr <- packGrob(fr, ggplotGrob(plot), side="top")
  fr
}
