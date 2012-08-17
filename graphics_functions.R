suppressPackageStartupMessages({
  source("helper_functions.R")
})

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

pmetric_plot <- function(measurements, data, fit, output, sim,
                         use_folded, average_bias, abs_bias, one_sided, ...) {
  extra.vars <- list(...);

  data <- mutate(data, seq <- order(trials_i))
  if(use_folded) {
    rates <- ddply(data, .(folded_displacement), with
                   , c(  displacement   = folded_displacement[1]
                       , p              = mean(folded_response)
                       , n              = length(folded_response)
                       , seq            = mean(seq)))
  } else {
    rates <- ddply(data, .(abs_displacement), with
                   , c(  displacement   = abs_displacement[1]
                       , p              = mean(abs_response)
                       , n              = length(abs_response)
                       , seq            = mean(seq)))
  }

  #then we show the fitted line.
  predictdata <- chain(  data.frame(displacement=seq(-0.75, 0.75, len=100))
                       , mutate(  folded_displacement=displacement
                                , abs_displacement=displacement
                                , not_folded = 0
                                )
                       , cbind(., predict(fit, ., type="response", se.fit=TRUE))
                       )
                       
  if (average_bias && length(unique(data$loaded_from)) > 1) {
    browser()
  }
  
  measurements <- as.data.frame(as.list(measurements))

  #to plot the bias measurement use the transform from the fit
  link <- fit$family$linkfun
  linkinv <- fit$family$linkinv

  measurements <- mutate(  measurements
                         , bias_p = linkinv(yint)
                         , bias_p_plus = linkinv(yint + yint.sd)
                         , bias_p_minus = linkinv(yint - yint.sd))

  browser()

  (ggplot(rates)
   + list(theme_bw()
          , opts(panel.grid.minor=theme_blank(), panel.grid.major=theme_blank())
          , aes(x=displacement)
          , geom_point(aes(y=p, size=n))
          , scale_area(  limits = c(0, max(30, max(rates$n)))
                       , to=c(0,10))
          , with_arg(alpha=0.3, geom_hline(y=0.5), geom_vline(x=0))
          , with_arg(  data=predictdata
                     , geom_line(aes(y=fit))
                     , with_arg(  linetype=3
                                , geom_line(aes(y=fit+se.fit))
                                , geom_line(aes(y=fit-se.fit))))
          , with_arg(  data=measurements, colour="red"
                     , with_arg(size=5
                                , geom_point(aes(x=0, y=bias_p))
                                , geom_point(aes(x=xint, y=0.5)))
                     , geom_segment(aes(x=0,xend=0,
                                        y=bias_p_minus, yend=bias_p_plus))
                     , geom_segment(aes(y=0.5,yend=0.5,
                                        x=xint.25., xend=xint.75.))
                     # Did you know, the slope of the logit function
                     # where it crosses the 50% level is about 1/4.
                     # That actually works out in odds.
                     , geom_segment(aes(  x=xint - 1/slope
                                        , xend=xint + 1/slope
                                        , y=0.25, yend=0.75)))
          )
   )

  
  
  ##how about the model predictions?

}
