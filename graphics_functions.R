suppressPackageStartupMessages({
  source("helper_functions.R")
})

with.caption <- function(plot, caption, gp=gpar(fontsize=8)) {
  library(gtable)
  library(grid)
   
  wrap <- paste(strwrap(caption, width=80), collapse="\n")  
  plotGrob <- editGrob(ggplotGrob(plot), name="plot")
  text <- textGrob(wrap, hjust=0, vjust=1, x=0, y=1, name="caption")

  subframe <- frameGrob(name="subframe")
  subframe <- packGrob(subframe, text, height=grobHeight(text) + unit(4, "mm"));

  tab <- gtable( heights=unit.c(unit(c(1),"null"), grobHeight(subframe)) ,
                 widths=unit(c(1), "null"))
  tab <- gtable_add_grob(tab, plotGrob, 1, 1)
  tab <- gtable_add_grob(tab, subframe, 2, 1)
  
  tab
}

pmetric_plot <- function(  measurements, data, fit, output, sim
                         , use_folded, average_bias, abs_bias
                         , one_sided, ...) {
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

  #then we show the fitted line. Note that we need to average over "loaded_from..."
  if (average_bias && length(unique(data$loaded_from)) > 1) {

    #zowie, it's a kludge here....
    multisession <- TRUE
    model <- fit
    predictdata <-
      chain(
        expand.grid(  displacement=seq(-0.75, 0.75, len=100)
                    , loaded_from=data$loaded_from
                    , not_folded = 0)
        , mutate(  folded_displacement=displacement,
                   abs_displacement=displacement * sign(not_folded))
        , cbind(., predict(model, ., type="link", se.fit=TRUE))
#        , ddply(., colnames(.) %-% "loaded_from", numcolwise(mean))
        , mutate(fit = family(model)$linkinv(fit), 
                 se.fit = se.fit*abs(family(model)$mu.eta(fit)))
        )
  } else {
    predictdata <-
      chain(  data.frame(displacement=seq(-0.75, 0.75, len=100))
            , mutate(  folded_displacement=displacement
                     , abs_displacement=displacement
                     , not_folded = 0
                     )
            , cbind(., predict(fit, ., type="response", se.fit=TRUE))
            )
    multisession <- FALSE
  }
  
  measurements <- as.data.frame(as.list(measurements))

  #to plot the bias measurement use the transform from the fit
  link <- fit$family$linkfun
  linkinv <- fit$family$linkinv

  measurements <- mutate(  measurements
                         , bias_p = linkinv(yint)
                         , bias_p_plus = linkinv(yint + yint.sd)
                         , bias_p_minus = linkinv(yint - yint.sd))

  print(ggplot(rates)
   + list(theme_bw()
          , opts(panel.grid.minor=theme_blank(), panel.grid.major=theme_blank())
          , aes(x=displacement)
          , geom_point(aes(y=p, size=n))
          , scale_size_area(  limits = c(0, max(30, max(rates$n)))
                       , max_size=10)
          , with_arg(  alpha=0.3, geom_hline(y=0.5), geom_vline(x=0) )
          , if (multisession) {
            with_arg( data=predictdata
                     , geom_line(aes(y=fit, color=loaded_from, group=loaded_from))
                     , geom_ribbon(aes(ymin=fit-se.fit, ymax=fit+se.fit
                                       , fill=loaded_from, group=loaded_from)
                                   , alpha=0.3
                                   ))
          } else {
            with_arg( data=predictdata
                     , geom_ribbon(aes( ymin=fit-se.fit
                                       , ymax=fit+se.fit )
                                   , alpha=0.3)
                     , geom_line(aes(y=fit))
                     )
          }
          , with_arg(  data=measurements, colour="red"
                     , with_arg(colour="green"
                                , geom_errorbar(aes(x=0, y = bias_p
                                                    , ymin=bias_p_minus
                                                    , ymax=bias_p_plus), width=0.025)
                                , geom_errorbarh(aes(y=0.5, x = xint
                                                      , xmin=xint.25.
                                                     , xmax=xint.75.),
                                                 height=0.025)
                                )
                     # bias
                     , geom_segment(aes(  x=0, xend=0
                                        , y=0.5, yend=bias_p))
                     , geom_text(aes(  x=0, y=(0.5+bias_p)/2
                                     , vjust=0.5
                                     , hjust= 0.5-0.6*sign(yint)*sign(slope))
                                 , label="bias")
                     # PSE
                     , geom_text(aes(  x=xint/2, y=0.5+0.25*sign(xint)*sign(slope)
                                     , vjust=0.5-1*sign(xint))
                                     , label="PSE", hjust=0.5)
                     , geom_segment(aes(  x=xint, xend=0
                                        , y=0.5+0.25*sign(xint)*sign(slope)
                                        , yend=0.5+0.25*sign(xint)*sign(slope)))
                     #Sensitivity
                     # Did you know, the slope of the logit function
                     # where it crosses the 50% level is about 1/4.
                     # That actually works out in odds.
                     , geom_segment(aes(  x=xint - 1/slope
                                        , xend=xint + 1/slope
                                        , y=0.25, yend=0.75))
                     , geom_text(aes(  x = ifelse(xint*slope>0, xint.75., xint.25.)
                                     , y = 0.5
                                     , angle = 180/pi * atan(slope*sign(slope)/4)
                                     , vjust = 0.5+0.5*sign(xint)*sign(slope)
                                     ), label="Sensitivity")
                     # Threshold
                     ## , geom_segment( aes( y=0.75, yend=0.75, x = xint, xend=xint+threshold) )
                     ## , geom_errorbarh(aes(  y=0.75, x=xint
                     ##                      , xmin=xint+threshold.25.
                     ##                      , xmax=xint+threshold.75.
                     ##                      , height=0.025))
                     ## , geom_text(  aes( y = 0.75, x = xint+threshold/2 )
                     ##             , hjust=0.5, vjust=-1
                     ##             , label="Threshold")
                     )
          )
   )
  
}
