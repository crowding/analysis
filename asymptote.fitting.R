  ## at this point, let's learn about constructing our own link
  ## function. All we need is a version of logit.2asym that varies per
  ## subject...  it seems that in order to do that, we have to mirror
  ## the data inside the link function. Here "cases" is a data frame
  ## that has columns "g" and "lam."

  ##We want to be able to slide around the asymptotes in relation to
  ##the data.  Here is a mini-class to help us do so. In principle we
  ##could parameterize the relationship between cases and asymptotes
  ##any which way...

  inverse.permutation <- function(x) {
    x[x] = 1:length(x)
    x
  }

  ##{{{ futzing with transformations.

  0 && {
    ##I'm having problems with the inverse hessian blowing up. It's
    ##related to using the colgistic cdf to compress the asymptote
    ##parameters. Some exploration follows.
    compress <- function(x, p=-1) {
      ifelse(x > 0,
             1-(2^(-1/p)+x)^p,
             (2^(-1/p)-x)^p )
    }
  
    decompress <- function(x, p=-1) {
      ifelse(x > 0.5,
             (1-x)^(1/p) - 2^(-1/p),
             -x^(1/p) + 2^(-1/p)
             )
    }

    ##one idea of measuring the scatter.
    scatter <- function(compressor, decompressor,
                        compressed.range = 0.001, magnify = 10) {
      function(x) {
        center <- decompressor(x)
        delta <- (decompressor(x + compressed.range) - decompressor(x - compressed.range))/2
        (compressor(center+delta*magnify) - compressor(center-delta*magnify))/magnify/compressed.range/2
      }
    }

    bleh <- scatter(force, force)
    curve(bleh, 0.01, 0.99)
    bleh <- scatter(function(x) x^2, sqrt)
    curve(bleh, 0.01, 0.99)
    bleh <- scatter(exp, log)  
    curve(bleh, 0.01, 0.99)
    bleh <- scatter(function(x)1/x, function(x)1/x)  
    curve(bleh, 0.01, 0.99)
    bleh <- scatter(plogis, qlogis)  
    curve(bleh, 0.01, 0.99)
    bleh <- scatter(compress, decompress)  
    curve(bleh, 0.01, 0.99, add=TRUE)
    #well, this kind of indicates that 1/x would be worse than logit....
    ##I think this means i need to bound the guess rates? I mean,
    ##compression would still have to be useful...

    ##second thought, we want a parameter that makes the second
    ##derivative of the log likelihood fairly constant: that way the
    ##Gaussian approximation ought to be valid.
    llf <- function(n, m, p) log(p^n*(1-p)^m)

    visplot <- function(compressor, decompressor, low = 0.90, high=0.9999) {
      curve(llf(100, 0, compressor(x)), decompressor(low),decompressor(high), ylim=c(-4, 0)) 
      curve(llf(99, 1, compressor(x)) + 5.6, add=TRUE)
      curve(llf(95, 5, compressor(x)) + 19.8, add=TRUE)
    }
    visplot(force, force)
    ##hm, the plain log likelihood is a striaght line. We ought to
    ##select a compression function that makes it into a parabola..
    visplot(plogis, qlogis) #pretty good when there are misses but fuckin flat when there are nonw

    visplot(compress, decompress) #effin terrible

    tanhcompress <- function(x) tanh(x)/2+0.5
    tanhdecompress <- function(x) atanh(x*2-1)
    visplot(tanhcompress, tanhdecompress) #also flat when there are no hits

    atancompress <- function(x) atan(x)/pi+0.5
    atandecompress <- function(x) tan((x-0.5)*pi)
    visplot(atancompress, atandecompress) #fickin awful

    ##oh man, but what about compressing with sine? maybe we want mirror
    ##boundery conditions. That might elegantly handle a lot of
    ##things.
    sincompress <- function(x) sin(x)/2+0.5
    sindecompress <- function(x) asin(x*2-1)
    visplot(sincompress, sindecompress) ##damn. it looks like we have a winner.

    ##NOW I can hit google and discover that this is called the "arcsine transform."
    arcsincompress <- function(x) sin(x^2)
    arcsindecompress <- function(x) sqrt(asin(x))
    visplot(arcsincompress, arcsindecompress) ##damn. it looks like we have a winner.

    sin2compress <- function(x) sin(x)^2
    sin2decompress <- function(x) asin(sqrt(x))
    visplot(sin2compress, sin2decompress)
    visplot.low(sin2compress, sin2decompress)
    
    visplot.low <- function(compressor, decompressor, low = 0.0001, high=0.1) {
      curve(llf(0, 100, compressor(x)), decompressor(low),decompressor(high), ylim=c(-4, 0)) 
      curve(llf(1, 99, compressor(x)) + 5.6, add=TRUE)
      curve(llf(5, 95, compressor(x)) + 19.8, add=TRUE)
    }
    visplot.low(sincompress, sindecompress) ##damn. it looks like we have a winner.
    visplot.low(arcsincompress, arcsindecompress) ##damn. it looks like we have a winner.
    0
  }

##}}}

  arcsincompress <- function(x) sin(x^2)
  arcsindecompress <- function(x) sqrt(asin(x))
  sincompress <- function(x) sin(x)/2+0.5
  sindecompress <- function(x) {
    x <- pmax(0, pmin(x, 1))
    asin(x*2-1)
  }

  joined.asymptote <- function(cases, familyfun = function(g,lam) binomial(logit.2asym.vector(g,lam))) {
    ##
    familyfun <- function(data, par) {
      glam <- glam(data, par)
      binomial(logit.2asym.vector(glam$g, glam$lam))
    }
    par <- function(cases=evalq(cases,parent.env(sys.frame(sys.nframe())))) {
      ##case thing to parameter vector
      force(cases)
      sindecompress(as.numeric(unlist(cases[,c("g","lam")])))
    }
    recase <- function(par=evalq(par(),parent.env(sys.frame(sys.nframe())))) {
      ##parameter vector to case thing
      cases[,c("g", "lam")] <- sincompress(par) ## this does not change parent cases, it copies it.
      cases
    }
    relink <- function(par=evalq(par(),parent.env(sys.frame(sys.nframe()))),
                       cases=recase(par)) {
      force(par)
      joined.asymptote(cases)
    }
    glam <- function(data,
                     par=evalq(par(),parent.env(sys.frame(sys.nframe()))),
                     cases=recase(par)) {
      ##extract appropriate g and lambda given model and parameters, or cases
      ##AUGH this needs to be ordered the same way the data were...
      data$..i <- 1:nrow(data)
      merged <- merge(data, cases,
                      by=setdiff(names(cases), c("g","lam")),
                      all.x=TRUE, suffixes=c(".model", "")
                      )[,c("..i", "g","lam")]
      merged[inverse.permutation(merged$..i),c("g", "lam")]
    }
    structure(list(cases     = cases,
                   par       = par,
                   recase    = recase,
                   relink    = relink,
                   glam      = glam,
                   call      = match.call(),
                   familyfun = familyfun),
              class = "link-asymptote")
  }

  ## test <- joined.asymptote(asymptote.cases)
  ## test$par()
  ## test$recase(test$par())
  ## test$recase()
  ## test$recase(-runif(8) - 2.5)
  ## test$relink()$cases
  ## test$relink(-runif(8) - 2.5)$cases
  ## test$relink(cases=test$recase(-runif(8)-2.5))$cases

logit.2asym.vector <- function (g, lam) 
{
    if (any(g < 0) || any(g > 1)) 
        stop("g must in (0, 1)")
    if (any(lam < 0) || any(lam > 1)) 
        stop("lam outside (0, 1)")
    linkfun <- function(mu) {
        mu <- pmin(mu, 1 - (lam + .Machine$double.eps))
        mu <- pmax(mu, g + .Machine$double.eps)
        sindecompress((mu - g)/(1 - g - lam))
    }
    linkinv <- function(eta) {
        g + (1 - g - lam) * .Call("logit_linkinv", eta, PACKAGE = "stats")
    }
    mu.eta <- function(eta) {
        (1 - g - lam) * .Call("logit_mu_eta", eta, PACKAGE = "stats")
    }
    valideta <- function(eta) TRUE
    link <- link <- "logit.2asym.vector( ... )" #paste("logit.2asym.vector( c(", paste(g, collapse=", "), "), c(", paste(lam, collapse=", "), ") )", sep = "")
    structure(list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta, 
        valideta = valideta, name = link), class = "link-glm")
}


  glm.2asym <- function(formula,
                        data,
                        link.2asym=joined.asymptote(data.frame(g=0.05, lam=0.05)),
                        trace=FALSE,
                        tol=1e-06, ...) {
    if(missing(data))
      data <- environment(formula)
    max.glm <- glm(formula, family=link.2asym$familyfun(model.frame(formula, data=data), link.2asym$par()), data=data, ...)
    max.ll <- logLik(max.glm)
    n <- 0
    ## this basically uses Newton's method. It goes on for a
    ## while. What is a good termination condition?
    opt.par <- optim(link.2asym$par(), fn=function(par) {
      est.glm <- glm(max.glm$formula, data, start=max.glm$coefs, family=link.2asym$familyfun(max.glm$model, par))
      ll <- logLik(est.glm)
      if (ll > max.ll) {
        max.ll <<- ll
        max.glm <<- est.glm # we track this separately to give better starting values for GLM.
      }
      if(trace > 5)
        print(data.frame(n=(n<<-n+1), ll=ll, link.2asym$recase(par)))
      else if(trace > 4)
        cat("n=", n<<-n+1, " ll=", ll, "\n")
      ll
    },
                     control=list(fnscale=-1, trace=trace, reltol=1e-6),
                     hessian=FALSE,
                     method="BFGS")
    max.glm$link.2asym <- link.2asym$relink(opt.par$par)
    max.glm$par <- opt.par
    max.glm$df.residual <- max.glm$df.residual - 2*length(max.glm$link.2asym$par())
    max.glm$call[[3]][[2]][[1]] <- as.name(substitute(link))
    class(max.glm) <- c("glm.2asym", "glm", "lm")
    max.glm
  }

hessian <- function(...) {
  UseMethod('hessian')
}

#this makes a giant Hessian which is hella ugly!
hessian.glm.2asym <- function(model, trace=TRUE) {
  require(numDeriv)
  ##we don't use the hessian from optim() because it refits the GLM
  ##every time. instead we try to estimate the complete Hessian here  
  n.2asym.par <- length(model$par$par)
  all.par <- c(model$par$par, model$coef)
  mm <- model.matrix(model)

  y <- model$y ##all this crud so we can call initialize. Ugly!
  nobs <- NROW(y)
  weights <- model$prior.weights
  eval(model$family$initialize) ## ugly corner of the glm code:
                                ## this sets n in case of count
                                ## data (versus standard yes-no)
  rank <- 2 * length(model$coefficients)
  n <- 0
  ll <- function(par) {
    model$family <- model$link.2asym$familyfun(model$model,par[1:n.2asym.par])
    model$coefficients[] <- par[-(1:n.2asym.par)]
    
    aic <- model$family$aic(y,
                            n,
                            model$family$linkinv(mm %*% model$coefficients),
                            weights, NULL) + 2*rank
    ll <- -(aic - 2*rank)/2
    if (trace) cat("n = ", n <<- n+1, " ll = ", ll, "\n")
    ll
  }
  numDeriv::hessian(ll, all.par)
}

## now we need this psychometric-function-generator....
predict.glm.2asym <- function(model, newdata=NULL, se.fit=FALSE, ...) {
  if (se.fit)
    stop("can't do standard errors of prediction for variable-asymptote data (yet).")
  
  mm <- model.frame(formula(model), rename(cbind(.response=NA, newdata),
                                           c(.response=names(model.frame(model))[[1]])),
                    na.action=NULL)
  model$family <- model$link.2asym$familyfun(mm, model$link.2asym$par())
  NextMethod(model=model, newdata=newdata, se.fit=se.fit, ...)
}

sample.data.frame <- function(df, ..., sorted=TRUE) {
  if(sorted)
    df[sort(sample(nrow(df), ...)),]
  else
    df[sample(nrow(df), ...),]
}

setOldClass("glm.2asym")

## and this simulation-generator...
setMethod("sim", "glm.2asym", function(object, n.sims=1000) {
  require(Matrix)
  summ <- summary(object, correlation = TRUE, dispersion = object$dispersion)
  beta.hat <- c(object$par$par, object$coefficients)
  if (all(eigen(-object$hessian)$values > 0))
    V.beta <- solve(-object$hessian)
  else
    V.beta <- solve(nearPD(-object$hessian)$mat)
  k <- length(beta.hat)
  beta <- array(NA, c(n.sims, k))
  dimnames(beta) <- list(NULL, names(beta.hat))
  beta <- mvrnorm(n.sims, beta.hat, V.beta)
  for (s in 1:n.sims) {
    beta[s, ] <- mvrnorm(1, beta.hat, V.beta)
  }
  sigma <- rep(sqrt(summ$dispersion), n.sims)
  ans <- new("sim", coef = beta, sigma = sigma)
  return(ans)
})


#
bootstrap.glm.2asym <- function(object, n = 100, data=object$data, cases=c()) {
  ##presuming already fitted, bootstrap resample the data and refits
  ##it to a new function.
  aaply(1:n, 1, function(x) {
    data <- ddply(data, cases, function(chunk) chunk[sample(nrow(chunk), replace=TRUE),])
    refit <- glm.2asym(object$formula,
                       data=data,
                       link.2asym=object$link.2asym,
                       start=object$coefficients)
    params <- c(refit$par$par, refit$coefficients)
    params
  }, .progress="text")
}

bootstrap <- function(object, n = 100, data=object$data, cases=c())
  UseMethod("bootstrap")

## how about just bootstrapping the model

##preate an ensemble of predictions from a series of simulated
##parameters. Unlike "predict" this function returns a data frame.
predict.ensemble.glm.2asym <- function(model, newdata=NULL, se.fit=FALSE,...,
                                       .progress="none",
                                       response.name=names(model.frame(model))[[1]],
                                       group.name="sim", coefs.name="sim",
                                       which=1:nrow(newdata)) {
  n <- length(model$par$par)
  newdata[,response.name] <- NULL
  pipe(switch(coefs.name,
              sim=model$sim@coef[which,],
              model[[coefs.name]]),
       adply(., 1, function(coefs) {
         model$link.2asym <- model$link.2asym$relink(coefs[1:n])
         model$coefficients[] <- coefs[-(1:n)]
         cbind(newdata,.response=predict(model, newdata=newdata, se.fit=se.fit, ...))
       }, .progress=.progress),
       rename(.,c(.response=response.name, X1=group.name)))
}

predict.ensemble.glm <- function(model, newdata=NULL, se.fit=FALSE,...,
                                 .progress="none",
                                 response.name=names(model.frame(model))[[1]],
                                 group.name="sim", coefs.name="sim",
                                 which=1:nrow(newdata)) {
  newdata[,response.name] <- NULL
  pipe(switch(coefs.name,
              sim=model$sim@coef[which,],
              model[[coefs.name]]),
       adply(., 1, function(coefs) {
         model$coefficients[] <- coefs
         cbind(newdata,.response=predict(model, newdata=newdata, se.fit=se.fit, ...))
       }, .progress=.progress),
       rename(.,c(.response=response.name, X1=group.name)))
}

predict.ensemble <- function(model, newdata=NULL, se.fit=FALSE, coefs.name="sim", ...)
  UseMethod("predict.ensemble")

find.intercept <- function(model, cases, varname, response=0.5, test.values=c(1,2), response.name="response", slope.name="slope", coef=NULL, ...)
  UseMethod("find.intercept")

##some require that you use the same sequence of draws, so I split up
##the drawing and the statistic-calculating, here.

factor.like <- function(f, like) {
  factor(f, levels=levels(like), labels=labels(like), is.ordered = is.ordered(like))
}

## and an intercept-finder
find.intercept.glm.2asym <- function(model, cases, varname, response=0.5, test.values=c(1,2), response.name="response", slope.name="slope", coef=NULL, par=NULL) {
  if (!is.null(coef)) {
    model$coefficients <- coef
  }
  ##locate the appropriate response level...
  yval <- model$link.2asym$familyfun(cases, model$link.2asym$par())$linkfun(response)
  
  cases[, varname] <- test.values[[1]]
  link1 <- predict(model, newdata=cases, type="link")
  cases[, varname] <- test.values[[2]]
  link2 <- predict(model, newdata=cases, type="link")

  slope <- (link2-link1)*(test.values[[2]]-test.values[[1]])
  xintercept <- test.values[[1]] - (link1-yval)/slope
  cases[,varname] <- xintercept
  cases[,response.name] <- response
  cases[, slope.name] <- slope
  cases

}
