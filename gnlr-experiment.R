  ##before we go all the way down this rabbit hole, let's see what gnlm can give us...
  ## I'm going through Ysad-Fesselier and Knoblach as a tutorial.
  library(gnlm)
  library(repeated)
  library(rmutil)

  #basic psychometric function...
  single.func.data <- subset(trials, considered.trials & subject == "dt" & trial.motion.process.radius==10)

  

  ##form of the logistic function with linear response in
  ##

## fit a single Weibull...
beta <- log(3.5) 
gamma <- 0.25 
lambda <- 0.05 
alpha <- 0.04 
p <- c(alpha, beta, gamma, lambda) 
num.tr <- 160 
cnt <- 10^seq(-2, -1, length = 6) 
wb <- function(p) { 
p[3] + (1 - p[3] - p[4]) * (1 - exp(-((cnt/p[1])^exp(p[2])))) 
} 
NumYes <- rbinom(length(cnt), num.tr, wb(p)) 
NumNo <- num.tr - NumYes 
phat <- NumYes/(NumYes + NumNo) 
resp.mat <- matrix(c(NumYes, NumNo), ncol = 2) 
sim.fit <- gnlr(y = resp.mat, distribution = "binomial", 
                mu = wb, pmu = c(0.04, log(3.4), 0.25, 0.017)) 


##fit a Weibull whose location parameter varies linearly...
atn <- function(x) { ## this merely compresses a parameter into (0, 0.5)
  (atan2(x, 1)/pi + 0.5)/20 
} 
tn <- function(x) { ## this uncompresses
  tan(pi * (20 * x - 0.5)) 
} 
wb2 <- function(p, linear) { 
  p[2] + (1 - p[2] - atn(p[3])) * 
    (1 - exp(-((cnt/exp(linear))^exp(p[1])))) 
} 
ecc2 <- read.table("ecc2.dat", header = TRUE, sep = "\t") 
subdata <- subset(ecc2, size == 12.4, select = Contr:nno) 
names(subdata) <- c("Contrast", "Task", "NumYes", 
                    "NumNo") 
resp <- subset(ecc2, size == 12.4 & task == "DET", 
               select = c(NumYes, NumNo)) 
cnt <- subset(ecc2, size == 12.4 & task == "DET")$Contr 
fit10D <- gnlr(y = resp, distribution = "binomial", 
               mu = wb, pmu = c(0.15, log(3.5), 0.25, tn(0.01))) 
resp <- subset(ecc2, size == 12.4 & task == "ID", 
               select = c(NumYes, NumNo)) 
cnt <- subset(ecc2, size == 12.4 & task == "ID")$Contr 
fit10I <- gnlr(y = resp, distribution = "binomial", 
               mu = wb, pmu = c(0.3, log(3.5), 0.25, tn(0.1))) 
Task <- subset(ecc2, size == 12.4)$task 
cnt <- subset(ecc2, size == 12.4)$Contr
