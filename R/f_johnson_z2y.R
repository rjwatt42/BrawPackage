f_johnson_z2y <- function(Z,coef,type){
  # - transform standard normal variates to Johnson variates
  #
  # USAGE: Y = f_johnson_z2y(Z,coef,type)
  #
  # Z    = column vector of standard normal variates
  # coef = parameters of the Johnson distribution as: [gamma delta xi lambda]
  # type = type of Johnson distribution as: 'SL', 'SU', 'SB', or 'SN'
  #
  # Y = column vector of Johnson variates
  #
  # SEE ALSO: f_johnson_fit, f_johnson_y2z
  
  # -----Notes:-----
  # This function is used to convert variates from a standard normal distribution
  # to those from the corresponding Johnson distribution. It was ported to Matlab
  # from Hill et al.'s (1976) original AS-100 FORTRAN source code, which was
  # obtained from: http://lib.stat.cmu.edu/apstat/100.
  #
  # One of the main purposes of this function is to generate a random sample drawn
  # from a specific Johnson distribution. If Z is a random sample taken from the
  # standard normal distribution, e.g., Z = randn(n,1), then Y will be a random
  # sample taken from a Johnson distribution defined by the gamma, delta, xi, and
  # lambda parameters specified in the 'coef' variable.
  
  # -----References:-----
  # George, F. and K. M. Ramachandran. 2011. Estimation of parameters of Johnson's
  #  system of distributions. Journal of Modern Applied Statistical Methods 10(2):
  #  494-504.
  # Hill, I. D. 1976. Algorithm AS 100: Normal-Johnson and Johnson-Normal
  #  Transformations. Journal of the Royal Statistical Society. Series C (Applied
  #  Statistics) 25(2): 190-192.
  # Hill, I. D., R. Hill, and R. L. Holder, 1976. Algorithm AS 99: Fitting Johnson
  #  Curves by Moments. Journal of the Royal Statistical Society. Series C
  #  (Applied Statistics) Vol. 25, No. 2, 180-189.
  
  # -----Author:-----
  # by David L. Jones, Mar-2014
  #
  # This file is part of the 'JOHNSON CURVE TOOLBOX FOR MATLAB'
  # and is released under the BSD 2-clause license.
  
  # -----Set defaults & check input:-----
  # Check size of input:

  # Check for missing values:
  if (any(is.na(Z))){
    print('Z contains NaNs!')
    return(c())
  }
  
  # Check coefficients (George & Ramachandran, 2011):
  if (any(c(coef[2],coef[4])<=0)){
    print('DELTA & LAMBDA must be > 0!')
    return(c())
  }
  # ----------------------
  
  nr <- length(Z)# # rows of input
  
  # Rename input variables to follow original algorithm:
  SNV <- Z
  
  # Extract coefficients:
  GAMMA <- coef[1]# gamma;
  DELTA <- coef[2]# delta;
  XI    <- coef[3]# xi;
  XLAM  <- coef[4]# lambda;
  switch(type,
  'SL'={
    AJV <- XLAM * exp((XLAM * SNV - GAMMA) / DELTA) + XI
  },       # Type SL: Exponential transformation (lognormal distribution):
  
  'SU'={
    W   <- exp((SNV - GAMMA) / DELTA)
    W   <- 0.5 * (W - 1 / W)
    AJV <- XLAM * W + XI
  },     # Type SU: Hyperbolic sine transformation (unbounded)
  
  'SB'={
    W   <- (SNV - GAMMA) / DELTA
    V   <- exp(-abs(W))
    V   <- (1 - V) / (1 + V)
    AJV <- (0.5 * XLAM) * (sub_sign(V, W) + 1) + XI
  },   # Type SB: Logistic transformation (bounded)
  
  'ST'={
    W   <- (SNV - GAMMA) / DELTA
    V   <- exp(-abs(W))
    V   <- (1 - V) / (1 + V)
    AJV <- (0.5 * XLAM) * (sub_sign(V, W) + 1) + XI
  },   # Type SB: Logistic transformation (bounded)
  
  'SN'={
    AJV <- (SNV - GAMMA) / DELTA
  }  # Type SN: Identity transformation (normal distribution)
  )

# Rename output variables:
Y <- AJV
}


################################################################################
#                               SUBFUNCTION:                                   #
################################################################################
sub_sign <- function(A,B){
  # - port of FORTRAN 'SIGN' function
  #
  # If B\ge 0 then the result is ABS(A), else it is -ABS(A).
  A      <- abs(A)
  A[B<0] <- A[B<0] * -1
  return(A)
}