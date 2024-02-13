f_Johnson_pdf<-function(X,coef,type) {
#   % - probability density function for a Johnson distribution
#   %
#   % USAGE: Y = f_johnson_pdf(X,coef,type);
#   %
#   % X    = column vector of Johnson variates
#   % coef = parameters of the Johnson distribution as: [gamma delta xi lambda]
#   % type = type of Johnson distribution as: 'SL', 'SU', 'SB', or 'SN'
#   %
#   % Y    = probability densities
#   %
#   % SEE ALSO: f_johnson_cdf, f_johnson_inv, normpdf
#   
#   % -----Notes:-----
#     % This function returns the PDF of the Johnson distribution with parameters COEF
#   % and family TYPE, evaluated at the values in X.
#   
#   % -----References:-----
#     % Based on a ported to Matlab from Robert E. Wheeler's 'dists.cc' C code in his
# % 'SuppDists package for R'.
# %
# % Hill, I. D., R. Hill, and R. L. Holder, 1976. Algorithm AS 99: Fitting Johnson
# %  curves by moments. Journal of the Royal Statistical Society. Series C
# %  (Applied Statistics) 25(2): 180-189.
# 
# % -----Author:-----
# % by David L. Jones, Apr-2014
# %
# % This file is part of the 'JOHNSON CURVE TOOLBOX FOR MATLAB'
# % and is released under the BSD 2-clause license.
# 
# % Apr-2014: now makes sure values of X don't exceed boundaries of bounded
#   %           distributions
  
  # Check for missing values:
  if (any(is.na(X))){
    print('X contains NaNs!')
    return(c())
  }
  
  # Check coefficients (George & Ramachandran, 2011):
  if (any(c(coef[2],coef[4])<=0)){
    print('DELTA & LAMBDA must be > 0!')
    return(c())
  }
  
# Extract coefficients:
  gamma <- coef[1]# gamma;
  delta <- coef[2]# delta;
  xi    <- coef[3]# xi;
  lambda  <- coef[4]# lambda;
  
  u      = (X-xi)/lambda
  ratio  = delta/lambda
  
  switch(type,
  'SN'={
    fu = u
    D  = ratio
  },  
  'SL'={
  D  = ratio/u
  fu = mylog(u)
  },
  'SU'={
  fu = u+sqrt(1+u*u)
  D  = ratio/sqrt(1.0+u*u)
  fu = mylog(fu)
  },
  'SB'={
  fu = u/(1-u)
  D  = ratio/(u*(1.0-u))
  fu = mylog(fu)
  }
  )  
  
  Z = gamma+delta*fu
  
  # % Return NaN for values exceeding bounds of fitted distribution:
  if (type=='SB') {
    Z[X<=xi | X>=(xi+lambda)] = NA
  }
  if (type=='SL') {
    if (lambda==1) Z[X<xi] = NA
    if (lambda==-1) Z[X>xi] = NA
  }
  
  # % Get probability densities:
    Y = dnorm(Z,0,1)*D
  
}

mylog<-function(x) {
  use<-x>0
  x[use]<-log(x[use])
  x[!use]<-NA
  x
}