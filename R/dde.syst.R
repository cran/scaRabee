
#Copyright (c) 2009-2011 Sebastien Bihorel
#All rights reserved.
#
#This file is part of scaRabee.
#
#    scaRabee is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    scaRabee is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with scaRabee.  If not, see <http://www.gnu.org/licenses/>.
#

dde.syst <- function(t=NULL,
                     y=NULL,
                     dde.parms=NULL){
  
  # Input validation
  if (dde.parms$check){
    if (is.null(dde.parms$codedde))
      stop('codedde argument is NULL.')
    
    if (!is.character(dde.parms$codedde))
      stop('codedde argument must be an object of class \'character\'')
    
    if (length(dde.parms$codedde)>1)
      stop('codedde argument must have a length of 1.')
  }
  
  # Evaluates system at past times
  lags <- dde.parms$lags
  
  names(lags) <- paste('alag',names(lags),sep='.')
  t0 <- dde.parms$xdata[1]
  ic <- dde.parms$ic
  
  ylag <- lapply(lags,
    function(x,...){
      if (t-x>=t0) {
        pastvalue(t-x)
      } else {
        ic
      }
    },
    t,t0,ic)
  rm(lags,t0,ic)
  
  # Evaluation of codedde
  cparms <- c(y,ylag,dde.parms$parms,dde.parms$derparms,dde.parms$lags,
              dde.parms$covdata)
  
  dadt <- with(cparms,{
    
    eval(parse(text=dde.parms$codedde))
    
    return(dadt)
  })
  
  # Get the variable size info and does some comparisons
  nstate <- dim(dadt)[2]
  
  # Initialize input
  input <- rep(0,nstate)
  
  # Build input
  if (dde.parms$has.dosing){
    for (i in dde.parms$dose.states){
      stdosing <- dde.parms$dosing[dde.parms$dosing[,2]==i,]
      input[i] <- approx(x=stdosing[,1],
                         y=stdosing[,4],
                         xout=t,
                         method='constant',
                         yleft=0,
                         yright=stdosing[dim(stdosing)[1],4],
                         ties='ordered')$y
    }
  }
  
  if (dde.parms$check){
    if (dim(dadt)[1]!=length(dde.parms$scale))
      stop('dadt must have the same dimensions as init and scale.')
  }
  
  # Add the input to the ode system
  dadt <- dadt + input/dde.parms$scale
  
  return(dadt)
  
}