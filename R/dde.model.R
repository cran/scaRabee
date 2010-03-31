
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

dde.model <- function(parms=NULL,
                      derparms=NULL,
                      code=NULL,
                      bolus=NULL,
                      infusion=NULL,
                      xdata=NULL,
                      covdata=NULL,
                      issim=0,
                      check=FALSE,
                      ddedt=0.1,
                      hbsize=10000){
  
  # Input validation
  if (check){
    if (length(parms)==0)
      stop('parms argument is NULL.')
    
    if (size(bolus,2)!=4)
      stop('bolus argument does not have a di x 4 dimesion.')
    
    if (size(infusion,2)<4)
      stop('infusion argument contain less than 4 columns.')
    
    if (length(xdata)<2)
      stop('xdata argument must contain at least 2 elements.')
    
    if (!is.null(ddedt)){
      if (!(is.numeric(ddedt) && ddedt>0))
        stop('ddedt argument is not numeric or not positive')
    }
    
    if (!is.null(hbsize)){
      if (!(is.numeric(hbsize) && hbsize>0))
        stop('hbsize argument is not numeric or not positive')
    }
  }
  
  # Sort dosing by time
  bolus <- bolus[order(bolus[,1]),]
  infusion <- infusion[order(infusion[,1]),]
  
  # Process dosing information
  dosing <- make.dosing(derparms=derparms,
                        bolus=bolus,
                        infusion=infusion,
                        check=check)
  
  has.dosing <- any(dosing[,4]>0)
  dose.states <- unique(dosing[dosing[,4]>0,2])
  
  # Determine integration intervals
  tspan <- create.intervals(xdata=xdata,
                            bolus=bolus,
                            infusion=infusion)
  nintervals <- size(tspan,2)
  
  # Determine the time points for model evaluation
  if (issim < 0.5){
    xdata.ori <- xdata
    if (tspan[1,1]<xdata[1]){
      xdata <- c(tspan[1,1],xdata)
      is.dosing.mintime <- TRUE
    } else {
      is.dosing.mintime <- FALSE
    }
    
  } else {
    obst <- xdata
    xdata <- NULL
    nint <- ceiling(1001/nintervals)
    # Checks that nint is odd; if not, adds 1
    if (!nint%%2)
      nint <- nint + 1
    
    # Create vector of time
    for (i in 1:nintervals){
      xtmp <- seq(tspan[1,i],tspan[2,i],length.out=nint)
      if (i==1){
        xdata <- c(xdata,xtmp)
      } else {
        xdata <- c(xdata,xtmp[2:length(xtmp)])
      }
    }
    xdata <- sort(unique(c(xdata, obst)))
    xdata.ori <- xdata
    
  }
  
  # Define initial conditions
  ic <- init.cond(parms=parms,
                  derparms=derparms,
                  codeic=code$ic,
                  dosing=bolus,
                  check=check)
  
  # Determine the scaling factors for inputs
  scale <- input.scaling(parms=parms,
                         derparms=derparms,
                         codescale=code$scale,
                         ic=ic,
                         check=check)
  
  # Get the delay parameters
  lags <- dde.lags(parms=parms,
                   derparms=derparms,
                   codelags=code$lag,
                   check=check)
  
  # Get vectors for switch functions
  switch.vectors <- get.switch.vectors(dosing=dosing)
  
  # Builds parameters list parm.list for integration
  parm.list <- list(parms=parms,
                    derparms=derparms,
                    lags=lags,
                    codedde=code$de,
                    dosing=dosing,
                    has.dosing=has.dosing,
                    dose.states=dose.states,
                    xdata=xdata,
                    covdata=covdata,
                    scale=scale,
                    times=switch.vectors$times,
                    signal=switch.vectors$signal,
                    ic=ic,
                    check=check)
  
  # Check dde system
  if (check){
    sol <- dde(y=init.update(a=ic,
                             t=xdata[1],
                             dosing=dosing,
                             scale=scale),
               times=c(0,0.1),
               func=dde.syst,
               parms=parm.list,
               switchfunc=dde.switch,
               mapfunc=dde.map,
               dt=ifelse(is.null(ddedt),
                 10^(floor(log10(switch.vectors$delta))-2),
                 ddedt),
               hbsize=hbsize)
    parm.list$check <- FALSE
  }
  
  # Solve DDE system (update initial conditions with bolus dosing if necessary
  sol <- dde(y=init.update(a=ic,
                           t=xdata[1],
                           dosing=dosing,
                           scale=scale),
             times=xdata,
             func=dde.syst,
             parms=parm.list,
             switchfunc=dde.switch,
             mapfunc=dde.map,
             dt=ifelse(is.null(ddedt),
               10^(floor(log10(switch.vectors$delta))-2),
               ddedt),
             hbsize=hbsize)
  
  # Define ouput from the system
  f <- de.output(f=transpose(as.matrix(sol)),
                 parms=parms,
                 derparms=derparms,
                 codeoutput=code$output,
                 dosing=dosing,
                 xdata=xdata)
  
  # Filter f to extract only time points in xdata.ori
  f <- f[,match(xdata.ori,sol[,1])]
  
  if (size(f,1)==1 & size(f,2)>1)
    f <- matrix(f,nrow=1)
  
  # Re-attach evaluation times xdata for simulation run only
  if (issim > 0.5){
    f <- rbind(xdata,f)
  }
  
  return(f)
  
}
