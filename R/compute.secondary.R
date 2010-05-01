
#Copyright (c) 2009, 2010 Sebastien Bihorel
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

compute.secondary <- function(subproblem=NULL,x=NULL){

  # Copies subproblem.init in newparam and replaces the new estimates in newparam
  newparam <- subproblem$init
  npar     <- length(newparam$names)
  estindex <- 1
  for (i in 1:npar){
    if (newparam$isfix[i]==0){
      newparam$value[i] <- x[estindex]
      estindex <- estindex + 1
    }
  }

  # Computes initial value of secondary parameters
  tmp <- do.call(eval(parse(text=subproblem$secfun)),
                 list(x=subproblem$init))
    init <- tmp$secparam
    names <- tmp$names
  rm(tmp)
  
  # Computes secondary parameter estimates
  tmp <- do.call(eval(parse(text=subproblem$secfun)),
                 list(x=newparam))
    estimates <- tmp$secparam
  rm(tmp)

  varargout <- list(init=init,estimates=estimates,names=names)

  return(varargout)

}

