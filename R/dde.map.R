
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

dde.map <- function(t=NULL,y=NULL,swID=NULL,dde.parms=NULL) {
  
  dosing <- dde.parms$dosing
  
  # Subset dosing for event occuring at time t (or close to)
  bolus <- dosing[which.min(abs(dosing[,1]-t)),,drop=FALSE]
  
  # Update y
  for (i in 1:size(bolus,1)) {
    y[bolus[i,2]] <- y[bolus[i,2]] + bolus[i,3]/dde.parms$scale[bolus[i,2]]
  }
  
  return(y)
  
}