
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

get.switch.vectors <- function(dosing=NULL){
  
  if (size(dosing,1)>0){
    if (length(dosing[dosing[,3]>0,3])>0){
      if (length(unique(dosing[,1]))==1){
        return(list(times=dosing[,1]+c(-1,1),
                    signal=c(1,-1),
                    delta=0.1))
      } else {
        bolus.times <- unique(dosing[dosing[,3]>0,1])
        delta <- 0.3*min(bolus.times[-1] - bolus.times[-length(bolus.times)])
        return(list(times=sort(c(bolus.times-delta,bolus.times+delta)),
                    signal=rep(c(1,-1),length(bolus.times)),
                    delta=delta))
      }
    } else {
      return(list(times=numeric(0),
                  signal=numeric(0),
                  delta=0.1))
    }
  } else {
    return(list(times=numeric(0),
                signal=numeric(0),
                delta=0.1))
  }
}
