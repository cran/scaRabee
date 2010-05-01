
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

create.intervals <- function(xdata=NULL,dosing=NULL){

  # Trim dosing of any duplicates time records
  dosing <- dosing[match(unique(dosing[,1]),dosing[,1]),]

  # Determines the min time to start the integration. The maximum has to be
  # the maximum observation time.
  mintime <- min(c(transpose(dosing[,1]),xdata))
  maxtime <- max(xdata)
  
  # Trims dosing from doses over maxtime
  dosing <- dosing[dosing[,1]<=maxtime,]
  
  # Initializes intervals
  if (dosing[1,1]==mintime){      # integration starts at first dose
    intervals <- c()
  } else {                        # integration starts before first dose
    intervals <- rbind(mintime,dosing[1,1])
  }
  
  # Buils intervals
  ndose <- size(dosing,1)
  for (i in 1:ndose){
    if (i!=ndose){                 # intermediate dose
      tmp <- rbind(dosing[i,1],dosing[i+1,1])
    } else {                       # last dose
      if (dosing[i,1]==maxtime){     # time of last dose = time of last observation
        tmp <- NULL
      } else {                       # otherwise
        tmp <- rbind(dosing[i,1],maxtime)
      }
    }
    intervals <- cbind(intervals,tmp)
  }

  row.names(intervals) <- c('start.time','end.time')

  return(intervals)

}

