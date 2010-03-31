
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

dde.switch <- function(t=NULL,y=NULL,dde.parms=NULL){
  
  if (length(dde.parms$times)!=0){
    approx(dde.parms$times,
           dde.parms$signal,
           xout=t,
           rule=2)$y
  } else {
    numeric(0)
  }
  
}