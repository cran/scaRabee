
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

get.layout <- function(nplot=NULL){

  if (is.null(nplot))
    stop('get.layout: nplot is NULL.',
         call.=FALSE)

  if (length(nplot)>1)
    stop('get.layout: nplot is not a scalar.',
         call.=FALSE)
         
  if (is.na(nplot))
    stop('get.layout: nplot is NA.',
         call.=FALSE)

  if ((nplot-as.integer(nplot))>.Machine$double.eps)
    stop('get.layout: nplot is not an integer.',
         call.=FALSE)

  if (nplot==1)
    mylayout <- c(1,1)
  if (nplot==2)
    mylayout <- c(1,2)
  if (nplot>2 & nplot <=4)
    mylayout <- c(2,2)
  if (nplot>4 & nplot <=6)
    mylayout <- c(2,3)
  if (nplot>6 & nplot <=9)
    mylayout <- c(3,3)
  if (nplot>=10)
    mylayout <- c(3,4)

  return(mylayout)

}

