
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

find.id <- function(x) {
  
  # Checks if x is null
  if (is.null(x))
    stop('find.id: The input of findID cannot be NULL.',
         call=FALSE)

  # Checks if x is a vector, then if its length is 0
  if (!is.vector(x))
    stop('find.id: The input of findID must be a vector.',
         call=FALSE)

  if (length(x)==0){
    return(data.frame(ID=numeric(0),starting=numeric(0),ending=numeric(0)))
  }
  
  # Checks if x contains only positive integers
  if (!is.numeric(x)){
    stop(paste('find.id: Non numerical values are not allowed in the ID column in the',
               'data, dosing, and covariate files.'),
         call=FALSE)
  }

  if (sum(round(x)-x)>1e-3){
    stop(paste('find.id: The ID column in the data, dosing, and covariate files should',
               'only contain integers.'),
         call=FALSE)
  }

  if (any(x<=0))
    stop(paste('find.id: The ID column in the data, dosing, and covariate files ',
               'should only contain positive integers.'),
         call=FALSE)

  # Checks if x is sorted
  if (is.unsorted(x))
    stop('find.id: The ID column in the data, dosing, or covariate files is not sorted',
         call=FALSE)

  # Builds table with 3 columns: ID starting ending
  starting <- ending <- c()
  for (id in 1:length(unique(x))){
    starting <- c(starting, min(which(x==id)))
    ending   <- c(ending, max(which(x==id)))
  }

  return(data.frame(ID=unique(x),starting,ending))
  
}

