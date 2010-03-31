%Copyright (c) 2009-2011 Sebastien Bihorel
%All rights reserved.
%
%This file is part of scaRabee.
%
%    scaRabee is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    scaRabee is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with scaRabee.  If not, see <http://www.gnu.org/licenses/>.
%

\name{get.parms.data}
\alias{get.parms.data}
\title{
  Extract data from scaRabee parameter table
}
\description{
  \code{get.parms.data} is a utility function in \pkg{scaRabee}. It allows to 
  extract from a parameter table \code{x} all the data of a given type
  \code{which} for a set of parameter of type \code{type}. \code{get.parms.data}
  is typically not called directly by users.
}
\usage{
  get.parms.data(x = NULL,
                 which = NULL,
                 type = NULL)
}
\arguments{
  \item{x}{A data.frame of parameter data with the following columns: 'names', 
    'type', 'value', 'isfix', 'lb', and 'ub'.}
  \item{which}{A single string of characters, either 'names', 'type', 'value' or
    'isfix'.}
  \item{type}{A single string of characters, either 'P', 'L', 'V' or 'IC'.}
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\value{
  Return as a vector the content of the \code{which} column of \code{x} 
  corresponding to the \code{type} parameters.
}
\keyword{ method }
