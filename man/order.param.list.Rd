%Copyright (c) 2009, 2010 Sebastien Bihorel
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

\name{order.param.list}
\alias{order.param.list}
\title{
  Sort a scaRabee parameter table
}
\description{
  \code{order.param.list} is a secondary function called during estimation runs.
  It reorder a data.frame of initial parameter estimates by type: structural
  ('P'), delays ('L'), initial conditions ('IC'), and finally variance ('V').
}
\usage{
order.param.list(x = NULL)
}
\arguments{
  \item{x}{A data.frame of parameter data with the following columns: 'names', 
    'type', 'value', 'isfix', 'lb', and 'ub'.}
}
\value{
A data.frame of parameter data with the following columns: 'names', 'type', 'value',
'isfix', 'lb', and 'ub'.
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }
