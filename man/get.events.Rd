%Copyright (c) 2009-2014 Sebastien Bihorel
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

\name{get.events}
\alias{get.events}
\title{
  Create events from bolus dosing records.
}
\description{
  \code{get.events} is a secondary function called by \code{dde.model}. It 
  creates a data.frame of events from the bolus dosing records found in the 
  dataset. \code{get.events} is typically not called directly by users.
}
\usage{
  get.events(bolus = NULL, 
             scale = NULL)
}
\arguments{
  \item{bolus}{b x 4 data.frame providing the instantaneous inputs}
  \item{scale}{s x 1 vector of scaling factors}
}
\value{
  Return a data.frame of events with the following elements: \describe{
    \item{var}{A name of the state affected by the event}
    \item{time}{The time of the event}
    \item{value}{The value associated with the event}
    \item{method}{How the event affects the state ('add' by default)}
  }
  See \code{\link[deSolve]{events}} for more details
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  code{\link[deSolve]{events}}
}
\keyword{ method }
