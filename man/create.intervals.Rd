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

\name{create.intervals}
\alias{create.intervals}
\title{
  Create Integration Intervals Based on Dosing History  
}
\description{
  \code{create.intervals} is a utility function that is called by the model
  template based on ordinary differential equations. It allows the overall
  integration interval to be split into sub-intervals based upon dosing history.
  This allows for the exact implementation of bolus inputs into the system.
}
\usage{
  create.intervals(xdata = NULL,
                   dosing = NULL)
}
\arguments{
  \item{xdata}{A vector of numerical observations times.}
  \item{dosing}{A d x 4 data.frame of dosing history containing the following 
    columns: \describe{
      \item{Time}{Dosing event times.}
      \item{State}{State where the input should be assigned to.}
      \item{Bolus}{Amount that should be assigned to system \code{state} at the
        corresponding \code{Time}.}
      \item{Infusion.Rate}{Rate of input that should be assigned to system
        \code{state} at the corresponding \code{Time}. See
        \code{vignette('scaRabee',package='scaRabee')} for more details about
        the interpolation of the input rate at time not specified in
        \code{dosing}.}
     }
  }
}
\details{
  \code{create.intervals} determines the number of unique bolus dosing events
  there is by system state in \code{dosing}. It then creates the sub-intervals
  using these unique event times. If the first dosing events occurs after the
  first observation time, an initial sub-interval is added. 
}
\value{
  Returns a 2 x v matrix of numerical values, giving the beginning and the end
  of the integration intervals.
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }
