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

\name{scarabee.analysis}
\alias{scarabee.analysis}
\title{
  Run a scaRabee Analysis
}
\description{
  \code{scarabee.analysis} is the \emph{de facto} gateway for running any kind 
  of analysis with \pkg{scaRabee}. All other functions distributed with this
  package are secondary functions called directly or indirectly by
  \code{scarabee.analysis}.

  Arguments for \code{scarabee.analysis} are best defined using the template
  distributed with the package.
  
}
\usage{
  scarabee.analysis(files = NULL,
                    method = 'population',
                    runtype = NULL,
                    debugmode = FALSE,
                    estim.options = NULL,
                    npts = NULL,
                    alpha = NULL,
                    dde.options=list(dt=0.1,
                                     hbsize=10000))
}

\arguments{
  \item{files}{A list of input used for the analysis. The following elements are
    expected and none of them could be null: \describe{
      \item{data}{A .csv file located in the working directory, which contains
        the dosing information, the observations of the dependent variable(s)
        to be modeled, and possibly covariate information. The expected format 
        of this file is described in details in \code{vignette('scaRabee',
        package='scaRabee')}.}
      \item{param}{A .csv file located in the working directory, which contains
        the initial guess(es) for the model parameter(s) to be optimized or used
        for model simulation. The expected format of this file is described in
        details in \code{vignette('scaRabee',package='scaRabee')}.}
      \item{model}{A text file located in the working directory, which defines 
        the model. Models specified with explicit, ordinary or delay 
        differential equations are expected to respect a certain syntax and 
        organization detailed in \code{vignette('scaRabee',package='scaRabee')}.
      }
    }
  }
  \item{method}{A character string, indicating the scale of the analysis. Should
    be 'population' or 'subject'.}
  \item{runtype}{A character string, indicating the type of analysis. Should be
    'simulation', 'estimation', or 'gridsearch'.}
  \item{debugmode}{A logical value, indicating the debug mode should be turn on
    (\code{TRUE}) or off (default). Used only for estimation runs. If turn on,
    the user could have access to error message returned when the model and
    residual variability are evaluated in \code{fitmle} before the likelihood
    is computed.}
  \item{estim.options}{A list of estimation options containing two elements
    \code{maxiter} (the maximum number of iterations) and \code{maxeval} (the
    maximum number of function evaluations).}
  \item{npts}{Only necessary if \code{runtype} is set to 'gridsearch'; \code{npts} 
    represents the number of points to be created by dimension of the grid 
    search.}
  \item{alpha}{Only necessary if \code{runtype} is set to 'gridsearch'; 
    \code{alpha} is a real number, representing the factor applied to the 
    initial estimates of the model parameters to determine the lower and upper 
    bounds to the grid search space.}
  \item{dde.options}{A list with the following levels:\describe{
    \item{ddedt}{A positive numeric scalar providing the step size for the 
      solver of delay differential equations.}
    \item{hbsize}{An integer scalar providing the size of the history buffer for
      the solver of delay differential equations.}
    }
    This variable is ignored if the model file does not contain a $DDE tag.
  }
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\value{
  Run an analysis until completion. See 
  \code{vignette('scaRabee',package='scaRabee')} for more details about the
  expected outputs for an estimation, a simulation, or a gird search run.
}
\seealso{
  \code{\link[scaRabee]{fitmle}}
}
\keyword{ method }
