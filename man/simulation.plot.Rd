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
\name{simulation.plot}
\alias{simulation.plot}
\title{
  Create Simulation Plots
}
\description{
  \code{simulation.plot} is a secondary function called at the end of the 
  simulation runs. It generates overlay plots of model predictions and
  observations for all the output system states. See
  \code{vignette('scaRabee',package='scaRabee')} for more details.
}
\usage{
  simulation.plot(problem = NULL,
                  Fsim = Fsim,
                  files = files)
}
\arguments{
  \item{problem}{A list containing the following levels:\describe{
     \item{data}{A list containing the following levels:\describe{
                  \item{xdata}{1 x m matrix of independent variable.}
                  \item{ydata}{n x m matrix of observations from model states.}
                  \item{ids}{Data.frame of indices for data subsetting (output
                    from \code{find.id}).}
                }}
     \item{dosing}{A list containing the following levels:\describe{
                     \item{history}{d x 4 data.frame of dosing history.}
                     \item{ids}{data.frame of indices for dosing subsetting
                       (output from \code{find.id}).}
                  }}
     \item{cov}{A list containing the following levels:\describe{
                  \item{data}{c x t data.frame of covariate history.}
                  \item{ids}{Data.frame of indices for cov subsetting (output
                    from \code{find.id}).}
               }}
     \item{states}{Indices of the states to be output by the model.}
     \item{init}{A data.frame of parameter data with the following columns:
       'names', 'type', 'value', 'isfix', 'lb', and 'ub'.}
     \item{debugmode}{Logical indicator of debugging mode.}
     \item{modfun}{Model function.}
     \item{varfun}{Variance function; if empty \code{weighting.additive} is
       used.}
     \item{secfun}{Secondary parameter function.}
     }
  }
  \item{Fsim}{A data.frame of simulated and observed data containing the
    following columns:\describe{
    \item{doseID}{Indicator of sub-problem.}
    \item{output}{Indicator ofsystem state.}
    \item{time}{Evaluation time.}
    \item{value}{Value of the system state.}
    \item{type}{Category of system state value, either 'pred' for system
      prediction or 'obs' for system observationsl}
    }
  }
  \item{files}{A list of input used for the analysis. The following elements are
    expected and none of them could be null: \describe{
      \item{data}{A .csv file located in the working directory, which contains
        the observations of the dependent variable(s) to be modeled. The
        expected format of this file is described in details in
        \code{vignette('scaRabee',package='scaRabee')}.}
      \item{param}{A .csv file located in the working directory, which contains
        the initial guess(es) for the model parameter(s) to be optimized or used
        for model simulation. The expected format of this file is described in
        details in \code{vignette('scaRabee',package='scaRabee')}.}
      \item{dose}{A .csv file located in the working directory, which contains
        the dosing information. The expected format of this file is described in
        details in \code{vignette('scaRabee',package='scaRabee')}.}
      \item{cov}{A .csv file located in the working directory, which contains
        the values of one or more covariates that may or may or be used within
        the model. The expected format of this file is described in details in
        \code{vignette('scaRabee',package='scaRabee')}.}
      \item{model}{A .R file located in the 'model.definition'
        sub-directory in the working directory, which defines the model. Models
        specified with explicit, ordinary or delayed differential equations
        should be preferentially defined using the provided templates. More
        details about the expected structure of this file is provided in
        \code{vignette('scaRabee',package='scaRabee')}, in case the user would
        want to develop her/his own template.}
      \item{var}{A .R file located in the 'model.definition' sub-directory
        in the working directory, which defines the model of residual
        variability. More details about the expected content of this file is
        provided in \code{vignette('scaRabee',package='scaRabee')}.}
      \item{sec}{A .R file located in the 'model.definition' sub-directory
        in the working directory, which defines the method of computation of
        secondary parameters (derived from the fixed or estimated model
        parameters). More details about the expected content of this file is
        provided in \code{vignette('scaRabee',package='scaRabee')}.}
    }
  }
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }
