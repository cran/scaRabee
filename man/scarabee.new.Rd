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

\name{scarabee.new}
\alias{scarabee.new}
\title{
  Create a scaRabee Analysis Folder
}
\description{
  \code{scarabee.new} creates a new \pkg{scaRabee} analysis folder.
}
\usage{
  scarabee.new(name = 'myanalysis',
               path = getwd(),
               type = 'simulation',
               template = 'ode',
               with.inputs = TRUE)
}

\arguments{
  \item{name}{A string of characters defining the name of the new folder; 
    \code{name} is also used to name the \pkg{scaRabee} analysis script. Default
    is 'myanalysis'.}
  \item{path}{A path where the new folder is created. Default is the current
    working directory.}
  \item{type}{A string of characters, either 'simulation' or 'estimation'.
    Default is 'simulation'.}
  \item{template}{A string of characters, either 'explicit', 'ode' or 'dde'.
    Default is 'ode'.}
  \item{with.inputs}{A logical indicator whether template input files should be
    created or not. Default is TRUE.}
}
\details{
  The content of new \pkg{scaRabee} analysis folder (\code{path}/\code{name})
  is:\describe{
    \item{model.definition}{A sub-folder containing: \describe{
        \item{model.R}{A template-based R script for the definition of the
          structural model. Depending on \code{template}, it is either based on
          a template for model defined with closed form solution ('explicit'),
          ordinary differential equations ('ode') or delay differential
          equations ('dde').}
        \item{weighting.R}{A template-based R script for the definition of the
          residual variability model.}
        \item{secondary.R}{A template-based R script for the definition of
          secondary parameters.}
      }
    }
    \item{\code{name}.R}{The template-based \pkg{scaRabee} analysis script.}
    \item{data.csv}{(optional) An empty comma-separated file for system
      observations; contains the following default headers: Dose ID, Time,
      Y(1).}
    \item{dosing.csv}{(optional) An empty comma-separated file for dosing
      events; contains the following default headers: Dose ID, Time, State,
      Bolus, Infusion Rate.}
    \item{initials.csv}{(optional) An empty comma-separated file for initial
      guesses of model parameter estimates; contains the following default
      headers: Parameter, Type, Value, Fixed, Lower bound, Upper bound.}
    \item{covariates.csv}{(optional) An empty comma-separated file for covariate
      observations; contains the following default headers: Dose ID, Time,
      Cov(1).}
  }
  See \code{vignette('scaRabee',package='scaRabee')} to learn about how to
  specify your model based on those template files.
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }
