%Copyright (c) 2009-2022 Sebastien Bihorel
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
               path = NULL,
               type = 'simulation',
               method = 'population',
               template = 'ode')
}
\arguments{
  \item{name}{A string of characters defining the name of the master analysis
    script; \code{name} is also appended to the $ANALYSIS tag in the model file.
    Default is 'myanalysis'.}
  \item{path}{A path where the analysis files must be created. The path must 
    not yet exist.}
  \item{type}{A string of characters, either 'simulation', 'estimation', or 
    'gridsearch'. Default is 'simulation'.}
  \item{method}{A string of characters, either 'population' or 'subject'.
    Default is 'population'.}
  \item{template}{A string of characters, either 'explicit', 'ode' or 'dde'.
    Default is 'ode'.}
}
\details{
  The content of new \pkg{scaRabee} analysis folder \code{path}/
  is:\describe{
    \item{\code{name}.R}{The template-based \pkg{scaRabee} analysis script.}
    \item{model.txt}{A template-based txt file for the definition of the
      structural model. Depending on \code{template}, this text file contains
      various tags which delimit blocks of R code needed when models are defined
      with closed form solution ('explicit'), ordinary differential equations 
      ('ode') or delay differential equations ('dde').}
    \item{data.csv}{(optional) An empty comma-separated file for dosing, 
      observations, and covariate information; contains the following default
      headers: OMIT, TRT, ID, TIME, AMT, RATE, CMT, EVID, DV, DVID, and MDV.}
    \item{initials.csv}{(optional) An empty comma-separated file for initial
      guesses of model parameter estimates; contains the following default
      headers: Parameter, Type, Value, Fixed, Lower bound, Upper bound.}
  }
  
  If the \code{path} argument is NULL, then it is coerced to \code{name}, thus 
  creating a new folder in the current working directory.
  
  See \code{vignette('scaRabee',package='scaRabee')} to learn about how to
  specify your model based on those template files.
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }
