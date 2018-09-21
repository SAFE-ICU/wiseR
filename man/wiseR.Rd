\name{wiser}
\alias{wiser}
\title{wiseR: Helping your decisions from complex datasets}
\usage{
wiser()
}
\description{
Powered by R/Shiny, wiseR is a GUI for learning and extending Bayesian networks into Decision Networks. wiseR can be used for probabilistic reasoning (in the observational setting), causal inference (in the interventional setting) and policy decisions (in Decision network setting). Functionalities include end-to-end implementations for data-preprocessing, Structure-learning, Exact Inference, Approximate Inference, extending the learned structure to Decision network, Policy optimization, Correlation networks, Community detection, Visualization, Export and Web-deployment as Shiny dashboards.  In addition to statistically rigorous methods of bootstraps, ensemble structure and cross-validation, wiseR includes resampling statistics for approximate-inference and comparison with exact inference. It also includes functions for community detection and visualization of network topology. wiseR is powered by shinydashboard, bnlearn, gRain, visNetwork and HydeNet among other packages.
}
\examples{
if (interactive()) {
  wiser()
}
}
\seealso{
\url{https://github.com/SAFE-ICU/wiseR}
}
