# wiseR
A Shiny application for end-to-end Bayesian Decision Network analysis and deployment. Includes functionalities for learning structure, exact inference, approximate inference, data-driven decision network, policy optimization, visualization layouts, cytoscape export, correlation networks, modularity detection and web-deployment of the learned models as Shiny applications. 

An example use of this package is to learn the best policy that will minimize bad patient-outcomes from a complex multivariate dataset where variables may be expected to have interaction among diseases and physiology. The nature and strength of these interactions is learned in the structure-learning step and these are quantified using inference-learning. The user can then set utility of a particular outcome, e.g. setting high preference for zero episodes of sepsis in the ICU and infer the best possible combination of actions that will minimize the probability of sepsis in the given setting. 

The CRAN release of the package can be installed by running install.packages("wiseR") and the development version can be installed by running devtools::install_github("SAFE-ICU/wiseR")

Authors: Shubham Maheshwari (shubham14101@iiitd.ac.in), Dr. Tavpritesh Sethi (tavpriteshsethi@iiitd.ac.in)