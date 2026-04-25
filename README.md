
# triangdist

The `triangdist` package provides a complete implementation of the **Triangular Distribution** for R. It includes functions for density, distribution, quantile generation, and random sampling.

## Installation

You can install the development version of `triangdist` from [GitHub](https://github.com/) with:

```r

# If you don't have remotes installed:
# install.packages("remotes")

remotes::install_github("Ipecu/triangdist")

This package implements the following four core functions:

dtriang(x, min, max, mode): Probability Density Function (PDF).

ptriang(q, min, max, mode): Cumulative Distribution Function (CDF).

qtriang(p, min, max, mode): Quantile Function.

rtriang(n, min, max, mode): Random Generation using the inversion method.

Example

library(triangdist)

# Generate 5 random values from a triangular distribution (0, 10, 5)
rtriang(5, min = 0, max = 10, mode = 5)

# Calculate the density at the mode
dtriang(5, min = 0, max = 10, mode = 5)

