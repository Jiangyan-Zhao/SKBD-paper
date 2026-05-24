# SKBD-paper

This repository contains the reproducibility materials for the manuscript

**Shared Keyboard: An improved Bayesian design for phase I clinical trials via Beta kernel process**

The repository is intended to support the numerical results reported in the main manuscript and Supplementary Material. It contains R scripts, saved simulation outputs, figures, and related files used in the paper.

## Relationship to the SKBD package

The statistical methods are implemented in the R package **SKBD**, available at:

<https://github.com/Jiangyan-Zhao/SKBD>

This repository is a companion reproducibility repository. It is not the R package itself. The package should be installed before running the reproduction scripts.

## Installation

Install the development version of **SKBD** from GitHub:

``` r
install.packages("pak")
pak::pak("Jiangyan-Zhao/SKBD")
```

## How to use this repository

This repository is not organised as a single sequential pipeline. The scripts are intended to be run according to the part of the manuscript that the user wants to reproduce.

A practical workflow is:

1.  Install the **SKBD** package.
2.  Open this repository as an RStudio project or set the working directory to the repository root.
3.  Identify the script corresponding to the result to be reproduced.
4.  Run the selected script with `source()`.
5.  Compare the generated output with the corresponding figure or table in the manuscript.

For example:

``` r
setwd("path/to/SKBD-paper")
source("scripts/main_fixed.R")
```

The repository should be run from its root directory so that relative file paths inside the scripts work correctly.

## Currently documented scripts

The table below describes the scripts that are currently documented for reproducing the results in the manuscript.

| Script | Purpose | Output in the manuscript |
|------------------------|------------------------|------------------------|
| `scripts/main_fixed.R` | Runs the SKBD simulation study for the 20 fixed toxicity scenarios. These scenarios correspond to the fixed scenarios used to compare SKBD with the standard Keyboard design. | Simulation outputs underlying Figures 5 and 6 and the corresponding supplementary tables. |
| `scripts/main_fixed_plot.R` | Uses the fixed-scenario simulation outputs to generate the accuracy and safety plots for the main manuscript. | Figure 5 and Figure 6. |
| `scripts/main_random*.R` | Runs or processes the randomly generated dose--toxicity scenario analyses. These scripts are used for the random-scenario comparison between SKBD and the standard Keyboard design. | Figure 7. |
| `scripts/main_fixed_insert.R` | Runs the fixed dose-insertion simulation study comparing Ins-SKBD with ADM. | Table 2 and Table 3. |
| `scripts/main_fixed_tite.R` | Runs the fixed-scenario simulation study for the time-to-event extension, TITE-SKBD. | Simulation outputs underlying Figure SM.4 and Figure SM.5. |
| `scripts/main_fixed_plot_tite.R` | Uses the fixed-scenario TITE-SKBD simulation outputs to generate the corresponding supplementary figures. | Figure SM.4 and Figure SM.5. |

Additional scripts in the repository reproduce other parts of the manuscript and Supplementary Material. Their descriptions can be added to this table as the repository is further documented.

## Computational reproducibility

The numerical values in the paper are simulation based. Small differences may occur if simulations are rerun with a different random seed, a different version of R, or different package versions. Where scripts set random seeds, keeping the same seed and simulation settings should reproduce the reported results up to ordinary computational variation.

## Data availability

No new clinical data were created or analysed in this study. The results are based on simulation studies. The R package implementing the proposed methods is available at <https://github.com/Jiangyan-Zhao/SKBD>. This repository provides the scripts, simulation outputs, figures, and related files used to reproduce the numerical results in the manuscript.

## Citation

If you use this repository, please cite the associated manuscript:

Zhao, J., Shi, X., and Xu, J. *Shared Keyboard: An improved Bayesian design for phase I clinical trials via Beta kernel process*.
