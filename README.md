# kmeRs2: k-Mers Similarity Score Matrix

This is an updated version of [kmeRs](https://github.com/RafalUrniaz/kmeRs) with support for both DNA and amino acid k-mers, along with faster package loading times, performance and similarity score heatmap support. *kmeRs2* contains tools to calculate Needleman-Wunsch or Smith-Waterman similarity score matrices with based on user's choice of PAM or BLOSUM substitution methods.

## Installation
```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("Biostrings")
# install.packages("devtools")
devtools::install_github("jlincbio/kmeRs2/")
```
Alternatively, use the release tarball and install via the command line:
```
R CMD INSTALL <kmeRs2-1.0.0.tar.gz>
```

## R Dependencies
* Biostrings
* RColorBrewer (suggested)

Citation
----------
Please cite the original kmeRs by R. Urniaz if you use kmeRs2:

Urniaz R., "kmeRs: an R package for k-mers similarity score calculations", Version 1.0, 2018. URL: [cran.r-project.org/package=kmeRs](https://cran.r-project.org/package=kmeRs).

Additionally, `kmeRs_generate_kmers` and `kmeRs_twoSeqSim` were sourced from [tcR](https://cran.r-project.org/web/packages/tcR/index.html) and [rDNAse](https://cran.r-project.org/web/packages/rDNAse/index.html), respectively.
