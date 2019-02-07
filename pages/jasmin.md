---
layout: page
title: Using eddystore via the command line
description: Using eddystore via the command line.
---

Processing jobs can be run on eddystore via the command line by logging on to JASMIN.

### Instructions

Get [account](pages/access.html)
Log on

    ssh login.jasmin.ac.uk
    ssh eddystore.jasmin.ac.uk

Run R

Eddystore works as a set of functions within R. To install the eddystore R package from github, type the following at the R command prompt:

    library(devtools)
    install_github("NERC-CEH/eddystore", auth_token = "cf75f3ae2091f58e6dd664ce9031bee3aa98f0f8")
    library(eddystore)

and help is available in the standard R manner:

    ?eddystore
    vignettes("eddystore")

The key functions needed are

    createJob()
    runJob()

Details are [here](pages/details.html) and in the help, examples in the vignette.