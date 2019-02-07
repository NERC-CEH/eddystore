---
layout: page
title: eddystore
tagline: Storage and parallel processing of eddy covariance data
description: Parallel processing of eddy covariance data on JASMIN
---

[eddystore](https://github.com/NERC-CEH/eddystore) is a facility for the storage and processing of eddy covariance data for the NERC community.
eddystore can automatically process uploaded data, allowing near-real-time flux calculation with automated raw-data uploads. Processing can be carried out in parallel to achieve fast calculation times for long time series. The system can be accessed via a web interface or via a Linux command-line interface.

eddystore comprises several components:

- the [JASMIN](http://www.jasmin.ac.uk/) storage and computation hardware
- three pieces of software: 
- the eddypro fortran program which performs the flux calculations
- an R package "eddystore" which contains:
  - functions which translate the user processing requirements into computation instructions on jasmin, and
  - R scripts which carry these instructions out on a scheduled basis as cron jobs
- a shiny app which allows jobs to be run on jasmin via a web browser

Processing jobs can be created manually via a [web browser](https://github.com/NERC-CEH/eddystore), or run automatically on a scheduled basis.
The web interface allows a processing job to be submitted for a particular site and time period.
Raw data can be uploaded via dropbox, or preferably directly to eddystore via SCP or rsync with a JASMIN account.
Output data can be [downloaded](http://gws-access.ceda.ac.uk/public/eddystore/) via a web browser or with SCP.

In order to use eddystore via the web browser interface, you need to do the following:
- [apply](https://www.ceh.ac.uk/) for eddystore access.

In order to use eddystore with full flexibility from a Linux command-line interface, you need to do the following:
- [apply](https://www.ceh.ac.uk/) for eddystore access, and also
- [apply](https://accounts.jasmin.ac.uk/) for a JASMIN account, with access to the eddystore group workspace.

In the latter case (via the Linux command-line interface), eddystore works as a set of functions within R.
To install the eddystore R package from github, type the following at the R command prompt:

    library(devtools)
    install_github("NERC-CEH/eddystore", auth_token = "cf75f3ae2091f58e6dd664ce9031bee3aa98f0f8")
    library(eddystore)

and help is available in the standard R manner:

    ?eddystore
    vignettes("eddystore")


- [Accessing eddystore](pages/access.html)
- [Uploading raw daw eddystore](pages/upload.html)
- [Using eddystore via a web browswer](pages/shiny.html)
- [Using eddystore via the command line](pages/jasmin.html)
- [How it works](pages/details.html)
- [Background](pages/background.html)
- [Resources](pages/resources.html)

If anything here is confusing, or if I've missed important details, please
[submit an issue](https://github.com/NERC-CEH/eddystore/issues).

---

The source for this site is based [this](https://github.com/kbroman/simple_site).
