---
layout: page
title: Setting up a new site
description: Uploading raw to eddystore data.
---


### Setting up a new site for processing

1. Apply for access to the eddystore group workspace (https://accounts.jasmin.ac.uk/services/group_workspaces/eddystore/).
See [here](pages/access.html).

2. Check you can access the disk and directory:
```
ls /gws/nopw/j04/eddystore
```

3. For each new site/station you want to add, decide on a unique site ID in the format "UK-XXX", checking it is different from existing sites in /gws/nopw/j04/eddystore/stations_new

Make a directory with this name ("UK-XXX") in /gws/nopw/j04/eddystore/stations.
```
cd /gws/nopw/j04/eddystore/stations
mkdir UK-XXX
cd UK-XXX
```

4. Make four sub-directories
```
mkdir metadata
mkdir raw_files
mkdir projects
mkdir output
```

5. Upload the raw (10- or 20-Hz) data to raw_files/ using WinSCP any other SFTP program.
Making at least annual sub-directories. The format/structure of sub-directories is not critical, as eddypro will search recursively within the annual directory.

6. Upload the eddypro project files (*.eddypro) to projects/.

7. Upload the eddypro metadata files (*.metadata) to metadata/.

8. Edit the data table of eddystore projects:
/gws/nopw/j04/eddystore/eddystore_projects/df_eddystore_projects.csv
Add a row for each eddypro project file, giving the siteID and dates it is applicable to.
stationID and procID can have arbitrary values if you only have one station (eddy flux system) per site and you only process the data with one set of processing settings.
