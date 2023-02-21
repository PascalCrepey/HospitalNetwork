### Context
This application is an interactive interface to the HospitalNetwork package. This package contains functions to help interested researchers construct networks from data on movement of individual between hospitals. It also provides the researcher with various tools to analyse and visualize the constructed network. The workflow is simple:

1. upload a database, and check it for potential errors,

2. compute the network with the desired parameters,

3. visualize and analyze the outputs.

This first page allows you to upload and check your database. It must contains at least four columns, corresponding to the minimal variables needed to construct the network:

* **subject ID** (sID): an identifier unique to each subject 
* **facility ID** (fID): an identifier unique to each facility
* **admission date** (Adate): the date of admission of the subject in the facility
* **discharge date** (Ddate): the date of discharge of the subject from the facility

You can also upload another optional database with GPS coordinates and bed capacitie for map visualisation. It must contains at least four columns, corresponding to the minimal variables needed to construct the map:

* **facility ID** (fID): an identifier unique to each facility
* **latitude** (lat): the latitude of the facility
* **longitude** (long): the longitude of the facility
* **bed capacity** (beds): bed capacity in the facility

The databases can be in one of five formats: a *csv*, *xls*, *xlsx* files, *Rdata*, or *RDS* files of *data.table* or *data.frame* objects. Columns `sID` and `fID` must be of type *character*, columns `Adate` and `Ddate` must be date-time objects of class *POSIXct*. Columns `fID` must be of type *character*, columns `lat` and `long` must be of type *double* and column `beds` must be of type *integer*.

#### Upload and check: workflow

1. Upload a database
2. Indicate in which format is the file by selecting the corresponding button
3. Check the database:
 - if the database was previously checked by the `checkBase()` function, it will not be checked again. Unless it is uploaded from a csv file, in which case the dates are read by default as character strings, and must therefore be parsed to date-time objects.
 - if the database is uploaded from a csv file, or was not previously checked, it must be checked. To check the database you must indicate: the names of the columns corresponding to the required variables, the dates format (times are optional), and how you want to deal with missing values and erroneous records.
4. Click the *Check base* button

You can then proceed to build the network on the **Construct network** page.
