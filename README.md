# HospitalNetwork
*Building networks of hospitals through patients transfers*

This R package contains functions to help interested researchers to build hospital networks from data on hospitalized patients transferred between hospitals. 

The aim of the project is to provide a common framework to build and analyse hospital networks.

This project is partly supported by the NeWIS (NetWorks to Improve Surveillance) initiative, funded by JPIAMR, and by Sphinx project, funded by ANR.

### Step 0: Installing the package:
- To install packages from GitHub, the package “devtools” needs to be installed first
```R
install.packages("devtools")
library(“devtools”)
```
- Then install the package from GitHub. Update or install all the required packages.
```R
install_github("PascalCrepey/HospitalNetwork")
```
### Step 1: Checking the consistency of the database 

The function `checkBase()`should be run first, and the resulting checked/repaired database should be used in the following step. The function checks if :

- all required variables are present,
- any records contain missing values,
- identifiers are in the correct format (character), 
- admission and discharge dates are in date format, and if not, convert them into dates,
- discharges happened at same or later day as admission
- if any hospital stays overlap, and correct if so.

The minimal way of running this is: `checkBase(base)` where base is the patient admission database. It takes the following parameters to adjust to the database in question:
(default values are indicated in bold characters)

* deleteErrors = 	“subject” or **“record”**: how to take care of missing or erroneous records. Delete just the record with an error, or delete all records of the patient with one or more erroneous records.
* convertDates = 	**TRUE**/FALSE: if dates should converted,
* dateFormat = 	The format of date as a character string (e.g. %y%m%d for 20190524, or %d-%m-%y for 24-05-2019)
* subjectID = 	the name of the column/variable containing the subject (i.e. patient) identifier
* facilityID = 		as above, for facility (i.e. hospital) identifier
* disDate = 	as above, for discharge date
* admDate = 	as above, for admissions date
* maxIteration = 	the maximum number of times the script runs through the database to correct for overlapping admissions. Ideally set to more than the required number of times.
* verbose = 	TRUE/**FALSE**: if the script prints out what it is doing.
                      

### Step 2: Reconstructing the network

The best way the reconstruct the hospital network is creating a HospiNet object from the patient database. This object also allows for easy calculation of the network metrics as well as plotting and printing of results. 
```R
hospinet_from_subject_database(checkedBase)
```
This function has a number of similar input parameters as the previous: subjectID, facilityID, disDate, admDate, verbose. Next to that, the following parameters can also be input:

* noloops = 	**TRUE**/FALSE, indicating if movements/transfers to the same hospital as discharge should be included. These self-referrals are not necessary for the analysis of the hospitals network. 
* window_threshold = 	the number of days allowed between discharge and next admission to be counted as a movement or transfer. Suggest to set at **365** for full year, and use 0,7,30,182, and 365 for the final analysis
* nmoves_threshold =  	Any edge/link between hospital with fewer or equal to this number of patients will be removed. Suggest to keep at default **NULL** or 0.
* create_MetricsTable = 	**TRUE**/FALSE, indicates if the network metrics need to be immediately calculated, or only when called for. 
* count_option =	**"successive"** or “all”: the way movements are counted, whether a sequence of admissions from hospital A to B to C are count as moves A→B and B→C (Successive) or A→B, B→C , and A→C (All)
* condition =	**"dates"**, "flags", or "both". Whether a move is counted based on the time difference between two stays (dates), or a variable indicating if the patient was directly transferred (flags), or both.

### Step 3: Exporting and plotting results.

The result of the reconstruction and analysis can be easily saved as an RDS file, using 
```R
saveRDS(HospiNet, "my_filename.RDS") 
```
since they are all stored in the HospiNet object. This object does not include the raw database, just the edge list (which is basically the same as the contact matrix), the various network metrics for each hospital, and metrics on the size of the used database (number of patients, admissions, hospitals, etc.). 

Currently, the reconstructed network can be plotted as a matrix using `plot(HospiNet)` or `plot(HospiNet, type=”matrix”)`. This can also be done as a clustered matrix: `plot(HospiNet, type=”clustered_matrix”)`. In addition, you can also visualise the degree distribution of the nodes in the network with `plot(HospiNet, type = “degree”)`. We will try to include easy ways to plot the network in other ways as well.

### Example of use: 
```R
install.packages("devtools") 	# install.packages only need to be run once 
# and can be commented after use
library(devtools)		# load the library allowing the HospitalNetwork package 
# download and installation

install_github("PascalCrepey/HospitalNetwork") # can be commented once it is installed

library(HospitalNetwork)	# load the HospitalNetwork library


# Here, we create a dummy database for testing purposes,
# final users can directly use their own database. This one looks like: 
#       pID hID      Adate      Ddate
#  1: p001 h09 2019-02-19 2019-02-26
#  2: p001 h10 2019-03-27 2019-03-31
#  3: p001 h09 2019-04-22 2019-04-25
#  4: p002 h08 2019-01-15 2019-01-20
#  5: p003 h11 2019-02-14 2019-02-19
#  ---                               
# 228: p098 h01 2019-02-08 2019-02-12
mydb = create_fake_patientDB(n_patients = 1000, n_hospital = 100)

# checking the database
mydb_checked = checkBase(mydb)

# building the hospital network in a HospiNet object
my_hosp_net = hospinet_from_subject_database(mydb_checked)

# plot the network as a "contact matrix"
plot(my_hosp_net)
#plot the network as a "contact matrix" ordered by clusters (if any)
plot(my_hosp_net, type = "clustered_matrix")
# plot the degree (number of neighbors) distribution of hospitals in the network
plot(my_hosp_net, type = "degree")

# save the network (not the original database)
saveRDS(my_hosp_net, file = "my_hosp_net.RDS")
```