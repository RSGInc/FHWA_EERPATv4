# FHWA_EERPATv4
FHWA's Energy and Emissions Reduction Policy Analysis Tool (EERPAT)

EERPAT is a screening tool to compare, contrast, and analyze various greenhouse gas (GHG) reduction policy scenarios for the transportation sector at a statewide level. The FHWA tool estimates GHG emissions from surface transportation, including fuel use (and electricity use for battery charging) by autos, light trucks, transit vehicles, and heavy trucks.

This version, ```EERPAT v4```, of the model includes a disaggregate freight model, which is a major enhancement from prior versions. The freight model includes a firm synthesis module, and relies on data from the Freight Analysis Framework to allocate commodity flows between firms. Commodity flows, in turn, are split between interstate and intrastate flow, enabling an estimate of freight VMT and GHGs.

## Installation Instructions

1. Either download the zipped release (https://github.com/RSGInc/FHWA_EERPATv4/releases/tag/v4.0.0) or use GitHub to create a local clone of the repository on your computer.
2. If you downloaded the zipped release, unzip to a folder on the hard drive of your computer
3. Install the version of ```R``` that has been tested with this model, which is currently ```R 4.0.4``` on Windows, from the CRAN R Project website at https://cran.r-project.org/bin/windows/base/R-4.0.4-win.exe.

## Running EERPATv4

1. Open the script ```Run_EERPATv4.R``` (located in the root directory of the model in ```R GUI``` or another R IDE such as ```RStudio```,
2. Follow the instructions in the scrpt to select a scenario to run and launch the model run
