LogCHEM is a tool for performing discriminative interactive mining of chemical fragments.
It couples a relational learning system with a molecular visualisation software that allows a user to graphically control the search for interesting patterns in chemical fragments.

The system requires one input file (in SDF format) with atom and bond data on a set of molecules and with a property that can be used to discriminate the compounds. LogCHEM uses the SDF format because it is highly popular and because it can convey 3D structure information. A rule discovery algorithm, that is implemented as an extension of an Inductive Logic Programming system, is used to find a set of rules that discriminate the given set of molecules. The rules found can be visualized in the matching compounds, manually modified and reevaluated, etc. The interaction with the system is made through a graphical user interface.


## Availability ##

LogCHEM can be downloaded as a tar ball (in the Downloads tab or Featured downloads on right of this page) or directly from the source code repository (Source tab). LogCHEM currently only works on Linux machines. For help with the installation please see the README and INSTALL files.

## Bug Reports ##

The best way to report a bug is with the bug report tool available through the Issues tab. With this tool one can look at previously reported bugs closed or open. If the problem you have encountered is not listed here, you can submit a new bug report by clicking the New Issue button.