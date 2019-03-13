This directory contains files that are useful to work with CABLE:

namelists:
folder containing namelist file 'cable.nml'

params:
folder containing parameter files for soil and vegetation


reset_restart.bash:
file needed for a cold restart

run_cable_site_CNP.py:
python script to run a single site in the terminal

run_cable_site_CNP_meta.py:
python script to run a single site or multiple sites in the terminal or on the cluster with command line arguments. For multiple sites, see 'workflow' below.

run_cable_casa.slurm:
slurm file to run multiple sites on Pearcey


site_runs_bash.ksh:
script to set up (if not existing) or run site-level simulations.



WORKFLOW:
1) specify sites and start and end years in a site list (e.g. OzFLUX_sitelist_v1.txt)
2) run site_runs_bash.ksh. This script loops over sites specified in the sitelist, sets up new sites (if necessary), adds the 'run_cable_site_CNP_meta.py' script and required command line arguments to the 'run_cable_casa.slurm' file and sends it to the cluster via 'sbatch'





More information to be added here...

