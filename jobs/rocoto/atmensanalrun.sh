#! /usr/bin/env bash

export STRICT="NO"
source "${HOMEgfs}/ush/preamble.sh"
export STRICT="YES"

###############################################################
# Source UFSDA workflow modules
. ${HOMEgfs}/ush/load_ufsda_modules.sh
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

export job="atmensanalrun"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
${HOMEgfs}/jobs/JGDAS_GLOBAL_ATMOS_ENSANAL_RUN
status=$?
exit ${status}
