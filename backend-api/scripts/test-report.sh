#! /bin/bash

CABAL_NAME="backend-api-0.1.0.0"

HPC_DIR=(`find -wholename *hpc/prof/mix -type d`)
HPC_REPO_DIR="dist-newstyle/hpc"
TIX="spec"

hpc report ${TIX} --hpcdir=${HPC_DIR}/${TIX} --hpcdir=${HPC_DIR}/${CABAL_NAME}
hpc markup ${TIX} --hpcdir=${HPC_DIR}/${TIX} --hpcdir=${HPC_DIR}/${CABAL_NAME} --destdir=${HPC_REPO_DIR}

xdg-open ${HPC_REPO_DIR}/hpc_index.html
