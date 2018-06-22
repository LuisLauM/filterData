:: Delete temporal files regarded to previous runs
DEL [/Q] "code\filterData.Rout"

:: Execute the script
R CMD BATCH --no-save --no-restore --no-timing code/filterData.R 