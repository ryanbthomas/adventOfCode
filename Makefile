common_files = R/input_utils.R R/testing_utils.R R/utils.R

common_functions.Rdata: R/load_common_files.R $(common_files) 
	Rscript $<

2021: 2021-01 2021-02 2021-03 2021-04 2021-05 2021-06
2023: 2023-01 2023-02 2023-03 2023-04 2023-05 2023-06 2023-07 2023-08

2021-01: common_functions.Rdata 2021/01/sol.R
	Rscript 2021/01/sol.R

2021-02: common_functions.Rdata 2021/02/sol.R
	Rscript 2021/02/sol.R

2021-03: common_functions.Rdata 2021/03/sol.R
	Rscript 2021/03/sol.R

2021-04: common_functions.Rdata 2021/04/sol.R
	Rscript 2021/04/sol.R

2021-05: common_functions.Rdata 2021/05/sol.R
	Rscript 2021/05/sol.R

2021-06: common_functions.Rdata 2021/06/sol.R
	Rscript 2021/06/sol.R

2021-07: common_functions.Rdata 2021/07/sol.R
	Rscript 2021/07/sol.R

2023-01: common_functions.Rdata 2023/01/sol.R
	Rscript 2023/01/sol.R

2023-02: common_functions.Rdata 2023/02/sol.R
	Rscript 2023/02/sol.R

2023-03: common_functions.Rdata 2023/03/sol.R
	Rscript 2023/03/sol.R

2023-04: common_functions.Rdata 2023/04/sol.R
	Rscript 2023/04/sol.R

2023-05: common_functions.Rdata 2023/05/sol.R
	Rscript 2023/05/sol.R

2023-06: common_functions.Rdata 2023/06/sol.R
	Rscript 2023/06/sol.R

2023-07: common_functions.Rdata 2023/07/sol.R
	Rscript 2023/07/sol.R

2023-08: common_functions.Rdata 2023/08/sol.R
	Rscript 2023/08/sol.R