
## Clean env
rm(list = ls())


## Load tables
library(data.table)
library(jsonlite)
library(readxl)
library(ggplot2)


## Global Variables
PATH = '/Users/zco7139/Library/CloudStorage/OneDrive-Takeda/Documents/PIC/UA/PFR/'
LSL = 34
USL = 42


## Read data
file = paste0(PATH, 'calculate_PFR.xlsx')
sheet_name = excel_sheets(file)
data = as.data.table(read_excel(file, sheet = 'Data'), col_types = 'text')


## Transform data
setnames(data, 'free fviii subunits', 'val')
data[,Date := as.POSIXct(Date, "%Y-%m-%dT%H:%M:%SZ", tz = 'UTC')]
data[,param_nm := 'Assay (%)']
data = data[order(-Date)]


## Calculate PFR
obs_ranges = c(5, 10, 20, 30)

txn_metric_dtls = list()
txn_batch_metric_mapping = list()

row_key_ind = 1
row_ind = 1
obs_range_ind = 1

while (obs_range_ind <= length(obs_ranges)) {
  row_indicies = row_ind:(row_ind + obs_ranges[obs_range_ind] - 1)
  vals = data[row_indicies, val]
  mean_vals = mean(vals)
  sd_vals = sd(vals)
  PFR_LSL = 100 * pnorm(LSL, mean_vals, sd_vals)
  PFR_USL = 100 * (1 - pnorm(USL, mean_vals, sd_vals))
  PFR = PFR_LSL + PFR_USL
  txn_metric_dtls[[row_key_ind]] = data.table(row_key = row_key_ind,
                                              metric_range_cd = paste0(obs_ranges[obs_range_ind], ' Lots'), 
                                              metric_range_start_dt = data[row_indicies, min(Date)],
                                              metric_range_end_dt = data[row_indicies, max(Date)],
                                              metric_actl_num = PFR)
  txn_batch_metric_mapping[[row_key_ind]] = data.table(map_row_key = row_key_ind,
                                                       batch_id = data[row_indicies, Lot])
  if (max(row_indicies) < nrow(data)) {
    row_key_ind = row_key_ind + 1
    row_ind = row_ind + 1
  } else {
    row_ind = 1
    obs_range_ind = obs_range_ind + 1
  }
}

txn_metric_dtls = rbindlist(txn_metric_dtls)
txn_batch_metric_mapping = rbindlist(txn_batch_metric_mapping)


u = ggplot(txn_metric_dtls, aes(x = metric_range_end_dt, y = metric_actl_num)) + facet_wrap(~metric_range_cd) + geom_line()
u = u + theme_bw()
u









## END


