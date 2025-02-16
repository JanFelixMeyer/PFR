
## Import libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import norm


## Global Variables
PATH = '/Users/zco7139/Library/CloudStorage/OneDrive-Takeda/Documents/PIC/UA/PFR/'
LSL = 34
USL = 42

## Read data
file = f'{PATH}calculate_PFR.xlsx'
sheet_name = pd.ExcelFile(file).sheet_names
data = pd.read_excel(file, sheet_name='Data', dtype=str)

## Transform data
data.rename(columns={'free fviii subunits': 'val'}, inplace=True)
data['Date'] = pd.to_datetime(data['Date'], format='%Y-%m-%dT%H:%M:%SZ', utc=True)
data = data.sort_values(by='Date', ascending=False).reset_index(drop=True)

## pnorm function
def pnorm(q, mean, sd):
    """
    Calculate the pnorm of a given value.
    Parameters:
        q: The value to calculate the pnorm for.
        mean: The mean of the distribution.
        sd: The standard deviation of the distribution.
    Returns:
        The pnorm of the given value.
    """
    try:
        nor = float(norm.cdf(float(q), loc=float(mean), scale=float(sd)))
        if np.isnan(nor):
            return None
        return nor
    except:
        return None

## Calculate PFR
obs_ranges = [5, 10, 20, 30]

txn_metric_dtls = []
txn_batch_metric_mapping = []

row_key_ind = 1
row_ind = 0
obs_range_ind = 0

while obs_range_ind < len(obs_ranges):
    row_indicies = list(range(row_ind, row_ind + obs_ranges[obs_range_ind]))
    vals = data.loc[row_indicies, 'val'].astype(float)
    mean_vals = vals.mean()
    sd_vals = vals.std()
    PFR_LSL = 100 * pnorm(LSL, mean_vals, sd_vals)
    PFR_USL = 100 * (1 - pnorm(USL, mean_vals, sd_vals))
    PFR = PFR_LSL + PFR_USL
    txn_metric_dtls.append({
        'row_key': row_key_ind,
        'metric_range_cd': f'{obs_ranges[obs_range_ind]} Lots',
        'metric_range_start_dt': data.loc[row_indicies, 'Date'].min(),
        'metric_range_end_dt': data.loc[row_indicies, 'Date'].max(),
        'metric_actl_num': PFR
    })
    txn_batch_metric_mapping.append({
        'map_row_key': row_key_ind,
        'batch_id': data.loc[row_indicies, 'Lot'].tolist()
    })
    if max(row_indicies) < len(data) - 1:
        row_key_ind += 1
        row_ind += 1
    else:
        row_ind = 0
        obs_range_ind += 1

txn_metric_dtls = pd.DataFrame(txn_metric_dtls)
txn_batch_metric_mapping = pd.DataFrame(txn_batch_metric_mapping)

## Plotting
sns.set(style="whitegrid")
g = sns.FacetGrid(txn_metric_dtls, col="metric_range_cd", col_wrap=2, height=4)
g.map(plt.plot, "metric_range_end_dt", "metric_actl_num")
plt.show()








## End
