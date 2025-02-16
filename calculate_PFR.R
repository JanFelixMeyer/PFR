
## Clean enviroment
rm(list = ls())


## Load packages
library(data.table)
library(readxl)
library(jsonlite)
library(uuid)


## Define functions
count_cases = function(json_str) {
  parsed_json = tryCatch(fromJSON(json_str), error = function(e) NULL)
  count = sum(grepl("stength_potency_val", names(parsed_json)))
  return(count)
}

flatten_jsonb = function(json_str) {
  # Preprocess the JSON string to ensure proper escaping of double quotes
  json_str_cleaned = gsub('(?<!\\\\)"', '\\"', json_str, perl = TRUE)     # Escape unescaped double quotes
  json_str_cleaned = gsub('\\"\\{', '{', json_str_cleaned, fixed = TRUE)  # Fix double escaping at the start
  json_str_cleaned = gsub('\\}\\"', '}', json_str_cleaned, fixed = TRUE)  # Fix double escaping at the end
  
  # Parse the JSON string
  json_data = tryCatch(
    fromJSON(json_str_cleaned, simplifyVector = TRUE),
    error = function(e) stop("Invalid JSON string: ", e$message)
  )
  
  # Flatten the JSON data
  json_flat = unlist(json_data, recursive = TRUE, use.names = TRUE)
  
  # Return as a data.table
  data.table(keys = names(json_flat), values = json_flat)
}

create_json = function(data = list(), exclude_keys = NULL) {
  combined_kvs = list()
  for (i in 1:nrow(data)) {
    key = data[i, as.character(keys)]
    if (!key %in% exclude_keys) {
      value = data[i, as.character(values)]
      if (key %in% names(combined_kvs)) {
        combined_kvs[[key]] = c(combined_kvs[[key]], value)
      } else {
        combined_kvs[[key]] = value
      }
    }
  }
  json_obj = toJSON(combined_kvs, auto_unbox = TRUE)
  return(json_obj)
}

remove_uom = function(data = list()) {
  data[keys == 'strength_potency', values := as.numeric(gsub("mg|MG", "", values))]
  data[keys == 'blend_strengh', values := as.numeric(gsub("%", "", values))]
  return(data)
}


## Read data
PATH = '/Users/zco7139/Library/CloudStorage/OneDrive-Takeda/Documents/PIC/UA/GPCM/'
ref_metric_raw = fread(paste0(PATH, 'ref_metric_raw_202501101700.csv'))
ref_metric_raw[,group_map_json_obj := gsub('""', '"', group_map_json_obj)]
txn_metric_raw = fread(paste0(PATH, 'txn_metric_raw_202501101700.csv'))
txn_metric_raw[,rec_dt := as.Date(rec_dt)]
txn_metric_raw[, param_actl_dts := as.POSIXct(param_actl_dts, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")]
txn_metric_raw[,rec_repl_id := as.numeric(rec_repl_id)]
txn_metric_raw[,param_actl_num := as.numeric(param_actl_num)]


## Perform checks to ref_metric_raw and txn_metric_raw 
if (FALSE) {
  # The param_nm should not contain any meta-data-fields.
  # The meta_data fields should all go into the group_map_json_obj, compute_param_json_obj columns
  ref_metric_raw[!grepl("Assay|Residual|Impurities|Related|Particle|Dissolution|Amphetamine|Uniformity|Metals|Decomp|Weight|Water|Moisture", param_nm), names(table(param_nm))]
  x = c("Vyvanse Chewables-Cincinnati.Process.Batch Size", "T/TG-Powder.BatchSize"  )
  k = 1
  txn_metric_raw[param_nm == x[k]]
  ref_metric_raw[param_nm == x[k]]
  
  # Check if there arent any wired values for rec_repl_id (should not be crazy high)
  txn_metric_raw[,table(rec_repl_id)]
  txn_metric_raw[rec_repl_id == 211 & !is.na(param_actl_num)]
  txn_metric_raw[rec_id == '3201306X' & param_nm == 'Process.CoA.Tablet Breaking Force.Individuals', list(rec_repl_id, param_actl_num)][order(rec_repl_id)]
  
  # Add a column to identify cases and count them
  ref_metric_raw[,index := 1:.N]
  ref_metric_raw[,count_of_stength_potency := count_cases(group_map_json_obj), by = 'index']
  ref_metric_raw[,table(count_of_stength_potency)]
  # -> If it is only 0 and 1, we can savely replace stength_potency_val_1, stength_potency_val_2, stength_potency_val_3,... with stength_potency_val
}


## Doing PFR calculation
file = paste0(PATH, 'SPECIFICAITON_MAPPING_10JAN25_1700.csv')
#sheet_name = excel_sheets(file)
#map_data = as.data.table(read_excel(file, sheet = sheet_name[1]), col_types = 'text')
map_data = fread(file)

# Only do it for Disco-data and only AGs for which we have data
ag_paths = ref_metric_raw[,unique(param_id)]
map_data = map_data[aud_src_sys_id %in% c('Discoverant PD', 'Discoverant PRD') & param_id %in% ag_paths]
map_data[,adjusted_LSL := as.numeric(adjusted_LSL)]
map_data[,adjusted_USL := as.numeric(adjusted_USL)]

# Perform PFR calculation based on index, where index is defined as a PK of all columns except param_nm
pfr_key = colnames(map_data)
pfr_key = pfr_key[!pfr_key %in% 'param_nm']
map_data[,pfr_index := .GRP, by = pfr_key]

# Define object for meta-data that can be easily searched
ref_metric_raw[,index := 1:.N]
ref_metric_raw_meta = ref_metric_raw[,flatten_jsonb(group_map_json_obj), by = 'index']
ref_metric_raw_meta[grepl("stength_potency_val_\\d+$", keys), keys := sub("_val_\\d+$", "", keys)]
ref_metric_raw_meta[grepl("stength", keys), keys := sub("stength", "strength", keys)]
ref_metric_raw_meta[,values := trimws(values, which = 'both')]
ref_metric_raw_meta[grepl("stength", keys), keys := sub("stength", "strength", keys)]
ref_metric_raw_meta = remove_uom(ref_metric_raw_meta)


## Do PFR calculation

# Setup containers to store values
ref_metric = list()
txn_metric = list()
txn_metric_mapping = list()

pfr_indicies = map_data[,unique(pfr_index)]
ranges = c(100, 50, 30, 10)
META_DATA_LEGACY_COLS = c('site_nm','prod_brand_nm', 'opu_desc', 'func_desc', 'grp_by_dim_nm', 'grp_by_dim_val', 'proc_stg_desc')


if (FALSE) { # Testing for duplicates
  path = '/Users/zco7139/Library/CloudStorage/OneDrive-Takeda/Documents/PIC/UA/GPCM/28DEC24/'
  data = fread(paste0(path, 'txn_metric.csv'))
  
  this_key = c('metric_id', 'metric_range_start_dt', 'metric_range_end_dt')
  setkeyv(data, this_key)
  data[,n := .N, by = this_key]
  data[,table(n)]
  data[n > 1]
  data[metric_id == 14]

  ref_data = fread(paste0(path, 'ref_metric.csv'))
  ref_data[metric_id == 14]
}


k = pfr_indicies[1]
range = 10
row_key_ind = 1
tmp_raw_j_prev = data.table()
for (k in pfr_indicies) {

  # Derive filtering variables
  id = map_data[pfr_index == k, unique(param_id)]
  nm = map_data[pfr_index == k, unique(param_nm)]
  json_cols = map_data[pfr_index == k, .(grp_by_dim_nm_1, grp_by_dim_val_1, grp_by_dim_nm_2, grp_by_dim_val_2, grp_by_dim_nm_3, grp_by_dim_val_3, grp_by_dim_nm_4, grp_by_dim_val_4)]
  json_cols = unique(json_cols) # If the pfr_index results in two rows, the json_filter object is the same
  json_cols_long = melt(
    json_cols,
    measure.vars = patterns("^grp_by_dim_nm", "^grp_by_dim_val"),
    value.name = c("keys", "values"),
    na.rm = TRUE
  )
  json_cols_long = json_cols_long[,.(values = unlist(strsplit(values, ",\\s*"))), by = c('variable', 'keys')]
  json_cols_long[,variable := NULL]
 
  additional_cols = melt(
    map_data[pfr_index == k, .(prod_brand_nm, site_nm, proc_stg_desc, opu_desc, func_desc, mfg_ext_flg, id = 1)],
    id.vars = 'id',
    variable.name = "keys",
    value.name = "values"
  )
  additional_cols = additional_cols[,.(values = unlist(strsplit(values, ",\\s*"))), by = c('id', 'keys')]
  additional_cols[,id := NULL]
  filter_long = rbind(json_cols_long, additional_cols)
  filter_long = remove_uom(filter_long)
  
  # Filter the rec_ids for PFR calculation twice, (1st) by param_id and param_nm (2nd) for grp_by_dim_nms and grp_by_dim_vals
  incidies = ref_metric_raw[param_id %in% id & param_nm %in% nm, index]
  result = ref_metric_raw_meta[index %in% incidies]
  filter_long[,is_in := TRUE]
  this_key = c('keys', 'values')
  setkeyv(result, this_key)
  setkeyv(filter_long, this_key)
  result = filter_long[result]
  result[,relevanz := keys %in% filter_long[,unique(keys)]]
  result[is.na(is_in) & relevanz == TRUE, is_in := FALSE]
  result[relevanz == TRUE, is_in_all := all(is_in == TRUE), by = 'index']
  incidies = result[is_in_all == TRUE, unique(index)]
  if (FALSE) {
    unique(result[is_in == TRUE, list(keys, values)])
  }
  result[,c('is_in', 'relevanz', 'is_in_all') := NULL]
  rec_ids = ref_metric_raw[index %in% incidies, rec_id]
  
  # Derive and sort data
  tmp_raw = txn_metric_raw[param_nm == nm & rec_id %in% rec_ids & !is.na(param_actl_num), list(rec_id, rec_repl_id, param_actl_num, param_actl_desc, rec_dt)]
  setkeyv(tmp_raw, c('rec_id', 'rec_repl_id', 'rec_dt'))
  
  if (nrow(tmp_raw) == 0) {
    next
  }

  # Get the limits
  LSL = map_data[pfr_index == k, adjusted_LSL]
  USL = map_data[pfr_index == k, adjusted_USL]
  
  # Get the aggregation function to handle discrete replicates or calculation of statistics
  agg_func = map_data[k, repl_agg_func]
  
  MIN_DATE = txn_metric_raw[,min(rec_dt)]
  MAX_DATE = txn_metric_raw[,max(rec_dt)]
  DATES = seq(MIN_DATE, MAX_DATE, 1)
  
  # Calculate history values of PFR and statistical values
  for (range in ranges) {
    
    tmp = data.table(metric_id = row_key_ind,
                     metric_range_cd = paste0(range, ' Lots'),
                     metric_nm = map_data[pfr_index == k, unique(metric_nm)],
                     prod_brand_nm = map_data[pfr_index == k, unique(prod_brand_nm)],
                     site_nm = map_data[pfr_index == k, unique(site_nm)],
                     opu_desc = map_data[pfr_index == k, unique(opu_desc)],
                     proc_stg_desc = map_data[pfr_index == k, unique(proc_stg_desc)],
                     opu_desc = map_data[pfr_index == k, unique(opu_desc)],
                     func_desc = map_data[pfr_index == k, unique(func_desc)])
    
    # Create the json columns
    tmp[,group_map_json_obj := create_json(filter_long)]
    tmp[,comp_param_json_obj := create_json(filter_long, exclude = META_DATA_LEGACY_COLS)]
    
    ref_metric[[length(ref_metric) + 1]] = tmp
    
    # For loop only exists in the initial data load, where the entire data is
    # processed. For the incremental/daily data load, dt = todays date.
    for (j in 1:length(DATES)) {
      rec_ids_j = tmp_raw[rec_dt <= DATES[j], unique(rec_id)]
      if (length(rec_ids_j) < range) {
        next
      } else {
        
        start_calc = Sys.time()
        
        tmp_raw_id_dt_j = unique(tmp_raw[rec_dt <= DATES[j], list(rec_id, rec_dt)])
        setkeyv(tmp_raw_id_dt_j, 'rec_dt')
        row_indicies_tmp = 1:nrow(tmp_raw_id_dt_j)
        row_indicies = rev(row_indicies_tmp)[1:range]
        min_dt = tmp_raw_id_dt_j[row_indicies, min(rec_dt)]
        max_dt = tmp_raw_id_dt_j[row_indicies, max(rec_dt)]
        tmp_raw_j = tmp_raw[rec_dt >= min_dt & rec_dt <= max_dt] # I ensure that I do the PFR calculation with at least range many batches
        
        if (identical(tmp_raw_j, tmp_raw_j_prev)) {
          next
        }
        
        perform_pfr_calculation = map_data[k, !is.na(adjusted_LSL) | !is.na(adjusted_USL)]
        if (perform_pfr_calculation) {
          if (!is.na(agg_func)) {
            this_agg_key = c('rec_id', 'rec_dt')
            if (agg_func == 'mean') {
              tmp_agg = tmp_raw_j[,.(param_actl_num = mean(param_actl_num)) , by = this_agg_key]
            }
            if (agg_func == 'max') {
              tmp_agg = tmp_raw_j[,.(param_actl_num = max(param_actl_num)) , by = this_agg_key]
            }
            if (agg_func == 'count') {
              tmp_agg = tmp_raw_j[,.(param_actl_num = .N) , by = this_agg_key]
            }
            if (agg_func == 'min') {
              tmp_agg = tmp_raw_j[,.(param_actl_num = min(param_actl_num)) , by = this_agg_key]
            }
            if (agg_func == 'stdev') {
              tmp_agg = tmp_raw_j[,.(param_actl_num = sd(param_actl_num)) , by = this_agg_key]
            }
          } else {
            tmp_agg = tmp_raw_j
          }
          
          # Perform the actual PFR calculation
          vals = tmp_agg[, param_actl_num]
          mean_vals = mean(vals)
          sd_vals = sd(vals)
          PFR_LSL = 0
          PFR_USL = 0
          
          if (sd_vals == 0) { # If we only observe constant values
            # Case 1: Handle PFR_LSL when values are constant and there is a value for LSL
            if (!is.na(LSL)) {
              if (LSL >= mean_vals) {
                PFR_LSL = 100
              } else {
                PFR_LSL = 0
              }
            }
            # Case 2: Handle PFR_USL when values are constant and there is a value for USL
            if (!is.na(USL)) {
              if (USL <= mean_vals) {
                PFR_USL = 100
              } else {
                PFR_USL = 0
              }
            }
            # Case 3: Handle PFR_LSL and PFR_USL when values are constant and there is a value for LSL and USL
            if (!is.na(LSL) & !is.na(USL)) {
              if (USL <= mean_vals | LSL >= mean_vals) {
                PFR_LSL = 50 # Since we calculate PFR = PFR_LSL + PFR_USL
                PFR_USL = 50 # Since we calculate PFR = PFR_LSL + PFR_USL
              } else {
                PFR_LSL = 0
                PFR_USL = 0
              }
            }
          } else { # Standard case when sd_vals > 0
            if (!is.na(LSL)) {
              PFR_LSL = 100 * pnorm(LSL, mean_vals, sd_vals)
            }
            if (!is.na(USL)) {
              PFR_USL = 100 * (1 - pnorm(USL, mean_vals, sd_vals))
            }
          }
          result = PFR_LSL + PFR_USL
        } else {
          if (agg_func == 'mean') {
            result = tmp_raw_j[,mean(param_actl_num)]
          }
          if (agg_func == 'max') {
            result = tmp_raw_j[,max(param_actl_num)]
          }
          if (agg_func == 'count') {
            result = nrow(tmp_raw_j)
          }
          if (agg_func == 'min') {
            result = tmp_raw_j[,min(param_actl_num)]
          }
          if (agg_func == 'stdev') {
            result = tmp_raw_j[,sd(param_actl_num)]
          }
        }
        
        calc_duration = difftime(Sys.time(), start_calc, 'secs')
        
        print(paste0('Iter = ', k, ' | Calculation for: ', nm, ' (', range, ' Lots) - From: ', min_dt, ', To: ', DATES[j], ' (', round(calc_duration, 2), ' secs)'))
        
        tnx_tmp = data.table(metric_id = row_key_ind,
                             metric_range_start_dt = min_dt,        # Its the date where the first batch was observed
                             metric_range_end_dt = DATES[j],        # Its the date where the calculation was performed
                             metric_actl_num = result)
        uuid_new = UUIDgenerate(n = 1)
        tnx_tmp[,row_key := uuid_new]
        
        setcolorder(tnx_tmp, c('metric_id', 'metric_actl_num', 'metric_range_start_dt', 'metric_range_end_dt', 'row_key'))
        txn_metric[[length(txn_metric) + 1]] = tnx_tmp
        
        tmp_mapping = data.table(map_row_key = uuid_new,
                                 rec_id = tmp_raw_id_dt_j[,unique(rec_id)],
                                 param_nm = nm)
        
        setcolorder(tmp_mapping, c('rec_id', 'param_nm', 'map_row_key'))
        txn_metric_mapping[[length(txn_metric_mapping) + 1]] = tmp_mapping
        
        tmp_raw_j_prev = copy(tmp_raw_j)
      }
    }
    row_key_ind = row_key_ind + 1
  }
}

ref_metric = rbindlist(ref_metric)
txn_metric = rbindlist(txn_metric)
txn_metric_mapping = rbindlist(txn_metric_mapping)


## Save files
fwrite(ref_metric, paste0(PATH, 'ref_metric_', Sys.Date(), '.csv'))
fwrite(txn_metric, paste0(PATH, 'txn_metric_', Sys.Date(), '.csv'))
fwrite(txn_metric_mapping, paste0(PATH, 'txn_metric_mapping_', Sys.Date(), '.csv'))


## Plot PFR/statistic values
if (FALSE) {
  library(ggplot2)
  
  setkeyv(ref_metric, 'metric_id')
  setkeyv(txn_metric, 'metric_id')
  plot_data = ref_metric[txn_metric]
  plot_data[,metric_id := as.numeric(metric_id)]
  plot_data[,plt_idx := .GRP, by = c('metric_nm', 'group_map_json_obj')]
  
  plt_idxs = plot_data[,unique(plt_idx)]
  pdf (file = paste0(PATH, '/GPCM_', Sys.Date(), '.pdf'), width = 20)
    for (idx in plt_idxs) {
      this_metric_nm = plot_data[plt_idx == idx, unique(metric_nm)]
      this_group_map_json_obj = plot_data[plt_idx == idx, unique(group_map_json_obj)]
      tmp = flatten_jsonb(this_group_map_json_obj)
      this_title = tmp[,paste0(keys, ' = ', values, collapse = ', ')]
      this_title = paste(strwrap(this_title, width = 200), collapse = '\n')
      u = ggplot(plot_data[plt_idx == idx], aes(x = metric_range_end_dt, y = metric_actl_num, color = metric_range_cd, group = metric_range_cd)) + geom_step() + geom_point(shape = 4)
      u = u + ylab(this_metric_nm)
      u = u + xlab('Date')
      u = u + theme_bw()
      u = u + ggtitle(this_title)
      print(u)
    }
  dev.off()
}







# END