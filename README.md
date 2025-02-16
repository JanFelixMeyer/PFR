Location of Mapping file: https://mytakeda.sharepoint.com/:x:/r/sites/GlobalProcessCapabilityDashboards/Shared%20Documents/PIC/SPECIFICAITON_MAPPING.xlsx?d=wa5592e1947a74485b8b73aa50f1634bd&csf=1&web=1&e=YCNCNE

To retrieve the latest version of ref_metric_raw and txn_metric_raw, run:

```
## Import packages
import psycopg2
import pandas as pd
from sqlalchemy import create_engine
import boto3
import json


## Define functions
def get_db_credentials(secret_arn, region_name):
    client = boto3.client('secretsmanager', region_name=region_name)
    secret_value = client.get_secret_value(SecretId=secret_arn)
    secret = json.loads(secret_value['SecretString'])
    return secret


## ARN for AWS Secrets Manager secret
rgn_nm = 'us-east-1'

# DEV2
secret_arn = "arn:aws:secretsmanager:us-east-1:760218684324:secret:rds-db-dev2-credentials/InsightCenter/gmsgq-ic-app-db-V31O0c"

# Retrieve credentials from AWS Secrets Manager
credentials = get_db_credentials(secret_arn, rgn_nm)

# Extract individual credentials
user_name = credentials['user_name']
password = credentials['password']
host = credentials['host']
port = credentials['port']
database = credentials['database']
connection_string = f"postgresql+psycopg2://{user_name}:{password}@{host}:{port}/{database}"
engine = create_engine(connection_string)


## Define relevant table names
ref_tbl_nm = 'metric.ref_metric'
txn_tbl_nm = 'metric.txn_metric_dtl'


## Create connection string and engine to PostgreSQL for SQLAlchemy
connection_string = f"postgresql+psycopg2://{user_name}:{password}@{host}:{port}/{database}"
engine = create_engine(connection_string)

query = f"SELECT * FROM {ref_tbl_nm}"
ref_data = pd.read_sql(query, engine)

query = f"SELECT * FROM {txn_tbl_nm}"
txn_data = pd.read_sql(query, engine)
```

In https://onetakeda-usdev.cloud.databricks.com/, download the entire ref-data by running:
```
display(ref_data)
```
Subsequently, click 'Download all rows (up to 5GB compressed)':
<img width="1451" alt="image" src="https://github.com/user-attachments/assets/04ffb40a-62dd-4a74-a5a4-66d968250ea1" />

Perform the same operation for txn-data.


