# pre-requisite: create a aws account and a bucket
# in this example, "test.xlsx"  is put in the aws bucket and "test_mod.csv" written in the same bucket from rstudio

library(tidyverse)
library(readxl)
library(aws.s3)
library(aws.ec2metadata)
library(jsonlite)
Sys.setenv("AWS_DEFAULT_REGION" = 'us-west-2')
# # In case you need to remove an environment variable AWS_SESSION_TOKEN, this will clear it
# Sys.unsetenv("AWS_SECRET_ACCESS_KEY")


## CREATE a key file ##
# eg. aws_key.json  that holds:
# {
#   "AWS_ACCESS_KEY_ID": "yourkeyid",
#   "AWS_SECRET_ACCESS_KEY": "yourkey"
#   
# }
## add aws_key.json to .gitignore at the Repo folder level so it is not pushed to git

# Load the JSON file
json_data <- fromJSON(txt = "~/rtdatasci_github/R/aws/aws_key.json")

# assign the key and secret
Sys.setenv("AWS_ACCESS_KEY_ID" = json_data$AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = json_data$AWS_SECRET_ACCESS_KEY)

# This returns a list of all your buckets if authentication was successful 
#cupofaws
bucketlist() 

# get the list of files from a bucket
file_list <- aws.s3::get_bucket("s3://cupofaws/")  # s3 object lists
file_df <- aws.s3::get_bucket_df("s3://cupofaws/")  # s3 object dataframe
all_files <- file_df$Key
all_files #test.xlsx" 



## METHOD 1: 
# read file from the bucket
myfile <- s3read_using(FUN = read_excel, object = "test.xlsx", bucket = "cupofaws")
# OR:
myfile <- s3read_using(FUN = read_excel, object = "s3://cupofaws/test.xlsx")  
head(myfile)
# # A tibble: 6 × 2
# col1 col2 
# <dbl> <chr>
#   1     1 34   
# 2     2 q53  
# 3     3 we4  
# 4     4 45   
# 5     5 34   
# 6     6 45  


## write file into the bucket
# create a modified file, for example:
myfile_mod <- myfile %>% mutate(new_col = ifelse(str_detect(col2, "[a-z]"), "alphanumeric", "numeric"))
s3write_using(myfile_mod, FUN = write_csv, object = "s3://cupofaws/test_mod.csv")



## METHOD 2: 
# get content and read csv
content <- aws.s3::get_object(object = "s3://cupofaws/test_mod.csv", bucket="s3://cupofaws/")
# convert the alphnumeric content to readable format
content %>% rawToChar %>% read.csv(text = .)
# col1 col2      new_col
# 1     1   34      numeric
# 2     2  q53 alphanumeric
# 3     3  we4 alphanumeric
# 4     4   45      numeric
# 5     5   34      numeric
# 6     6   45      numeric





