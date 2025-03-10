library(tidyverse)


file_index <- c("https://nex-gddp-cmip6.s3-us-west-2.amazonaws.com/index_md5.txt",
                "https://nex-gddp-cmip6.s3-us-west-2.amazonaws.com/index_v1.1_md5.txt",
                "https://nex-gddp-cmip6.s3-us-west-2.amazonaws.com/index_v1.2_md5.txt")



df <-
  # read tables
  map_dfr(file_index, read_delim, delim = " ", col_names = FALSE) |> 
  # keep only filenames
  select(X3) |> 
  rename("path" = 1)


df <- 
  df |> 
  # format
  separate(path, into = c("project", "model", "scenario", "run", "variable", "filename"), sep = "/") |>
  select(model, scenario, variable, filename) |> 
  
  # add version
  mutate(version = if_else(str_detect(filename, "_v\\d+\\.\\d+"), 
                          as.numeric(str_extract(filename, "\\d+\\.\\d+")),
                          1.0),
         
         year = as.numeric(str_extract(filename, "\\d{4}(?=\\.nc)"))) |> 

  # add path
  bind_cols(df) |> 
  
  # filter latest version
  group_by(model, scenario, variable) |> 
  filter(version == max(version)) |> 
  ungroup()
