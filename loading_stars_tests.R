# TESTS TO ASSESS THE FASTEST WAY TO LOAD A LARGE RASTER
# BEST OPTION: DOWNLOAD TO DISK AND LOAD WITH read_ncdf


# TEST 1

tictoc::tic()
s <- 
  str_glue("/vsicurl/https://nex-gddp-cmip6.s3-us-west-2.amazonaws.com/{f}") |> 
  read_mdim()
tictoc::toc()

# ~50.55 sec





# TEST 2

tictoc::tic()

download.file(str_glue("https://nex-gddp-cmip6.s3-us-west-2.amazonaws.com/{f}"),
              fs::path_file(f),
              method = "wget",
              quiet = T)

tictoc::tic("just reading")
s <- 
  fs::path_file(f) |> 
  read_mdim()
tictoc::toc()

tictoc::toc()

# ~24.66 sec: just reading
# ~31.25 sec: all



# TEST 3

tictoc::tic()

download.file(str_glue("https://nex-gddp-cmip6.s3-us-west-2.amazonaws.com/{f}"),
              fs::path_file(f),
              method = "wget",
              quiet = T)

tictoc::tic("just reading")
s <- 
  fs::path_file(f) |> 
  read_ncdf(proxy = F)
tictoc::toc()

tictoc::toc()

# ~11.88 sec: just reading
# ~19.65 sec: all




# TEST 4

tictoc::tic()

download.file(str_glue("https://nex-gddp-cmip6.s3-us-west-2.amazonaws.com/{f}"),
              fs::path_file(f),
              method = "wget",
              quiet = T)

tictoc::tic("just reading")


s_proxy <- 
  fs::path_file(f) |> 
  read_ncdf(proxy = T)

ss <- 
  future_map(seq(dim(s_proxy)[3]), \(timestep){
    
    fs::path_file(f) |> 
      read_ncdf(ncsub = cbind(start = c(1,1,timestep),
                              count = c(NA,NA, 1))) |> 
      suppressMessages()
    
  })

sss <- 
  do.call(c, ss)

tictoc::toc()

tictoc::toc()

# 29.25 just reading
# 39.56 all


