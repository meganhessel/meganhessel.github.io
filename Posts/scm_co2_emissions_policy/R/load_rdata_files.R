load_rdata_files <- function(data_dir = "Data") {
    files <- list.files(data_dir, pattern = "\\.Rdata$", full.names = TRUE, ignore.case = TRUE)
    
    for (f in files) {
        env <- new.env()
        load(f, envir = env)
        obj_name <- tools::file_path_sans_ext(basename(f))
        assign(obj_name, as.data.frame(get(ls(env)[1], envir = env)), envir = .GlobalEnv)
    }
}