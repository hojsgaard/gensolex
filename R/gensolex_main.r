## file_name <- "clustering_iris.rmd"
# 
#' @title Generate exercise file and solution file
#' @description Generate exercise file and solution file
#' @param file_name Character string naming a .Rmd file
#' @param compile Should generated documents be compiled?
#'
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom rmarkdown render
#' @examples
#' 
#' tf <- system.file("example", "iris.rmd", package = "gensolex")
#' 
#' @export
gensolex <- function(file_name, compile=TRUE) {
  if (!file.exists(file_name))
    stop(cat(sprintf("fils %s does not exist\n")))
  
  ext  <- tools::file_ext(file_name)
  name <- tools::file_path_sans_ext(file_name)
  
  lns <- readLines(file_name)

  
  sol_lines <- grep("#+ *SOLUTION", lns)
  sol_lines
  
  backtick_lines <- grep("```", lns)
  backtick_lines
  
  ff <- function(i) {
    s <- sol_lines[i]
    beg <- s - 1
    backtick_lines[which(backtick_lines > s)[1]]
    end <- backtick_lines[which(backtick_lines > s)[1]]
    beg:end
  }
  
  sol_chunks <-
    lapply(seq_along(sol_lines), function(i) {
      ff(i)
    })
  
  sol_chunks
  sol_chunks

  bb <- grep("<!--- *SOLUTION", lns)
  ee <- grep("--->", lns)

  gg <- function(i) {
    s <- bb[i]
    beg <- s
    ee[which(ee > s)[1]]
    end <- ee[which(ee > s)[1]]
    beg:end
  }
  
  sol2_chunks <-
    lapply(seq_along(bb), function(i) {
      gg(i)
    })


  lns_no_sol <- lns[-unlist(c(sol_chunks, sol2_chunks))]

  lns_sol <- lns
  for (i in seq_along(sol2_chunks)){
      x <- sol2_chunks[[i]]
      b <- x[1]
      e <- rev(x)[1]
      print(c(b,e))
      lns_sol[[b]] <- "*SOLUTION comment*"
      lns_sol[[e]] <- "*end of SOLUTION comment*"      
  }
  lns_sol

  
  sol_file <- file.path(paste0(name, "_solution.", ext))
  exe_file <- file.path(paste0(name, "_exercise.", ext))
  
  extra <- c(
    sprintf(
      "<!-- This file has been generated automatically from %s -->\n",
      file_name
    ),
    sprintf("<!-- time: %s do not edit manually -->\n", Sys.time())
  )
  
  cat(sprintf("Writing files:\n %s \n %s\n", exe_file, sol_file))
  writeLines(c(extra, lns_no_sol), exe_file)
  ## writeLines(c(extra, lns), sol_file)
  writeLines(c(extra, lns_sol), sol_file)

  ## if (require(rmarkdown)){
      if (compile){
          rmarkdown::render(exe_file)
          rmarkdown::render(sol_file)
      }
  ## }
}
