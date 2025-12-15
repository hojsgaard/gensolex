## library(fs)  # For file handling

# Step 1: Modify `## noshow` chunks for temporary rendering (echo=FALSE)
modify_noshow_for_rendering <- function(lines) {
  inside_chunk <- FALSE
  modified_lines <- c()

  for (i in seq_along(lines)) {
    line <- lines[i]

    # Detect chunk start
    if (grepl("^```\\{r", line)) {
      inside_chunk <- TRUE
      chunk_header <- line

      # Check if next line contains '## noshow'
      if (i + 1 <= length(lines) && grepl("^## noshow\\s*$", lines[i + 1])) {
        # Set echo=FALSE for rendering
        if (grepl("echo\\s*=\\s*", chunk_header)) {
          chunk_header <- sub("echo\\s*=\\s*[^,}]+", "echo=FALSE", chunk_header)
        } else {
          chunk_header <- sub("\\{r", "{r echo=FALSE", chunk_header)
        }
      }

      modified_lines <- c(modified_lines, chunk_header)
      next
    }

    modified_lines <- c(modified_lines, line)
  }

  return(modified_lines)
}

## ## Step 2: Modify `## noshow` chunks for students (echo=TRUE + ## yourcode, removing code)
## modify_noshow_for_students <- function(lines) {
##   inside_chunk <- FALSE
##   remove_chunk <- FALSE
##   modified_lines <- c()

##   for (i in seq_along(lines)) {
##     line <- lines[i]

##     # Detect chunk start
##     if (grepl("^```\\{r", line)) {
##       inside_chunk <- TRUE
##       remove_chunk <- FALSE
##       chunk_header <- line

##       # Check if next line contains '## noshow'
##       if (i + 1 <= length(lines) && grepl("^## noshow\\s*$", lines[i + 1])) {
##         remove_chunk <- TRUE
##         # Set echo=TRUE for students
##         if (grepl("echo\\s*=\\s*", chunk_header)) {
##           chunk_header <- sub("echo\\s*=\\s*[^,}]+", "echo=TRUE", chunk_header)
##         } else {
##           chunk_header <- sub("\\{r", "{r echo=TRUE", chunk_header)
##         }
##         modified_lines <- c(modified_lines, chunk_header, "## yourcode", "```")  # Close chunk immediately
##         inside_chunk <- FALSE
##         remove_chunk <- TRUE  # Skip the next lines inside this chunk
##         next
##       }
##     }

##     # Skip content inside removed chunks
##     if (remove_chunk) {
##       if (grepl("^```\\s*$", line)) {  # Detect closing backticks
##         remove_chunk <- FALSE
##       }
##       next  # Skip all lines inside `## noshow` chunks
##     }

##     # Preserve all other lines
##     modified_lines <- c(modified_lines, line)
##   }

##   return(modified_lines)
## }




## Step 2: Modify `## noshow` chunks for students (echo=TRUE + ## yourcode, removing code)
modify_noshow_for_students <- function(lines) {
  inside_chunk <- FALSE
  remove_chunk <- FALSE
  modified_lines <- c()

  for (i in seq_along(lines)) {
    line <- lines[i]

    # Detect chunk start
    if (grepl("^```\\{r", line)) {
      inside_chunk <- TRUE
      remove_chunk <- FALSE
      chunk_header <- line

      # Check if next line contains '## noshow'
      if (i + 1 <= length(lines) && grepl("^## noshow\\s*$", lines[i + 1])) {
        remove_chunk <- TRUE
        # Set echo=TRUE for students
        if (grepl("echo\\s*=\\s*", chunk_header)) {
          chunk_header <- sub("echo\\s*=\\s*[^,}]+", "echo=TRUE", chunk_header)
        } else {
          chunk_header <- sub("\\{r", "{r echo=TRUE", chunk_header)
        }

        sss <- c(chunk_header, "## yourcode", "```")
        print(sss)
        
        modified_lines <- c(modified_lines, chunk_header, "## yourcode", "```")

        inside_chunk <- FALSE
        remove_chunk <- TRUE  # Skip the next lines inside this chunk
        next
      }
    }

    # Skip content inside removed chunks
    if (remove_chunk) {
      if (grepl("^```\\s*$", line)) {  # Detect closing backticks
        remove_chunk <- FALSE
      }
      next  # Skip all lines inside `## noshow` chunks
    }

    # Preserve all other lines
    modified_lines <- c(modified_lines, line)
  }

  return(modified_lines)
}




# Function to read a file into a vector of lines
read_text_file <- function(file_path) {
  return(readLines(file_path, warn = FALSE))
}

# Function to write a vector of lines to a new file
write_text_file <- function(lines, file_path) {
  writeLines(lines, file_path)
}

# Function to handle any input file and generate correct outputs


#' @importFrom fs file_copy

## file_name <- "clustering_iris.rmd"
# 
#' @title Generate exercise file and solution file
#' @description Generate exercise file and solution file
#' @param input_file Character string naming a .Rmd file
#'
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom rmarkdown render
#' @examples
#' 
#' file_name <- system.file("example", "iris.rmd", package = "gensolex")
#' 
#' @export
## gensolex <- function(input_file) {
##     ## Get file extension
##     file_ext <- tools::file_ext(input_file)

##     ## Generate solution and exercise file names
##     exr_file <- sub(paste0("\\.", file_ext, "$"), paste0("_exr.", file_ext), input_file, ignore.case = TRUE)
##     sol_file <- sub(paste0("\\.", file_ext, "$"), paste0("_sol.", file_ext), input_file, ignore.case = TRUE)
##     exr_html <- sub(paste0("\\.", file_ext, "$"), "_exr.html", input_file, ignore.case = TRUE)
##     sol_html <- sub(paste0("\\.", file_ext, "$"), "_sol.html", input_file, ignore.case = TRUE)
    
##     ## 1. Copy input file to foo_exr.ext and foo_sol.ext
##     file_copy(input_file, exr_file, overwrite = TRUE)
##     file_copy(input_file, sol_file, overwrite = TRUE)
##     cat("Copied", input_file, "to", exr_file, "and", sol_file, "\n")
    
##     ## 2. Modify foo_exr.ext temporarily for rendering (echo=FALSE)
##     lines <- read_text_file(exr_file)    
##     modified_lines <- modify_noshow_for_rendering(lines)
##     write_text_file(modified_lines, exr_file)

##     ## 3. Render both versions to HTML 
##     quarto::quarto_render(exr_file, output_format = "html")
##     quarto::quarto_render(sol_file, output_format = "html")

##     cat("Rendered", exr_file, "to", exr_html, "and", sol_file, "to", sol_html, "\n")

##     ## 4. Modify foo_exr.ext for students (echo=TRUE + ## yourcode, removing code)
##     ## lines <- read_text_file(exr_file)
##     modified_lines <- modify_noshow_for_students(lines)
##     write_text_file(modified_lines, exr_file)
##     quarto::quarto_render(exr_file, output_format = "html")    
##     cat("Overwritten", exr_file, "with modified version (exercise template, echo=TRUE + ## yourcode inserted, code removed, backticks fixed)\n")
## }


gensolex <- function(input_file) {
    ## Get file extension
    file_ext <- tools::file_ext(input_file)

    ## Generate solution and exercise file names
    exr_file <- sub(paste0("\\.", file_ext, "$"), paste0("_exr.", file_ext), input_file, ignore.case = TRUE)

    exr2_file <- sub(paste0("\\.", file_ext, "$"), paste0("_exr2.", file_ext), input_file, ignore.case = TRUE)

    sol_file <- sub(paste0("\\.", file_ext, "$"), paste0("_sol.", file_ext), input_file, ignore.case = TRUE)
    exr_html <- sub(paste0("\\.", file_ext, "$"), "_exr.html", input_file, ignore.case = TRUE)
    sol_html <- sub(paste0("\\.", file_ext, "$"), "_sol.html", input_file, ignore.case = TRUE)
    
    ## 1. Copy input file to foo_exr.ext and foo_sol.ext
    file_copy(input_file, exr_file, overwrite = TRUE)
    file_copy(input_file, sol_file, overwrite = TRUE)
    cat("Copied", input_file, "to", exr_file, "and", sol_file, "\n")
    
    ## 2. Modify foo_exr.ext temporarily for rendering (echo=FALSE)
    in_lines <- read_text_file(exr_file)    
    out_lines <- modify_noshow_for_rendering(in_lines)
    write_text_file(out_lines, exr_file)

    ## 3. Render both versions to HTML 
    quarto::quarto_render(exr_file, output_format = "html")
    quarto::quarto_render(sol_file, output_format = "html")

    cat("Rendered", exr_file, "to", exr_html, "and", sol_file, "to", sol_html, "\n")

    ## 4. Modify foo_exr.ext for students (echo=TRUE + ## yourcode, removing code)
    ## in_lines <- read_text_file(exr_file)
    out_lines2 <- modify_noshow_for_students(in_lines)

    in_lines <<- in_lines
    out_lines <<- out_lines
    out_lines2 <<- out_lines2

    write_text_file(out_lines2, exr2_file)
    quarto::quarto_render(exr2_file, output_format = "html")    
    cat("Overwritten", exr_file, "with modified version (exercise template, echo=TRUE + ## yourcode inserted, code removed, backticks fixed)\n")
}
