# 1-3 My working directory is my Data_Course_LARSEN folder,
#reading in the csv files as an object into my global environment.
# 4
csv_files <-list.files(path="./Data/", 
                       recursive = TRUE, pattern=".csv", full.names = TRUE)

# 5 how many csv files do we have?
length(csv_files)
# We have 145

# 6, opening the wingspan file
df<-read.csv("Data/wingspan_vs_mass.csv")
             

# 7 first five lines of the data.
head(df)

# 8 listing all files that start with b
#bf for b files or blue files as I keep calling them
bf <-list.files(pattern="^b", recursive = TRUE,)
#there's 8

# 9 The for loop with b files. I asked chatgpt for help with this one.
#I don't understand what tryCatch really does, the help page didn't explain it to me
for (f in bf) {
  cat("File:", f, "\n")
  
  first_line <- tryCatch(
    readLines(f, n = 1, skipNul = TRUE),
    error = function(e) "<could not read file>"
  )
  
  cat(first_line, "\n\n")
}

# 10 Now let's try doing this for all the csv files instead.
#can't use the exact same code because it returns with <could not read file> every time.
#I again used chatgpt to help me. It pointed out that I should add full.names = TRUE to my initial line
#I will be looking more into what it showed me to do, because I didn't know how to do this before.
for (f in csv_files) {
  cat("==>", f, "<==\n")
  lines <- readLines(f, n = 1, warn = FALSE)
  if (length(lines) > 0) {
    cat(lines, "\n\n")
  } else {
    cat("[empty file]\n\n")
  }
}
