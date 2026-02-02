

# YOUR REMAINING HOMEWORK ASSIGNMENT (Fill in with code) ####

# 1.  Get a subset of the "iris" data frame where it's just even-numbered rows

even_dat <- dat[seq(2,nrow(dat),2),]
print(even_dat)
#The brackets are subsetting the data, and the seq(2,nrow,(dat),2) is what is making it even numbered rows

# 2.  Create a new object called iris_chr which is a copy of iris, except where every column is a character class

iris_chr<-as.character(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width, iris$Species)
str(iris_chr)
#Maybe a bit more extreme, but I did each one because wasn't sure how exactly it worked.
#This may not be quite what you wanted. 

# 3.  Create a new numeric vector object named "Sepal.Area" which is the product of Sepal.Length and Sepal.Width

Sepal.Area<-as.vector(iris$Sepal.Length * iris$Sepal.Width)
#length * width

# 4.  Add Sepal.Area to the iris data frame as a new column

iris$Sepal_Area <-Sepal.Area
print(iris$Sepal_Area)
#using the reverse of subsetting almost, neat.

# 5.  Create a new dataframe that is a subset of iris using only rows where Sepal.Area is greater than 20 
      # (name it big_area_iris)

big_area_iris <- iris$Sepal_Area[iris$Sepal_Area>=20]
print(big_area_iris)
#Brackets to further subset the data, and then >= to make sure it only takes things greater to or 
#equal to 20.


# 6.  Upload the last numbered section of this R script (with all answers filled in and tasks completed) 
      # to canvas
      # I should be able to run your R script and get all the right objects generated

