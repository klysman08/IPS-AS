

# this is my first line of R code

print("Welcome to R!")



? print()


# in R, you can store data in a variable by using "<-". You can name a 
# variable anything you want as long as it's not already the name of something else.
# I find that a short phrase (without spaces) is generally best.

textToPrint <- "this is some text to print"

# if you give R the name of a variable, it will print whatever is in that variable

textToPrint

# note that capitalization does matter! This line will generate an error becuase 
# there is nothing called "texttoprint"

texttoprint

####### Your turn! Create a variable named "aSentence" and store a sentence in it

# our old friend print()
print(textToPrint)

# the nchar() function tells you the number of characters in a variable
nchar(textToPrint)

# the c() function concatenates (strings together) all its arguments
c(textToPrint, textToPrint, textToPrint)

# What we've seen so far are characters. This is the type of data you'll use for text
varStr <- "someText"

# we can check the data type of a variable using the function str() (like "structure")
str(varStr)
# we can tell this is a character because it's structure is "chr"

# What we've seen so far are characters. This is the type of data you'll use for text
anExampleOfCharacters <- "someText"

# we can check the data type of a variable using the function str() (like "structure")
str(anExampleOfCharacters)
# we can tell this is a character because it's structure is "chr"

# let's create some numeric variables
hoursPerDay <- 24
daysPerWeek <- 7

# we can check to make sure that these actually are numeric
class(hoursPerDay)
class(daysPerWeek)

# since this is numeric data, we can do math with it! 
# "*" is the symbol for multiplication
hoursPerWeek <- hoursPerDay * daysPerWeek
hoursPerWeek



# Important! Just becuase something is a *number* doesn't mean R thinks it's numeric!

a <- 5
b <- "6"

# this will get you the error "non-numeric argument to binary operator", becuase b isn't
# numeric, even though it's a number!
a * b

# You can change character data to numeric data using the as.numeric() function.
# This will let you do math with it again. :)
a * as.numeric(b)

a * b


# check out the stucture: note that b changes from "chr" to "num
str(b)
str(as.numeric(b))

# to fix b to be a number permentantly
b <- as.numeric(b)

str (b)

#--- Boolean types

# You'll get a boolean back if you ask R "are these two things the same?" using "=="

var1 <- "a" == "b"
var2 <- 1 == 1

var1
var2

str (var1)


# vectors

# let's make a vector!
listOfNumbers <- c(1,5,91,42.8,100008.41)
listOfNumbers

str (listOfNumbers)

# becuase this is a numeric vector, we can do math on it! When you do math to a vector,
# it happens to every number in the vector. (If you're familiar with matrix 
# mutiplication, it's the same thing as multiplying a 1x1 matrix by a 1xN matrix.)

# multiply every number in the vector by 5
5 * listOfNumbers


# add one to every number in the vector
listOfNumbers + 1

listOfNumbers

v2 <- listOfNumbers/2

v2

# get the third item from "listOfNumbers"
listOfNumbers[3]

# and store it in another varibale

varA <- listOfNumbers[3]

varA

k <- listOfNumbers[1] > listOfNumbers[2]

k

