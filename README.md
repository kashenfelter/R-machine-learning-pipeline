---
output: pdf_document
---


# Kaggle Competitions Scripts 


## Set up system




###Installing R

I prefer Ubuntu 15.04. First add this line:

``` deb http://cran.rstudio.com/bin/linux/ubuntu vivid/ ```

to the file **/etc/apt/sources.list**

Then add key by typing:

```sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9```

And finally install R by typing next commands:

```sudo apt-get update```

```sudo apt-get install r-base r-base-dev```

For 14.10 , 14.04 and 12.04 please check [this page](https://cran.r-project.org/bin/linux/ubuntu/README).

### R packages used

There are number of R packages used for various operations on data sets that we are using in this project.

1. Data manipulation

  + sqldf
  + dplyr
  + data.table
  + stringr

2. Text Minning

  + tm
  + RTextTools

3. Machine Learning

  + caret
  + clue
  + xgboost

4. MultiCore

  + parallel

5. Test Driven Development

  + testthat
  
6. Feature Selection

  + FSelector



### Rstudio IDE for R

Rstudio is great IDE for R development and it is updated very often with new features. Latest version can be downloaded from :

http://www.rstudio.com/products/rstudio/download/preview/

### Set Configuration



### Running with bash









