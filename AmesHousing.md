Ames Housing Prices Model
================
Connor Allison
2025-05-04

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(randomForest)
```

    ## randomForest 4.7-1.2
    ## Type rfNews() to see new features/changes/bug fixes.
    ## 
    ## Attaching package: 'randomForest'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(caret)
```

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(xgboost)
```

    ## 
    ## Attaching package: 'xgboost'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     slice

``` r
library(e1071)    
library(BART)       
```

    ## Loading required package: nlme
    ## 
    ## Attaching package: 'nlme'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse
    ## 
    ## Loading required package: survival
    ## 
    ## Attaching package: 'survival'
    ## 
    ## The following object is masked from 'package:caret':
    ## 
    ##     cluster

``` r
library(car)       
```

    ## Loading required package: carData
    ## 
    ## Attaching package: 'car'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
library(corrplot) 
```

    ## corrplot 0.94 loaded

``` r
library(ranger)
```

    ## 
    ## Attaching package: 'ranger'
    ## 
    ## The following object is masked from 'package:randomForest':
    ## 
    ##     importance

Load the data

``` r
home_train = read_csv("~/Library/CloudStorage/OneDrive-Personal/Desktop/School/MSDA/Spring 2025/Analytics Applications/Housing Data Project/train.csv")
```

    ## Rows: 1460 Columns: 81
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (43): MSZoning, Street, Alley, LotShape, LandContour, Utilities, LotConf...
    ## dbl (38): Id, MSSubClass, LotFrontage, LotArea, OverallQual, OverallCond, Ye...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
home_test = read_csv("~/Library/CloudStorage/OneDrive-Personal/Desktop/School/MSDA/Spring 2025/Analytics Applications/Housing Data Project/test.csv")
```

    ## Rows: 1459 Columns: 80
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (43): MSZoning, Street, Alley, LotShape, LandContour, Utilities, LotConf...
    ## dbl (37): Id, MSSubClass, LotFrontage, LotArea, OverallQual, OverallCond, Ye...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Check the structure

``` r
str(home_train)
```

Check if there are any NAs if the `SalePrice` column

``` r
anyNA(home_train$SalePrice)
```

    ## [1] FALSE

Combine the two data frames to clean more efficiently

``` r
temp_home_test = home_test %>% 
  mutate(SalePrice = NA)
all_homes = rbind(temp_home_test, home_train)
```

Get rid of redundant columns

``` r
all_homes = all_homes %>% 
  select(-Fireplaces, -MiscFeature)
```

We also don’t need the ID column

``` r
all_homes = all_homes %>% 
  select(-Id)
```

Let’s look at the categorical variables

``` r
identify_categorical = function(data, unique_threshold = 10) {
  is_categorical = sapply(data, function(x) {
    if(is.character(x) || is.factor(x) || is.logical(x)) {
      return(TRUE)
    }
    
    if(is.numeric(x) && length(unique(x)) <= unique_threshold) {
      return(TRUE)
    }
    
    return(FALSE)
  })
  
  return(names(data)[is_categorical])
}

#Identify categorical variables
categorical_vars = identify_categorical(all_homes)
cat("Identified categorical variables:\n")
```

    ## Identified categorical variables:

``` r
print(categorical_vars)
```

    ##  [1] "MSZoning"      "Street"        "Alley"         "LotShape"     
    ##  [5] "LandContour"   "Utilities"     "LotConfig"     "LandSlope"    
    ##  [9] "Neighborhood"  "Condition1"    "Condition2"    "BldgType"     
    ## [13] "HouseStyle"    "OverallQual"   "OverallCond"   "RoofStyle"    
    ## [17] "RoofMatl"      "Exterior1st"   "Exterior2nd"   "MasVnrType"   
    ## [21] "ExterQual"     "ExterCond"     "Foundation"    "BsmtQual"     
    ## [25] "BsmtCond"      "BsmtExposure"  "BsmtFinType1"  "BsmtFinType2" 
    ## [29] "Heating"       "HeatingQC"     "CentralAir"    "Electrical"   
    ## [33] "BsmtFullBath"  "BsmtHalfBath"  "FullBath"      "HalfBath"     
    ## [37] "BedroomAbvGr"  "KitchenAbvGr"  "KitchenQual"   "Functional"   
    ## [41] "FireplaceQu"   "GarageType"    "GarageFinish"  "GarageCars"   
    ## [45] "GarageQual"    "GarageCond"    "PavedDrive"    "PoolQC"       
    ## [49] "Fence"         "YrSold"        "SaleType"      "SaleCondition"

``` r
#Plot
plot_categorical = function(data, var_names) {
  plots = list()
  
  for(var in var_names) {
    if(!is.factor(data[[var]])) {
      data[[var]] = as.factor(data[[var]])
    }
    
    var_counts = data %>%
      count(!!sym(var)) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      arrange(desc(n))
    
    p = ggplot(var_counts, aes(x = reorder(!!sym(var), -n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
      geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
                vjust = -0.5, size = 3) +
      labs(title = paste("Distribution of", var),
           subtitle = paste(length(unique(data[[var]])), "unique values"),
           x = var,
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold"),
            panel.grid.minor = element_blank())
    
    if(length(unique(data[[var]])) > 15) {
      p = p + scale_x_discrete(guide = guide_axis(n.dodge = 2))
      cat(paste0("Note: Variable '", var, "' has many categories (", 
                 length(unique(data[[var]])), "). Consider grouping some categories.\n"))
    }
    
    plots[[var]] = p
    
    print(p)
  }
  
  return(plots)
}

cat_plots = plot_categorical(all_homes, categorical_vars)
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-5.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-6.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-7.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-8.png)<!-- -->

    ## Note: Variable 'Neighborhood' has many categories (25). Consider grouping some categories.

![](AmesHousing_files/figure-gfm/unnamed-chunk-9-9.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-10.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-11.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-12.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-13.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-14.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-15.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-16.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-17.png)<!-- -->

    ## Note: Variable 'Exterior1st' has many categories (16). Consider grouping some categories.

![](AmesHousing_files/figure-gfm/unnamed-chunk-9-18.png)<!-- -->

    ## Note: Variable 'Exterior2nd' has many categories (17). Consider grouping some categories.

![](AmesHousing_files/figure-gfm/unnamed-chunk-9-19.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-20.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-21.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-22.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-23.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-24.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-25.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-26.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-27.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-28.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-29.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-30.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-31.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-32.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-33.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-34.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-35.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-36.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-37.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-38.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-39.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-40.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-41.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-42.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-43.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-44.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-45.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-46.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-47.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-48.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-49.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-50.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-51.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-9-52.png)<!-- -->

Lets look at missing values

``` r
colSums(is.na(all_homes))
```

    ##    MSSubClass      MSZoning   LotFrontage       LotArea        Street 
    ##             0             4           486             0             0 
    ##         Alley      LotShape   LandContour     Utilities     LotConfig 
    ##          2721             0             0             2             0 
    ##     LandSlope  Neighborhood    Condition1    Condition2      BldgType 
    ##             0             0             0             0             0 
    ##    HouseStyle   OverallQual   OverallCond     YearBuilt  YearRemodAdd 
    ##             0             0             0             0             0 
    ##     RoofStyle      RoofMatl   Exterior1st   Exterior2nd    MasVnrType 
    ##             0             0             1             1            24 
    ##    MasVnrArea     ExterQual     ExterCond    Foundation      BsmtQual 
    ##            23             0             0             0            81 
    ##      BsmtCond  BsmtExposure  BsmtFinType1    BsmtFinSF1  BsmtFinType2 
    ##            82            82            79             1            80 
    ##    BsmtFinSF2     BsmtUnfSF   TotalBsmtSF       Heating     HeatingQC 
    ##             1             1             1             0             0 
    ##    CentralAir    Electrical      1stFlrSF      2ndFlrSF  LowQualFinSF 
    ##             0             1             0             0             0 
    ##     GrLivArea  BsmtFullBath  BsmtHalfBath      FullBath      HalfBath 
    ##             0             2             2             0             0 
    ##  BedroomAbvGr  KitchenAbvGr   KitchenQual  TotRmsAbvGrd    Functional 
    ##             0             0             1             0             2 
    ##   FireplaceQu    GarageType   GarageYrBlt  GarageFinish    GarageCars 
    ##          1420           157           159           159             1 
    ##    GarageArea    GarageQual    GarageCond    PavedDrive    WoodDeckSF 
    ##             1           159           159             0             0 
    ##   OpenPorchSF EnclosedPorch     3SsnPorch   ScreenPorch      PoolArea 
    ##             0             0             0             0             0 
    ##        PoolQC         Fence       MiscVal        MoSold        YrSold 
    ##          2909          2348             0             0             0 
    ##      SaleType SaleCondition     SalePrice 
    ##             1             0          1459

Get rid of alley, poolqc and fence because most values are missing and
pool. We also want to get rid of columns where over 95% of responses are
in the same category. The condition holds true for Street, Utilities,
LandSlope, Condition2, RoofMatl, Heating, and KitchenAbvGr

``` r
all_homes = all_homes %>% 
  select(-Fence, -Alley, -PoolQC, -PoolArea, -Street, -Utilities, -LandSlope, -Condition2, -RoofMatl, -Heating, -KitchenAbvGr)
```

See if there are any duplicates

``` r
any(duplicated(all_homes) == TRUE)
```

    ## [1] FALSE

Plot histograms for continuous variables

``` r
# Function to identify continuous variables
identify_continuous = function(data, unique_threshold = 10) {
  is_continuous = sapply(data, function(x) {
    # Check if numeric and has more unique values than our threshold
    if(is.numeric(x) && length(unique(x)) > unique_threshold) {
      return(TRUE)
    }
    return(FALSE)
  })
  
  return(names(data)[is_continuous])
}

# Identify continuous variables
continuous_vars = identify_continuous(all_homes)
cat("Identified continuous variables:\n")
```

    ## Identified continuous variables:

``` r
print(continuous_vars)
```

    ##  [1] "MSSubClass"    "LotFrontage"   "LotArea"       "YearBuilt"    
    ##  [5] "YearRemodAdd"  "MasVnrArea"    "BsmtFinSF1"    "BsmtFinSF2"   
    ##  [9] "BsmtUnfSF"     "TotalBsmtSF"   "1stFlrSF"      "2ndFlrSF"     
    ## [13] "LowQualFinSF"  "GrLivArea"     "TotRmsAbvGrd"  "GarageYrBlt"  
    ## [17] "GarageArea"    "WoodDeckSF"    "OpenPorchSF"   "EnclosedPorch"
    ## [21] "3SsnPorch"     "ScreenPorch"   "MiscVal"       "MoSold"       
    ## [25] "SalePrice"

``` r
# Create histograms for each continuous variable
plot_histograms = function(data, var_names) {
  plots = list()
  
  for(var in var_names) {
    # Generate basic histogram
    p = ggplot(data, aes(x = !!sym(var))) +
      geom_histogram(fill = "steelblue", color = "white", bins = 30) +
      labs(title = paste("Distribution of", var),
           subtitle = paste("n =", sum(!is.na(data[[var]])), 
                           "| NA =", sum(is.na(data[[var]]))),
           x = var,
           y = "Count") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            panel.grid.minor = element_blank())
    
    # Add a vertical line for the mean
    p = p + geom_vline(aes(xintercept = mean(!!sym(var), na.rm = TRUE)),
                       color = "darkred", linetype = "dashed", linewidth = 1)
    
    # Save plot to list
    plots[[var]] = p
    
    # Display plot
    print(p)
  }
  
  return(plots)
}

# Generate and display all histograms
hist_plots = plot_histograms(all_homes, continuous_vars)
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-5.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-6.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-7.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-8.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-9.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-10.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-11.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-12.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-13.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-14.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-15.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-16.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-17.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-18.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-19.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-20.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-21.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-22.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-23.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-24.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-13-25.png)<!-- -->

``` r
# If you want to save all plots to PDF
save_histograms = function(plot_list, filename = "continuous_histograms.pdf") {
  pdf(filename, width = 10, height = 7)
  for(p in plot_list) {
    print(p)
  }
  dev.off()
  cat(paste0("All histograms saved to ", filename, "\n"))
}

save_histograms(hist_plots)
```

    ## All histograms saved to continuous_histograms.pdf

Typo 2207 changed to 2007

``` r
which(all_homes$GarageYrBlt == max(range(all_homes$GarageYrBlt, na.rm = T)))
```

    ## [1] 1133

``` r
max(range(all_homes$GarageYrBlt, na.rm = T))
```

    ## [1] 2207

``` r
all_homes$GarageYrBlt[1133] = 2007
all_homes$GarageYrBlt[1133]
```

    ## [1] 2007

Some columns seem to have major outliers. We are going to get ride of
these observations.

``` r
outliers = c()

outliers = append(outliers, which(all_homes$MiscVal > 100))
outliers = append(outliers, which(all_homes$LotArea > 30000))
outliers = append(outliers, which(all_homes$LotFrontage == max(range(all_homes$LotFrontage, na.rm = T))))
outliers = append(outliers, which(all_homes$MasVnrArea > 1000))
outliers = append(outliers, which(all_homes$BsmtFinSF1 == max(range(all_homes$BsmtFinSF1, na.rm = T))))
outliers = append(outliers, which(all_homes$BsmtFinSF2 == max(range(all_homes$BsmtFinSF2, na.rm = T))))
outliers = append(outliers, which(all_homes$TotalBsmtSF == max(range(all_homes$TotalBsmtSF, na.rm = T))))
outliers = append(outliers, which(all_homes$`1stFlrSF` == max(range(all_homes$`1stFlrSF`, na.rm = T))))
outliers = append(outliers, which(all_homes$`2ndFlrSF` == max(range(all_homes$`2ndFlrSF`, na.rm = T))))
outliers = append(outliers, which(all_homes$LowQualFinSF > 50))
outliers = append(outliers, which(all_homes$GrLivArea == max(range(all_homes$GrLivArea, na.rm = T))))
outliers = append(outliers, which(all_homes$WoodDeckSF == max(range(all_homes$WoodDeckSF, na.rm = T))))
outliers = append(outliers, which(all_homes$OpenPorchSF == max(range(all_homes$OpenPorchSF, na.rm = T))))
outliers = append(outliers, which(all_homes$EnclosedPorch > 400))
outliers = append(outliers, which(all_homes$`3SsnPorch` > 100))
outliers = append(outliers, which(all_homes$ScreenPorch == max(range(all_homes$ScreenPorch, na.rm = T))))
outliers = unique(outliers)
all_homes = all_homes[-outliers, ]
```

Lets look at the histograms again now that we have removed outliers

``` r
hist_plots = plot_histograms(all_homes, continuous_vars)
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-5.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-6.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-7.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-8.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-9.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-10.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-11.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-12.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-13.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-14.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-15.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-16.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-17.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-18.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-19.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-20.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-21.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-22.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-23.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-24.png)<!-- -->![](AmesHousing_files/figure-gfm/unnamed-chunk-16-25.png)<!-- -->

Looking at the distribution without outliers shows us which continuous
variables should be removed

``` r
all_homes = all_homes %>% 
  select(-MiscVal, -ScreenPorch, -`3SsnPorch`, -EnclosedPorch, -LowQualFinSF, -BsmtFinSF2)
```

For Garage or Basement related columns replace NAs with 0

``` r
garage_bsmt_cols = grep("Garage|Bsmt", names(all_homes), value = TRUE)

all_homes = all_homes %>%
  mutate(across(all_of(garage_bsmt_cols), 
                ~if(is.numeric(.)) replace_na(., 0) else 
                  if(is.character(.)) replace_na(., "0") else 
                    if(is.factor(.)) {
                      f = .
                      if(!("0" %in% levels(f))) {
                        levels(f) = c(levels(f), "0")
                      }
                      f[is.na(f)] = "0"
                      f
                    } else .))

na_counts = colSums(is.na(all_homes[, garage_bsmt_cols, drop = FALSE]))
print(na_counts)
```

    ##     BsmtQual     BsmtCond BsmtExposure BsmtFinType1   BsmtFinSF1 BsmtFinType2 
    ##            0            0            0            0            0            0 
    ##    BsmtUnfSF  TotalBsmtSF BsmtFullBath BsmtHalfBath   GarageType  GarageYrBlt 
    ##            0            0            0            0            0            0 
    ## GarageFinish   GarageCars   GarageArea   GarageQual   GarageCond 
    ##            0            0            0            0            0

Lets see where we are at with missing values

``` r
colSums(is.na(all_homes))
```

    ##    MSSubClass      MSZoning   LotFrontage       LotArea      LotShape 
    ##             0             2           435             0             0 
    ##   LandContour     LotConfig  Neighborhood    Condition1      BldgType 
    ##             0             0             0             0             0 
    ##    HouseStyle   OverallQual   OverallCond     YearBuilt  YearRemodAdd 
    ##             0             0             0             0             0 
    ##     RoofStyle   Exterior1st   Exterior2nd    MasVnrType    MasVnrArea 
    ##             0             1             1            23            22 
    ##     ExterQual     ExterCond    Foundation      BsmtQual      BsmtCond 
    ##             0             0             0             0             0 
    ##  BsmtExposure  BsmtFinType1    BsmtFinSF1  BsmtFinType2     BsmtUnfSF 
    ##             0             0             0             0             0 
    ##   TotalBsmtSF     HeatingQC    CentralAir    Electrical      1stFlrSF 
    ##             0             0             0             1             0 
    ##      2ndFlrSF     GrLivArea  BsmtFullBath  BsmtHalfBath      FullBath 
    ##             0             0             0             0             0 
    ##      HalfBath  BedroomAbvGr   KitchenQual  TotRmsAbvGrd    Functional 
    ##             0             0             1             0             1 
    ##   FireplaceQu    GarageType   GarageYrBlt  GarageFinish    GarageCars 
    ##          1325             0             0             0             0 
    ##    GarageArea    GarageQual    GarageCond    PavedDrive    WoodDeckSF 
    ##             0             0             0             0             0 
    ##   OpenPorchSF        MoSold        YrSold      SaleType SaleCondition 
    ##             0             0             0             1             0 
    ##     SalePrice 
    ##          1361

``` r
all_homes$FireplaceQu = replace_na(all_homes$FireplaceQu, "0")

all_homes = all_homes %>% 
  select(-LotFrontage)
```

For the rest of the data replace NAs with mean or mode depending on type
of column. DO NOT CHANGE NA’S IN THE SALESPRICE COLUMN

``` r
get_mode = function(x) {
  # Remove NAs for calculation
  x = x[!is.na(x)]
  
  # If empty vector, return NA
  if(length(x) == 0) return(NA)
  
  # Find most frequent value
  unique_values = unique(x)
  unique_values[which.max(tabulate(match(x, unique_values)))]
}

# Function to replace NAs in the dataframe
replace_nas = function(data) {
  result = data
  
  for(col in names(data)) {
    # Skip if no NAs
    if(!any(is.na(data[[col]]))) next
    
    # For numeric columns, replace with mean
    if(is.numeric(data[[col]])) {
      mean_val = mean(data[[col]], na.rm = TRUE)
      result[[col]][is.na(result[[col]])] = mean_val
      cat(paste("Replaced NAs in numeric column", col, "with mean:", mean_val, "\n"))
    }
    # For character columns, replace with mode
    else if(is.character(data[[col]])) {
      mode_val = get_mode(data[[col]])
      result[[col]][is.na(result[[col]])] = mode_val
      cat(paste("Replaced NAs in character column", col, "with mode:", mode_val, "\n"))
    }
    # For factor columns, also replace with mode
    else if(is.factor(data[[col]])) {
      mode_val = get_mode(as.character(data[[col]]))
      # Make sure mode is in the levels
      if(!mode_val %in% levels(result[[col]])) {
        levels(result[[col]]) = c(levels(result[[col]]), mode_val)
      }
      result[[col]][is.na(result[[col]])] = mode_val
      cat(paste("Replaced NAs in factor column", col, "with mode:", mode_val, "\n"))
    }
  }
  
  return(result)
}
```

``` r
all_homes_clean = all_homes %>% 
  select(-SalePrice) %>% 
  replace_nas()
```

    ## Replaced NAs in character column MSZoning with mode: RL 
    ## Replaced NAs in character column Exterior1st with mode: VinylSd 
    ## Replaced NAs in character column Exterior2nd with mode: VinylSd 
    ## Replaced NAs in character column MasVnrType with mode: None 
    ## Replaced NAs in numeric column MasVnrArea with mean: 97.8452914798206 
    ## Replaced NAs in character column Electrical with mode: SBrkr 
    ## Replaced NAs in character column KitchenQual with mode: TA 
    ## Replaced NAs in character column Functional with mode: Typ 
    ## Replaced NAs in character column SaleType with mode: WD

``` r
na_count = sapply(all_homes_clean, function(x) sum(is.na(x)))
if(any(na_count > 0)) {
  cat("\nRemaining NAs in columns:\n")
  print(na_count[na_count > 0])
} else {
  cat("\nSuccess! All NAs have been replaced.\n")
}
```

    ## 
    ## Success! All NAs have been replaced.

Lets make sure we now only see missing values in SalesPrice

``` r
all_homes_clean$SalePrice = all_homes$SalePrice
colSums(is.na(all_homes_clean))
```

    ##    MSSubClass      MSZoning       LotArea      LotShape   LandContour 
    ##             0             0             0             0             0 
    ##     LotConfig  Neighborhood    Condition1      BldgType    HouseStyle 
    ##             0             0             0             0             0 
    ##   OverallQual   OverallCond     YearBuilt  YearRemodAdd     RoofStyle 
    ##             0             0             0             0             0 
    ##   Exterior1st   Exterior2nd    MasVnrType    MasVnrArea     ExterQual 
    ##             0             0             0             0             0 
    ##     ExterCond    Foundation      BsmtQual      BsmtCond  BsmtExposure 
    ##             0             0             0             0             0 
    ##  BsmtFinType1    BsmtFinSF1  BsmtFinType2     BsmtUnfSF   TotalBsmtSF 
    ##             0             0             0             0             0 
    ##     HeatingQC    CentralAir    Electrical      1stFlrSF      2ndFlrSF 
    ##             0             0             0             0             0 
    ##     GrLivArea  BsmtFullBath  BsmtHalfBath      FullBath      HalfBath 
    ##             0             0             0             0             0 
    ##  BedroomAbvGr   KitchenQual  TotRmsAbvGrd    Functional   FireplaceQu 
    ##             0             0             0             0             0 
    ##    GarageType   GarageYrBlt  GarageFinish    GarageCars    GarageArea 
    ##             0             0             0             0             0 
    ##    GarageQual    GarageCond    PavedDrive    WoodDeckSF   OpenPorchSF 
    ##             0             0             0             0             0 
    ##        MoSold        YrSold      SaleType SaleCondition     SalePrice 
    ##             0             0             0             0          1361

We also need to address the sqft redundancy. We are going to replace
1stFlrSF and 2ndFlrSF with percents that represent how much of the total
sf they account for.

``` r
all_homes_clean$perc1stSF = all_homes_clean$`1stFlrSF`/all_homes_clean$GrLivArea
all_homes_clean$perc2ndSF = all_homes_clean$`2ndFlrSF`/all_homes_clean$GrLivArea
all_homes_clean = all_homes_clean %>% 
  select(-`1stFlrSF`, -`2ndFlrSF`)
```

``` r
training_index = is.na(all_homes_clean$SalePrice)
train = all_homes_clean[training_index == F, ]
test = all_homes_clean[training_index == T, ]
test = test %>% 
  select(-SalePrice)
```

Review the two new data frames

``` r
dim(train)
```

    ## [1] 1337   60

``` r
dim(test)
```

    ## [1] 1361   59

## Exploratory Analysis

Now that our data is clean, lets explore potential relationships between
variables

#### Fireplaces

Do people still like fireplaces as much as I do? Does the quality of the
fireplace matter?

``` r
train %>% 
  group_by(FireplaceQu) %>% 
  summarise(Count = n())
```

    ## # A tibble: 6 × 2
    ##   FireplaceQu Count
    ##   <chr>       <int>
    ## 1 0             640
    ## 2 Ex             22
    ## 3 Fa             31
    ## 4 Gd            351
    ## 5 Po             18
    ## 6 TA            275

``` r
fire = train %>%
  mutate(FireplaceStatus = ifelse(FireplaceQu == "0", "No Fireplace", "Has Fireplace"))

fire %>% 
  ggplot(aes(x = FireplaceStatus, y = SalePrice/1000, fill = FireplaceStatus, colour = FireplaceStatus)) +
  geom_boxplot(alpha = 0.5) +
  scale_y_continuous(labels = scales::label_dollar()) +
  xlab("Fireplace Status") +
  ylab("Sale Price in 1000s") +
  ggtitle("Are Fireplaces Adding Value to Homes?") +
  theme_minimal() +
  theme(legend.position = "none")
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-28-1.png)<!-- --> It
looks like homes with fireplaces tend to have a higher sales price

``` r
train %>% 
  ggplot(aes(x = factor(FireplaceQu, levels = c("Ex", "Gd", "TA", "Fa", "Po", "0"), ordered = TRUE), y = SalePrice/1000, fill = FireplaceQu, colour = FireplaceQu)) +
  geom_boxplot(alpha = 0.5) +
  scale_y_continuous(labels = scales::label_dollar()) +
  xlab("Fireplace Quality") +
  ylab("Sale Price in 1000s") +
  ggtitle("Are Quality Fireplaces Adding Value to Homes?") +
  theme_minimal() +
  theme(legend.position = "none") 
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-29-1.png)<!-- --> This
box plot does suggest a relationship between quality of fireplace and
sale price.

#### Home Functionality

I am curious to see how it related to other variables, specifically age
because it seems like a home that was considered functional 50 years ago
may not be considered functional today

``` r
all_homes_clean %>% 
  group_by(Functional) %>% 
  summarise(Count = n())
```

    ## # A tibble: 7 × 2
    ##   Functional Count
    ##   <chr>      <int>
    ## 1 Maj1          15
    ## 2 Maj2           7
    ## 3 Min1          50
    ## 4 Min2          59
    ## 5 Mod           25
    ## 6 Sev            2
    ## 7 Typ         2540

``` r
all_homes_clean %>% 
  ggplot(aes(x = factor(Functional, levels = c("Typ", "Min1", "Min2", "Mod", "Maj1", "Maj2", "Sev"), ordered = TRUE), y = YearBuilt, fill = Functional, colour = Functional)) +
  geom_boxplot(alpha = 0.5) +
  xlab("Functionality") +
  ylab("Year Built") +
  ggtitle("Are Older Homes Considered Less Functional?") +
  theme_minimal() +
  theme(legend.position = "none") 
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-31-1.png)<!-- --> It does
look like there might be a relationship between functionality and age of
a home but the relationship does not seem very strong. Additionally, a
large majority of the homes were rated as typical functionality (Typ).
It would be helpful to have more data from the other groups.

#### Half Baths and Multi-story homes

It seems like most two story homes have a half bath on the main level. I
want to see if the number of half baths a home has is related to the
number of stories.

``` r
all_homes_clean %>% 
  group_by(HalfBath) %>% 
  summarise(Count = n())
```

    ## # A tibble: 3 × 2
    ##   HalfBath Count
    ##      <dbl> <int>
    ## 1        0  1689
    ## 2        1   987
    ## 3        2    22

``` r
stories = all_homes_clean %>%
  mutate(numStories = ifelse(perc2ndSF == "0", "One Story", "Two Story"))
```

``` r
stories %>% 
  ggplot(aes(fill = numStories, x = HalfBath)) +
    geom_bar(position = "dodge", alpha = 0.5) +
    xlab("Half Baths") +
    ylab("Count") +
    ggtitle("Are 2-Story Homes More Likely to have a Half Bath?") +
    labs(fill = "Number of Stories") +
    theme_minimal() 
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-33-1.png)<!-- --> From
this plot, it seems clear to me that 2-story houses are more likely to
have a half bath.

Relationship between Overall Quality and Sales Price

``` r
train %>% 
  select(SalePrice, OverallQual) %>% 
  ggplot(aes(x = as.factor(OverallQual), y = SalePrice, fill = as.factor(OverallQual), colour = OverallQual)) +
  geom_boxplot(alpha = .5) +
  scale_y_continuous(labels = scales::label_dollar()) +
  xlab("Overall Quality") +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Sales Price and Overall Quality")
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

### Variable selection throught randomForest importance metric

Split the training data into train and validate

``` r
set.seed(23)

trainIndex = createDataPartition(train$SalePrice, p = 0.7, list = FALSE)
train_set = train[trainIndex, ]
validation_set = train[-trainIndex, ]

cat("Training set size:", nrow(train_set), "\n")
```

    ## Training set size: 937

``` r
cat("Validation set size:", nrow(validation_set), "\n")
```

    ## Validation set size: 400

Build RF and get the 10 most important variables

``` r
set.seed(23)

rf_model = randomForest(
  x = train_set[, setdiff(names(train_set), "SalePrice")],
  y = train_set$SalePrice,
  importance = TRUE,
  ntree = 500
)

importance_df = as.data.frame(randomForest::importance(rf_model))
importance_df$Variable = rownames(importance_df)

sorted_importance = importance_df[order(importance_df$`%IncMSE`, decreasing = TRUE), ]

top_10_vars = head(sorted_importance$Variable, 10)

cat("Top 10 most important variables:\n")
```

    ## Top 10 most important variables:

``` r
print(top_10_vars)
```

    ##  [1] "GrLivArea"   "OverallQual" "TotalBsmtSF" "BsmtFinSF1"  "GarageArea" 
    ##  [6] "YearBuilt"   "ExterQual"   "FireplaceQu" "MSZoning"    "LotArea"

Plot of variable importance

``` r
sorted_importance[1:10, ] %>%
  ggplot(aes(reorder(x = Variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_bar(stat = "identity", fill = "orange", color = "black", width = 0.5) +
  coord_flip() +
  labs(x = "Variables", y = "Variable importance") +
  theme_minimal()
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

Create a dataframe with only the top 10 variables plus SalePrice

``` r
train_set_top10 = train_set[, c(top_10_vars, "SalePrice")]
validation_set_top10 = validation_set[, c(top_10_vars, "SalePrice")]

cat("Dimensions of reduced training set:", dim(train_set_top10), "\n")
```

    ## Dimensions of reduced training set: 937 11

``` r
cat("Dimensions of reduced validation set:", dim(validation_set_top10), "\n")
```

    ## Dimensions of reduced validation set: 400 11

##### Linear Regression Model

Check for multicollienarity issues

``` r
linear_train_top10 = train_set_top10 %>% 
  mutate(ExterQual = as.factor(ExterQual), MSZoning = as.factor(MSZoning), FireplaceQu = as.factor(FireplaceQu))
linear_val_top10 = validation_set_top10 %>% 
  mutate(ExterQual = as.factor(ExterQual), MSZoning = as.factor(MSZoning), FireplaceQu = as.factor(FireplaceQu))
correlation_matrix = linear_train_top10 %>% 
  select(-ExterQual, -MSZoning, -FireplaceQu) %>% 
  cor()
print("Correlation matrix of predictors:")
```

    ## [1] "Correlation matrix of predictors:"

``` r
print(round(correlation_matrix, 2))
```

    ##             GrLivArea OverallQual TotalBsmtSF BsmtFinSF1 GarageArea YearBuilt
    ## GrLivArea        1.00        0.60        0.41       0.13       0.49      0.22
    ## OverallQual      0.60        1.00        0.54       0.18       0.57      0.58
    ## TotalBsmtSF      0.41        0.54        1.00       0.44       0.46      0.39
    ## BsmtFinSF1       0.13        0.18        0.44       1.00       0.22      0.24
    ## GarageArea       0.49        0.57        0.46       0.22       1.00      0.47
    ## YearBuilt        0.22        0.58        0.39       0.24       0.47      1.00
    ## LotArea          0.40        0.18        0.32       0.17       0.29      0.06
    ## SalePrice        0.73        0.81        0.66       0.37       0.64      0.56
    ##             LotArea SalePrice
    ## GrLivArea      0.40      0.73
    ## OverallQual    0.18      0.81
    ## TotalBsmtSF    0.32      0.66
    ## BsmtFinSF1     0.17      0.37
    ## GarageArea     0.29      0.64
    ## YearBuilt      0.06      0.56
    ## LotArea        1.00      0.38
    ## SalePrice      0.38      1.00

``` r
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45)
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

### Model using top 10 features

Using the 10 most important variables, I am going to explore different
models.

``` r
set.seed(23)

ctrl = trainControl(
  method = "cv",           
  number = 10,              
  verboseIter = TRUE,      
  savePredictions = "final" 
)


build_and_evaluate = function(model_type, train_data, test_data, tune_grid = NULL) {
  formula = as.formula("SalePrice ~ .")
  
  # Train model
  if (!is.null(tune_grid)) {
    model = train(
      formula,
      data = train_data,
      method = model_type,
      trControl = ctrl,
      metric = "RMSE",
      tuneGrid = tune_grid
    )
  } else {
    model = train(
      formula,
      data = train_data,
      method = model_type,
      trControl = ctrl,
      metric = "RMSE"
    )
  }
  
  # Make predictions
  predictions = predict(model, newdata = test_data)
  
  # Calculate RMSE
  rmse = sqrt(mean((predictions - test_data$SalePrice)^2))
  
  # Calculate R-squared
  rsq = 1 - sum((test_data$SalePrice - predictions)^2) / 
    sum((test_data$SalePrice - mean(test_data$SalePrice))^2)
  
  # Return results
  return(list(
    model = model,
    predictions = predictions,
    rmse = rmse,
    rsq = rsq
  ))
}
```

Build Linear Regression models

``` r
initial_lm = lm(SalePrice ~ ., data = linear_train_top10)
vif_values = vif(initial_lm)
print("Variance Inflation Factors (VIF):")
```

    ## [1] "Variance Inflation Factors (VIF):"

``` r
print(vif_values)
```

    ##                 GVIF Df GVIF^(1/(2*Df))
    ## GrLivArea   2.150069  1        1.466311
    ## OverallQual 3.533919  1        1.879872
    ## TotalBsmtSF 1.939798  1        1.392766
    ## BsmtFinSF1  1.340646  1        1.157863
    ## GarageArea  1.806414  1        1.344029
    ## YearBuilt   2.404847  1        1.550757
    ## ExterQual   2.926777  3        1.196001
    ## FireplaceQu 1.805504  5        1.060864
    ## MSZoning    1.794050  4        1.075794
    ## LotArea     1.565582  1        1.251232

``` r
models = list()
models$lm = build_and_evaluate("lm", linear_train_top10, linear_val_top10)
```

    ## + Fold01: intercept=TRUE 
    ## - Fold01: intercept=TRUE 
    ## + Fold02: intercept=TRUE 
    ## - Fold02: intercept=TRUE 
    ## + Fold03: intercept=TRUE 
    ## - Fold03: intercept=TRUE 
    ## + Fold04: intercept=TRUE 
    ## - Fold04: intercept=TRUE 
    ## + Fold05: intercept=TRUE 
    ## - Fold05: intercept=TRUE 
    ## + Fold06: intercept=TRUE 
    ## - Fold06: intercept=TRUE 
    ## + Fold07: intercept=TRUE 
    ## - Fold07: intercept=TRUE 
    ## + Fold08: intercept=TRUE 
    ## - Fold08: intercept=TRUE 
    ## + Fold09: intercept=TRUE 
    ## - Fold09: intercept=TRUE 
    ## + Fold10: intercept=TRUE 
    ## - Fold10: intercept=TRUE 
    ## Aggregating results
    ## Fitting final model on full training set

Set tuning parameters for each model we will be looking at

``` r
# Random Forest
rf_grid = expand.grid(
  mtry = c(3, 4, 5, 6)
)

# SVM
svm_grid = expand.grid(
  sigma = c(0.01, 0.1, 1),
  C = c(1, 10, 100)
)

# XGBoost
xgb_grid = expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 6),
  eta = c(0.1, 0.3),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 1
)
```

##### RF, SVM and XGB

``` r
set.seed(23)

# Random Forest
models$rf = build_and_evaluate("rf", train_set_top10, validation_set_top10, rf_grid)

# SVM
models$svm = build_and_evaluate("svmRadial", train_set_top10, validation_set_top10, svm_grid)

# XGBoost
models$xgb = build_and_evaluate("xgbTree", train_set_top10, validation_set_top10, xgb_grid)
```

### Evaluate and Compare Models

``` r
# Compare results
results = data.frame(
  Model = character(),
  RMSE = numeric(),
  Rsquared = numeric(),
  stringsAsFactors = FALSE
)

for (name in names(models)) {
  if (!is.null(models[[name]])) {
    results = rbind(results, data.frame(
      Model = name,
      RMSE = models[[name]]$rmse,
      Rsquared = models[[name]]$rsq
    ))
  }
}

print(results[order(results$RMSE), ])
```

    ##   Model     RMSE  Rsquared
    ## 3   svm 24320.06 0.8962633
    ## 2    rf 24583.72 0.8940019
    ## 4   xgb 24625.06 0.8936450
    ## 1    lm 27081.94 0.8713640

### Can we beat the svm

Lets try some more models

``` r
library(glmnet)     # For ridge and lasso regression
library(rpart)      # For decision trees
library(gbm)        # For gradient boosting
library(earth)      # For MARS (Multivariate Adaptive Regression Splines)
library(Cubist)     # For Cubist models
library(kernlab)    # For kernlab SVM implementation

set.seed(23)

# 1. Ridge Regression
ridge_grid = expand.grid(alpha = 0, 
                         lambda = 10^seq(-3, 3, length = 10))
models$ridge = build_and_evaluate("glmnet", train_set_top10, validation_set_top10, ridge_grid)

# 2. Lasso Regression
lasso_grid = expand.grid(alpha = 1, 
                         lambda = 10^seq(-3, 3, length = 10))
models$lasso = build_and_evaluate("glmnet", train_set_top10, validation_set_top10, lasso_grid)

# 3. Elastic Net (combination of Ridge and Lasso)
elasticnet_grid = expand.grid(alpha = seq(0, 1, length = 5), 
                              lambda = 10^seq(-3, 3, length = 6))
models$elasticnet = build_and_evaluate("glmnet", train_set_top10, validation_set_top10, elasticnet_grid)

# 4. Decision Tree with tuning
rpart_grid = expand.grid(cp = seq(0.001, 0.1, length = 10))
models$rpart = build_and_evaluate("rpart", train_set_top10, validation_set_top10, rpart_grid)

# 5. Gradient Boosting Machine (GBM)
gbm_grid = expand.grid(
  n.trees = c(100, 500),
  interaction.depth = c(3, 5, 7),
  shrinkage = c(0.01, 0.1),
  n.minobsinnode = 10
)
models$gbm = build_and_evaluate("gbm", train_set_top10, validation_set_top10, gbm_grid)

# 6. MARS (Multivariate Adaptive Regression Splines)
mars_grid = expand.grid(
  degree = 1:3,
  nprune = c(5, 10, 15, 20)
)
models$mars = build_and_evaluate("earth", train_set_top10, validation_set_top10, mars_grid)

# 7. Cubist model (rule-based model)
cubist_grid = expand.grid(
  committees = c(1, 10, 50, 100),
  neighbors = c(0, 5, 9)
)
models$cubist = build_and_evaluate("cubist", train_set_top10, validation_set_top10, cubist_grid)

# 8. Gaussian Process
models$gausspr = build_and_evaluate("gaussprRadial", train_set_top10, validation_set_top10)

# 9. k-Nearest Neighbors
knn_grid = expand.grid(k = c(3, 5, 7, 9))
models$knn = build_and_evaluate("knn", train_set_top10, validation_set_top10, knn_grid)

# Update results table with all models
results = data.frame(
  Model = character(),
  RMSE = numeric(),
  Rsquared = numeric(),
  stringsAsFactors = FALSE
)

for (name in names(models)) {
  if (!is.null(models[[name]])) {
    results = rbind(results, data.frame(
      Model = name,
      RMSE = models[[name]]$rmse,
      Rsquared = models[[name]]$rsq
    ))
  }
}
```

``` r
print(results[order(results$RMSE), ])
```

    ##         Model     RMSE  Rsquared
    ## 9         gbm 24026.14 0.8987556
    ## 3         svm 24320.06 0.8962633
    ## 11     cubist 24461.43 0.8950538
    ## 2          rf 24583.72 0.8940019
    ## 4         xgb 24625.06 0.8936450
    ## 10       mars 25081.77 0.8896634
    ## 12    gausspr 26721.34 0.8747668
    ## 1          lm 27081.94 0.8713640
    ## 7  elasticnet 27089.73 0.8712900
    ## 6       lasso 27093.72 0.8712521
    ## 5       ridge 27926.98 0.8632111
    ## 8       rpart 32609.29 0.8134970
    ## 13        knn 42022.22 0.6902859

##### Plot RMSE

``` r
ggplot(results, aes(x = reorder(Model, -RMSE), y = RMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(RMSE, 0)), hjust = -0.1) +
  coord_flip(ylim = c(20000,45000)) +
  labs(title = "Model Comparison by RMSE",
       x = "Model",
       y = "RMSE (lower is better)") +
  theme_minimal()
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

##### Plot Rsq

``` r
ggplot(results, aes(x = reorder(Model, Rsquared), y = Rsquared)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = round(Rsquared, 3)), hjust = -0.1) +
  coord_flip(ylim = c(0.65, 1.0)) +
  labs(title = "Model Comparison by R-squared",
       x = "Model",
       y = "R-squared (higher is better)") +
  theme_minimal()
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

Identify top 3 models

``` r
top3_models = results$Model[order(results$RMSE)][1:3]
cat("Top 3 models:", paste(top3_models, collapse=", "), "\n")
```

    ## Top 3 models: gbm, svm, cubist

Ensemble method: Average predictions from top 3 models

``` r
set.seed(23)

# Create average predictions
ensemble_preds = numeric(nrow(validation_set_top10))
for (model_name in top3_models) {
  preds = models[[model_name]]$predictions
  if (length(preds) != nrow(validation_set_top10)) {
    stop(paste("Predictions from", model_name, "are not the right length"))
  }
  ensemble_preds = ensemble_preds + preds
}
ensemble_preds = ensemble_preds / length(top3_models)


# Calculate performance metrics for ensemble
ensemble_rmse = sqrt(mean((ensemble_preds - validation_set_top10$SalePrice)^2))
ensemble_rsq = 1 - sum((validation_set_top10$SalePrice - ensemble_preds)^2) / 
  sum((validation_set_top10$SalePrice - mean(validation_set_top10$SalePrice))^2)

# Add ensemble to results
results = rbind(results, data.frame(
  Model = "ensemble_top3",
  RMSE = ensemble_rmse,
  Rsquared = ensemble_rsq
))

print(results[order(results$RMSE), ])
```

    ##            Model     RMSE  Rsquared
    ## 14 ensemble_top3 23491.98 0.9032073
    ## 9            gbm 24026.14 0.8987556
    ## 3            svm 24320.06 0.8962633
    ## 11        cubist 24461.43 0.8950538
    ## 2             rf 24583.72 0.8940019
    ## 4            xgb 24625.06 0.8936450
    ## 10          mars 25081.77 0.8896634
    ## 12       gausspr 26721.34 0.8747668
    ## 1             lm 27081.94 0.8713640
    ## 7     elasticnet 27089.73 0.8712900
    ## 6          lasso 27093.72 0.8712521
    ## 5          ridge 27926.98 0.8632111
    ## 8          rpart 32609.29 0.8134970
    ## 13           knn 42022.22 0.6902859

##### Plot the Best Predictions

``` r
plot_data = data.frame(
  Actual = validation_set_top10$SalePrice,
  Predicted = ensemble_preds
)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_x_continuous(labels = scales::label_dollar()) +
  labs(
    title = "Actual vs. Predicted Sale Prices (Ensemble)",
    x = "Actual Sale Price",
    y = "Predicted Sale Price"
  ) +
  theme_minimal()
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->

### Predict on the Test/Predict Set

``` r
top3_models
```

    ## [1] "gbm"    "svm"    "cubist"

Get predictions for the top 3 models and take an average

``` r
set.seed(23)

avg_test_preds = c()

predictions1 = predict(models$svm$model, newdata = test)
predictions2 = predict(models$cubist$model, newdata = test)
predictions3 = predict(models$gbm$model, newdata = test)

avg_test_preds = ((predictions1 + predictions2 + predictions3)/3)
test$SalePricePred = round(avg_test_preds,-2)
```

``` r
head(test[,56:60])
```

    ## # A tibble: 6 × 5
    ##   SaleType SaleCondition perc1stSF perc2ndSF SalePricePred
    ##   <chr>    <chr>             <dbl>     <dbl>         <dbl>
    ## 1 WD       Normal            1         0            120300
    ## 2 WD       Normal            0.570     0.430        177100
    ## 3 WD       Normal            0.577     0.423        182000
    ## 4 WD       Normal            1         0            188600
    ## 5 WD       Normal            0.461     0.539        173100
    ## 6 WD       Normal            0.539     0.461        163000

Plot distributions of predictions for the test set

``` r
test %>% 
  ggplot(aes(x = SalePricePred)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  geom_vline(xintercept = mean(test$SalePricePred), color = "red", linetype = "dashed") +
  geom_vline(xintercept = median(test$SalePricePred), color = "darkgreen") +
  scale_x_continuous(labels = scales::label_dollar()) +
  ggtitle("Distribution of Sale Price Predictions") +
  xlab("Sale Price Predictions") +
  ylab("Count") +
  theme_minimal()
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-55-1.png)<!-- --> Plot of
the distribution of `SalePrice` from the training set

``` r
train %>% 
  filter(SalePrice < 570000) %>% 
  ggplot(aes(x = SalePrice)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  geom_vline(xintercept = mean(train$SalePrice), color = "red", linetype = "dashed") +
  geom_vline(xintercept = median(train$SalePrice), color = "darkgreen") +
  scale_x_continuous(labels = scales::label_dollar()) +
  ggtitle("Distribution of Sale Price") +
  xlab("Sale Price") +
  ylab("Count") +
  theme_minimal()
```

![](AmesHousing_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

``` r
median(test$SalePricePred)
```

    ## [1] 158600

``` r
median(train$SalePrice)
```

    ## [1] 162000

``` r
mean(test$SalePricePred)
```

    ## [1] 178798.6

``` r
mean(train$SalePrice)
```

    ## [1] 179207.5
