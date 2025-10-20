
<!-- README.md is generated from README.Rmd. Please edit the README.Rmd file -->

# Lab report \#3 - instructions

Follow the instructions posted at
<https://ds202-at-isu.github.io/labs.html> for the lab assignment. The
work is meant to be finished during the lab time, but you have time
until Monday evening to polish things.

Include your answers in this document (Rmd file). Make sure that it
knits properly (into the md file). Upload both the Rmd and the md file
to your repository.

All submissions to the github repo will be automatically uploaded for
grading once the due date is passed. Submit a link to your repository on
Canvas (only one submission per team) to signal to the instructors that
you are done with your submission.

# Lab 3: Avenger’s Peril

## As a team

Extract from the data below two data sets in long form `deaths` and
`returns`

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(readr)
av <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/avengers/avengers.csv", stringsAsFactors = FALSE)
head(av)
```

    ##                                                       URL
    ## 1           http://marvel.wikia.com/Henry_Pym_(Earth-616)
    ## 2      http://marvel.wikia.com/Janet_van_Dyne_(Earth-616)
    ## 3       http://marvel.wikia.com/Anthony_Stark_(Earth-616)
    ## 4 http://marvel.wikia.com/Robert_Bruce_Banner_(Earth-616)
    ## 5        http://marvel.wikia.com/Thor_Odinson_(Earth-616)
    ## 6       http://marvel.wikia.com/Richard_Jones_(Earth-616)
    ##                    Name.Alias Appearances Current. Gender Probationary.Introl
    ## 1   Henry Jonathan "Hank" Pym        1269      YES   MALE                    
    ## 2              Janet van Dyne        1165      YES FEMALE                    
    ## 3 Anthony Edward "Tony" Stark        3068      YES   MALE                    
    ## 4         Robert Bruce Banner        2089      YES   MALE                    
    ## 5                Thor Odinson        2402      YES   MALE                    
    ## 6      Richard Milhouse Jones         612      YES   MALE                    
    ##   Full.Reserve.Avengers.Intro Year Years.since.joining Honorary Death1 Return1
    ## 1                      Sep-63 1963                  52     Full    YES      NO
    ## 2                      Sep-63 1963                  52     Full    YES     YES
    ## 3                      Sep-63 1963                  52     Full    YES     YES
    ## 4                      Sep-63 1963                  52     Full    YES     YES
    ## 5                      Sep-63 1963                  52     Full    YES     YES
    ## 6                      Sep-63 1963                  52 Honorary     NO        
    ##   Death2 Return2 Death3 Return3 Death4 Return4 Death5 Return5
    ## 1                                                            
    ## 2                                                            
    ## 3                                                            
    ## 4                                                            
    ## 5    YES      NO                                             
    ## 6                                                            
    ##                                                                                                                                                                              Notes
    ## 1                                                                                                                Merged with Ultron in Rage of Ultron Vol. 1. A funeral was held. 
    ## 2                                                                                                  Dies in Secret Invasion V1:I8. Actually was sent tto Microverse later recovered
    ## 3 Death: "Later while under the influence of Immortus Stark committed a number of horrible acts and was killed.'  This set up young Tony. Franklin Richards later brought him back
    ## 4                                                                               Dies in Ghosts of the Future arc. However "he had actually used a hidden Pantheon base to survive"
    ## 5                                                      Dies in Fear Itself brought back because that's kind of the whole point. Second death in Time Runs Out has not yet returned
    ## 6                                                                                                                                                                             <NA>

Get the data into a format where the five columns for Death\[1-5\] are
replaced by two columns: Time, and Death. Time should be a number
between 1 and 5 (look into the function `parse_number`); Death is a
categorical variables with values “yes”, “no” and ““. Call the resulting
data set `deaths`.

``` r
deaths <- av |>
  select(Name.Alias, starts_with("Death")) |>
  pivot_longer(
    cols= starts_with("Death"),
    names_to = "Time",
    values_to = "Death"
    ) |>
  mutate(
    Time = parse_number(Time)
  ) |>
  filter(Death != "") |>
  mutate(Death = tolower(Death))

head(deaths, n=10)
```

    ## # A tibble: 10 × 3
    ##    Name.Alias                       Time Death
    ##    <chr>                           <dbl> <chr>
    ##  1 "Henry Jonathan \"Hank\" Pym"       1 yes  
    ##  2 "Janet van Dyne"                    1 yes  
    ##  3 "Anthony Edward \"Tony\" Stark"     1 yes  
    ##  4 "Robert Bruce Banner"               1 yes  
    ##  5 "Thor Odinson"                      1 yes  
    ##  6 "Thor Odinson"                      2 yes  
    ##  7 "Richard Milhouse Jones"            1 no   
    ##  8 "Steven Rogers"                     1 yes  
    ##  9 "Clinton Francis Barton"            1 yes  
    ## 10 "Clinton Francis Barton"            2 yes

Similarly, deal with the returns of characters.

``` r
return <- av |>
  select(Name.Alias, starts_with("Return")) |>
  pivot_longer(
    cols= starts_with("Return"),
    names_to= "Time",
    values_to= "Return"
  ) |>
  mutate(Time= parse_number(Time)) |>
  filter(Return != "") |>
  mutate(Return = tolower(Return))

head(return, n= 10)
```

    ## # A tibble: 10 × 3
    ##    Name.Alias                       Time Return
    ##    <chr>                           <dbl> <chr> 
    ##  1 "Henry Jonathan \"Hank\" Pym"       1 no    
    ##  2 "Janet van Dyne"                    1 yes   
    ##  3 "Anthony Edward \"Tony\" Stark"     1 yes   
    ##  4 "Robert Bruce Banner"               1 yes   
    ##  5 "Thor Odinson"                      1 yes   
    ##  6 "Thor Odinson"                      2 no    
    ##  7 "Steven Rogers"                     1 yes   
    ##  8 "Clinton Francis Barton"            1 yes   
    ##  9 "Clinton Francis Barton"            2 yes   
    ## 10 "Pietro Maximoff"                   1 yes

Based on these datasets calculate the average number of deaths an
Avenger suffers.

``` r
summary <- deaths |>
  group_by(Name.Alias) |>
  summarise(
    death_count = sum(Death == "yes", na.rm = TRUE)
  )
mean(summary$death_count, na.rm = TRUE)
```

    ## [1] 0.5460123

## Individually

For each team member, copy this part of the report.

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### FiveThirtyEight Statement

> Quote the statement you are planning to fact-check.

“Out of 173 listed Avengers, my analysis found that 69 had died at least
one time after they joined the team. That’s about 40 percent of all
people who have ever signed on to the team.”

### Include the code

Make sure to include the code to derive the (numeric) fact for the
statement

``` r
total_avengers <- av |>
  summarise(total = n())
total_avengers
```

    ##   total
    ## 1   173

``` r
dead_avengers <- deaths |>
  group_by(Name.Alias) |>
  summarise(any_death= any(Death == "yes")) |>
  summarise(count = sum(any_death))

dead_avengers / total_avengers * 100 # Death percentage
```

    ##      count
    ## 1 36.99422

### Include your answer

The author claims that about 40 percent of all people who have ever
signed to the team has died at least once, the data says that about 37%
of people who join die, therefore their claim is accurate.

Include at least one sentence discussing the result of your
fact-checking endeavor.

Upload your changes to the repository. Discuss and refine answers as a
team.
