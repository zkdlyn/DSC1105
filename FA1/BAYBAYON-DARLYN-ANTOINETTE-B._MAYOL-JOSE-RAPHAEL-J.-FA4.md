FA4 Conditional Probability
================
Baybayon, Darlyn Antoinette \| Mayol, Jose Raphael

### Question 4

A geospatial analysis system has four sensors supplying images. The
percentage of images supplied by each sensor and the percentage of
images relevant to a query are shown in the following table.

| Sensor | % of Images Supplied | % of Relevant Images |
|-------:|---------------------:|---------------------:|
|      1 |                   15 |                   50 |
|      2 |                   20 |                   15 |
|      3 |                   25 |                   80 |
|      4 |                   40 |                   85 |

What is the overall percentage of relevant images?

Let P(S) be the probability of images supplied, and P(R\|S) be the
probability of relevant image given images supplied.

Considering the law of total probability, we can obtain the overall
percentage of relevant images using the formula:

overall_R = P(S1) \* P(R\|S1) + P(S2) \* P(R\|S2) + P(S3) \* P(R\|S3) +
P(S4) \* P(R\|S4)

``` r
overall_R <- (0.15*0.5)+(0.2*0.6)+(0.25*0.8)+(0.4*0.85)
print(paste("Overall Percentage: ", overall_R*100))
```

    ## [1] "Overall Percentage:  73.5"

**Therefore, the overall percentage of relevant images is 73.5%.**

### Question 6

A fair coin is tossed twice. Let E1 be the event that both tosses have
the same outcome, that is, E1 = (HH, TT). Let E2 be the event that the
first toss is a head, that is, E2 = (HH, HT). Let E3 be the event that
the second toss is a head, that is, E3 = (TH, HH). Show that E1, E2, and
E3 are pairwise independent but not mutually independent.

First, we must define the sample space for the situation where a fair
coin is tossed twice. In this case, we have 2\*2 or 4 possible outputs:
HH, HT, TH, & TT. Then, we must also define the given sets of E1, E2,
and E3.

``` r
# Sample Space
sample_space <- c("HH", "HT", "TH", "TT")

# Given Events
E1 <- c("HH", "TT")
E2 <- c("HH", "HT")
E3 <- c("TH", "HH")
```

Next, we have to create the process for calculating the probability of
each set happening. This value can be obtained by dividing the number of
outcomes in the set by the total sample space. To make the process
simpler, we can make a function for this:

``` r
# Probability Function
get_probability <- function(event, sample_space) {
  count <- sum(sample_space %in% event)
  probability <- count / length(sample_space)
  return(probability)
}
```

Now, we can get the probabilities for each event.

``` r
# Probabilities for each event
P_E1 <- get_probability(E1, sample_space)
P_E2 <- get_probability(E2, sample_space)
P_E3 <- get_probability(E3, sample_space)
P_E1_E2 <- get_probability(intersect(E1, E2), sample_space)
P_E1_E3 <- get_probability(intersect(E1, E3), sample_space)
P_E2_E3 <- get_probability(intersect(E2, E3), sample_space)
P_E1_E2_E3 <- get_probability(intersect(intersect(E1, E2), E3), sample_space)

cat("P(E1):", P_E1,"; P(E2):", P_E2,"; P(E3):", P_E3,"\n",
    "P(E1 ∩ E2):", P_E1_E2,"; P(E1 ∩ E3):", P_E1_E3,"; P(E2 ∩ E3):", P_E2_E3,"\n",
    "P(E1 ∩ E2 ∩ E3):", P_E1_E2_E3,"\n"
    )
```

    ## P(E1): 0.5 ; P(E2): 0.5 ; P(E3): 0.5 
    ##  P(E1 ∩ E2): 0.25 ; P(E1 ∩ E3): 0.25 ; P(E2 ∩ E3): 0.25 
    ##  P(E1 ∩ E2 ∩ E3): 0.25

By the Definition of the Independence of Two Events, A and B are said to
be independent of one another if it satisfies the condition: P(A ∩ B) =
P(A)P(B). We can observe that P(E1 ∩ E2) = P(E1)P(E2) can be simplified
to 0.25 = (0.5)(0.5), which is true. The same can be said for P(E1 ∩ E3)
= P(E1)P(E3) & P(E2 ∩ E3) = P(E2)P(E3). This means that **E1, E2, & E3
are pairwise independent events**.

For mutual independence, it must again satisfy the Definition of the
Independence of Events. In this case, E1, E2, & E3 must satisfy the
condition: P(E1 ∩ E2 ∩ E3) = P(E1)P(E2)P(E3). From the probabilities
obtained, we can see that P(E1 ∩ E2 ∩ E3) = 0.25, but P(E1)P(E2)P(E3) =
(0.5)(0.5)(0.5) = 0.125. Hence by contradiction, the premise P(E1 ∩ E2 ∩
E3) = P(E1)P(E2)P(E3) is NOT TRUE, which also implies that **E1, E2, &
E3 are NOT mutually independent events.**

``` r
# Check pairwise independence
pairwise_independence <- (P_E1_E2 == P_E1 * P_E2) &
  (P_E1_E3 == P_E1 * P_E3) &
  (P_E2_E3 == P_E2 * P_E3)

# Check mutual independence
mutual_independence <- (P_E1_E2_E3 == P_E1 * P_E2 * P_E3)

cat("Pairwise Independence:", pairwise_independence, "\n",
    "Mutual Independence:", mutual_independence, "\n")
```

    ## Pairwise Independence: TRUE 
    ##  Mutual Independence: FALSE

Thus, we have shown that **E1, E2, & E3 are pairwise independent but not
mutually independent**.
