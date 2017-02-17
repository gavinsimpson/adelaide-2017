df <- data.frame(matrix(rpois(10*5, 4), ncol = 5))
names(df) <- paste("spp", seq_len(ncol(df)))
df

## transformations
library("analogue")
?tran

tran(df, method = "percent")

tran(df, method = "pa")

tran(df, method = "hellinger")

tran(df, method = "logRatio")

## why transform
set.seed(1)
vec <- rlnorm(100, 0.25, 3)

plot(density(vec))
plot(density(log10(vec)))

## selecting some species/variables

### remove a variable

names(df)
df[, -3]
df[, -which(names(df) == "spp 3")]
df[, !names(df) == "spp 3"]

df[3:5, ]

## removing rare species
chooseTaxa(df, max = 7)
take <- chooseTaxa(df, max = 7,
                   value = FALSE)
take
df[, take]

## putting data in tidy format

set.seed(1)
df2 <- data.frame(Site = 1:10,
                  `1970` = rnorm(10),
                  `1980` = rnorm(10),
                  `1990` = rnorm(10),
                  check.names = FALSE)
df2

library("tidyr")
tidydf2 <- gather(df2, key = Year,
                  value = Area, -Site)
head(tidydf2)

## applying functions to columns/rows
apply(df, 2, mean)
apply(tran(df, "percent"), 1L, sum)

rowSums(df)
rowSums(tran(df, "proportion"))

## species richness
rowSums(df > 0)




## Censoring
df3 <- data.frame(A = 1:5,
                  B = c(1,2,"<1",2,1))
df3

df3 <- transform(df3, B = as.character(B),
                 stringsAsFactors = FALSE)
str(df3)
df3 <-
  transform(df3,
            Cens = grepl("<", B),
            B = as.numeric(gsub("<", "",
                                B)))
df3

df3 <-
  transform(df3,
            B = ifelse(Cens, B / 2, B))
df3
