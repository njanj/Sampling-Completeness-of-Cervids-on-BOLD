#Reindeer, moose, and white-tailed deer (Rangifer tarandus, Alces alces, and Odocoileus virginianus, respectively) are common North American members of the Cervidae family (Gilbert et al., 2006). Cervidae is a family of large, four-legged, and often antlered mammals who are highly abundant, exhibit great diversity, and are culturally relevant in many of the places that they inhabit. Commonly referred to as cervids, members of this family are of great interest to conservation organizations due to their importance in their local ecosystems. They fill a unique role as large prey animals and are frequently hunted and farmed by humans. As a natural resource and a source of admirability among wild animals, accurate monitoring of their population levels is essential for their conservation. (Kumar et al., 2018)

#Compared to other members of the order Artiodactyla, cervids are rather well represented on BOLD with the second highest record count in the order (Artiodactyla | Taxonomy Browser | BOLDSYSTEMS, 2021).  In the past, however, phylogenetic analyses using cervid DNA barcodes have not kept up with other species (Gilbert et al., 2006). In the case of Kumar et al. (2018), illegal distribution of poached meat created a demand for a reliable way to identify at-risk deer species without being able to use morphological methods. Meat was often processed and mixed before authorities could identify what animal it came from. This study aimed to improve the sampling richness of DNA barcodes for local deer species and addressed a lack of comprehensive sampling in the region. Inspired by this study, the following analysis will assess the sampling completeness of the family Cervidae using global data from BOLD.

#The tidyverse (Wickham et al, 2019) and vegan (Kumar et al, 2020) packages will be used in this analysis. These packages are loaded below:

library('tidyverse')
library('vegan')

#The data can be retrieved using the BOLD API. The link is provided below:

Cervidae <- read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Cervidae&format=tsv")

#The data are written to disk and assigned to a data frame.

write_tsv(Cervidae, "Cervidae_BOLD_data.tsv")
dfCervidae <- read_tsv("Cervidae_BOLD_data.tsv")

#Below are some summary statistics of the data:

summary(dfCervidae)
dim(dfCervidae)

#As a preliminary analysis, a rarefaction curve shows the BIN richness of Cervidae using all available data.

#We begin by creating a data frame containing the information needed for a global analysis of species richness using a rarefaction curve. We filter out BINs with value NA, group the data by BIN, then add a column for the counts of each BIN.

bin_count <- dfCervidae %>%
  filter(!is.na(bin_uri)) %>%
  group_by(bin_uri) %>%
  count(bin_uri)

#Next, we arrange our new data frame into the format required by the vegan package:

bin_count_spread <- pivot_wider(data = bin_count, names_from  = bin_uri, values_from = n)

#And finally, we generate a rarefaction curve:

x <- rarecurve(bin_count_spread, xlab = "Individuals Barcoded", ylab = "BIN richness")

#We are also interested in how sampling completeness depends on the countries sampled. Below, we filter out countries and BINs valued as NA (since there are quite a few in the data), then create a database that shows the counts of each BIN in each country.

bins_by_country <- dfCervidae %>%
  filter(!is.na(bin_uri)) %>%
  filter(!is.na(country)) %>%
  group_by(bin_uri, country) %>%
  count(bin_uri)

#To analyse sampling completeness, we will create an accumulation curve. The line below spreads our new database into a format readable by vegan:

bins_by_country_spread <- pivot_wider(data = bins_by_country, names_from = bin_uri, values_from = n)

#Then, since accumulation curves require numeric values, we remove the country names from the data and move them to be row names instead:

bins_by_country_spread <- bins_by_country_spread %>%
  remove_rownames %>%
  column_to_rownames(var = 'country')

#NA entries are converted to zero:

bins_by_country_spread[is.na(bins_by_country_spread)] <- 0

#And the accumulation curve is plotted.

accum_curve <- specaccum(bins_by_country_spread)

plot(accum_curve, xlab = "Countries Sampled", ylab = "BIN Richness", main = "BIN Richness vs Number of Countries Samples")

#Figure 1 shows a rarefaction curve built using all available data. Since the curve flattens as the number of individuals increases, it appears as though many rare BINs are detected in this data. There is still a slight upward trend towards the end of the curve, indicating that more sampling may be required. Since Kumar et al. (2018) raise the concern that cervid DNA barcode sampling may be lacking in certain countries, it is helpful to see sampling completeness by country. Figure 2 shows a species accumulation curve by country. Overall, while sloping downward slightly, the entire curve is quite linear, and BIN richness seems to increase with each country sampled. This indicates that further sampling may be required globally to obtain complete records of cervid DNA barcodes. Barcoding is not widely used in the conservation of cervids; I suspect that due to their size, visibility, and distinct morphological characteristics between species, morphological identification is seen as sufficient. As demonstrated by Kumar et al. (2018), however, DNA barcoding can also be a highly effective tool.
