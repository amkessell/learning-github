# script to parse and make some plots of example SARS-CoV-2 SNP data

# Anthony Kessell
# September 16, 2024
# amkessell@dons.usfca.edu

# load packages
library(ggplot2)
library(dplyr)

# load in SNP data
snp_data <- read.csv("data/sample_snp_data.csv")

# use dplyr to subset out just the first sample from the data
sample_SRR12433063 <- snp_data %>%
  filter(sample == "SRR12433063")

# use group by and summarize to calculate mean
# of the wall column, but grouped by sample
snp_data %>%
  group_by(sample) %>%
  summarize(mean_qual = mean(qual),
            sd_quality = sd(qual))
# sample one and three are the best after running that code line

snp_data %>%
  filter(qual > 200) %>%
  group_by(sample) %>%
  tally() # counts up rows in each group

# use select to pull out only a few columns of interest
snp_data %>%
  select(sample, pos, ref, alt)

# introducing ggplot
ggplot(data = iris,
       aes(x = Sepal.Length,
           y = Sepal.Width,
           color = Species)) +
  geom_point(aes(size = Petal.Length)) +
# can add color = "some color" in the ()
# for specific color,NOT in the ggplot script above
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("red", "black", "purple")) +
# scale_color_viridis_d() **this is the "universal" pallet for color blind
  labs(title = "Flower measurments of iris spp.",
       x = "Sepal Length",
       y = "Sepal Width") +
  theme_classic()

# put together dplyr and ggplot to visualize the quality score of a
# SNP call by position in the genome colored by sample, but only for
# SNPs with quality > 150

snp_data %>%
  filter(qual > 50) %>%
  filter(pos > 8000) %>%
  ggplot(aes(x = pos,
             y = qual,
             color = sample)) +
  geom_point(size = 5) +
  theme_classic() +
  labs(title = "SNP Quailty By Sample",
       x = "Position in Genome",
       y = "SNP Quality Score")
