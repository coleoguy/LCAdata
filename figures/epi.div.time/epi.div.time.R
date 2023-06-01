ref <- read.csv("../all.data/ref.csv")

# Data that is only unique species-phenotype combinations
thin <- read.csv("../results/thinned.comp.csv")

# Add div.time column to matching rows
thin$div.time <- NA

# Find matching rows between thin and ref for file name and assign div.time values to thin file
matching_rows <- thin$file %in% ref$new.file.name
thin$div.time[matching_rows] <- ref$divergence.time.MYA.[match(thin$file[matching_rows], ref$new.file.name)]

# Remove NAs and 0s and outlier high div time
thin_clean <- na.omit(thin)
thin_cleannew <- thin_clean[thin_clean$div.time != 0 & thin_clean$div.time != 26.8,]

# Plot
plot(y=(thin_cleannew$epi), x=thin_cleannew$div.time, 
     ylab="proportion of trait divergence that is epistatic", 
     xlab="divergence time", col=rgb(0.6, 0.2, 1, 0.5), pch=16)
summary(lm(thin_cleannew$epi~thin_cleannew$div.time))

