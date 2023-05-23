ref <- read.csv("../all.data/ref.csv")

# Data that is only unique species-phenotype combinations
thin <- read.csv("../results/thinned.comp.csv")


# Add div.time column to matching rows
thin$div.time <- NA

# Find matching rows between thin and ref for file name and assign div.time values to thin file
matching_rows <- thin$file %in% ref$new.file.name
thin$div.time[matching_rows] <- ref$divergence.time.MYA.[match(thin$file[matching_rows], ref$new.file.name)]

# Remove NAs and 0s
thin_clean <-na.omit(thin)
thin_clean <- thin[thin$div.time != 0,]

# Plot
plot(y=(thin_clean$epi), x=thin_clean$div.time, 
     ylab="proportion of trait divergence that is epistatic", 
     xlab="divergence time", col=rgb(0.6, 0.2, 1, 0.5), pch=16)

