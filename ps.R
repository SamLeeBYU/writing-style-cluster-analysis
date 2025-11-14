source("kmeans.R")

#Use X.pca[,1:13]

#Compute ps for each value of k

#For each k,
# Do a train test split (or you could also do KF-validation)
# Determine cluster membership using k clusters and using kmeans clustering on the training data set
# "Predict" cluster membership via nearest centroid for X test

#use kmeans_clusters to create models on the training data

# Then, for each cluster j,
# Loop over each cluster in X test
# Compute the ps(k) by computing D (which is the indicator) for the corresponding predicted clusters:
# Sum up how many of the cases we "predicted" correctly
# Normalize by dividing by 1/n(n-1)

# ps(k) = minimum of those normalized sums above
