source("llr_functions.R")
library(reshape2) # package that contains "french_fries"
library(microbenchmark) # for benchmarking

# Try out functions to see if it works on some data:
data(french_fries)
french_fries = french_fries[complete.cases(french_fries),]
z = seq(0, 15, length.out = 100)

# Benchmarking the llr function with omega = 2
benchmark_result <- microbenchmark(
  fits = llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = 2),
  times = 10  # Run the benchmark multiple times
)

# Print out the timing result
cat("Benchmark results for llr function with omega = 2:\n")
print(benchmark_result)

# Loop through different omega values and benchmark them
for (i in seq(1, 5, 0.5)) {
  benchmark_result <- microbenchmark(
    fits = llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = i),
    times = 10
  )
  # Print out the timing result for each omega
  cat("Benchmark results for llr function with omega =", i, ":\n")
  print(benchmark_result)
  
  # Plot the fit
  plot(z, fits)
}

# We can see that the fit is getting smoother when we increase the value of omega.
# In locally weighted regression, the parameter omega controls how much of the
# surrounding data influences the fit at any given point.

