# r-fsrs

R bindings for [fsrs-rs](https://github.com/open-spaced-repetition/fsrs-rs), the Rust implementation of the Free Spaced Repetition Scheduler algorithm.

## Installation
```r
# Install Rust first: https://rustup.rs
remotes::install_github("chrislongros/fsrsr")
```

## Usage
```r
library(fsrsr)

# Get default parameters
params <- fsrs_default_parameters()

# New card, rated "Good"
state <- fsrs_initial_state(3)
print(state)
# $stability: 3.17
# $difficulty: 5.31

# Review after 3 days, rated "Good"
new_state <- fsrs_next_state(state$stability, state$difficulty, 3.0, 3)
print(new_state)

# Calculate next interval for 90% retention
interval <- fsrs_next_interval(new_state$stability, 0.9)
print(interval)  # e.g., 8 days

# Check recall probability after 5 days
prob <- fsrs_retrievability(new_state$stability, 5.0)
print(prob)  # e.g., 0.87
```

## License

MIT
