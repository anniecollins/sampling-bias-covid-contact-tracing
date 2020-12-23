library(pbapply)
library(ggplot2)


# INSTITUTIONAL EXAMPLE 1
# Population infection rate of 5%, self-tracing accuracy of 20% (back to spreader event)
# n = 2000; 2 institutional settings of 50 people each and 180 small gatherings of 10 each
# Results in approx 20% of infections being traced to institutions when 5% existed in sample

# Repeat contact tracing simulation 50,000 times
set.seed(100)
props1 <- pbsapply(1:50000, function(m) {
  # Allocate uninfected people to events (everyone uninfected at this point)
  ppl <- data.frame(
    event = c(
      rep(paste0("institution", 1:2), 100),
      rep(paste0("other", 1:180), 10)
    ),
    infected = FALSE,
    traced = NA
  )

  # Infect 5% of people uniformly and independently
  ppl$infected[sample.int(nrow(ppl), nrow(ppl) * 0.05)] <- TRUE

  # Primary contact tracing: everyone in an institution setting is traced back to their institution.
  # Non-institution affiliated people have a 20% chance of being traced back to their spreader event.
  ppl$traced[ppl$infected & substr(ppl$event, 1, 11) == "institution"] <- TRUE

  ppl$traced[ppl$infected & substr(ppl$event, 1, 5) == "other"] <- runif(sum(ppl$infected & substr(ppl$event, 1, 5) == "other")) < 0.2
  summary(ppl$traced)

  # Secondary contact tracing
  event_trace_counts <- table(ppl$event[ppl$traced])
  # Find the events where two individual cases were traced to the same event
  events_traced <- names(event_trace_counts)[event_trace_counts >= 2]
  # If someone at the event was infected AND the event had two or more cases linked to it, consider it traced
  # (Will result in more cases traced since more infected people that were previously untraced will be linked to individual events)
  ppl$traced[ppl$infected & ppl$event %in% events_traced] <- TRUE
  summary(ppl$traced)

  # Summarize results
  ppl$event_type <- substr(ppl$event, 1, 1)

  institution_infections <- sum(ppl$infected & ppl$event_type == "i")
  other_infections <- sum(ppl$infected & ppl$event_type == "o")
  p_institution_infections <- institution_infections / (institution_infections + other_infections)

  institution_traces <- sum(ppl$infected & ppl$traced & ppl$event_type == "i")
  other_traces <- sum(ppl$infected & ppl$traced & ppl$event_type == "o")
  p_institution_traces <- institution_traces / (other_traces + institution_traces)

  # Vector of proportion of wedding infection and proportion of traced infections connected to weddings
  c(p_institution_infections, p_institution_traces)
})

# Mutate props1 into dataframe, label columns "Infections" and "Traces"
props1_df <- setNames(as.data.frame(t(props1)), c("Infections", "Traces"))

# Histogram of true infection rate from institutional
plot1 <- ggplot(props1_df) +
  geom_histogram(aes(x = Infections), fill = "black", alpha = 0.75, binwidth = 0.05) +
  xlab("Proportion of infections resulting from transmission within institutions") +
  theme(axis.text.y = element_blank()) + ylab("Frequency") +
  geom_histogram(aes(x = Traces), fill = "red", alpha = 0.75, binwidth = 0.05) +
  annotate("text", 0.15, 25000, label = "True proportion", color = "black", alpha = 0.75, hjust = 0) +
  annotate("text", 0.5, 10000, label = "Observed proportion", color = "red", alpha = 0.75, hjust = 0) +
  ggtitle(
    "Proportion of infections resulting from institutional transmission",
    "Institutional contact tracing with 100% accuracy, other contact tracing with 20% accuracy"
  )
ggsave("outputs/simulations/simulation1-plot.pdf", plot = plot1)


# INSTITUTIONAL EXAMPLE 2
# Population infection rate of 5%, contact tracing accuracy of 50% (back to spreader event)
# n = 2000; 2 institutional settings of 100 people each and 180 small gatherings of 10 each

# Repeat contact tracing simulation 50,000 times
set.seed(100)
props1 <- pbsapply(1:50000, function(m) {
  # Allocate uninfected people to events (everyone uninfected at this point)
  ppl <- data.frame(
    event = c(
      rep(paste0("institution", 1:2), 100),
      rep(paste0("other", 1:180), 10)
    ),
    infected = FALSE,
    traced = NA
  )

  # Infect 5% of people uniformly and independently
  ppl$infected[sample.int(nrow(ppl), nrow(ppl) * 0.05)] <- TRUE

  # Primary contact tracing: everyone in an institution setting is traced back to their institution.
  # Non-institution affiliated people have a 20% chance of being traced back to their spreader event.
  ppl$traced[ppl$infected & substr(ppl$event, 1, 11) == "institution"] <- TRUE

  ppl$traced[ppl$infected & substr(ppl$event, 1, 5) == "other"] <- runif(sum(ppl$infected & substr(ppl$event, 1, 5) == "other")) < 0.5
  summary(ppl$traced)

  # Secondary contact tracing
  event_trace_counts <- table(ppl$event[ppl$traced])
  # Find the events where two individual cases were traced to the same event
  events_traced <- names(event_trace_counts)[event_trace_counts >= 2]
  # If someone at the event was infected AND the event had two or more cases linked to it, consider it traced
  # (Will result in more cases traced since more infected people that were previously untraced will be linked to individual events)
  ppl$traced[ppl$infected & ppl$event %in% events_traced] <- TRUE
  summary(ppl$traced)

  # Summarise results
  ppl$event_type <- substr(ppl$event, 1, 1)

  institution_infections <- sum(ppl$infected & ppl$event_type == "i")
  other_infections <- sum(ppl$infected & ppl$event_type == "o")
  p_institution_infections <- institution_infections / (institution_infections + other_infections)

  institution_traces <- sum(ppl$infected & ppl$traced & ppl$event_type == "i")
  other_traces <- sum(ppl$infected & ppl$traced & ppl$event_type == "o")
  p_institution_traces <- institution_traces / (other_traces + institution_traces)

  # Vector of proportion of wedding infection and proportion of traced infections connected to weddings
  c(p_institution_infections, p_institution_traces)
})

# Mutate props1 into dataframe, label columns "Infections" and "Traces"
props1_df <- setNames(as.data.frame(t(props1)), c("Infections", "Traces"))

# Histogram of true infection rate from institutional
plot2 <- ggplot(props1_df) +
  geom_histogram(aes(x = Infections), fill = "black", alpha = 0.75, binwidth = 0.05) +
  xlab("Proportion of infections resulting from transmission within institutions") +
  theme(axis.text.y = element_blank()) + ylab("Frequency") +
  geom_histogram(aes(x = Traces), fill = "red", alpha = 0.75, binwidth = 0.05) +
  annotate("text", 0.15, 25000, label = "True proportion", color = "black", alpha = 0.75, hjust = 0) +
  annotate("text", 0.25, 15000, label = "Observed proportion", color = "red", alpha = 0.75, hjust = 0) +
  ggtitle(
    "Proportion of infections resulting from institutional transmission",
    "Institutional contact tracing with 100% accuracy, other contact tracing with 50% accuracy"
  )
ggsave("outputs/simulations/simulation2-plot.pdf", plot = plot2)


# INSTITUTIONAL EXAMPLE 3
# Population infection rate of 5%, contact tracing accuracy of 80%
# n = 2000; 2 institutional settings of 100 people each and 180 small gatherings of 10 each

# Repeat contact tracing simulation 50,000 times
set.seed(100)
props1 <- pbsapply(1:50000, function(m) {
  # Allocate uninfected people to events (everyone uninfected at this point)
  ppl <- data.frame(
    event = c(
      rep(paste0("institution", 1:2), 100),
      rep(paste0("other", 1:180), 10)
    ),
    infected = FALSE,
    traced = NA
  )

  # Infect 5% of people uniformly and independently
  ppl$infected[sample.int(nrow(ppl), nrow(ppl) * 0.05)] <- TRUE

  # Primary contact tracing: everyone in an institution setting is traced back to their institution.
  # Non-institution affiliated people have a 20% chance of being traced back to their spreader event.
  ppl$traced[ppl$infected & substr(ppl$event, 1, 11) == "institution"] <- TRUE

  ppl$traced[ppl$infected & substr(ppl$event, 1, 5) == "other"] <- runif(sum(ppl$infected & substr(ppl$event, 1, 5) == "other")) < 0.8
  summary(ppl$traced)

  # Secondary contact tracing
  event_trace_counts <- table(ppl$event[ppl$traced])
  # Find the events where two individual cases were traced to the same event
  events_traced <- names(event_trace_counts)[event_trace_counts >= 2]
  # If someone at the event was infected AND the event had two or more cases linked to it, consider it traced
  # (Will result in more cases traced since more infected people that were previously untraced will be linked to individual events)
  ppl$traced[ppl$infected & ppl$event %in% events_traced] <- TRUE
  summary(ppl$traced)

  # Summarise results
  ppl$event_type <- substr(ppl$event, 1, 1)

  institution_infections <- sum(ppl$infected & ppl$event_type == "i")
  other_infections <- sum(ppl$infected & ppl$event_type == "o")
  p_institution_infections <- institution_infections / (institution_infections + other_infections)

  institution_traces <- sum(ppl$infected & ppl$traced & ppl$event_type == "i")
  other_traces <- sum(ppl$infected & ppl$traced & ppl$event_type == "o")
  p_institution_traces <- institution_traces / (other_traces + institution_traces)

  # Vector of proportion of wedding infection and proportion of traced infections connected to weddings
  c(p_institution_infections, p_institution_traces)
})

# Mutate props1 into dataframe, label columns "Infections" and "Traces"
props1_df <- setNames(as.data.frame(t(props1)), c("Infections", "Traces"))

# Histogram of true infection rate from institutional
plot3 <- ggplot(props1_df) +
  geom_histogram(aes(x = Infections), fill = "black", alpha = 0.75, binwidth = 0.05) +
  xlab("Proportion of infections resulting from transmission within institutions") +
  theme(axis.text.y = element_blank()) + ylab("Frequency") +
  geom_histogram(aes(x = Traces), fill = "red", alpha = 0.75, binwidth = 0.05) +
  annotate("text", 0.15, 27000, label = "True proportion", color = "black", alpha = 0.75, hjust = 0) +
  annotate("text", 0.2, 15000, label = "Observed proportion", color = "red", alpha = 0.75, hjust = 0) +
  ggtitle(
    "Proportion of infections resulting from institutional transmission",
    "Institutional contact tracing with 100% accuracy, other contact tracing with 80% accuracy"
  )
ggsave("outputs/simulations/simulation3-plot.pdf", plot = plot3)