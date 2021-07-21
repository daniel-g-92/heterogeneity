# heterogeneity

These are the files that generate the key pieces of work making up my latest paper.

Starting with the main simulation R file, this will generate heterogenous time-to-event follow-up replicating a phase III trial.
It then fits models to this, assesses their plausibiltiy and stores estimates of life-years (mean survival).
Note there is a small error in the capture of life-year estimates from the Kaplan-Meier follow-up, as this is captured from an in-built R command. 
It only affects 3% of simulations when the sample size is large.

Once these have been run, the Stata file generates helpful summaries which can be pasted into a table.
It also reshapes the data, dropping unecessary variables such that it can be used by plotly.

Finally, there is the code to create the plot-ly violin plots within R.




