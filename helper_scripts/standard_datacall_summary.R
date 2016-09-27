summarize(jobs_saved = sum(Jobs.Saved, na.rm = TRUE), jobs_created = sum(Jobs.Created, na.rm = TRUE),
          jobs_saved_created = sum(jobs_saved, jobs_created), pi = sum(Private.Investment, na.rm = TRUE),
          project_count = n(), eda_funding = sum(EDA.Funding, na.rm = TRUE))