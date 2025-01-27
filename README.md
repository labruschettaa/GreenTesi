# Getting started
```bash
  pip install -r requirements.txt
```

# Running Experiments
`testing.py` runs the experiments based on the settings specified in `testing_settings.json`, both can be found in the directory `src/testing/`.
### Arguments

- --`envMode`: The environment to generate for the tests.
  - `curatedEnv`: Generates a curated environment.
  - `realisticEnv`: Generates a random/realistic environment.
- --`csvWritingMode`: CSV file writing mode.
  - `write`: Overwrites the CSV file if it already exists for the environment.
  - `append`: Appends the results to the existing file.

### Example
```bash
py testing.py --envMode curatedEnv --csvWritingMode write
```
Runs the experiments in a curated environment and overwrites the results in the corresponding CSV file.


# Plot Generation  

`src/testing/plot.py` generates plots from CSV files produced during experiments to visualize **accuracy** or **scalability** of the solutions.  

#### Arguments  
- `--modeEnv`: Environment type (`CRTD` for curated, `RND` for random).  
- `--modeTest`: Test type (`OPT` for exhaustive, `QUICK0`, `QUICK1`, `QUICK2` for heuristics).  
- `--parameter`: Parameter to plot:  
  - `accuracy`: Y-axis = proximity to the optimal solution.  
  - `scalability`: Y-axis = time.  

#### Example  
```bash
py plot.py --modeEnv crtd --modeTest quick0 --parameter accuracy
```