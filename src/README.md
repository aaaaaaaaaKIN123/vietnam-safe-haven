# Source Code Folder

Reusable research code belongs here. Suggested modules: `data_pipeline/`, `diagnostics/`, `models/`, `visualization/`, and `utils/`. Scripts should read from `data/02_model_input/` or `data/01_processed/` and write outputs to `results/` or `data/03_model_output/`.

## Model Pipeline

The model pipeline is controlled by `models/model_control.yaml`. The academic-method mapping is in `models/model_method_map.yaml`, and agent-specific model rules are in `models/AGENT_MODEL_GUIDE.md`.

Run model code from the project root:

```r
source("src/models/run_model_pipeline.R")
run_model_pipeline()
```
