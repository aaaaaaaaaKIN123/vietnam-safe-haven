# compute_univariate_diagnostics.py
import pandas as pd
import numpy as np
import statsmodels.api as sm
from statsmodels.stats.diagnostic import acorr_ljungbox, het_arch

# Load best GARCH models table
best_models = pd.read_csv("results/tables/model_selection/univariate_garch_best_by_bic.csv")
# Load residuals
residuals_df = pd.read_csv("results/diagnostics/residual/univariate_garch_std_residuals.csv")

results = []

for idx, row in best_models.iterrows():
    asset = row['asset']
    model = row['model']
    dist = row['distribution']
    
    # Filter residuals for this asset
    asset_resid = residuals_df[residuals_df['asset'] == asset]['std_resid'].dropna().values
    n_obs = len(asset_resid)
    
    # 1. Ljung-Box on standardized residuals (lag 12)
    lb_res = acorr_ljungbox(asset_resid, lags=[12])
    lb_stat = lb_res.loc[12, 'lb_stat']
    lb_p = lb_res.loc[12, 'lb_pvalue']
    
    # 2. Ljung-Box on squared standardized residuals (lag 12)
    lb_sq_res = acorr_ljungbox(asset_resid**2, lags=[12])
    lb_sq_stat = lb_sq_res.loc[12, 'lb_stat']
    lb_sq_p = lb_sq_res.loc[12, 'lb_pvalue']
    
    # 3. ARCH-LM on standardized residuals (lag 12)
    # het_arch returns: lm_stat, p_value, f_stat, f_p_value
    lm_stat, lm_p, _, _ = het_arch(asset_resid, maxlag=12)
    
    results.append({
        'asset': asset,
        'model': model,
        'distribution': dist,
        'observations': n_obs,
        'lb_stat': lb_stat,
        'lb_p_value': lb_p,
        'lb_sq_stat': lb_sq_stat,
        'lb_sq_p_value': lb_sq_p,
        'arch_stat': lm_stat,
        'arch_p_value': lm_p
    })

diagnostics_df = pd.DataFrame(results)
diagnostics_df.to_csv("results/diagnostics/residual/univariate_garch_residual_diagnostics.csv", index=False)
print("Diagnostics completed and saved to results/diagnostics/residual/univariate_garch_residual_diagnostics.csv")
print(diagnostics_df)
