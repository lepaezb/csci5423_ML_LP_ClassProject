#!/usr/bin/env python3


import numpy as np
import networkx as nx
import matplotlib.pyplot as plt
import seaborn as sns
import itertools
import pandas as pd
from joblib import Parallel, delayed
from tqdm import tqdm

from Simulations.Two_team_pass_sim_viz import TwoTeamSimulation, build_success_prob_matrix


rhos   = np.linspace(0.0, 0.999, 15)         # Evaporation
deltas = np.linspace(0.001, 3.0,   15)       # Reinforcement
etas   = np.linspace(0.001, 3.0,   15)       # Exploration
betas  = np.linspace(1e-6,  0.01,  15)       # Distance sensitivity

n_trials = 10
n_steps  = 500 # num passes

sns.set_theme(style="white", context="talk")

def compute_metrics(sim):
    stats = []
    for team in (sim.team_A, sim.team_B):
        A = team.A
        G = nx.DiGraph(A)

        density = nx.density(G)

        if G.number_of_edges() == 0:
            reciprocity = 0.0           # define as 0 for empty graph (high beta willl make empty graphs)
        else:
            reciprocity = nx.reciprocity(G) or 0.0

        centrality = np.mean(list(nx.degree_centrality(G).values())) if G.number_of_nodes() > 1 else 0.0
        stats.append((density, reciprocity, centrality))

    return np.mean(stats, axis=0)

def run_one_setting(rho, delta, eta, beta):
    metrics = []
    for _ in range(n_trials):
        sim = TwoTeamSimulation(num_steps=n_steps)

        # Apply parameters to both teams
        for team in (sim.team_A, sim.team_B):
            team.rho         = rho
            team.delta       = delta
            team.exploration = eta
            team.S           = build_success_prob_matrix(team.coords, beta)

        sim.run()
        metrics.append(compute_metrics(sim))

    metrics = np.array(metrics)
    avg = metrics.mean(axis=0)
    std = metrics.std(axis=0)

    return {
        "rho": rho,
        "delta": delta,
        "eta": eta,
        "beta": beta,
        "density_mean":     avg[0],
        "reciprocity_mean": avg[1],
        "centrality_mean":  avg[2],
        "density_std":     std[0],
        "reciprocity_std": std[1],
        "centrality_std":  std[2],
    }

param_grid = list(itertools.product(rhos, deltas, etas, betas))
results = Parallel(n_jobs=-1)(
    delayed(run_one_setting)(rho, delta, eta, beta)
    for rho, delta, eta, beta in tqdm(param_grid)
)
results_df = pd.DataFrame(results)
results_df.to_csv("parameter_sweep_results_reciprocity.csv", index=False)

fig, axes = plt.subplots(1, 3, figsize=(18, 6))

sns.histplot(results_df["density_mean"], bins=20, kde=True,
             ax=axes[0], color="skyblue")
axes[0].set_title("Density Distribution")
axes[0].set_xlabel("Average Network Density")

sns.histplot(results_df["reciprocity_mean"], bins=20, kde=True,
             ax=axes[1], color="orange")
axes[1].set_title("Reciprocity Distribution")
axes[1].set_xlabel("Average Reciprocity")

sns.histplot(results_df["centrality_mean"], bins=20, kde=True,
             ax=axes[2], color="lightgreen")
axes[2].set_title("Degree Centrality Distribution")
axes[2].set_xlabel("Mean Degree Centrality")

plt.tight_layout()
plt.savefig("parameter_sweep_histograms_reciprocity.png", dpi=200)
plt.show()
