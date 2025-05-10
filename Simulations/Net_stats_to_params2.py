#!/usr/bin/env python3

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

sns.set_theme(style="white", context="talk")

def main():
    csv_file = "parameter_sweep_results_reciprocity.csv"
    df = pd.read_csv(csv_file)

    # show top 20
    top_density      = df.sort_values("density_mean",      ascending=False).head(20)
    top_reciproc     = df.sort_values("reciprocity_mean",  ascending=False).head(20)
    top_centrality   = df.sort_values("centrality_mean",   ascending=False).head(20)

    print("\n Top 20 by Density")
    print(top_density)
    print("\n Top 20 by Reciprocity")
    print(top_reciproc)
    print("\n Top 20 by Centrality")
    print(top_centrality)

    plt.figure(figsize=(8, 6))
    scatter = plt.scatter(
        df["density_mean"],
        df["reciprocity_mean"],
        c=df["centrality_mean"],
        cmap="viridis"
    )
    plt.xlabel("Network Density")
    plt.ylabel("Reciprocity")
    cbar = plt.colorbar(scatter)
    cbar.set_label("Centrality Mean")
    plt.title("Density vs. Reciprocity vs. Centrality")
    plt.tight_layout()
    plt.savefig("tradeoff_density_reciprocity.png", dpi=200)
    plt.show()
    print("[Info] Saved scatter plot to 'tradeoff_density_reciprocity.png'.")

if __name__ == "__main__":
    main()