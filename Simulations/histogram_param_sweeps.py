
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

def plot_sweep_histograms(
    csv_file="parameter_sweep_results.csv",
    output_png="param_sweep_histograms.png",
    xlim_density=None,
    xlim_clustering=None,
    xlim_centrality=None
):
    df = pd.read_csv(csv_file)
    sns.set_theme(style="white", context="talk")
    fig, axes = plt.subplots(1, 3, figsize=(18, 6))

    sns.histplot(data=df, x="density_mean", bins=20, kde=True, ax=axes[0], color="skyblue")
    axes[0].set_title("Density Distribution")
    axes[0].set_xlabel("Network Density")
    if xlim_density is not None:
        axes[0].set_xlim(xlim_density)

    sns.histplot(data=df, x="clustering_mean", bins=20, kde=True, ax=axes[1], color="salmon")
    axes[1].set_title("Clustering Coefficient Distribution")
    axes[1].set_xlabel("Global Clustering Coefficient")
    if xlim_clustering is not None:
        axes[1].set_xlim(xlim_clustering)

    sns.histplot(data=df, x="centrality_mean", bins=20, kde=True, ax=axes[2], color="lightgreen")
    axes[2].set_title("Degree Centrality Distribution")
    axes[2].set_xlabel("Mean Degree Centrality")
    if xlim_centrality is not None:
        axes[2].set_xlim(xlim_centrality)

    plt.tight_layout()
    plt.savefig(output_png, dpi=200)
    print(f"[INFO] Saved histogram figure to '{output_png}'")
    plt.show()

if __name__ == "__main__":
    plot_sweep_histograms(
        csv_file="parameter_sweep_results.csv",
        output_png="param_sweep_histograms_broad.png",
        
    )
