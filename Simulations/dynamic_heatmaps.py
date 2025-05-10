import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import seaborn as sns
from Simulations.Two_team_pass_sim_viz import Team  # Assuming your Team class is in this module

sns.set_theme(style="white", context="talk")
class PheromoneVisualizer:
    def __init__(self, sim):
        self.sim = sim
        self.team = sim.team_A  # Focus on team A for visualization
        self.exploration = self.team.exploration

        self.fig, (self.ax_tij, self.ax_pij) = plt.subplots(1, 2, figsize=(12, 5))
        self.ax_tij.set_title("Pheromone Matrix $T_{ij}(t)$")
        self.ax_pij.set_title("Passing Probability Matrix $P_{ij}(t)$")

        self.heat_tij = None
        self.heat_pij = None

    def normalize_row(self, row):
        total = np.sum(row)
        return row / total if total > 1e-12 else row

    def compute_probability_matrix(self):
        T = self.team.T.copy()
        P = np.zeros_like(T)
        for i in range(T.shape[0]):
            row = T[i, :].copy()
            row[i] = 0.0  # No self pass
            row += self.exploration  
            P[i, :] = self.normalize_row(row)
        return P

    def update(self, frame):
        success, _, _ = self.team.simulate_step()

        # Update Tij 
        if self.heat_tij:
            for c in self.heat_tij.collections:
                c.remove()
        self.heat_tij = sns.heatmap(self.team.T, ax=self.ax_tij, cbar=False, vmin=0, vmax=np.max(self.team.T), cmap='Blues', square=True)

        # Update Pij 
        P = self.compute_probability_matrix()
        if self.heat_pij:
            for c in self.heat_pij.collections:
                c.remove()
        self.heat_pij = sns.heatmap(P, ax=self.ax_pij, cbar=False, vmin=0, vmax=1.0, cmap='Oranges', square=True)

    def animate(self, steps=100):
        ani = animation.FuncAnimation(self.fig, self.update, frames=steps, repeat=False, interval=400)
        plt.tight_layout()
        from matplotlib.animation import FFMpegWriter

        writer = FFMpegWriter(fps=10, metadata=dict(artist='Your Name'), bitrate=1800)
        ani.save("soccer_simulation.mp4", writer=writer)

if __name__ == "__main__":
    from matplotlib import rcParams
    rcParams['font.family'] = 'serif'

    class DummySim:
        def __init__(self):
            self.team_A = Team(team_id='A', y_offset=0, flip_x=False, seed=7)

    sim = DummySim()
    viz = PheromoneVisualizer(sim)
    viz.animate(steps=100)
