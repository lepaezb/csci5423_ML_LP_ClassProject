import numpy as np
import random
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import networkx as nx
import matplotlib.cm as cm

def get_player_positions():
    return {
        0: (5, 50),
        1: (20, 20), 2: (20, 40), 3: (20, 60), 4: (20, 80),
        5: (40, 30), 6: (40, 50), 7: (40, 70), 8: (60, 50),
        9: (80, 40), 10: (80, 60)
    }

def build_distance_matrix(coords):
    N = len(coords)
    d = np.zeros((N, N))
    for i in range(N):
        for j in range(N):
            if i != j:
                xi, yi = coords[i]
                xj, yj = coords[j]
                d[i, j] = np.hypot(xi - xj, yi - yj)
    return d

def build_success_prob_matrix(coords, beta=0.05):
    d = build_distance_matrix(coords)
    S = np.exp(-beta * d)
    np.fill_diagonal(S, 0)
    return S

def position_map():
    return ['GK', 'D', 'D', 'D', 'D', 'M', 'M', 'M', 'M', 'F', 'F']

def build_initial_pheromone(positions, alpha_map, epsilon=0.01):
    N = len(positions)
    T = np.zeros((N, N))
    for i in range(N):
        for j in range(N):
            if i != j:
                pi, pj = positions[i], positions[j]
                base = alpha_map.get((pi, pj), 0.1)
                T[i, j] = base + epsilon * random.random()
    return T

def normalize_row(row):
    total = np.sum(row)
    return row / total if total > 1e-12 else row

class AnimatedSoccerPassingSim:
    def __init__(self, num_steps=500, rho=0.2, beta=0.0005,
                 delta=0.5, exploration=0.2, seed=42):
        random.seed(seed)
        np.random.seed(seed)

        self.N = 11
        self.coords = get_player_positions()
        self.positions = position_map()
        self.S = build_success_prob_matrix(self.coords, beta)

        self.alpha_map = {
            ('GK','D'): 5.0, ('GK','M'): 2.0, ('GK','F'): 0.5,
            ('D','GK'): 1.0, ('D','D'): 2.0, ('D','M'): 4.0, ('D','F'): 1.5,
            ('M','GK'): 0.5, ('M','D'): 2.0, ('M','M'): 3.0, ('M','F'): 3.0,
            ('F','GK'): 0.2, ('F','D'): 1.0, ('F','M'): 2.0, ('F','F'): 2.0,
        }
        self.T = build_initial_pheromone(self.positions, self.alpha_map)
        self.A = np.zeros((self.N, self.N), dtype=int)
        self.current_holder = 6
        self.rho = rho
        self.delta = delta
        self.exploration = exploration
        self.num_steps = num_steps
        self.history = []

    def simulate_step(self):
        self.T *= (1 - self.rho)
        row = self.T[self.current_holder, :] + self.exploration
        row[self.current_holder] = 0.0
        probs = normalize_row(row)

        j = np.random.choice(self.N, p=probs)
        success = (np.random.rand() < self.S[self.current_holder, j])

        if success:
            old_weight = self.A[self.current_holder, j]
            self.history.append((self.current_holder, j, True, old_weight))
            self.A[self.current_holder, j] += 1
            self.T[self.current_holder, j] += self.delta
            self.current_holder = j
        else:
            self.history.append((self.current_holder, j, False, None))
            eligible = [k for k in range(self.N) if k != self.current_holder]
            self.current_holder = random.choice(eligible)

    def run(self):
        for _ in range(self.num_steps):
            self.simulate_step()
        self.subframes = self._create_subhistory()

    def _create_subhistory(self, num_subframes=10):
        frames = []
        for (i, j, success, old_weight) in self.history:
            xi, yi = self.coords[i]
            xj, yj = self.coords[j] if success else self.coords[j]
            for sub in range(num_subframes):
                frac = sub / float(num_subframes - 1)
                x_curr = xi + frac * (xj - xi)
                y_curr = yi + frac * (yj - yi)
                if success and sub == num_subframes - 1:
                    frames.append((x_curr, y_curr, (i, j, old_weight)))
                elif (not success) and sub == num_subframes - 1:
                    frames.append((x_curr, y_curr, None))
                else:
                    frames.append((x_curr, y_curr, None))
        return frames

    def animate(self):
        self.G = nx.DiGraph()
        self.G.add_nodes_from(range(self.N))
        pos = self.coords
        node_colors = ['gold' if p == 'GK' else 'lightblue' if p == 'D' else 'lightgreen' if p == 'M' else 'salmon' for p in self.positions]

        fig, ax = plt.subplots(figsize=(10, 6))
        ax.set_facecolor('palegreen')
        ax.set_xlim(0, 100)
        ax.set_ylim(0, 100)
        ax.axis('off')

        nx.draw_networkx_nodes(self.G, pos, node_color=node_colors, node_size=500, ax=ax)
        nx.draw_networkx_labels(self.G, pos, labels={i: i for i in range(self.N)}, ax=ax)
        ball_marker, = ax.plot([], [], 'ro', markersize=12)
        edge_artists = {}

        def update(frame_idx):
            if frame_idx >= len(self.subframes):
                return
            x_curr, y_curr, edge_info = self.subframes[frame_idx]
            ball_marker.set_data([x_curr], [y_curr])
            if edge_info is not None:
                i, j, old_weight = edge_info
                if old_weight is None:
                    return
                norm_w = min(old_weight / 10.0, 1.0)
                color_intensity = cm.Greens(0.2 + 0.7 * norm_w)
                width = 0.5 + 3.5 * norm_w
                self.G.add_edge(i, j)
                if (i, j) not in edge_artists:
                    edge = nx.draw_networkx_edges(
                        self.G, pos, edgelist=[(i, j)],
                        width=width, edge_color=[color_intensity],
                        ax=ax, arrows=True, connectionstyle='arc3,rad=0.1')
                    edge_artists[(i, j)] = edge[0]
                else:
                    edge_artists[(i, j)].set_color(color_intensity)
                    edge_artists[(i, j)].set_linewidth(width)

        ani = animation.FuncAnimation(fig, update, frames=len(self.subframes), interval=50, repeat=False)
        plt.show()

if __name__ == "__main__":
    sim = AnimatedSoccerPassingSim(num_steps=200)
    sim.run()
    sim.animate()