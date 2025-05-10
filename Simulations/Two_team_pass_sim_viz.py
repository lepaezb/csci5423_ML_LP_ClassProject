import numpy as np
import random
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import networkx as nx
import matplotlib.cm as cm

def get_team_positions(y_offset, flip_x=False):
    base_positions = {
        0: (5, 50),
        1: (20, 20), 2: (20, 40), 3: (20, 60), 4: (20, 80),
        5: (40, 30), 6: (40, 50), 7: (40, 70), 8: (60, 50),
        9: (80, 40), 10: (80, 60)
    }
    return {
        i: (100 - x if flip_x else x, y + y_offset)
        for i, (x, y) in base_positions.items()
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
                base = alpha_map.get((positions[i], positions[j]), 0.1)
                T[i, j] = base + epsilon * random.random()
    return T

def normalize_row(row):
    s = row.sum()
    return row / s if s > 1e-12 else row

class Team:
    def __init__(self, team_id, y_offset=0, flip_x=False,
                 beta=0.005, rho=0.3, delta=2, exploration=0.2,
                 seed=18):
        random.seed(seed)
        np.random.seed(seed)

        self.N = 11
        self.coords = get_team_positions(y_offset, flip_x)
        self.positions = position_map()

        self.S = build_success_prob_matrix(self.coords, beta)
        self.alpha_map = {
            ('GK','D'):5.0, ('GK','M'):2.0, ('GK','F'):0.5,
            ('D','GK'):1.0, ('D','D'):2.0, ('D','M'):4.0, ('D','F'):1.5,
            ('M','GK'):0.5, ('M','D'):2.0, ('M','M'):3.0, ('M','F'):3.0,
            ('F','GK'):0.2, ('F','D'):1.0, ('F','M'):2.0, ('F','F'):2.0,
        }
        self.T = build_initial_pheromone(self.positions, self.alpha_map)
        self.A = np.zeros((self.N, self.N), dtype=int)
        self.current_holder = 6

        self.rho = rho
        self.delta = delta
        self.exploration = exploration
        self.history = []

    def simulate_step(self):
        self.T *= (1.0 - self.rho)

        row = self.T[self.current_holder, :] + self.exploration
        row[self.current_holder] = 0.0
        probs = normalize_row(row)

        j = np.random.choice(self.N, p=probs)
        success = (np.random.rand() < self.S[self.current_holder, j])

        if success:
            i = self.current_holder  # Save current holder before updating
            old_weight = self.A[i, j]
            self.history.append((i, j, True, old_weight))
            self.A[i, j] += 1
            self.T[i, j] += self.delta
            self.current_holder = j

            
            return True, self.coords[j], (i, j, old_weight)
        else:
            self.history.append((self.current_holder, j, False, None))
            
            return False, self.coords[j], None
        

class TwoTeamSimulation:
    def __init__(self, num_steps=200):
        self.team_A = Team(team_id='A', y_offset=35, flip_x=False)
        self.team_B = Team(team_id='B', y_offset=-35, flip_x=True)
        self.num_steps = num_steps
        self.ball_team = self.team_A
        self.other_team = self.team_B
        self.subframes = []

    def switch_teams(self):
        self.ball_team, self.other_team = self.other_team, self.ball_team

    def run(self, steps_per_pass=10):
        for _ in range(self.num_steps):
            success, target_xy, edge_info = self.ball_team.simulate_step()
            i_from = self.ball_team.history[-1][0]
            x_i, y_i = self.ball_team.coords[i_from]
            x_j, y_j = target_xy

            for sub in range(steps_per_pass):
                frac = sub / float(steps_per_pass-1)
                x_t = x_i + frac*(x_j - x_i)
                y_t = y_i + frac*(y_j - y_i)
                
                self.subframes.append((x_t, y_t, edge_info if sub == steps_per_pass - 1 and success else None, self.ball_team))

            if not success:
                self.switch_teams()

    def animate(self):
        fig, ax = plt.subplots(figsize=(12,8))
        ax.set_xlim(0,100)
        ax.set_ylim(-20,120)
        ax.axis('off')

        pos_all = {}
        for i, xy in self.team_A.coords.items():
            pos_all[f'A{i}'] = xy
        for i, xy in self.team_B.coords.items():
            pos_all[f'B{i}'] = xy

        G = nx.DiGraph()
        G.add_nodes_from(pos_all.keys())

        node_colors = ['skyblue']*11 + ['green']*11
        nx.draw_networkx_nodes(G, pos_all, node_color=node_colors, node_size=500, ax=ax)
        nx.draw_networkx_labels(G, pos_all, labels={k:k for k in pos_all}, font_size=9, font_weight='bold', ax=ax)

        (ball_marker,) = ax.plot([], [], 'ro', markersize=12)
        edge_artists = {}

        def update(frame_idx):
            if frame_idx >= len(self.subframes):
                return

            x, y, edge_info, which_team = self.subframes[frame_idx]
            ball_marker.set_data([x],[y])

            if edge_info is not None:
                i, j, old_weight = edge_info
                if old_weight is None:
                    return

                prefix = 'A' if which_team is self.team_A else 'B'
                u = f'{prefix}{i}'
                v = f'{prefix}{j}'

                norm_w = min(old_weight/10.0, 1.0)
                color_map = cm.Blues if which_team is self.team_A else cm.Greens
                color_intensity = color_map(0.2 + 0.7*norm_w)
                width = 0.5 + 3.5*norm_w

                if (u,v) not in G.edges():
                    G.add_edge(u, v)
                    edge = nx.draw_networkx_edges(G, pos_all, edgelist=[(u,v)], width=width, edge_color=[color_intensity], ax=ax, arrows=True, connectionstyle='arc3,rad=0.1')
                    if edge:
                        edge_artists[(u,v)] = edge[0]
                else:
                    if (u,v) in edge_artists:
                        edge_artists[(u,v)].set_color(color_intensity)
                        edge_artists[(u,v)].set_linewidth(width)

        ani = animation.FuncAnimation(fig, update, frames=len(self.subframes), interval=50, repeat=False)
        plt.tight_layout()
        
        plt.show()

if __name__=="__main__":
    sim = TwoTeamSimulation(num_steps=200)
    sim.run()
    sim.animate()