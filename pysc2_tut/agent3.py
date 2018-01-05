import math
import numpy as np
from pysc2.agents import base_agent
from pysc2.lib import actions
from pysc2.lib import features
from pysc2.env import sc2_env, run_loop, available_actions_printer
from pysc2 import maps
from absl import flags

_AI_RELATIVE = features.SCREEN_FEATURES.player_relative.index
_AI_SELECTED = features.SCREEN_FEATURES.selected.index
_NO_OP = actions.FUNCTIONS.no_op.id
_MOVE_SCREEN = actions.FUNCTIONS.Attack_screen.id
_SELECT_ARMY = actions.FUNCTIONS.select_army.id
_SELECT_POINT = actions.FUNCTIONS.select_point.id
_MOVE_RAND = 1000
_MOVE_MIDDLE = 2000
_BACKGROUND = 0
_AI_SELF = 1
_AI_ALLIES = 2
_AI_NEUTRAL = 3
_AI_HOSTILE = 4
_SELECT_ALL = [0]
_NOT_QUEUED = [0]
EPS_START = 0.9
EPS_END = 0.025
EPS_DECAY = 2500

possible_actions = [
    _NO_OP,
    _SELECT_ARMY,
    _SELECT_POINT,
    _MOVE_SCREEN,
    _MOVE_RAND,
    _MOVE_MIDDLE
]

def get_eps_threshold(steps_done):
    return EPS_END + (EPS_START - EPS_END) * math.exp(-1. * steps_done / EPS_DECAY)

def get_state(obs):
    ai_view = obs.observation['screen'][_AI_RELATIVE]
    beaconxs, beaconys = (ai_view == _AI_NEUTRAL).nonzero()
    marinexs, marineys = (ai_view == _AI_SELF).nonzero()
    marinex, mariney = marinexs.mean(), marineys.mean()
        
    marine_on_beacon = np.min(beaconxs) <= marinex <=  np.max(beaconxs) and np.min(beaconys) <= mariney <=  np.max(beaconys)
        
    ai_selected = obs.observation['screen'][_AI_SELECTED]
    marine_selected = int((ai_selected == 1).any())
    
    return (marine_selected, int(marine_on_beacon)), [beaconxs, beaconys]

class QTable(object):
    def __init__(self, actions, lr=0.01, reward_decay=0.9, load_qt=None, load_st=None):
        self.lr = lr
        self.actions = actions
        self.reward_decay = reward_decay
        self.states_list = set()
        self.load_qt = load_qt
        if load_st:
            temp = self.load_states(load_st)
            self.states_list = set([tuple(temp[i]) for i in range(len(temp))])
        
        if load_qt:
            self.q_table = self.load_qtable(load_qt)
        else:
            self.q_table = np.zeros((0, len(possible_actions))) # create a Q table
        
    def get_action(self, state):
        if not self.load_qt and np.random.rand() < get_eps_threshold(steps):
            return np.random.randint(0, len(self.actions))
        else:
            if state not in self.states_list:
                self.add_state(state)
            idx = list(self.states_list).index(state)
            q_values = self.q_table[idx]
            return int(np.argmax(q_values))
    
    def add_state(self, state):
        self.q_table = np.vstack([self.q_table, np.zeros((1, len(possible_actions)))])
        self.states_list.add(state)
    
    def update_qtable(self, state, next_state, action, reward):
        if state not in self.states_list:
            self.add_state(state)
        if next_state not in self.states_list:
            self.add_state(next_state)
        state_idx = list(self.states_list).index(state)
        next_state_idx = list(self.states_list).index(next_state)
        q_state = self.q_table[state_idx, action]
        q_next_state = self.q_table[next_state_idx].max()
        q_targets = reward + (self.reward_decay * q_next_state)
        loss = q_targets - q_state
        self.q_table[state_idx, action] += self.lr * loss
        return loss
    
    def get_size(self):
        print(self.q_table.shape)
        
    def save_qtable(self, filepath):
        np.save(filepath, self.q_table)
        
    def load_qtable(self, filepath):
        return np.load(filepath)
        
    def save_states(self, filepath):
        temp = np.array(list(self.states_list))
        np.save(filepath, temp)
        
    def load_states(self, filepath):
        return np.load(filepath)
    
class Agent3(base_agent.BaseAgent):
    def __init__(self, load_qt=None, load_st=None):
        super(Agent3, self).__init__()
        self.qtable = QTable(possible_actions, load_qt='agent3_qtable.npy', load_st='agent3_states.npy')
        
    def step(self, obs):
        '''Step function gets called automatically by pysc2 environment'''
        super(Agent3, self).step(obs)
        state, beacon_pos = get_state(obs)
        action = self.qtable.get_action(state)
        func = actions.FunctionCall(_NO_OP, [])
        
        if possible_actions[action] == _NO_OP:
            func = actions.FunctionCall(_NO_OP, [])
        elif state[0] and possible_actions[action] == _MOVE_SCREEN:
            beacon_x, beacon_y = beacon_pos[0].mean(), beacon_pos[1].mean()
            func = actions.FunctionCall(_MOVE_SCREEN, [_NOT_QUEUED, [beacon_y, beacon_x]])
        elif possible_actions[action] == _SELECT_ARMY:
            func = actions.FunctionCall(_SELECT_ARMY, [_SELECT_ALL])
        elif state[0] and possible_actions[action] == _SELECT_POINT:
            ai_view = obs.observation['screen'][_AI_RELATIVE]
            backgroundxs, backgroundys = (ai_view == _BACKGROUND).nonzero()
            point = np.random.randint(0, len(backgroundxs))
            backgroundx, backgroundy = backgroundxs[point], backgroundys[point]
            func = actions.FunctionCall(_SELECT_POINT, [_NOT_QUEUED, [backgroundy, backgroundx]])
        elif state[0] and possible_actions[action] == _MOVE_RAND:
            beacon_x, beacon_y = beacon_pos[0].max(), beacon_pos[1].max()
            movex, movey = np.random.randint(beacon_x, 64), np.random.randint(beacon_y, 64)
            func = actions.FunctionCall(_MOVE_SCREEN, [_NOT_QUEUED, [movey, movex]])
        elif state[0] and possible_actions[action] == _MOVE_MIDDLE:
            func = actions.FunctionCall(_MOVE_SCREEN, [_NOT_QUEUED, [32, 32]])
        return func
    
'''
This has been modified a bit from the notebook to run properly with:

python -m pycs2.bin.agent --agent agent3.Agent3 --map MoveToBeacon
'''