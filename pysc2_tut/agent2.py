import numpy as np
from pysc2.agents import base_agent
from pysc2.lib import actions
from pysc2.lib import features
from pysc2.env import sc2_env, run_loop, available_actions_printer
from pysc2 import maps
from absl import flags

_AI_RELATIVE = features.SCREEN_FEATURES.player_relative.index
_NO_OP = actions.FUNCTIONS.no_op.id
_MOVE_SCREEN = actions.FUNCTIONS.Attack_screen.id
_SELECT_ARMY = actions.FUNCTIONS.select_army.id
_BACKGROUND = 0
_AI_SELF = 1
_AI_ALLIES = 2
_AI_NEUTRAL = 3
_AI_HOSTILE = 4
_SELECT_ALL = [0]
_NOT_QUEUED = [0]

def get_beacon_location(ai_relative_view):
    '''returns the location indices of the beacon on the map'''
    return (ai_relative_view == _AI_NEUTRAL).nonzero() 
    
class Agent2(base_agent.BaseAgent):
    """An agent for doing a simple movement form one point to another."""
    def step(self, obs):
        '''Step function gets called automatically by pysc2 environment'''
        super(Agent2, self).step(obs)
        if _MOVE_SCREEN in obs.observation['available_actions']:
            ai_view = obs.observation['screen'][_AI_RELATIVE]
            # get the beacon coordinates
            beacon_xs, beacon_ys = get_beacon_location(ai_view)
            if not beacon_ys.any():
                return actions.FunctionCall(_NO_OP, [])
            # get the middle of the beacon and move there
            target = [beacon_ys.mean(), beacon_xs.mean()]
            return actions.FunctionCall(_MOVE_SCREEN, [_NOT_QUEUED, target])
        else:
            return actions.FunctionCall(_SELECT_ARMY, [_SELECT_ALL])
        
FLAGS = flags.FLAGS
FLAGS(['run_sc2'])

viz = True
steps = 2000
save_replay = False
steps_per_episode = 0 # 0 actually means unlimited

# create a map
beacon_map = maps.get('MoveToBeacon')

# create an envirnoment
with sc2_env.SC2Env(agent_race=None, bot_race=None, difficulty=None, map_name=beacon_map, visualize=viz) as env:
    agent=Agent2()
    run_loop.run_loop([agent], env, steps)
    if save_replay:
        env.save_replay(Agent2.__name__)
        
print('Average reward per episode: {}'.format(agent.reward/agent.episodes))

'''
Run script with:
python -m pysc2.bin.agent --map MoveToBeacon --agent agent1.Agent1
# or
python -m pysc2.bin.play --map MoveToBeacon --replay Agent1/NAME_OF_REPLAY
# replays don't work on mac really (versions are wrong)
'''