import random
from boopak.package import *
from boodle import agent, stereo
from boodle import builtin

play = bimport('org.boodler.play')

nature = bimport('org.emacspeak.nature')
water = bimport('org.boodler.old.water')

wavesounds = [
    water.waves_lapping, water.waves_light, water.waves_rough,
    water.waves_floopy, water.water_rushing, water.water_pouring,
    water.water_rapids
]


class SurfWaves(agent.Agent):
    """Orchestrate wave agents SurfWaveSounds and SurfBackgroundWaves"""
    def run(self):

        
        ag = nature.BirdCalls()
        self.sched_agent(ag)
            
        for i in range(8):
            y = 1 + i * 0.05
            sc = self.new_channel_pan(y)
            ag = SurfBackgroundWaves()
            self.sched_agent(ag, i * 5, sc)
            sc = self.new_channel_pan(y)
            ag = SurfWaveSounds()
            self.sched_agent(ag, i * 10, sc)


class SurfWaveSounds(agent.Agent):
    def run(self):
        ag = play.IntermittentSoundsList(mindelay=1.0,
                                         maxdelay=8.0,
                                         minpitch=0.2,
                                         maxpitch=1.0,
                                         minvol=0.2,
                                         maxvol=0.5,
                                         maxpan=1.25,
                                         sounds=wavesounds)
        self.sched_agent(ag)


class SurfBackgroundWaves(agent.Agent):
    def run(self):
        p = random.uniform(0.2, 1.0)
        v = random.uniform(0.01, 0.75)
        d = random.uniform(1.5, 12.0)
        pan = random.uniform(-1.25, 1.25)
        dur = self.sched_note_pan(water.waves_light, pan, pitch=p, volume=v)
        self.resched(dur * d)
