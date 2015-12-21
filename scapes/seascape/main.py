import random
from boopak.package import *
from boodle import agent
from boodle import builtin

play = bimport('org.boodler.play')
manage = bimport('org.boodler.manage')

water = bimport('org.boodler.old.water')

wavesounds = [
    water.waves_lapping,
    water.waves_light,
    water.waves_rough,
    water.waves_floopy,
    water.water_rushing,
    water.water_pouring,
    water.water_rapids
]

class SurfWaves(agent.Agent):
    """Orchestrate wave agents SurfWaveSounds and SurfBackgroundWaves"""
    def run(self):
        for i in range(8):
            ag=SurfBackgroundWaves()
            self.sched_agent(ag)
        for i in range(16):
            ag=SurfWaveSounds()
            self.sched_agent(ag)

class SurfWaveSounds(agent.Agent):
    def run(self):
        ag = play.IntermittentSoundsList (
            mindelay=1.0, maxdelay=5.0,
            minpitch=0.2, maxpitch=0.6,
            minvol=0.1, maxvol=0.8,
            maxpan=1.5, sounds=wavesounds)
        ag2 = manage.VolumeModulateAgent(ag, 0.075)
        self.sched_agent(ag2)

class SurfBackgroundWaves(agent.Agent):
    def run(self):
        p=random.uniform(0.2,0.7)
        v=random.uniform(0.05, 0.15)
        d=random.uniform(0.3,1.0)
        pan= random.uniform(-1.5, 1.5)
        dur=self.sched_note_pan(water.waves_light, pan, pitch=p, volume=v)
        self.resched(dur*d)
