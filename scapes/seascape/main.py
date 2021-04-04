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

        bc = self.new_channel_pan()
        ag = nature.Nightingales(
            0, 30,  # Duration
            0.75, 1.0,  # volume
            0)
        self.sched_agent(ag, 0, bc)
        bc.set_pan(1,random.uniform(1, 7))

        bc = self.new_channel_pan()
        ag = nature.Cuckoos(
            0, 90,  # Duration
            0.1, 0.5,  # volume
            1)
        bc.set_pan(-1, random.uniform(1,4))
        self.sched_agent(ag, 0, bc)
        ag = nature.FlMockingBirds(
            0, 30,  # Duration
            0.5, 0.95,  # volume
            1)
        bc.set_pan(1,random.uniform(1, 6))
        self.sched_agent(ag, 0, bc)
        for i in range(8):
            y = -0.8 + (i * 0.2) # -0.8, 0.8
            target = 1
            sc = self.new_channel_pan(y)
            ag = SurfBackgroundWaves()
            bc.set_pan (target, random.uniform(1, 4))
            target = (target * -1)
            self.sched_agent(ag, i * 5, sc)
            sc = self.new_channel_pan(y)
            ag = SurfWaveSounds()
            sc.set_pan(-1, random (1, 5))
            self.sched_agent(ag, i * 10, sc)


class SurfWaveSounds(agent.Agent):
    def run(self):
        ag = play.IntermittentSoundsList(mindelay=1.0,
                                         maxdelay=8.0,
                                         minpitch=0.2,
                                         maxpitch=1.0,
                                         minvol=0.02,
                                         maxvol=0.5,
                                         maxpan=1.25,
                                         sounds=wavesounds)
        self.sched_agent(ag)


class SurfBackgroundWaves(agent.Agent):
    def run(self):
        p = random.uniform(0.2, 1.0)
        v = random.uniform(0.01, 0.5)
        d = random.uniform(0.3, 12.0)
        pan = random.uniform(-1.25, 1.25)
        dur = self.sched_note_pan(water.waves_light, pan, pitch=p, volume=v)
        self.resched(dur * d)
