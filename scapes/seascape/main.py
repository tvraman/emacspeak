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
    water.water_rapids]


class SurfWaves(agent.Agent):
    """Orchestrate wave agents SurfWaveSounds and SurfBackgroundWaves"""

    def run(self):

        bc = self.new_channel_pan(
            stereo.compose(stereo.scalexy(1.2), stereo.shiftxy(0, 1.25)))
        ag = nature.Nightingales(
            0, 25,  # Duration
            0.1, 0.75,  # volume
            1)
        self.sched_agent(ag, 0, bc)

        bc = self.new_channel_pan(
            stereo.compose(stereo.scalexy(1.2), stereo.shiftxy(0, -1.25)))
        ag = nature.Cuckoos(
            0, 25,  # Duration
            0.1, 0.75,  # volume
            1)
        self.sched_agent(ag, 0, bc)
        for i in range(16):
            y = 1 + i * 0.025
            sc = self.new_channel_pan(
                stereo.compose(stereo.scalexy(1.2), stereo.shiftxy(0, y)))
            ag = SurfBackgroundWaves()
            self.sched_agent(ag, i * 2, sc)

        for i in range(16):
            y = 1 + i * 0.025
            sc = self.new_channel_pan(
                stereo.compose(stereo.scalexy(1.2), stereo.shiftxy(0, -y)))
            ag = SurfWaveSounds()
            self.sched_agent(ag, i * 8, sc)


class SurfWaveSounds(agent.Agent):

    def run(self):
        ag = play.IntermittentSoundsList(
            mindelay=1.0, maxdelay=10.0,
            minpitch=0.2, maxpitch=1.0,
            minvol=0.02, maxvol=0.25,
            maxpan=1.25, sounds=wavesounds)
        self.sched_agent(ag)


class SurfBackgroundWaves(agent.Agent):

    def run(self):
        p = random.uniform(0.2, 1.0)
        v = random.uniform(0.01, 0.1)
        d = random.uniform(0.3, 8.0)
        pan = random.uniform(-1.25, 1.25)
        dur = self.sched_note_pan(water.waves_light, pan, pitch=p, volume=v)
        self.resched(dur * d)
