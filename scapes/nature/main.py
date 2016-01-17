# org.emacspeak.nature

import random
from boopak.package import *
from boopak.argdef import *
from boodle import agent
from boodle import builtin
manage = bimport('org.boodler.manage')
play = bimport('org.boodler.play')
birds = bimport('org.emacspeak.birds')
water = bimport('org.boodler.sample.water')
wind = bimport('org.boodler.sample.wind')
rain = bimport('org.boodler.old.water')
soft = bimport('org.boodler.sample.rain')
cricket = bimport('org.boodler.old.insect')
trill = bimport('org.boodler.sample.insect')

ca_mocks = [
    birds.mocking_1, birds.mocking_2, birds.mocking_3,  # CA Mocking Bird
    birds.mocking_4, birds.mocking_5, birds.mocking_6]

fl_mocks = [
    birds.fl_mocking_1, birds.fl_mocking_2, birds.fl_mocking_3,
    birds.fl_mocking_4, birds.fl_mocking_5, birds.fl_mocking_6]

song_birds = [
    birds.songbird_001, birds.songbird_002, birds.songbird_003,
    birds.songbird_004, birds.songbird_005, birds.songbird_006,
    birds.songbird_007, birds.songbird_008, birds.songbird_009,
    birds.songbird_010, birds.songbird_011, birds.songbird_012,
    birds.songbird_013, birds.songbird_014, birds.songbird_015,
    birds.songbird_016, birds.songbird_017, birds.songbird_018,
    birds.songbird_019, birds.songbird_020, birds.songbird_021,
    birds.songbird_022, birds.songbird_023, birds.songbird_024,
    birds.songbird_025, birds.songbird_026, birds.songbird_027,
    birds.songbird_028, birds.songbird_029, birds.songbird_030,
    birds.songbird_031, birds.songbird_032, birds.songbird_033,
    birds.songbird_034, birds.songbird_035, birds.songbird_036,
    birds.songbird_037, birds.songbird_038, birds.songbird_039,
    birds.songbird_040]

showers = [
    rain.rain_thin, rain.rain_splatter,
    rain.rain_splashy_low, rain.rain_on_leaves,
    rain.rain_med,
    soft.soft_sprinkle, soft.soft_gentle, soft.soft_drizzle]

streams = [
    water.stream_rushing_1, water.stream_rushing_2, water.stream_rushing_3,
    rain.water_bubbling, soft.soft_sprinkle,
    soft.soft_gentle, soft.soft_drizzle, ]

winds = [
    wind.soft_low_1, wind.soft_low_2, wind.soft_low_3,
    wind.gust_soft_1, wind.gust_soft_2, wind.gust_soft_3,
    wind.soft_whistly_1, wind.soft_whistly_2, wind.soft_whistly_3,
    wind.soft_rushy]

bugs = [
    cricket.complex_trilling_trig, cricket.everglades_conehead,
    cricket.fastcalling_tree_cricket, cricket.pine_tree_cricket,
    cricket.house_cricket, cricket.jamaican_field_cricket,
    cricket.japanese_burrowing_cricket, cricket.melodious_ground_cricket,
    cricket.robust_shieldback, cricket.sand_field_cricket,
    cricket.seashore_meadow_katydid, cricket.slightly_musical_conehead,
    cricket.southern_ground_cricket, cricket.syncopated_scaly_cricket,
    cricket.texas_meadow_katydid, cricket.tinking_trig,
    cricket.tinkling_ground_cricket, cricket.tropical_house_cricket,
    cricket.vocal_field_cricket, trill.cricket_loud,
    trill.cricket_ratchety, trill.cricket_scritchy,
    trill.crickets_distant, trill.crickets_low,
    trill.crickets_many_1, trill.cricket_solo_1,
    trill.cricket_solo_2, trill.cricket_solo_3,
    trill.crickets_one_swell,
]


# helper: Pendulum generator:

def pendulum(n):
    """Generate an oscilating sequence."""
    i = 0
    while 1:
        yield abs(i)
        i = i + 1
        if i == n:
            i = -n


class GardenBackground (agent.Agent):

    def init(self, time=0.0):
        self.time = time
        self.pendulum = pendulum(30)

    def run(self):
        count = self.pendulum.next()  # [0, 30]
        gurgle = random.choice(streams)
        breeze = random.choice(winds)
        vol = random.uniform(0.2, 0.6)
        if (count < 7 or count > 22):
            pitch = random.uniform(0.5, 1.1)
        else:
            pitch = random.uniform(0.7, 1.3)
        pan = (count - 15) * 0.1  # [-1.5, 1.5]
        d0 = self.sched_note_pan(gurgle, pan, pitch, vol, self.time)
        self.sched_note_pan(breeze, -1 * pan, pitch, vol,
                            self.time + random.uniform(1.0, 2.0))
        if ((count % 6) == 0):
            shower = random.choice(showers)
            self.sched_note_pan(shower, -1.2 * pan, pitch,
                                vol * 0.2,  self.time + random.uniform(1.0, 2.0))
        self.resched(d0 + random.uniform(-1.0, -0.1))


class FlMockingBirds(agent.Agent):

    _args = ArgList(Arg(type=float), Arg(type=float), Arg(type=float),
                    Arg(type=float), Arg(type=float))

    def init(self,
             minDelay=5.0,
             maxDelay=10.0,
             minVol=0.1,
             maxVol=1.0,
             pan=1.0):
        self.minDelay = minDelay
        self.maxDelay = maxDelay
        self.minVol = minVol
        self.maxVol = maxVol
        self.pan = pan

    def run(self):
        ag = play.IntermittentSoundsList(
            self.minDelay, self.maxDelay,
            0.9, 1.1,  # pitch
            self.minVol, self.maxVol,
            self.pan,
            fl_mocks)
        self.sched_agent(ag)


class CaMockingBirds(agent.Agent):

    _args = ArgList(Arg(type=float), Arg(type=float), Arg(type=float),
                    Arg(type=float), Arg(type=float))

    def init(self,
             minDelay=4.0,
             maxDelay=12.0,
             minVol=0.1,
             maxVol=1.0,
             pan=1.0):
        self.minDelay = minDelay
        self.maxDelay = maxDelay
        self.minVol = minVol
        self.maxVol = maxVol
        self.pan = pan

    def run(self):
        ag = play.IntermittentSoundsList(
            self.minDelay, self.maxDelay,
            0.9, 1.1,  # pitch
            self.minVol, self.maxVol,
            self.pan,
            ca_mocks)
        self.sched_agent(ag)


class SongBirds(agent.Agent):

    _args = ArgList(Arg(type=float), Arg(type=float), Arg(type=float),
                    Arg(type=float), Arg(type=float))

    def init(self,
             minDelay=4.0,
             maxDelay=12.0,
             minVol=0.1,
             maxVol=1.0,
             pan=1.0):
        self.minDelay = minDelay
        self.maxDelay = maxDelay
        self.minVol = minVol
        self.maxVol = maxVol
        self.pan = pan

    def run(self):
        ag = play.IntermittentSoundsList(
            self.minDelay, self.maxDelay,
            0.9, 1.1,  # pitch
            self.minVol, self.maxVol,
            self.pan,
            song_birds)
        self.sched_agent(ag)


class MockingBirds (agent.Agent):

    def run(self):
        nature = GardenBackground(0.0)
        self.sched_agent(nature)

        ag = CaMockingBirds(5.0, 10.0, 0.1, 0.5, 1.0)
        self.sched_agent(ag)
        ag = CaMockingBirds(30.0, 60.0, 0.1, 0.4, 1.2)
        self.sched_agent(ag)
        ag = CaMockingBirds(60.0, 90.0, 0.1, 0.3, 1.5)
        self.sched_agent(ag)
        ag = FlMockingBirds(5.0, 30.0, 0.05, 0.3, 1.0)
        self.sched_agent(ag)
        ag = FlMockingBirds(30.0, 75.0, 0.1, 0.3, 1.2)
        self.sched_agent(ag)
        ag = FlMockingBirds(10.0, 120.0, 0.1, 0.4, 1.5)
        self.sched_agent(ag)


class ManyMockingBirds (agent.Agent):

    def run(self):
        nature = GardenBackground(0.0)
        self.sched_agent(manage.VolumeModulateAgent(nature, 0.75))

        for _ in xrange(8):
            ag = CaMockingBirds(
                0.0, 120.0,
                0.1, 0.2,
                1.2)
            self.sched_agent(ag)
            ag = FlMockingBirds(
                7.0, 157.0,
                0.1, 0.3,
                1.2)
            self.sched_agent(ag)


class Crickets(agent.Agent):

    _args = ArgList(Arg(type=float), Arg(type=float), Arg(type=float),
                    Arg(type=float), Arg(type=float))

    def init(self,
             minDelay=0.0,
             maxDelay=9.0,
             minVol=0.1,
             maxVol=1.0,
             pan=1.0):
        self.minDelay = minDelay
        self.maxDelay = maxDelay
        self.minVol = minVol
        self.maxVol = maxVol
        self.pan = pan

    def run(self):
        ag = play.IntermittentSoundsList(
            self.minDelay, self.maxDelay,
            0.9, 1.1,  # pitch
            self.minVol, self.maxVol, self.pan,
            bugs)
        self.sched_agent(ag)


class Nightscape (agent.Agent):

    def run(self):
        nature = GardenBackground(0.0)
        self.sched_agent(manage.VolumeModulateAgent(nature, 0.5))

        for i in xrange(6):
            ag = Crickets(
                0.0, 12.0,
                0.1, 0.18, 1.2)
            self.sched_agent(ag)


class BirdChorus (agent.Agent):

    def init(self):
        self.agents = [CaMockingBirds, FlMockingBirds, SongBirds]

    def run(self):
        nature = GardenBackground(0.0)
        self.sched_agent(manage.VolumeModulateAgent(nature, 0.5))

        for i in xrange(len(self.agents)):
            for j in xrange(10):
                start = 30 * i + 10 * j
                ag = self.agents[i](
                    start, 30 + start,  # duration
                    0.075, 0.225,  # volume
                    0.7 + j * 0.05)
                self.sched_agent(ag)


class BirdSongs (agent.Agent):

    def init(self):
        self.agents = [CaMockingBirds, FlMockingBirds, SongBirds]

    def run(self):
        nature = GardenBackground(10.0)
        self.sched_agent(manage.VolumeModulateAgent(nature, 0.5))

        for i in xrange(len(self.agents)):
            for j in xrange(6):
                start = 5 * i + 15 * j
                ag = self.agents[i](
                    start, 30 + start,  # duration
                    0.05, 0.2,  # volume
                    0.8 + j * 0.07)
                self.sched_agent(ag)
