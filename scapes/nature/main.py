# org.emacspeak.nature

import random
from boopak.package import *
from boopak.argdef import *
from boodle import agent, stereo
from boodle import builtin
manage = bimport('org.boodler.manage')
play = bimport('org.boodler.play')
aRain = bimport('com.eblong.pw.rain')
birds = bimport('org.emacspeak.birds')
water = bimport('org.boodler.sample.water')
wind = bimport('org.boodler.sample.wind')
rain = bimport('org.boodler.old.water')
soft = bimport('org.boodler.sample.rain')
cricket = bimport('org.boodler.old.insect')
trill = bimport('org.boodler.sample.insect')

ca_mocks = [
    birds.mocking_1, birds.mocking_2, birds.mocking_3, birds.mocking_4,
    birds.mocking_5, birds.mocking_6, birds.mocker_01, birds.mocker_02,
    birds.mocker_03, birds.mocker_04, birds.mocker_05, birds.mocker_06,
    birds.mocker_07, birds.mocker_08, birds.mocker_09, birds.mocker_10,
    birds.mocker_11, birds.mocker_12, birds.mocker_13, birds.mocker_14,
    birds.mocker_15, birds.mocker_16, birds.mocker_17, birds.mocker_18,
    birds.mocker_19, birds.mocker_20, birds.mocker_21, birds.mocker_22,
    birds.mocker_23, birds.mocker_24, birds.mocker_25, birds.mocker_26,
    birds.mocker_27, birds.mocker_28, birds.mocker_29, birds.mocker_30,
    birds.mocker_31, birds.mocker_32, birds.mocker_33, birds.mocker_34,
    birds.mocker_35, birds.mocker_36, birds.mocker_37, birds.mocker_38,
    birds.mocker_39, birds.mocker_40
]

fl_mocks = [
    birds.fl_mocking_1,
    birds.fl_mocking_2,
    birds.fl_mocking_3,
    birds.fl_mocking_4,
    birds.fl_mocking_5,
    birds.fl_mocking_6,
    birds.thrasher_01,
    birds.thrasher_02,
    birds.mocker_41,
    birds.mocker_42,
    birds.mocker_43,
    birds.mocker_44,
    birds.mocker_45,
    birds.mocker_46,
    birds.mocker_47,
    birds.mocker_48,
    birds.mocker_49,
    birds.mocker_50,
    birds.mocker_51,
    birds.mocker_52,
    birds.mocker_53,
    birds.mocker_54,
    birds.mocker_55,
    birds.mocker_56,
    birds.mocker_57,
    birds.mocker_58,
    birds.mocker_59,
    birds.mocker_60,
    birds.mocker_61,
    birds.mocker_62,
    birds.mocker_63,
    birds.mocker_64,
    birds.mocker_65,
    birds.mocker_66,
    birds.mocker_67,
    birds.mocker_68,
    birds.mocker_69,
    birds.mocker_70,
    birds.mocker_71,
    birds.mocker_72,
    birds.mocker_73,
    birds.mocker_74,
    birds.mocker_75,
    birds.mocker_76,
    birds.mocker_77,
    birds.mocker_78,
    birds.mocker_79,
    birds.mocker_80,
]

cuckoos = [
    birds.cuckoo_01, birds.cuckoo_02, birds.cuckoo_03, birds.cuckoo_04,
    birds.cuckoo_05, birds.cuckoo_06, birds.cuckoo_07, birds.cuckoo_08
]

nightingales = [
    birds.blackbird_01, birds.blackbird_02, birds.blackbird_03,
    birds.nightingale_001, birds.nightingale_002, birds.nightingale_003,
    birds.nightingale_004, birds.nightingale_005, birds.nightingale_006,
    birds.nightingale_007, birds.nightingale_008, birds.nightingale_009,
    birds.nightingale_010, birds.nightingale_011, birds.nightingale_012,
    birds.nightingale_013, birds.nightingale_014, birds.nightingale_015,
    birds.nightingale_016, birds.nightingale_017, birds.nightingale_018,
    birds.nightingale_019, birds.nightingale_020, birds.nightingale_021
]

ia_birds = [
    birds.cardinal_01,
    birds.screech_owl,
    birds.mt_quail,
    birds.morning_dove,
    birds.osprey,
    birds.lark_01,
    birds.lark_02,
    birds.wren_01,
    birds.ia_bird_1,
    birds.ia_bird_2,
    birds.ia_bird_3,
    birds.ia_bird_4,
    birds.ia_bird_5,
    birds.ia_bird_6,
    birds.ia_bird_7,
    birds.ia_bird_8,
    birds.ia_bird_9,
]

tropical_birds = [
    birds.tropical_01, birds.tropical_02, birds.tropical_03, birds.tropical_04,
    birds.tropical_05, birds.tropical_06, birds.tropical_07
]

song_birds = [
    birds.songbird_001, birds.songbird_002, birds.songbird_003,
    birds.songbird_004, birds.songbird_005, birds.songbird_006,
    birds.songbird_007, birds.songbird_008, birds.songbird_009,
    birds.songbird_010
]

# for b in [tropical_birds,  ca_mocks, fl_mocks, cuckoos, ia_birds, nightingales]:
# song_birds.extend(b)

showers = [
    rain.rain_thin, rain.rain_splatter, rain.rain_splashy_low,
    rain.rain_on_leaves, rain.rain_med, soft.soft_sprinkle, soft.soft_gentle,
    soft.soft_drizzle
]

streams = [
    rain.droplet_bloink,
    rain.droplet_plink,
    rain.droplet_plink_reverb_2,
    rain.droplet_plink_reverb_3,
    rain.droplet_plink_reverb,
    water.stream_rushing_1,
    water.stream_rushing_2,
    water.stream_rushing_3,
    rain.water_bubbling,
    soft.soft_sprinkle,
    soft.soft_gentle,
    soft.soft_drizzle,
]

winds = [
    wind.soft_low_1, wind.soft_low_2, wind.soft_low_3, wind.gust_soft_1,
    wind.gust_soft_2, wind.gust_soft_3, wind.soft_whistly_1,
    wind.soft_whistly_2, wind.soft_whistly_3, wind.soft_rushy
]

bugs = [
    cricket.complex_trilling_trig,
    cricket.everglades_conehead,
    cricket.fastcalling_tree_cricket,
    cricket.pine_tree_cricket,
    cricket.house_cricket,
    cricket.jamaican_field_cricket,
    cricket.japanese_burrowing_cricket,
    cricket.melodious_ground_cricket,
    cricket.robust_shieldback,
    cricket.sand_field_cricket,
    cricket.seashore_meadow_katydid,
    cricket.slightly_musical_conehead,
    cricket.southern_ground_cricket,
    cricket.syncopated_scaly_cricket,
    cricket.texas_meadow_katydid,
    cricket.tinking_trig,
    cricket.tinkling_ground_cricket,
    cricket.tropical_house_cricket,
    cricket.vocal_field_cricket,
    trill.cricket_loud,
    trill.cricket_ratchety,
    trill.cricket_scritchy,
    trill.crickets_distant,
    trill.crickets_low,
    trill.crickets_many_1,
    trill.cricket_solo_1,
    trill.cricket_solo_2,
    trill.cricket_solo_3,
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


# Helper:  doNature


def doNature(this):
    "Helper to run GardenBackground agents."
    nature = builtin.FadeInOutAgent(GardenBackground(0.0), 2, 60)
    # in front
    nc = this.new_channel_pan(
        stereo.compose(stereo.scalexy(1.2), stereo.shiftxy(0, 1.2)))
    this.sched_agent(nature, 0, nc)
    # behind
    nature = builtin.FadeInOutAgent(GardenBackground(60.0), 2, 60)
    nc = this.new_channel_pan(
        stereo.compose(stereo.scalexy(1.2), stereo.shiftxy(0, -1.1)))
    this.sched_agent(nature, 0, nc)


class GardenBackground(agent.Agent):
    def init(self, time=0.0):
        self.time = time
        self.pendulum = pendulum(60)

    def run(self):
        self.sched_agent(aRain.LightWind())
        count = self.pendulum.next()  # [0, 60]
        gurgle = random.choice(streams)
        breeze = random.choice(winds)
        vol = random.uniform(0.1, 0.8)
        pitch = random.uniform(0.7, 1.3)
        pan = (count - 30) * 0.04  # [-1.2, 1.2]
        d0 = self.sched_note_pan(gurgle, pan, pitch, min(0.8, 2 * vol),
                                 self.time)
        self.sched_note_pan(breeze, pan, pitch, vol, self.time)
        if ((count % 10) == 0):
            shower = random.choice(showers)
            self.sched_note_pan(shower, -1.2 * pan, pitch, vol,
                                abs(d0 + random.uniform(-1.0, 1.0)))
        self.resched(1.1 * d0)


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
            self.minDelay,
            self.maxDelay,
            0.9,
            1.1,  # pitch
            self.minVol,
            self.maxVol,
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
            self.minDelay,
            self.maxDelay,
            0.9,
            1.1,  # pitch
            self.minVol,
            self.maxVol,
            self.pan,
            ca_mocks)
        self.sched_agent(ag)


class TropicalBirds(agent.Agent):

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
            self.minDelay,
            self.maxDelay,
            0.9,
            1.1,  # pitch
            self.minVol,
            self.maxVol,
            self.pan,
            tropical_birds)
        self.sched_agent(ag)


class Cuckoos(agent.Agent):

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
            self.minDelay,
            self.maxDelay,
            0.9,
            1.1,  # pitch
            self.minVol,
            self.maxVol,
            self.pan,
            cuckoos)
        self.sched_agent(ag)


class Nightingales(agent.Agent):

    _args = ArgList(Arg(type=float), Arg(type=float), Arg(type=float),
                    Arg(type=float), Arg(type=float))

    def init(self,
             minDelay=4.0,
             maxDelay=12.0,
             minVol=0.7,
             maxVol=1.0,
             pan=1.0):
        self.minDelay = minDelay
        self.maxDelay = maxDelay
        self.minVol = minVol
        self.maxVol = maxVol
        self.pan = pan

    def run(self):
        ag = play.IntermittentSoundsList(
            self.minDelay,
            self.maxDelay,
            0.9,
            1.1,  # pitch
            self.minVol,
            self.maxVol,
            self.pan,
            nightingales)
        self.sched_agent(ag)


class ManyNightingales(agent.Agent):
    def run(self):
        doNature(self)
        y = [-1.25, -1.2, -1.15, -1.1, -1.05, -1, 1, 1.05, 1.1, 1.15, 1.2]
        for i in xrange(len(y)):
            bc = self.new_channel_pan(
                stereo.compose(stereo.scalexy(1.2), stereo.shiftxy(0, y[i])))
            ag = Nightingales(0.0, 50 - 2 * i, 0.1, 0.4, 1.0)
            self.sched_agent(ag, 0, bc)


class SongBirds(agent.Agent):

    _args = ArgList(Arg(type=float), Arg(type=float), Arg(type=float),
                    Arg(type=float), Arg(type=float))

    def init(self,
             minDelay=4.0,
             maxDelay=12.0,
             minVol=0.5,
             maxVol=1.0,
             pan=1.0):
        self.minDelay = minDelay
        self.maxDelay = maxDelay
        self.minVol = minVol
        self.maxVol = maxVol
        self.pan = pan

    def run(self):
        ag = play.IntermittentSoundsList(
            self.minDelay,
            self.maxDelay,
            0.9,
            1.1,  # pitch
            self.minVol,
            self.maxVol,
            self.pan,
            song_birds)
        self.sched_agent(ag)


class IABirds(agent.Agent):

    _args = ArgList(Arg(type=float), Arg(type=float), Arg(type=float),
                    Arg(type=float), Arg(type=float))

    def init(self,
             minDelay=4.0,
             maxDelay=12.0,
             minVol=0.3,
             maxVol=1.0,
             pan=1.0):
        self.minDelay = minDelay
        self.maxDelay = maxDelay
        self.minVol = minVol
        self.maxVol = maxVol
        self.pan = pan

    def run(self):
        ag = play.IntermittentSoundsList(
            self.minDelay,
            self.maxDelay,
            0.9,
            1.1,  # pitch
            self.minVol,
            self.maxVol,
            self.pan,
            ia_birds)
        self.sched_agent(ag)


class MockingBirds(agent.Agent):
    def run(self):
        doNature(self)
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


class ManyMockingBirds(agent.Agent):
    def run(self):
        doNature(self)
        y = [-1.5, -1.25, -1.125, -1, 0, 1, 1.125, 1.25, 1.5]
        for i in xrange(len(y)):
            for j in xrange(3):
                k = (i + j) % len(y)
                bc = self.new_channel_pan(
                    stereo.compose(stereo.scalexy(1.2),
                                   stereo.shiftxy(0, y[k])))
                ag = CaMockingBirds(
                    0,
                    120,  # Duration
                    0.15,
                    0.75,  # volume
                    1 + 0.025 * j  # pan
                )
                self.sched_agent(ag, k * 20, bc)
                ag = FlMockingBirds(
                    0,
                    120,  # Duration
                    0.15,
                    0.75,  # volume
                    1 + 0.025 * j  # pan
                )
                self.sched_agent(ag, k * 20, bc)


class MoreMockingBirds(agent.Agent):
    def run(self):
        doNature(self)
        y = [-1.5, -1.25, -1.125, -1, 0, 1, 1.125, 1.25, 1.5]
        for i in xrange(len(y)):
            for j in xrange(2):
                k = (i + j) % len(y)
                bc = self.new_channel_pan(
                    stereo.compose(stereo.scalexy(1.2),
                                   stereo.shiftxy(0, y[k])))
                ag = CaMockingBirds(
                    0,
                    120,  # Duration
                    0.15,
                    0.75,  # volume
                    1 + 0.025 * j  # pan
                )
                self.sched_agent(ag, k * 20, bc)
                ag = FlMockingBirds(
                    0,
                    120,  # Duration
                    0.15,
                    0.75,  # volume
                    1 + 0.025 * j  # pan
                )
                self.sched_agent(ag, k * 20, bc)


class SomeMockingBirds(agent.Agent):
    def run(self):
        doNature(self)
        y = [-1.5, -1.25, -1.125, -1, 0, 1, 1.125, 1.25, 1.5]
        for i in xrange(len(y)):
            k = (i + 1) % len(y)
            bc = self.new_channel_pan(
                stereo.compose(stereo.scalexy(1.2), stereo.shiftxy(0, y[k])))
            ag = CaMockingBirds(
                0,
                150,  # Duration
                0.15,
                0.75,  # volume
                1 + 0.025  # pan
            )
            self.sched_agent(ag, k * 20, bc)
            ag = FlMockingBirds(
                0,
                150,  # Duration
                0.15,
                0.75,  # volume
                1 + 0.025 * 1  # pan
            )
            self.sched_agent(ag, k * 20, bc)


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
            self.minDelay,
            self.maxDelay,
            0.9,
            1.1,  # pitch
            self.minVol,
            self.maxVol,
            self.pan,
            bugs)
        self.sched_agent(ag)


class Nightscape(agent.Agent):
    def run(self):
        doNature(self)
        for i in xrange(10):
            y = 1 + i * 0.025
            nc = self.new_channel_pan(
                stereo.compose(stereo.scalexy(1.25), stereo.shiftxy(0, y)))
            ag = Crickets(0.0, 10.0, 0.1, 0.7, 1.4)
            self.sched_agent(ag, 0, nc)


class BirdChorus(agent.Agent):
    def init(self):
        self.agents = [
            CaMockingBirds, SongBirds, IABirds, Cuckoos, Nightingales, 
            TropicalBirds,  FlMockingBirds
        ]

    def run(self):
        doNature(self)
        y = [-1.3, -1.2, -1.1, 0, 1.1, 1.2, 1.34]
        for i in xrange(len(self.agents)):
            for j in xrange(len(self.agents)):
                k = (i + j) % len(self.agents)
                bc = self.new_channel_pan(stereo.fixed(y[k]), 1.0)
                start = 10 * (i + j)
                ag = self.agents[i](
                    start,
                    90 + start,  # duration
                    0.5,
                    1.0,  # volume
                    1 + 0.05 * k  # pan
                )
                self.sched_agent(ag, k * 5, bc)


class MockingCuckoos(agent.Agent):
    def init(self):
        self.agents = [CaMockingBirds, Cuckoos, FlMockingBirds]

    def run(self):
        doNature(self)
        for i in xrange(len(self.agents)):
            for j in xrange(8):
                # compute y using i and j
                # i = 0 approaches, i=1 no change, i=2 recedes
                y = (i - 1) * (1.6 - j * 0.05)
                bc = self.new_channel_pan(
                    stereo.compose(stereo.shiftxy(0, y), stereo.scalexy(1.8)))
                ag = self.agents[i](
                    0,
                    120,
                    0.25,
                    0.5,  # volume
                    1.0  # pan
                )
                self.sched_agent(ag, 0, bc)


class BirdCalls(agent.Agent):
    def init(self):
        self.agents = [
            CaMockingBirds, IABirds, FlMockingBirds, SongBirds, TropicalBirds,
            Cuckoos, Nightingales
        ]

    def run(self):
        y = [-1.5, -1.2, -1.1, 0, 1.1, 1.2, 1.5]
        for i in xrange(len(self.agents)):
            for j in xrange(len(y)):
                k = (i + j) % len(y)
                bc = self.new_channel_pan(
                    stereo.compose(stereo.scalexy(1.4),
                                   stereo.shiftxy(0, y[j])))
                ag = self.agents[i](
                    0,
                    60,
                    0.1,
                    0.75,  # volume
                    1 + k * 0.1  # pan
                )
                self.sched_agent(ag, 0, bc)


class BirdSongs(agent.Agent):
    def init(self):
        self.agents = [
            TropicalBirds, IABirds, CaMockingBirds, Nightingales,
            FlMockingBirds, Cuckoos, SongBirds
        ]

    def run(self):
        doNature(self)
        y = [-1.5, -1.25, -1.125, 0, 1.125, 1.25, 1.5]
        for i in xrange(len(self.agents)):
            for j in xrange(5):
                k = (i + j) % len(self.agents)
                bc = self.new_channel_pan(
                    stereo.compose(stereo.scalexy(1.2),
                                   stereo.shiftxy(0, y[k])))
                ag = self.agents[i](
                    0,
                    120,  # Duration
                    0.15,
                    0.75,  # volume
                    1 + 0.025 * j  # pan
                )
                self.sched_agent(ag, k * 10, bc)
