# org.emacspeak.nature
from boopak.package import *
from boodle import agent
from boodle import builtin

play = bimport('org.boodler.play')
birds = bimport('org.emacspeak.birds')
mocks = [
    birds.mocking_1, birds.mocking_2, birds.mocking_3, #Northern Mocking Bird
    birds.mocking_4, birds.mocking_5,
    birds.fl_mocking_1, birds.fl_mocking_2, birds.fl_mocking_3, # Florida Mocking Bird
    birds.fl_mocking_4, birds.fl_mocking_5, birds.fl_mocking_6]


class MockingBirds(agent.Agent):

    def run(self):
        ag = play.IntermittentSoundsList(
            5.0, 10.0,  # delay
            1.0, 1.0,  # pitch
            0.1, 0.5,  # volume
            1.5,  # pan
            mocks)
        self.sched_agent(ag)
        ag = play.IntermittentSoundsList(
            10.0, 20.0,  # delay
            1.0, 1.0,  # pitch
            0.1, 0.4,  # volume
            1.0,  # pan
            mocks)
        self.sched_agent(ag)
        ag = play.IntermittentSoundsList(
            20.0, 40.0,  # delay
            1.0, 1.0,  # pitch
            0.1, 0.3,  # volume
            1.8,  # pan
            mocks)
        self.sched_agent(ag)
