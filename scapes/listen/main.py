# org.emacspeak.listen
"""Package org.emacspeak.listen: Implement  a Soundscape Listener.

Class SoundscapePanel implements a Listener Agent.
Emacs module soundscape.el starts this Agent with a  list of 
Boodler agents that implement the various Soundscapes.
Module Soundscape then uses the remote control functionality to switch Soundscapes on the fly.
All bounds checking etc is done on the Elisp side in soundscape.el. """

from boopak.package import *
from boodle import agent, builtin
from boopak.argdef import *

from boodle import builtin

manage = bimport('org.boodler.manage')


class Agents(agent.Agent):

    selected_event = 'agent'

    def init(self, fadetime=0.25):
        self.fadetime = fadetime
        self.prevchannel = None

    def run(self):
        self.listen(hold=True)

    def receive(self, event, *arglist):
        clas = self.load_described(arglist)
        self.trigger(clas)

    def trigger(self, scape):
        if (self.prevchannel != None and self.prevchannel.active):
            self.sched_agent(builtin.FadeOutAgent(self.fadetime),
                             chan=self.prevchannel)
        self.prevchannel = self.new_channel(0)
        self.prevchannel.set_volume(1, self.fadetime)
        self.sched_agent(scape, chan=self.prevchannel)


class SoundscapePanel(agent.Agent):
    _args = ArgList(ArgExtra(ListOf(Wrapped(agent.Agent))))
    selected_event = 'soundscape'

    def init(self, *agents):
        if (len(agents) == 0):
            raise Exception(
                'SoundscapePanel requires at least one agent argument')
        self.classlist = agents
        self.fadetime = 0.25
        self.pos = 0
        self.workagent = None

    def run(self):
        self.workagent = Agents(self.fadetime)
        self.post_listener_agent(self.workagent, hold=True)
        self.listen()
        a = self.classlist[self.pos]()
        sim = manage.Simultaneous(a)
        self.workagent.trigger(sim)

    def receive(self, event, *chans):
        print chans
        pick = []
        for chan in chans:
            pos = int(chan)
            pick.append(self.classlist[pos]())
        sim = manage.Simultaneous(*pick)
        self.workagent.trigger(sim)
