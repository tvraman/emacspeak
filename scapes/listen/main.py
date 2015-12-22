# org.emacspeak.listen

from boopak.package import *
from boodle import agent, builtin
from boopak.argdef import *

from boodle import builtin
manage = bimport('org.boodler.manage')


class Agents(agent.Agent):

    selected_event = 'agent'

    def init(self, fadetime=1.0):
        self.fadetime = fadetime
        self.prevchannel = None

    def run(self):
        self.listen(hold=True)

    def receive(self, event, *arglist):
        clas = self.load_described(arglist)
        self.trigger(clas)

    def trigger(self, scape):
        if (self.prevchannel != None and self.prevchannel.active):
            self.sched_agent(builtin.FadeOutAgent(
                self.fadetime), chan=self.prevchannel)
        self.prevchannel = self.new_channel(0)
        self.prevchannel.set_volume(1, self.fadetime)
        self.sched_agent(scape, chan=self.prevchannel)


class Catalog(agent.Agent):
    _args = ArgList(ArgExtra(ListOf(Wrapped(agent.Agent))))
    selected_event = 'remote'

    def init(self, *agents):
        if (len(agents) == 0):
            raise Exception('Catalog requires at least one agent argument')
        self.classlist = agents
        self.fadetime = 1.0
        self.lastEvent = None
        self.pos = 0
        self.workagent = None

    def run(self):
        self.workagent = Agents(self.fadetime)
        self.post_listener_agent(self.workagent, hold=True)
        self.listen()
        a = self.classlist[self.pos]()
        sim = manage.Simultaneous(a)
        self.workagent.trigger(sim)

    def receive(self, event):
        if (self.lastEvent is not  None and self.lastEvent is event):
                return
        key = event.split('.')[-1]
        chans = key.split(',')
        pick =[]
        for chan in chans:
                if ('c' in chan):
                        pos = int(key[1:])
                        pick.append(self.classlist[pos]())
        sim = manage.Simultaneous(*pick)
        self.workagent.trigger(sim)
        
