# org.emacspeak.listen

from boopak.package import *
from boodle import agent, builtin
from boopak.argdef import *


class Agents(agent.Agent):
    selected_event = 'agent'

    def init(self, fadetime=2.0):
        self.fadetime = fadetime
        self.prevchannel = None

    def run(self):
        self.listen(hold=True)

    def receive(self, event, *arglist):
        clas = self.load_described(arglist)
        self.trigger(clas)

    def trigger(self, clas):
        if (self.prevchannel != None and self.prevchannel.active):
            self.sched_agent(builtin.FadeOutAgent(
                self.fadetime), chan=self.prevchannel)
        self.prevchannel = self.new_channel(0)
        self.prevchannel.set_volume(1, self.fadetime)
        self.sched_agent(clas(), chan=self.prevchannel)


class Catalog(agent.Agent):
    _args = ArgList(ArgExtra(ListOf(Wrapped(agent.Agent))))
    selected_event = 'remote'

    def init(self, *agents):
        if (len(agents) == 0):
            raise Exception('Catalog requires at least one agent argument')
        self.classlist = agents
        self.fadetime = 2.0
        self.pos = 0
        self.workagent = None

    def run(self):
        self.workagent = Agents(self.fadetime)
        self.post_listener_agent(self.workagent, hold=True)
        self.listen()
        self.workagent.trigger(self.classlist[self.pos])

    def receive(self, event):
        key = event.split('.')[-1]
        newpos = self.pos
        count = len(self.classlist)
        if (key == 'chanup'):
            newpos = ((self.pos + 1) % count)
        elif (key == 'chandown'):
            newpos = ((self.pos + count - 1) % count)
        elif (key in ['num1', 'num2', 'num3', 'num4', 'num5', 'num6', 'num7', 'num8', 'num9']):
            val = int(key[-1]) - 1
            if (val >= 0 and val < count):
                newpos = val
        if (newpos != self.pos):
            self.pos = newpos
            self.workagent.trigger(self.classlist[self.pos])
