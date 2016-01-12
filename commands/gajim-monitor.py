#!/usr/bin/env python2

# TODO - figure out how to do this in python3.
# the only problem is gobject, which I only need for the mainloop function

import os
import sys
import gobject
import dbus
if getattr(dbus, 'version', (0,0,0)) >= (0,41,0):
    import dbus.glib
from time import strftime

OBJ_PATH = '/org/gajim/dbus/RemoteObject'
INTERFACE = 'org.gajim.dbus.RemoteInterface'
SERVICE = 'org.gajim.dbus'

def sig_get_content(sig):
    return sig[1][1]
def sig_get_user(sig):
    return sig[1][0]

def timestamp_str():
    return strftime("%Y-%m-%d_%H:%M:%S")

def out(s):
    sys.stdout.write(s)
def color(c):
    # TODO - add flag to turn off colors if I want
    if c == "default":
        out("\033[0m")
    elif c == "red":
        out("\033[31m")
    elif c == "green":
        out("\033[32m")
    elif c == "brown":
        out("\033[33m")
    elif c == "blue":
        out("\033[34m")
    elif c == "magenta":
        out("\033[35m")
    elif c == "cyan":
        out("\033[36m")
    elif c == "white":
        out("\033[37m")

# TODO -- add RGB, bold, italic, bg colors, etc

def new_message(sig):
    # message structure: 
    # sig[0] = domain (maybe account?)
    # sig[1][0] = user on other end (with resource)
    other_user = sig[1][0]
    content = sig[1][1]
    # sig[1][2] = ?? int32
    # sig[1][3] = ?? int32
    # sig[1][4] = ?? string "chat"
    # sig[1][5] = ?? int32
    # sig[1][6] = state (string "active")
    # sig[1][7+] = ??

    color("red")
    out(timestamp_str())
    out(" ")
    color("green")
    out(other_user)
    out(": ")
    color("cyan")
    out(content)
    out("\n")
    color("default")

def message_sent(sig):
    # these include real messages and state updates (active, composing, etc)

    # message structure: 
    # sig[0] = domain (maybe account?)
    # sig[1][0] = other user, no resource
    other_user = sig[1][0]
    content = sig[1][1]
    if content == "":
        return
    # sig[1][2] = ?? int or string
    # sig[1][3] = status (active, composing, inactive)
    # sig[1][4+] = ??

    color("red")
    out(timestamp_str())
    color("blue")
    out(" Me -> ")
    out(other_user)
    out(": ")
    color("brown")
    out(content)
    out("\n")
    color("default")

bus = dbus.SessionBus()
proxy_obj = bus.get_object(SERVICE, OBJ_PATH)
dbus_iface = dbus.Interface(proxy_obj, INTERFACE)

dbus_iface.connect_to_signal('NewMessage', new_message)
dbus_iface.connect_to_signal('MessageSent', message_sent)

mainloop = gobject.MainLoop()
mainloop.run()



