#!/usr/bin/env python

# My status bar script
import time
import os
import sys

# ========================================
# Edit here

_update_interval = 1.5 # in seconds
_music_update_interval=2 # update music every n udpates
_max_music_status_length = 45 # max length of music status

# How various players should be displayed
_info = { "amarok" : "A > ",
          "mpd"    : "D > ",
          "mocp"   : "C > "}

# Colors of various elements in the bar
_color = { "time"  : "^fg(white)",
           "music" : "^fg(#7FA3FF)",
           "cpuinfo" :  "^fg(#7DFF26)", # not yet supported
           "smiley" : "^fg(#FF8E1D)"
           }

# ========================================

def smiley():
    return " :-) "

def gettime():
    return time.strftime("%H:%M")

def getoutput(command):
    return os.popen(command).readlines()

# numbering starts from one
def getoutputline(command, line):
    a = os.popen(command)
    for i in range(line):
        s = a.readline()
    a.close()
    return s

def isrunning(programname):
    s = os.popen("pgrep %s"%programname).read()
    if s == '':
        return False
    else:
        return True

class music:
    def __init__(self):
        self.time = 0
        self.status = ""

    def update(self):
        if self.time == 0:
            self.status = self.getNewStatus()

        self.time = (self.time + 1) % _music_update_interval

        return self.status

    def getNewStatus(self):
        status = self.getMPCstatus()
        player = "mpd"
        if status == "" :
            status = self.getAMAROKstatus()
            player = "amarok"
            if status == "":
                status = self.getMOCPstatus()
                player = "mocp"

        if status != "":
            return (_info[player] + status)[:_max_music_status_length]

        
        return ""

    def getMOCPstatus(self):
        if isrunning("mocp"):
            output = getoutput("mocp -i")
            if output[0].find("PLAY") == -1:
                return ""
            else:
                # length of "Artist:"
                artist = output[3][7:].strip()
                # length of "SongTitle:"
                song   = output[4][10:].strip()
                if len(artist) != 0 or len(song) != 0:
                    return "%s - %s"%(song, artist)
                else:
                    # length of "Title: "
                    file = output[2][7:].strip()
                    return file

        return ""
            

    def getMPCstatus(self):
        if isrunning("mpd"):
            statuslist = getoutput('mpc --format "%title% - %artist%"')
            status = statuslist[0].strip()
            if status.find("volume:") != -1:
                return ""
            if len(statuslist) > 1:
                if statuslist[1].find("playing") == -1:
                    return ""
            if status == "-":
                return ""

            return status

        return ""

            
    def getAMAROKstatus(self):
        if isrunning("amarokapp"):
            status = getoutputline("dcop amarok player status", 1).strip()
            if status == "1":
                return ""
            else:
                return getoutputline("dcop amarok player nowPlaying", 1).strip()
        return ""


def main():
    m = music()
    while True:
        print " ",
        print _color["music"],
        print m.update(),
        print _color["smiley"],
        print smiley(),
        print _color["time"],
        print gettime(),
        print "^fg()",
	sys.stdout.flush()
        #The new line
        print 
        time.sleep(_update_interval)
        # os.system("sleep %f " %(_update_interval))
        


if __name__ == '__main__':
    if len(sys.argv) > 1:
        _update_interval = int(sys.argv[1])
    main()


