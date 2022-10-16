#!/usr/bin/env python
"""Print the stream URL to stdout."""
from random import choice
import os
import sys



def validate_callsign(cs):
	'''
	Normal callsign format is 'WWWWFFAAA', where 'WWWW' is the radio station
	callsign, 'FF' is either 'AM' or 'FM', and 'AAA' is always 'AAC'.
	For this function, we expect the 'WWWWFF' part as input.
	'''
	if not cs or not isinstance(cs, str):
		raise ValueError('callsign \'%s\' is not a string.' % cs)
	if len(cs) < 6:
		raise ValueError('callsign \'%s\' is too short.' % cs)
	if not cs.endswith('AAC'):
		cs = cs + 'AAC'
	band = cs[-5:-3]
	if band != 'AM' and band != 'FM':
		raise ValueError('callsign \'%s\' is missing \'FM\' or \'AM\'.' % cs)
	return cs

def url(callsign):
	host = 'playerservices.streamtheworld.com'
	print( 'http://%s/api/livestream?version=1.2&mount=%s&lang=en' % (host, callsign))






if __name__ == '__main__':
	if len(sys.argv) < 2:
		print('usage: station callsign must be the first argument')
		sys.exit(1)

	callsign = validate_callsign(sys.argv[1])
	url(callsign)
