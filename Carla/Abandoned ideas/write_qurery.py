#!/usr/bin/python

import cgitb; cgitb.enable()
import cgi
import sys

form = cgi.FieldStorage()
head = form['head'].value
print 'Hello ' + head
