#!/usr/bin/env python

import sys
import re
import os
import shutil
import math

nt=210
nl=37


ifile = open('layer_kwjx.dat','r') 


ifile.readline()
ifile.readline()
ifile.readline()
ifile.readline()
ifile.readline()

print(' time length (nt)')
print(str(nt).rjust(12,' '))
print(' number of pressure levels (np)')
print(str(nl).rjust(12,' '))
print(' pressure levels P(np)')
print(' mb')

numrows=int(math.ceil(float(nl)/5.0))

# read ifile line by line and write out transformed values:
for plevel in range(numrows):
    row = ifile.readline().split()
    for column in row:
        print ' ' + '{: .7E}'.format(float(column))


numrows=int(math.ceil(float(nt)/5.0))

print ' time TIME(nt)'
ifile.readline()

for ttime  in range(numrows):
    row = ifile.readline().split()
    for column in row:
        print ' ' + '{: .7E}'.format(float(column))

print ' year YY(nt)'
ifile.readline()

for ttime  in range(numrows):
    row = ifile.readline().split()
    for column in row:
        print ' ' + '{: .7E}'.format(float(column))

print ' month MO(nt)'
ifile.readline()

for ttime  in range(numrows):
    row = ifile.readline().split()
    for column in row:
        print ' ' + '{: .7E}'.format(float(column))

print ' day DD(nt)'
ifile.readline()

for ttime  in range(numrows):
    row = ifile.readline().split()
    for column in row:
        print ' ' + '{: .7E}'.format(float(column))

print ' hour HH(nt)'
ifile.readline()

for ttime  in range(numrows):
    row = ifile.readline().split()
    for column in row:
        print ' ' + '{: .7E}'.format(float(column))

print ' minute MM(nt)'
ifile.readline()

for ttime  in range(numrows):
    row = ifile.readline().split()
    for column in row:
        print ' ' + '{: .7E}'.format(float(column))

ifile.readline()
ifile.readline()

print '  Number of Multi-Level Fields:'
print '           9'


ifile.readline()

print '           1  & Temp_(K)'
print 'K'

thisfield=[]
row=[]

for nlevel in range(nl):
    for quinttime in range(numrows):
        row += ifile.readline().split()
    thisfield.append(row)
    row=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))



ifile.readline()

print '           2  & H2O_Mixing_Ratio_(kg/kg'
print 'kg/kg'

thisfield=[]
row=[]

for nlevel in range(nl):
    for quinttime in range(numrows):
        row += ifile.readline().split()
    thisfield.append(row)
    row=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime])/1000.0)

ifile.readline()

print '           3  & u_wind_(m/s)'
print 'm/s'

thisfield=[]
row=[]

for nlevel in range(nl):
    for quinttime in range(numrows):
        row += ifile.readline().split()
    thisfield.append(row)
    row=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))

ifile.readline()

print '           4  & v_wind_(m/s)'
print 'm/s'

thisfield=[]
row=[]

for nlevel in range(nl):
    for quinttime in range(numrows):
        row += ifile.readline().split()
    thisfield.append(row)
    row=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))


ifile.readline()

print '           5  & omega_(mb/hour)'
print 'mb/hour'

thisfield=[]
row=[]

for nlevel in range(nl):
    for quinttime in range(numrows):
        row += ifile.readline().split()
    thisfield.append(row)
    row=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))


# skip Wind_Div
ifile.readline()
for nlevel in range(nl):
    for quinttime in range(numrows):
        ifile.readline()


ifile.readline()

print '           6  & Horizontal_Temp__Advec_(K/hour)'
print 'K/hour'

thisfield=[]
row=[]

for nlevel in range(nl):
    for quinttime in range(numrows):
        row += ifile.readline().split()
    thisfield.append(row)
    row=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))



ifile.readline()

print '           7  & Vertical_T_Advec(K/hour)'
print 'K/hour'

thisfield=[]
row=[]

for nlevel in range(nl):
    for quinttime in range(numrows):
        row += ifile.readline().split()
    thisfield.append(row)
    row=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))



ifile.readline()

print '           8  & Horizontal_q_Advec_(g/kg/hour)'
print 'g/kg/hour'

thisfield=[]
row=[]

for nlevel in range(nl):
    for quinttime in range(numrows):
        row += ifile.readline().split()
    thisfield.append(row)
    row=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))



ifile.readline()

print '           9  & Vertical_q_Advec(g/kg/hour)'
print 'g/kg/hour'

thisfield=[]
row=[]

for nlevel in range(nl):
    for quinttime in range(numrows):
        row += ifile.readline().split()
    thisfield.append(row)
    row=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))



ifile.close()

ifile = open('surface_kwjx.dat','r') 


ifile.readline()
ifile.readline()
ifile.readline()
ifile.readline()

#skip day, year, month, day, hour, minute

for foo in range(6):
    ifile.readline()
    for quinttime in range(numrows):
        ifile.readline()

precip=[]
LH=[]
SH=[]
ps=[]
ts=[]


ifile.readline()
for quinttime in range(numrows):
    precip += ifile.readline().split()
 
ifile.readline()
for quinttime in range(numrows):
    LH += ifile.readline().split()
 
ifile.readline()
for quinttime in range(numrows):
    SH += ifile.readline().split()
 
ifile.readline()
for quinttime in range(numrows):
    ps += ifile.readline().split()

## skip SST
#ifile.readline()
#for quinttime in range(numrows):
#    ifile.readline()
 
ifile.readline()
for quinttime in range(numrows):
    ts += ifile.readline().split()
 
print '  Number of Single-Level Fields:'
print '           5'

print '           1  & SH_(Upward w/m2)'
print 'Upward w/m2'
for ttime in range(nt):
    print ' ' + '{: .7E}'.format(float(SH[ttime]))

print '           2  & LH_(Upward w/m2)'
print 'Upward w/m2'
for ttime in range(nt):
    print ' ' + '{: .7E}'.format(float(LH[ttime]))

print '           3  & Ts_Air(K)'
print 'K'
for ttime in range(nt):
    print ' ' + '{: .7E}'.format(float(ts[ttime])+273.15)


print '           4  & Ps(mb)'
print 'mb'
for ttime in range(nt):
    print ' ' + '{: .7E}'.format(float(ps[ttime]))

print '           5  & Prec(mm/hr)'
print 'mm/hr'
for ttime in range(nt):
    print ' ' + '{: .7E}'.format(float(precip[ttime]))









ifile.close()





sys.exit(1)

