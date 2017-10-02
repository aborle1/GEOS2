#!/usr/bin/env python

import sys
import re
import os
import shutil

nt=478
nl=40


ifile = open('layer_coare.dat','r') 


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


# read ifile line by line and write out transformed values:
for plevel in range(nl/5):
    quint= ifile.readline().split()
    for row in range(5):
        print '  ' + quint[row].upper()

print ' time TIME(nt)'
ifile.readline()

for ttime  in range((nt/5)+1):
    row = ifile.readline().split()
    for column in row:
        print '  ' + column.upper()

print ' year YY(nt)'
ifile.readline()

for ttime  in range((nt/5)+1):
    row = ifile.readline().split()
    for column in row:
        print '  ' + column.upper()

print ' month MO(nt)'
ifile.readline()

for ttime  in range((nt/5)+1):
    row = ifile.readline().split()
    for column in row:
        print '  ' + column.upper()

print ' day DD(nt)'
ifile.readline()

for ttime  in range((nt/5)+1):
    row = ifile.readline().split()
    for column in row:
        print '  ' + column.upper()

print ' hour HH(nt)'
ifile.readline()

for ttime  in range((nt/5)+1):
    row = ifile.readline().split()
    for column in row:
        print '  ' + column.upper()

print ' minute MM(nt)'
ifile.readline()

for ttime  in range((nt/5)+1):
    row = ifile.readline().split()
    for column in row:
        print '  ' + column.upper()

ifile.readline()
ifile.readline()

print '  Number of Multi-Level Fields:'
print '           9'


ifile.readline()

print '           1  & Temp_(K)'
print 'K'

thisfield=[]
inrow=[]

for nlevel in range(nl):
    for quinttime in range((nt/5)+1):
        inrow += ifile.readline().split()
    thisfield.append(inrow)
    inrow=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print '  ' + thisfield[plevel][ttime].upper()



ifile.readline()

print '           2  & H2O_Mixing_Ratio_(kg/kg'
print 'kg/kg'

thisfield=[]
inrow=[]

for nlevel in range(nl):
    for quinttime in range((nt/5)+1):
        inrow += ifile.readline().split()
    thisfield.append(inrow)
    inrow=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
#        print '  ' + thisfield[plevel][ttime].upper()
#        print '  ' + '  {0:03.7E}'.format(float(thisfield[plevel][ttime])/1000.0)
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime])/1000.0)

ifile.readline()

print '           3  & u_wind_(m/s)'
print 'm/s'

thisfield=[]
inrow=[]

for nlevel in range(nl):
    for quinttime in range((nt/5)+1):
        inrow += ifile.readline().split()
    thisfield.append(inrow)
    inrow=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))

ifile.readline()

print '           4  & v_wind_(m/s)'
print 'm/s'

thisfield=[]
inrow=[]

for nlevel in range(nl):
    for quinttime in range((nt/5)+1):
        inrow += ifile.readline().split()
    thisfield.append(inrow)
    inrow=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))


ifile.readline()

print '           5  & omega_(mb/hour)'
print 'mb/hour'

thisfield=[]
inrow=[]

for nlevel in range(nl):
    for quinttime in range((nt/5)+1):
        inrow += ifile.readline().split()
    thisfield.append(inrow)
    inrow=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))


# skip Wind_Div
ifile.readline()
for nlevel in range(nl):
    for quinttime in range((nt/5)+1):
        ifile.readline()


ifile.readline()

print '           6  & Horizontal_Temp__Advec_(K/hour)'
print 'K/hour'

thisfield=[]
inrow=[]

for nlevel in range(nl):
    for quinttime in range((nt/5)+1):
        inrow += ifile.readline().split()
    thisfield.append(inrow)
    inrow=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))



ifile.readline()

print '           7  & Vertical_T_Advec(K/hour)'
print 'K/hour'

thisfield=[]
inrow=[]

for nlevel in range(nl):
    for quinttime in range((nt/5)+1):
        inrow += ifile.readline().split()
    thisfield.append(inrow)
    inrow=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))



ifile.readline()

print '           8  & Horizontal_q_Advec_(g/kg/hour)'
print 'g/kg/hour'

thisfield=[]
inrow=[]

for nlevel in range(nl):
    for quinttime in range((nt/5)+1):
        inrow += ifile.readline().split()
    thisfield.append(inrow)
    inrow=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))



ifile.readline()

print '           9  & Vertical_q_Advec(g/kg/hour)'
print 'g/kg/hour'

thisfield=[]
inrow=[]

for nlevel in range(nl):
    for quinttime in range((nt/5)+1):
        inrow += ifile.readline().split()
    thisfield.append(inrow)
    inrow=[]

for ttime in range(nt):
    print ' nt=' + str(ttime+1).rjust(12,' ')
    for plevel in range(nl):
        print ' ' + '{: .7E}'.format(float(thisfield[plevel][ttime]))



ifile.close()

ifile = open('surface_coare.dat','r') 


ifile.readline()
ifile.readline()
ifile.readline()
ifile.readline()

#skip day, year, month, day, hour, minute

for foo in range(6):
    ifile.readline()
    for quinttime in range((nt/5)+1):
        ifile.readline()

precip=[]
LH=[]
SH=[]
ps=[]
ts=[]


ifile.readline()
for quinttime in range((nt/5)+1):
    precip += ifile.readline().split()
 
ifile.readline()
for quinttime in range((nt/5)+1):
    LH += ifile.readline().split()
 
ifile.readline()
for quinttime in range((nt/5)+1):
    SH += ifile.readline().split()
 
ifile.readline()
for quinttime in range((nt/5)+1):
    ps += ifile.readline().split()

# skip SST
ifile.readline()
for quinttime in range((nt/5)+1):
    ifile.readline()
 
ifile.readline()
for quinttime in range((nt/5)+1):
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

