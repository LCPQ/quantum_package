#!/usr/bin/env python2
# -*- coding: utf-8 -*-
import sys                
import matplotlib.pyplot as plt
from matplotlib import lines
from pylab import *

if __name__ == '__main__':
    inputfile = sys.argv[1]
with open(str(inputfile),'r') as f:
    vec_s1 = [float(x) for x in f.readline().split()]   
    mat_I = np.array([[float(x) for x in ln.split()] for ln in f])    
#mat_I = np.loadtxt(str(inputfile),skiprows=1)

print vec_s1
print mat_I
#enable the following lines if you dont have a working X display
#import matplotlib
#matplotlib.use('Agg')

#MUTUALI INFORMATION PLOT:

# edit the following line to select the orbitals...
zselect=False
if zselect:
    select =[]
    select.extend(range(34,132))
    select = [x-1 for x in select]
    nselect = len(select)
    vec_s1_s = np.zeros(nselect)
    mat_I_s = np.zeros((nselect,nselect))
    print mat_I_s, vec_s1_s

    for i,x in enumerate(select):
        vec_s1_s[i]=vec_s1[x]
    for i,x in enumerate(select):
        for j,y in enumerate(select):
            mat_I_s[i,j] = mat_I[x,y]

    
    print vec_s1_s
    print mat_I_s
    vec_s1 = vec_s1_s
    mat_I = mat_I_s 

    
    

#end selection


N= len(mat_I)
print N
theta = np.zeros(N)
r=np.zeros(N)
labels=np.zeros(N)
area=np.zeros(N)

for i in range(N):
  theta[i]=-2*pi/N*i+pi/2
  r[i]=1.0
  if zselect:
      labels[i]=select[i]+1
  else:
      labels[i]=i+1
  area[i]=vec_s1[i]*500

if (len(sys.argv) > 2):
 for j in range(2,len(sys.argv)):
   if (sys.argv[j] == '-c'):
     coordfile = sys.argv[j+1]
     with open(str(coordfile),'r') as f:
       for i in range(N):
         line = f.readline().split()
         theta[i] = float(line[0]) +pi/2
         r[i] = float(line[1])
print 'theta=',theta
print 'r=',r
 
 


ax = plt.subplot(111,polar=True)
ax.set_xticklabels([])
ax.set_yticklabels([])
ax.grid(b=False)
c = plt.scatter(theta,r,c="Red",s=area)


if (len(sys.argv) > 2):
  for j in range(2,len(sys.argv)):
    print 'sys.argv',j,sys.argv[j]
    if (sys.argv[j] == '-t'):
      plt.title(sys.argv[j+1])
      break
    else:
      plt.title('N = '+str(N/2)+', Results file: '+sys.argv[1])

#this is dummy:
#c1 = plt.scatter(theta,(r+0.05),c="red",s=0)

for i in range(N):
#  plt.annotate(int(labels[i]),xy=(theta[i],(r[i]+0.2)),size='xx-large',)
  plt.text(theta[i],(r[i]+0.25),int(labels[i]),size='x-small',ha='center',va='center')
  for j in range(i,N):
    x =[theta[i],theta[j]]
    y =[r[i],r[j]]
    #y =[1,1]
    if mat_I[i,j] >= 0.1:
      line1 = lines.Line2D(x, y, linewidth=3, color='black',linestyle='-', alpha=0.8,label='0.1')
      ax.add_line(line1)
    elif mat_I[i,j] >=0.01:
      line2 = lines.Line2D(x, y, linewidth=3, color='green',linestyle='-', alpha=0.8,label='0.01')
      ax.add_line(line2)
    elif mat_I[i,j] >=0.001:
      line3 = lines.Line2D(x, y, linewidth=3, color='gray',linestyle='-', alpha=0.6,label='0.001')
      ax.add_line(line3)
params = {'legend.fontsize': 16,
          'legend.linewidth': 2}

#need to check if the three types of lines really exist.
if 'line1' in locals() and 'line2' in locals() and 'line3' in locals():
  ax.legend([line1, line2, line3],[line1.get_label(),line2.get_label(),line3.get_label()],bbox_to_anchor=(0.00,1.0),fancybox=True,shadow=True)
elif 'line1' in locals() and 'line2' in locals():
  ax.legend([line1, line2],[line1.get_label(),line2.get_label()],bbox_to_anchor=(0.00,1.0),fancybox=True,shadow=True)
elif 'line1' in locals() and 'line3' in locals():
  ax.legend([line1, line3],[line1.get_label(),line3.get_label()],bbox_to_anchor=(0.00,1.0),fancybox=True,shadow=True)
elif 'line2' in locals() and 'line3' in locals():
  ax.legend([line2, line3],[line2.get_label(),line3.get_label()],bbox_to_anchor=(0.00,1.0),fancybox=True,shadow=True)
elif 'line1' in locals():
  ax.legend([line1],[line1.get_label()],bbox_to_anchor=(0.00,1.0),fancybox=True,shadow=True)
elif 'line2' in locals():
  ax.legend([line3],[line2.get_label()],bbox_to_anchor=(0.00,1.0),fancybox=True,shadow=True)
elif 'line3' in locals():
  ax.legend([line2],[line3.get_label()],bbox_to_anchor=(0.00,1.0),fancybox=True,shadow=True)
#probably not the best way to code it.

#for saving, enable matplotlib.use('Agg') 
#plt.savefig(str(sys.argv[1])+'.eps')
plt.show()
