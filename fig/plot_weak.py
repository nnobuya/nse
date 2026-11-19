#! /usr/bin/env python


z_lmp = []; a_lmp = []; label_lmp = []

for line in open('./ecap.z219'):
    dat = line.split()
    if len(dat) == 3:
        z_lmp.append(int(dat[0]))
        a_lmp.append(int(dat[1]))
        label_lmp.append(dat[2])

n_lmp = [ j - i for i, j in zip(z_lmp,a_lmp) ]



z_ana = []; a_ana = []; label_ana = []

for line in open('./ecap.z3859'):
    dat = line.split()
    if len(dat) == 3:
        z_ana.append(int(dat[0]))
        a_ana.append(int(dat[1]))
        label_ana.append(dat[2])

n_ana = [ j - i for i, j in zip(z_ana,a_ana) ]




import matplotlib.pyplot as plt

for i, name in enumerate(label_lmp):
    if   name == 'LMP':
        plt.plot(n_lmp[i], z_lmp[i], 'o', c = 'b')
    elif name == 'FFN':
        plt.plot(n_lmp[i], z_lmp[i], 'o', c = 'r')

plt.xlim(xmin=-1)
plt.ylim(ymin=-1)

plt.savefig('nz.pdf')
#plt.show()

plt.close()

for i, name in enumerate(label_ana):
    if   name == 'LMP':
        plt.plot(n_ana[i], z_ana[i], 'o', c = 'b')
    elif name == 'ANA':
        plt.plot(n_ana[i], z_ana[i], 'o', c = 'r')

plt.xlim(xmin=-1)
plt.ylim(ymin=-1)

plt.savefig('nz_ana.pdf')

exit()
