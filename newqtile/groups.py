from libqtile import Group
from libqtile.lazzy import lazy

grps = ['I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'IIX', 'IX']

groups = [Group(i) for i in grps]

grplist = []
for i in range(len(groups) + 1):
    grplist.append(i)

grplist.remove(0)
