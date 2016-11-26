import random
import sys

n = int(sys.argv[1])
ls = list(range(1,n))
random.shuffle(ls)
with open('input', 'w') as f:
    for i in ls:
        f.write('{}\n'.format(i))
