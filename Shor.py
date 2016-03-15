#/usr/bin/python3

import io
import re
import os
import sys
import random
import fractions
import subprocess
import math

N = sys.argv[1]

#Input: a product of two primes n=pq, p!=q
#Output: a nontrivial factor of n
def Shor(n):
    if n%2==0:
        return 2
    x = random.randrange(2,n-1)
    if fractions.gcd(x,n)>1:
        return fractions.gcd(x,n)
    n = 3
    x = 2
    L = math.ceil(math.log2(n))
    t = 2*L + 1
    M = t+L
    head = "#States:{}#Operators:{U = MultModN(" + str(L) + "," + str(n) + "," + str(x) + ");}#SpectralDecoms:{}#Commands:{\n"
    init = "InitializeTo: Int(" + str(t+L) + ",1);\n"
    hadamard = ""
    for i in range(1,t):
        hadamard = hadamard + "Apply: BitwiseHadamard(" + str(M) + "," + str(i) + ");\n"
    ctrlU = ""
    for i in reversed(range(1,t)):
        ctrlU = ctrlU + (2**(t-i-1)) * ("ApplyCtrl:" + str(M) + "," + str(i) + "," + str(t+1) + ",U;\n")
    qft = "Apply: InverseQFT(" + str(M) + "," + str(1) + "," + str(t) + ");\n"
    measure = "SpectMeasure: CompBasis(" + str(M) + "," + str(1) + "," + str(t) + ");\n"
    cmd = head + init + hadamard + ctrlU + qft + measure+ "}"
    f = open('tempfile','w')
    f.write(cmd)
    f.close()
    out = subprocess.check_output(["./dist/build/Quantum/Quantum", "tempfile"]).decode("utf-8")
    #os.remove('tempfile')
    result = fractions.Fraction(int(re.search("Result=(\d+)",out).group(1)),2**t)
    print(cfrac(result))
    
    
def cfrac(x):
    f = math.floor(x)
    if x==f:
        return [f]
    else:
        return [f] + cfrac(1/(x-f))

print(Shor(int(N)))

