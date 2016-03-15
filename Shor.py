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
    L = math.ceil(math.log2(n))
    t = 2*L + 1
    M = t+L
    head = "#States:{}#Operators:{U = MultModN(" + str(L) + "," + str(n) + "," + str(x) + ");}#SpectralDecoms:{}#Commands:{\n"
    init = "InitializeTo: Int(" + str(t+L) + ",1);\n"
    hadamard = ""
    for i in range(1,t+1):
        hadamard = hadamard + "Apply: BitwiseHadamard(" + str(M) + "," + str(i) + ");\n"
    ctrlU = ""
    for i in reversed(range(1,t+1)):
        ctrlU = ctrlU + (1<<(t-i)) * ("ApplyCtrl:" + str(M) + "," + str(i) + "," + str(t+1) + ",U;\n")
    qft = "Apply: InverseQFT(" + str(M) + "," + str(1) + "," + str(t) + ");\n"
    measure = "SpectMeasure: CompBasis(" + str(M) + "," + str(1) + "," + str(t) + ");\n"
    cmd = head + init + hadamard + ctrlU + qft + measure+ "}"
    def powmod(m): return (x**m)%n
    r = 1
    f = open('tempfile','w')
    f.write(cmd)
    f.close()
    cond = True
    while(cond):
        out = subprocess.check_output(["./dist/build/Quantum/Quantum", "tempfile"]).decode("utf-8")
        result = fractions.Fraction(int(re.search("Result=(\d+)",out).group(1)),2**t)
        result = approx(cfrac(result))
        print(result)
        for i in range(0,len(result)):
            if powmod(result[i].denominator)==1:
                r = result[i].denominator
                cond = False
    if (r%2==0) and (powmod(r/2)!=n-1):
        a = fractions.gcd(powmod(r/2)-1,n) 
        b = fractions.gcd(powmod(r/2)+1,n)
        if a>1:
            return a
        if b>1:
            return b
        return "algorithm failed"
        
                    

    #os.remove('tempfile')    
def cfrac(x):
    f = math.floor(x)
    if x==f:
        return [f]
    else:
        return [f] + cfrac(1/(x-f))

def approx(cfracs):
    result = []
    for i in range(1,len(cfracs)+1):
        r = cfracs[i-1]
        for j in reversed(range(0,i-1)):
            r = cfracs[j] + fractions.Fraction(1,r)
        result = result + [r]
    return result
        
print(Shor(int(N)))


