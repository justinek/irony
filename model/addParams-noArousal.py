import sys, re, string, numpy

p_states = numpy.arange(0.1, 1, 0.1)
#p_valences = numpy.arange(0.1, 0.6, 0.1)

alphas = range(1, 4, 1)

for p_s in p_states:
    p_v = 1 - p_s
    #for p_v in p_valences:
        #for p_a in p_arousals:
    for alpha in alphas:
        filename = str(p_s) + "_" + str(alpha) + ".church"
        wF = open("modelsWithParams_noArousal/" + filename, "w")
        wF.write("(define p-state " + str(p_s) + ")\n" + "(define p-valence " + str(p_v) + ")\n"+ "(define alpha " + str(alpha) + ")")
        f = open(sys.argv[1], "r")
        for l in f:
            wF.write(l)
        f.close()
