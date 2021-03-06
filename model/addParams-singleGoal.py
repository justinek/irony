import sys, re, string, numpy

p_states = numpy.arange(0.1, 0.9, 0.1)
#p_valences = numpy.arange(0.1, 0.6, 0.1)
#p_arousals = numpy.arange(0.1, 0.6, 0.1)
alphas = range(1, 4, 1)

for p_s in p_states:
    p_valences = numpy.arange(0.1, 1-p_s, 0.1)
    for p_v in p_valences:
        p_a = 1 - p_s - p_v
        for alpha in alphas:
            filename = str(p_s) + "_" + str(p_v) + "_" + str(p_a) + "_" + str(alpha) + ".church"
            wF = open("modelsWithParams_singleGoal/" + filename, "w")
            wF.write("(define p-state " + str(p_s) + ")\n" + "(define p-valence " + str(p_v) + ")\n" + "(define p-arousal " + str(p_a) + ")\n" + "(define alpha " + str(alpha) + ")")
            f = open(sys.argv[1], "r")
            for l in f:
                wF.write(l)
            f.close()
            #print str(p_s) + "," + str(p_v) + "," + str(p_a) + "," + str(alpha)
