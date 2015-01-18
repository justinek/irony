import sys, re, string

f = open(sys.argv[1], "r")

utteranceList = ["terrible", "bad", "ok", "good", "amazing"]

print "imageID,utterance,state,valence,arousal,probability"
for l in f:
    l = l.strip()
    images = l.split("))) ((((")
    for imageID in range(len(images)):
        image = images[imageID]
        utterances = image.split(")) (((")
        for utteranceIndex in range(len(utterances)):
            utterance = utterances[utteranceIndex]
            toks = utterance.split(")) (")
            labels = toks[0].split(") (")
            probs = toks[1].split(" ")
            for i in range(len(labels)):
                print str(imageID + 1) + "," + utteranceList[utteranceIndex] + "," + labels[i].replace(" ", ",").replace("(((((", "")  + "," +  probs[i].replace("))))", "")

    
