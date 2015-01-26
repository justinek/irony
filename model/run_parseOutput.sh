#!/bin/bash

for FILENAME in $(ls /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/outputsWithParams_noArousal)
do
	python parseOutput.py /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/outputsWithParams_noArousal/$FILENAME > /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/parsedOutputsWithParams_noArousal/$FILENAME
done
