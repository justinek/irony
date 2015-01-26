#!/bin/bash

for FILENAME in $(ls /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/outputsWithParams_singleGoal)
do
	python parseOutput.py /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/outputsWithParams_singleGoal/$FILENAME > /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/parsedOutputsWithParams_singleGoal/$FILENAME
done
