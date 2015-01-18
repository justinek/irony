#!/bin/bash

for FILENAME in $(ls /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/outputsWithParams_smoothed)
do
	python parseOutput.py /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/outputsWithParams_smoothed/$FILENAME > /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/parsedOutputsWithParams_smoothed/$FILENAME
done
