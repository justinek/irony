#!/bin/bash

for FILENAME in $(ls /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/modelsWithParams_smoothed)
do
	node test/run_sandbox.js /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/modelsWithParams_smoothed/$FILENAME > /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/outputsWithParams_smoothed/$FILENAME
done

