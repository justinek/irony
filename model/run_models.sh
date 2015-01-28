#!/bin/bash

for FILENAME in $(ls /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/modelsWithParams_noArousal)
do
	node test/run_sandbox.js /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/modelsWithParams_noArousal/$FILENAME > /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/outputsWithParams_noArousal/$FILENAME
done

