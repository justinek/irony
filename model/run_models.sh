#!/bin/bash

for FILENAME in $(ls /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/modelsWithParams_singleGoal)
do
	node test/run_sandbox.js /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/modelsWithParams_singleGoal/$FILENAME > /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/model/outputsWithParams_singleGoal/$FILENAME
done

