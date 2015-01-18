#!/usr/bin/env sh
pushd /Applications/aws-mturk-clt-1.3.0/bin
./loadHITs.sh $1 $2 $3 $4 $5 $6 $7 $8 $9 -label /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/mturkscripts/priors/launch/priors -input /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/mturkscripts/priors/launch/priors.input -question /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/mturkscripts/priors/launch/priors.question -properties /Users/justinek/Dropbox/Work/Grad_school/Research/Irony/mturkscripts/priors/launch/priors.properties -maxhits 1
popd