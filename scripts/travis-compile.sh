#!/bin/sh

stack --no-terminal test --haddock --no-haddock-deps
binary=$(find .stack-work/install/ -name "tslaq-event-tracker-exe")
$binary --js
cd react
yarn
yarn build
cd ..
