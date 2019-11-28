#!/bin/sh

stack --no-terminal test --haddock --no-haddock-deps
cd react
yarn build
cd ..
