#!/bin/sh

mkdir -p deploy/react
binary=$(find .stack-work/install/ -name "tslaq-event-tracker-exe")
cp $binary deploy
cp react/dist/* deploy/react
cp appspec.yml deploy
cd deploy
zip -r $TRAVIS_COMMIT .
cd ..
mv deploy/$TRAVIS_COMMIT.zip .
mkdir -p upload
mv $TRAVIS_COMMIT.zip upload/$TRAVIS_COMMIT.zip
