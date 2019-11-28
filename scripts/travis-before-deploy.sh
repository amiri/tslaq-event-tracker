#!/bin/sh

binary=$(find .stack-work/install/ -name "tslaq-event-tracker-exe")
echo $binary
zip -r $TRAVIS_COMMIT $binary react/dist/
mkdir -p upload
mv $TRAVIS_COMMIT.zip upload/$TRAVIS_COMMIT.zip
