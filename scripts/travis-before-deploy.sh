#!/bin/sh

binary=$(find .stack-work/install/ -name "tslaq-event-tracker-exe")
mkdir -p deploy/react
cd deploy
cp $binary .
cp ../react/dist/* react
zip -r $TRAVIS_COMMIT .
echo "deploy dir contents after zip:"
ls -lastr
cd ..
mv deploy/$TRAVIS_COMMIT.zip .
mkdir -p upload
mv $TRAVIS_COMMIT.zip upload/$TRAVIS_COMMIT.zip
