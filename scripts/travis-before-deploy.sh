#!/bin/sh

mkdir -p deploy/react
binary=$(find .stack-work/install/ -name "tslaq-event-tracker-exe")
echo "mv $binary deploy"
mv $binary deploy
cp react/dist/* deploy/react
cp appspec.yml deploy
cp -r etc deploy
cd deploy
zip -r $TRAVIS_COMMIT .
cd ..
echo "mv deploy/$TRAVIS_COMMIT.zip ."
mv deploy/$TRAVIS_COMMIT.zip .
mkdir -p upload
echo "mv $TRAVIS_COMMIT.zip upload/$TRAVIS_COMMIT.zip"
mv $TRAVIS_COMMIT.zip upload/$TRAVIS_COMMIT.zip
