rm profile/lambdasound.*
cabal run --enable-profiling lambdasound-profile -- +RTS -s -hc -p &&
mv lambdasound-profile.* profile &&
cd profile &&
hp2ps -c lambdasound-profile.hp &&
convert -rotate -90 lambdasound-profile.ps lambdasound-profile.png
