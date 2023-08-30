rm profile/lambdasound.*
cabal run --enable-profiling lambdasound-profile -- +RTS -s -hy &&
mv lambdasound-profile.* profile &&
cd profile &&
hp2ps -c lambdasound-profile.hp &&
convert lambdasound-profile.ps lambdasound-profile.png
