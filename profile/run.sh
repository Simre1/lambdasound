rm profile/lambdasound-profile.*
cabal run --enable-profiling lambdasound-profile -- +RTS -s -hy -p &&
mv lambdasound-profile.* profile &&
cd profile &&
hp2ps -c lambdasound-profile.hp &&
convert -rotate -90 -background white -alpha remove lambdasound-profile.ps lambdasound-profile.png
