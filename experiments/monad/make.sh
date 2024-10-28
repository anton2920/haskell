#!/bin/sh

ghc -Wall -Wno-tabs Main.hs -optl-static && ./Main
