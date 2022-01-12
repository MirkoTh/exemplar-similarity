@ECHO OFF
:: This batch file runs python code and writes back fitted parameters to a pickle file
TITLE Running GP Models in python
ECHO Running batch file executing pyton code fitting GP models

call conda activate exemplar-similarity
python python\main.py

ECHO running python code is over; back to .bat
PAUSE
