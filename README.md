native sbcl FFI interface to Portmidi 2.0 with virtual ports on linux and osx
no need of external cl library

the windows libportmidi.dll is provided, no virtual port for windows but this is the last version of portmidi precompiled. 

for another platform you have to install libportmidi.

Work in progress, this is inspired by https://github.com/PortMidi/pm_cl but I use the more conventional lisp names convention.

I prefere write-short vs WriteShort 
