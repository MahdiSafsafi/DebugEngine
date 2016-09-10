# DebugEngine
## What is DebugEngine ?
DebugEngine is a collection of utils related to debug staff (stack trace, CPU registers snaphot, debug info,...).
Basically, I started to write a commercial error log plugin for Delphi, then I noticed that my internal framework got bigger and bigger. So I decided to share it with the community in hope it will be useful.

## Features:
DebugEngine has a lot of functions and utilities allowing to you to do for example:
- Support x86 and x64 architecture.
- Accessing Delphi debug info.
- Delphi map parsing and map converter to binary format.
- Remove and restore Delphi debug info from PE file.
- Smart [stack trace](https://github.com/MahdiSafsafi/DebugEngine/wiki/Stack-trace).
- Try blocks trace.
- Updatting resource of Delphi app even if it was linked with debug info.
- Inserting custom debug info into PE file.
- CPU registers snapshot.
- Accessing vector registers.
- [Disasm and comment function with Debug info](https://github.com/MahdiSafsafi/DebugEngine/wiki/Getting-started#disasm-and-comment-function).
- Enumerating exception handlers.
- Delphi exception stack trace hook.
- Delphi string detection.
- PE utils.
- Disasm utils.
- ...

## Getting started:
Please refer to the [Wiki page](https://github.com/MahdiSafsafi/DebugEngine/wiki) and see [Demo](https://github.com/MahdiSafsafi/DebugEngine/tree/master/Demo) included with the library. 
Note that all public functions are documented (XML doc). However if you don't understand something, please feel free to contact me.


