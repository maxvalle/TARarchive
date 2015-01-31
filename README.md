# TARarchive
A Xojo class to handle TAR archives

This is a pretty complete implementation of the TAR specifications with the Apple extensions.
It handles data and resource forks, AppleDouble for OS X, custom flags, long names, special files, links, etc.

There is no documentation, but an example is provided to show how to create, add files/folder and extract TAR archives.
The code is also extensively commented.

Notes:
    * This code is very old (about 2007) and was initially written to work for Carbon. I updated it for Cocoa
    * I don't use it from years but it still live in some existing product and never failed.
    * It uses some functions from MBS plugin
    * Some parts may be refactored to better code and using some new features of Xojo not available on RS
    * I can't give guarantee it works as expected. Take it as it as.
    * It was used and tested mainly on Mac, but it should work on Windows too. Linux is surely missing some parts.
