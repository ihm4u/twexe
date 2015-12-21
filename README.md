Quickstart
==========
To download, ready-to-go binaries and more info go to: https://ihm4u.github.io/twexe
The rest of the information in this readme is for developers.

How to build
============
If you want to build it yourself, you need Free Pascal 3.0 and Lazarus.  The 
easiest way is to download Lazarus 1.6RC1 which includes Free Pascal 3.0. 
The older versions of lazarus (1.4.x) have an older version of Free Pascal. Twexe
requires Free Pascal 3.0 to build. Here are the download links for Lazarus 1.6:

For Linux :
[Lazarus for Linux distributions](http://sourceforge.net/projects/lazarus/files/)

For windows you can download it from here:
[Lazarus for Windows](http://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2032%20bits/Lazarus%201.6RC1/)


1. Open the `twexe.lpi` file in lazarus and go to Run -> Build
2. You will get a twexe binary in the `build/i386-win32/` or `build/x86_64-linux/` directory depending on your architecture.
   **THIS IS NOT THE FINAL BINARY**, you need another step:
3. From the twexe directory run `build/<yourarch>/twexe src/tw5editions/twexe.html` this will generate a twexe executable in the `tw5editions` directory, it simply contains an empy TiddlyWiki5 from Tiddlywiki.com; _this is the final binary_.

That's it! Easy!

Another option: Makefile
========================
Another option is to use the Makefile. It may work for you.  I develop under linux, so most likely it won't work under windows unless you have a unix-like environment setup. The Makefile also tries to build a windows executable using Wine under linux. The `WINEAPPS` variable points to the directory where lazarus is intalled in the wine environment. So you would do something like `make WINEAPPS=/home/me/mywineapps` and it will build two executables in the rel directory.

I don't recommend you use the Makefile option, because it is tailored for the development environment and the release management setup that I use with git.
