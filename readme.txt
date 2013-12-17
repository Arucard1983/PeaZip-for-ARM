To compile sources you need Lazarus IDE (https://sourceforge.net/projects/lazarus).
Currently, the application is compiled using the 32 bit version of Lazarus, both on
Windows and on Linux systems.
Open .lpi project files and do "build all" to compile the executables.
Please read the following documentation to understand what is contained in the 
source package and please see precompiled program's packages to know what third 
parts executables (7z, arc, paq...) are needed by PeaZip.


Source package content:


SOURCES:

- project_pea.lpi: PEA, the actual engine for PEA file format support; 
- project_peach.lpi: PEACH, PEA launCHer, that compiles to the main executable PeaZip
  and act as GUI frontend for PEA, 7z and other utilities;
- project_gwrap.lpi: PeaLauncher, a GUI wrapper using pipes for 7z console application;
- project_demo_lib.lpi: a demo application using PEA source as a lib.

peazip-setup_script.iss in "installer" path is a setup script file for Inno Setup for 
creating Windows installer with file association for PeaZip; forms were created using
Inno Setup Form Designer, which is not needed to compile the installation script.

*.desktop files in "FreeDesktop_integration" path are files for integration of applications 
in desktop environments compliant with FreeDesktop standars (i.e. Gnome and KDE)

.res and resulting .rc files are used on Windows platform to give to the application's
executables manifest and binaries information (author, version etc)


MEDIA AND DOCUMENTATION:

Readme_*.txt files contain some hints for the Windows and Linux users.

"copying.txt" is the license file for PeaZip project sources, released under LGPL.

"media" path contains graphic for PeaZip project.

"lang" path contains featured translations of application's text. 


THIRD PARTS:

Units from Wolfgang Ehrhardt's crypto and utilities library, 
http://home.netsurf.de/wolfgang.ehrhardt/index.html are intellectual 
property of Wolfgang Ehrhardt, released uner Zlib license.
The unit FCAES256.PAS developed with the contribution of both me and, mainly,
of Wolfgang Ehrhardt.
Latest libraries used:
aes_2013-01-07.zip
crc_hash_2013-01-07.zip
fca_2013-01-07.zip
tf_2013-01-07.zip
util_2013-01-07.zip

7z (LGPL), ARC (GPL), LPAQ/PAQ8* (GPL), UnACE (royalty free), QUAD (LGPL), BALZ (public
domain), strip and UPX (GPL) binaries are needed to support mainstream file formats,
they are not included in source package (but are included in the program's precompiled
packages) and are intellectual property of respective Authors.

In PeaZip interface are used some icons from Tango Desktop Project, Crystal/Crystal Clear,
and NuoveXT, which are released under Creative Commons Attribution Share-Alike and LGPL 
licenses.