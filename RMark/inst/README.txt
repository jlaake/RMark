System Requirements

Windows:
RMark requires installation of MARK version 6.2 or higher which is available at http://www.phidot.org/software/mark/download/.
You only need mark32.exe or mark64.exe  and rel_32.exe from the installation unless you are going to use the MARK interface as well.  If you 
run RMark from 32 bit R it will run mark32.exe by default and likewise for 64 bit. You can override which is used by naming one of them
mark.exe and it will always use it. RMark will search in c:/Program Files/Mark and c:/Program Files (x86)/Mark.  If your markxx.exe are
not in either of those then you can add the directory for their location to the Path statement (Control Panel/System/Advanced),
or you can also define in R an object MarkPath with a string value which gives the directory specification (e.g., MarkPath="c:/Program Files (x86)/mark").  
Don't include the filename. RMark also uses notepad.exe to show output files.  You can optionally set MarkViewer in R to an executable that will be used to display the
output files. 

Linux: 
RMark also requires a mark executable but it is not generally available. It is
available at http://www.phidot.org/software/mark/rmark/linux/ for GNU/Linux. Instructions
are provided for setup.  For Linux operating system, release.gof 
will not work because rel_32.exe is only available on windows.  The default 
viewer on Linux is pico.  As with windows you can set the object name MarkViewer 
with an alternate. 

Mac:
At present there is no mark.exe available so you'll need to run the Windows version under an emulator.