System Requirements

Windows:
RMark requires installation of MARK which is available at http://www.phidot.org/software/mark/download/.
You only need mark.exe and rel_32.exe from the installation unless you are going to use the MARK interface as well.  If you use
the default directory of c:/Program Files/Mark for the installation then RMark will find the executables. For a 64 bit
machine, MARK will install in c:/Program Files (x86)/Mark.  If you add this driectory to the Path statement (Control Panel/System/Advanced),
RMark will find the executables.  Or you can also define an object MarkPath with a string value which gives the
path (e.g., MarkPath="c:/Program Files (x86)/mark").  RMark also uses 
notepad.exe to show output files.  You can optionally set MarkViewer to an executable that will be used to display the
output files. 

Linux: 
RMark also requires a mark executable but this is not generally available. It is
available at http://www.phidot.org/software/mark/rmark/linux/ for GNU/Linux. Instructions
are provided for setup.  For operating systems other than windows, release.gof 
will not work because rel_32.exe is only available on windows.  The default 
viewer on Linux is pico.  As with windows you can set the object name MarkViewer 
with an alternate. 