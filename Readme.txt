

The procedure of making our Lazarus Bindings library includes the following steps and is based on several articled on the web.

Obtaining the Mupdf DLL library.
Learning the essential concepts and export functions in Mupdf. 
Writing the Pascal functions.

If you have once downloaded and compiled the source code of Mupdf, you would probably find that the compiled output are several EXE files. 
There is no DLL library at all. So if you are not familiar with MAKE files, it can take you quite some hours to learn and modify the MAKE file in order to get the DLL library.

Fortunately there are always nice guys in this world. The developers of SumatraPDF, a slim PDF viewer program utilizing the power of Mupdf, are our savers. 
They have released their code files on GoogleCode and the compile result of their project does lay down a DLL library for you to reuse. So the steps can be quite simple.

Goto the project host of SumatraPDF: http://code.google.com/p/sumatrapdf/
Download the source code package (or use an SVN tool to synchronize with their latest work). 
Open Visual C++ (you can use the free Express version here) and load the project.
Select "Release" as the build configuration and compile the project. 
Find out the "libmupdf.dll" file out of the release folder.
You have the Mupdf DLL now.
The developers of SumatraPDF are two of the most diligent programmers. They keep quit a closed track with the latest development of Mupdf and update their code frequently.
Therefore, you can quite well trust them and use their library instead of trying to compile your own Mupdf DLL out of the official code.