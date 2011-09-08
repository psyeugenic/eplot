Eplot
=====

Eplot is a small graph drawing tool that can be used from command prompt via the
escript eplot or it could be used from erlang code using egd_graph module.

Dependencies: 
-------------
 - Erlang/OTP, preferable R13B but earlier should work.
	
 - wx, if you plan to view graph immediately Erlang should be compiled
   with wx support enabled. Otherwise only file output can be used.
		

Eplot usage:
------------
    eplot [options] file1 file2 ...

where options are:

    -o Filename,        defaults to png image-format,
    -type Type,         bitmap_raw | png | eps,
    -width W,           Width, in pixels, of output,
    -height H,          Height, in pixels, of output,
    -render_engine RE,  alpha | opaque, type of render engine,
    -plot Plot,         plot2d | bar2d, plot type
    -x_label Label,     X-axis label,
    -y_label Label,     Y-axis label,
    -x_ticksize TS,     X-axis ticksize,
    -y_ticksize TS,     y-axis ticksize,

example:

    $> bin/eplot -o test1.png example/data1.dat example/data2.dat


egd_chart.erl usage:
--------------------
See [source file](https://github.com/psyeugenic/eplot/blob/master/src/egd_chart.erl) for info. 



Eplot ToDo:
-----------
 - document stuff
 - different symbols for different line entries
 - support multiple font and sizes (egd dependent)
 - line thickness (egd dependent)
 - additional graph types

EGD ToDo:
---------
eplot uses EGD as an backend to draw graphs. EGD lacks some features which
should be implemented.

 - polygon triangulation, filled triangles can be drawn fine but not
   polygons.
 - Truetype support
 - Line thickness/stroke size



