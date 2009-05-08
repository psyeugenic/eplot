all: Makefile
	erlc -oebin src/egd_chart.erl 
	erlc -oebin src/eplot_main.erl -oebin
	erlc -oebin src/eview.erl -oebin

clean:
	rm ebin/*.beam
