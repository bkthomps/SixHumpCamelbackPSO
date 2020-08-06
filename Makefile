.DEFAULT_GOAL := run

run:
	stack build --exec SixHumpCamelbackPSO-exe

clean:
	rm *.csv
