# otawa-clp plugin

This repository implements an [OTAWA](http://otawa.fr) plug-in dedicated to
the analysis of values according CLP approach.

## User Installation

To install it, either otawa-config command must be on the path:

	$ cmake .
	$ make install

Or you can pass otawa-config from a custom location:

	$ cmake . -DOAWA_CONFIG=path_to_otawa_config
	$ make install

In both cases, this plug-in is installed in the plug-in directory
of the `otawa-config` installation.


## Testing

Basically, data for testing must be downloaded before performing tests:


	$ cd test
	$ ./make.py prepare

It will download benchmarks and otawa-icat reference data from OTAWA website.
To run the test, one has to type:

	$ ./make.py test
