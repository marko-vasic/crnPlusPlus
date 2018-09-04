# CRN++: Molecular Programming Language

This project contains implementation of the CRN++ language. CRN++ is a
language for programming deterministic (mass-action) chemical kinetics
to perform computation. This project contains a compiler, translating
CRN++ programs into chemical reactions, as well as simulation
framework for simulation of the Chemical Reaction Networks. We rely on
the Mathematica
[package](http://users.ece.utexas.edu/~soloveichik/crnsimulator.html)
developed by David Soloveichik for the simulation of Chemical Reaction
Networks.

## Publication

If you are interested in knowing more about the project, please check
our publication. If you would like to reference CRN++ in an academic
publication, please cite our DNA publication:

```
@inproceedings{VasicETAL18CrnPlusPlus,
	title = {CRN++: Molecular Programming Language},
	author = {Vasic, Marko and Soloveichik, David and Khurshid, Sarfraz},
        booktitle = {Proceedings of the 24th International Conference on {DNA} Computing and Molecular Programming},
        year = {2018}
}
```

## Requirements

Mathematica version 11.2 or higher. Older versions of Mathematica
might be supported as well, but we have not tested.

## Project Structure

Examples of the CRN++ programs are under the *examples*
directory. Each example is divided into two files, mathematica ('.m')
file containing the core part of the code (actual program), and
notebook ('.nb') file containing the 'glue' code for simulating and
plotting the results. User can simply open the notebook file and see
simulation results of the program. The CRN++ compiler and simulation
framework are stored in the *packages* directory.

## Writing New Programs

If you would like to develop and test new CRN++ programs, please
follow the structure of examples under the examples directory.