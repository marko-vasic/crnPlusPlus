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

If you want to gain a more in-depth understanding of how our technique
works, please take a look at our
[publication](https://arxiv.org/pdf/1809.07430.pdf
"publication"). Also, If you would like to reference CRN++ in an
academic publication, please cite our journal publication by including
the following bibtex entry:

```
@article{vasic2020crn++,
  title={CRN++: Molecular programming language},
  author={Vasi{\'c}, Marko and Soloveichik, David and Khurshid, Sarfraz},
  journal={Natural Computing},
  volume={19},
  number={2},
  pages={391--407},
  year={2020},
  publisher={Springer}
}
```
<!-- ``` -->
<!-- @inproceedings{VasicETAL18CrnPlusPlus, -->
<!-- 	title = {CRN++: Molecular Programming Language}, -->
<!-- 	author = {Vasic, Marko and Soloveichik, David and Khurshid, Sarfraz}, -->
<!--         booktitle = {Proceedings of the 24th International Conference on {DNA} Computing and Molecular Programming}, -->
<!--         year = {2018} -->
<!-- } -->
<!-- ``` -->

## Requirements

Mathematica version 11.2 or higher. Earlier versions of Mathematica
might be supported, but project was not tested under earlier versions.

## Project Structure

Examples of the CRN++ programs are under the *examples*
directory. Each example is divided into two files, mathematica ('.m')
file containing the core part of the code (actual program), and
notebook ('.nb') file containing the 'glue' code for simulating and
plotting the results. User can simply open the notebook file, see
simulation results of the program, and update the simulation
parameters if desired. The CRN++ compiler and simulation framework are
stored in the *packages* directory.

## Writing New Programs

If you would like to develop and test new CRN++ programs, please
follow the structure of examples under the examples directory.
