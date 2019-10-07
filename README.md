# haskell-course2019-hw

`Functional Programming` course in Saint-Peterburg State University

Repo for homework submissions 

# Submission guidlines

### Tasks 1,2,3,4 (minilanguage implementation)

* Create a directory `tasks1234/LANGUAGE` where LANGUAGE is a mini-language you 
are implementing (`tasks1234/haskell`, `tasks1234/sql`, etc.)
* Put into `tasks1234/LANGUAGE/Authors` first names, last names and emails of
fellas who are implementing this.
* Put into `tasks1234/LANGUAGE/README.md` current state of things about homework:
  * which features are implemented
  * which are not but planned

  Update it regularly.
* Put into `tasks1234/LANGUAGE/` other files required for compilation of your homework.
  Using haskell build system called `stack` with resolver 
  [lts-13.19](https://gitlab.com/Kakadu/haskell-course-demos/blob/master/Expr/stack.yaml#L20)
  is very much recommended. It will allow all the students use the same version of 
  Haskell ecosystem that was used for course 
  [demos](https://gitlab.com/Kakadu/haskell-course-demos).
* Create a pull requests for this repo when ready.

See information about deadlines in [another file](./tasks1234/README.md)


