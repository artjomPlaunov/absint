# absint
interval analysis by abstract interpretation on a simple language.

### specs
* assignment
* arithmetic limited to addition and subtraction 
* assume *Cond* where *Cond* is an equality comparison between variables and variable and constants
* nondeterministic choice *Cmd1*[]*Cmd2*
* sequencing *Cmd1*;*Cmd2*

### todo
* fix while
* widening operator
* refactor to support more abstract domains
* sign analysis
* prove soundness using whyml
