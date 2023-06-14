# absint
interval analysis by abstract interpretation on a simple language.

### language
* assignment
* arithmetic limited to addition and subtraction 
* assume *b* where *b* is an equality comparison between variables and variable and constants
* nondeterministic choice S1[]S2
* sequencing S1;S2

### todo
* fix while
* widening operator
* refactor to support more abstract domains
* sign analysis
* prove soundness using whyml
