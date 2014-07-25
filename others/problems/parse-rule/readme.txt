How to generate all patterns in the following rule:

# as a separator, { and } means choose a string between { and }.

For example:

Input: a string
{a # b} c {d # e {f # g}}
Output should be:

a c d
b c d
a c e f
a c e g 

===============
Liu Xinyu:

Solution:
Translate the rule to BNF syntax:

# rule = term + rule
# term = token | lst
# token = a..z
# lst = { rule # lst }

Parse the rule string to syntax tree, then traverse the tree.