#| Exercise 5.31 |#

#| a. |#

(f 'x 'y)

#| 
1. Save and restore env around the
evaluator is useless as it's just 
a lookup (no application evaluation is
done)

2. Saving and restoring the env around 
the evaluation of each operand is
useless as well because they are 
directly evaluated

3. Saving argl is also useless 
as no operand needs any argl

4. Save and restore proc is usefull because
the proc will host f and (f)

|#

#| b. |#

((f) 'x 'y)

#| 
1. Saving and restoring env around
the operator is useful because we 
might construct a new env when evaluating
the application (f)

2. Useless for the rest

|#

#| c. |#

(f (g 'x) y)

#| 
1. Useless

2. It is useful around the evaluation
of the first operand as it will create
a new environment

3. Useful because the argl will be used in the
evaluation of the first operand

4. Usefull because the proc register
will be used in the evaluation
of the first operand

|#

#| d. |#

(f (g 'x) 'y)

#| 
Same as c. ? 
|#

#| Review http://community.schemewiki.org/?sicp-ex-5.31 |#