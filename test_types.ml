type outcome = Success | Failure

type expr = 
| Desc of string
| Args of string list
| Outcome of outcome * string

                         

