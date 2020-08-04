type outcome = Success | Failure | Unknown | Empty

type expr = 
| Desc of string
| Args of string list
| Outcome of outcome * string

                         

