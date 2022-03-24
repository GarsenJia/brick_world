# lisp_brick_world
src.lisp 5 function mentioned in the ./lisp1 are defined in order to create some basic structure of the "Blocks World" 

To run the program, go the workspace in the department system and run command 

sbcl

In the REPL use command

(load "src")

(Load "test")

to load the lisp file



Once encountered error message "No enought blocks for variable" or "Invalid structure description" use command

(continue)

to keep test since these two errors are purposely triggered for demonstration purpose

Notice do change the value of *STRUCT-DESCR* on both line 12 and 182 for testing purpose


In part 3, I pioritize the description about color since color has strong binding power for now than on-table, on, and astride
once binded, the variable and the block are removed from a list of available blocks and a list of unbinded variabls. An error is thrown once we try to bind a unbinded variable and notice no available blocks.
Depite binding color specified description, variable in other description are binding to arbitrary available blocks(the first of the available blocks list).


In part 5, position descriptions are stored in a hashtable by the hash value of the base block (for case "ON" "ASTRIDE"), or it will be store by the hash value of "ON-TABLE".
Therefore by find the list under certain hash value we could access the blocks exactly on it. Therefore start from hash("ON-TABLE"), we could find every layer from bottom to top iteratively.




