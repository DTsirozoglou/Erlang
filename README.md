
Implemented in Erlang the functionality of a server that executes atomic transactions concurrently. 
An atomic transaction (the term coming from database systems) is a series of operations performed on some State that together appear to be one single operation (atomic). Thus, the transaction either results in all operations being
performed without any other operations being performed in-between, or none of the operations
being performed (all or nothing).
