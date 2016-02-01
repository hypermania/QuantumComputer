# QuantumComputer

According to standard requirements for a working quantum computer, this quantum computer simulator will be:

* capable of initializing any number of qubits to arbitrary values
* consisting of universal unitary quantum gate set
* capable of performing arbitrary measurements and obtaining the output
* consists of a way for the user to sequence these operations (such as reading from a script of operations)

For this repo, the goal is to create an executable that takes from stdin simple commands for a quantum computer (analog to bit level commands for a classical computer), and print in stdout the results of its measurements. Since many useful quantum algorithms contain both classical and quantum routines, I think this approach is appropriate.