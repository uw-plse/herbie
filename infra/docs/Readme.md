Floating-Point Testing with Z3
Overview
This script uses the Z3 SMT solver to verify the correctness of floating-point properties across different rounding modes. It tests all rules that are thought of as FP-safe. 

Explanation
Floating-Point Setup:

The script defines FP variables (x, y, z) using Float16 precision.
It considers multiple rounding modes:
RNE (Round to Nearest, Even)
RNA (Round to Nearest, Away)
RTZ (Round Toward Zero)
RTP (Round Toward Positive)
RTN (Round Toward Negative)

The script constructs mathematical expressions and uses Z3 to check if they hold under all conditions.
If a discrepancy (counterexample) is found, the function is marked as unsafe.

To run:
Create function to be tested. 
Put the function name into the "equations" array and run the python file.

Output:
The script prints a list of safe functions and those that exhibit exceptions under certain rounding modes along with the exception.

Functions that hold across all rounding modes are considered FP-safe.
Classifying the Results:
1.Found any exceptions.
2.Found RNE exceptions.
3.Found no exceptions.
4.Can't Run
