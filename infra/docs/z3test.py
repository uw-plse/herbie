from z3 import *

# ---------------------------------------
# Initial Setup
# ---------------------------------------
rms = [RNE(), RNA(), RTZ(), RTP(), RTN()]#all rounding modes
x = FP("x", Float16())  # Try multiple levels of precision. 
y = FP("y", Float16())
z = FP("z", Float16())

# ---------------------------------------
# Distribution Properties
# ---------------------------------------
def distribute_lft_neg_in(rm):
    z1 = fpNeg(fpMul(rm, x, y))
    z2 = fpMul(rm, fpNeg(x), y)
    return (z1, z2)

def distribute_rgt_neg_in(rm):
    z1 = fpNeg(fpMul(rm, x, y))
    z2 = fpMul(rm, x, fpNeg(y))
    return (z1, z2)

def distribute_lft_neg_out(rm):
    z1 = fpMul(rm, fpNeg(x), y)
    z2 = fpNeg(fpMul(rm, x, y))
    return (z1, z2)

def distribute_rgt_neg_out(rm):
    z1 = fpMul(rm, x, fpNeg(y))
    z2 = fpNeg(fpMul(rm, x, y))
    return (z1, z2)

def distribute_neg_in(rm):
    z1 = fpNeg(fpAdd(rm, x, y))
    z2 = fpAdd(rm, fpNeg(x), fpNeg(y))
    return (z1, z2)

def distribute_neg_out(rm):
    z1 = fpAdd(rm, fpNeg(x), fpNeg(y))
    z2 = fpNeg(fpAdd(rm, x, y))
    return (z1, z2)

def distribute_frac_neg(rm):
    z1 = fpDiv(rm, fpNeg(x), y)
    z2 = fpNeg(fpDiv(rm, x, y))
    return (z1, z2)

def distribute_frac_neg2(rm):
    z1 = fpDiv(rm, x, fpNeg(y))
    z2 = fpNeg(fpDiv(rm, x, y))
    return (z1, z2)

def distribute_frac_neg_inv(rm):
    z2 = fpDiv(rm, fpNeg(x), y)
    z1 = fpNeg(fpDiv(rm, x, y))
    return (z1, z2)

def distribute_frac_neg2_inv(rm):
    z2 = fpDiv(rm, x, fpNeg(y))
    z1 = fpNeg(fpDiv(rm, x, y))
    return (z1, z2)

# ---------------------------------------
# Floating-Point Cancellation
# ---------------------------------------
def fp_cancel_sign_sub(rm):
    z1 = fpSub(rm, x, (fpMul(rm, fpNeg(y), z)))
    z2 = fpAdd(rm, x, fpMul(rm, y, z))
    return (z1, z2)

def fp_cancel_sub_sign(rm):
    z1 = fpAdd(rm, x, (fpMul(rm, fpNeg(y), z)))
    z2 = fpSub(rm, x, fpMul(rm, y, z))
    return (z1, z2)

# ---------------------------------------
# Commutative Properties
# ---------------------------------------
def commutative1(rm):
    z1 = fpAdd(rm, x, y)
    z2 = fpAdd(rm, x, y)
    return (z1, z2)

def commutative2(rm):
    z1 = fpMul(rm, x, y)
    z2 = fpMul(rm, x, y)
    return (z1, z2)

# ---------------------------------------
# Square & Absolute Value Properties
# ---------------------------------------
def sqr_neg(rm):
    z1 = fpMul(rm, fpNeg(x), fpNeg(x))
    z2 = fpMul(rm, x, x)
    return (z1, z2)

def sqr_abs(rm):
    z1 = fpMul(rm, fpAbs(x), fpAbs(x))
    z2 = fpMul(rm, x, x)
    return (z1, z2)

def sqr_abs_rev(rm):
    z1 = fpMul(rm, x, x)
    z2 = fpMul(rm, fpAbs(x), fpAbs(x))
    return (z1, z2)

def sqr_neg_rev(rm):
    z1 = fpMul(rm, x, x)
    z2 = fpMul(rm, fpNeg(x), fpNeg(x))
    return (z1, z2)

# ---------------------------------------
# Absolute Value & Negation Properties
# ---------------------------------------
def fabs_fabs():
    z1 = fpAbs(fpAbs(x))
    z2 = fpAbs(x)
    return (z1, z2)

def fabs_sub(rm):
    z1 = fpAbs(fpSub(rm, x, y))
    z2 = fpAbs(fpSub(rm, y, x))
    return (z1, z2)

def fabs_neg():
    z1 = fpAbs(fpNeg(x))
    z2 = fpAbs(x)
    return (z1, z2)

def fabs_sqr(rm):
    z1 = fpAbs(fpMul(rm, x, x))
    z2 = fpMul(rm, x, x)
    return (z1, z2)

# ---------------------------------------
# Floating-Point Safe Functions
# ---------------------------------------
def exp_0():
    z1 = fpExp(0)
    z2 = 1
    return (z1, z2)

def exp_1_e():
    z1 = fpExp(1)
    z2 = E
    return (z1, z2)

def one_exp():
    z1 = 1
    z2 = fpExp(0)
    return (z1, z2)

def e_exp_1():
    z1 = E
    z2 = fpExp(1)
    return (z1, z2)

# ---------------------------------------
# Power Properties
# ---------------------------------------
def unpow1():
    z1 = fpPow(x, 1)
    z2 = x
    return (z1, z2)

def unpow0():
    z1 = fpPow(x, 0)
    z2 = 1
    return (z1, z2)

def pow_base_1():
    z1 = fpPow(1, x)
    z2 = 1
    return (z1, z2)

def pow1():
    z1 = x
    z2 = fpPow(x, 1)
    return (z1, z2)

def pow_base_0():
    z1 = fpPow(0, x)
    z2 = 0
    return (z1, z2)

# ---------------------------------------
# Equation Lists
# ---------------------------------------
all_function_names = [
    # Distribution Properties
    distribute_lft_neg_in,
    distribute_rgt_neg_in,
    distribute_lft_neg_out,
    distribute_rgt_neg_out,
    distribute_neg_in,
    distribute_neg_out,
    distribute_frac_neg,
    distribute_frac_neg2,
    distribute_frac_neg_inv,
    distribute_frac_neg2_inv,
    
    # Floating-Point Cancellation
    fp_cancel_sign_sub,
    fp_cancel_sub_sign,
    
    # Commutative Properties
    commutative1,
    commutative2,
    
    # Square & Absolute Value Properties
    sqr_neg,
    sqr_abs,
    sqr_abs_rev,
    sqr_neg_rev,
    
    # Absolute Value & Negation Properties
    fabs_fabs,
    fabs_sub,
    fabs_neg,
    fabs_sqr,
    
    # Floating-Point Safe Functions
    exp_0,
    exp_1_e,
    one_exp,
    e_exp_1,
    
    # Power Properties
    unpow1,
    unpow0,
    pow_base_1,
    pow1,
    pow_base_0
]
functions_without_input = [
    fabs_fabs,
    fabs_neg
]    
functions_with_input = [
    # Distribution Properties
    distribute_lft_neg_in,
    distribute_rgt_neg_in,
    distribute_lft_neg_out,
    distribute_rgt_neg_out,
    distribute_neg_in,
    distribute_neg_out,
    distribute_frac_neg,
    distribute_frac_neg2,
    distribute_frac_neg_inv,
    distribute_frac_neg2_inv,

    # Floating-Point Cancellation
    fp_cancel_sign_sub,
    fp_cancel_sub_sign,

    # Commutative Properties
    commutative1,
    commutative2,

    # Square & Absolute Value Properties
    sqr_neg,
    sqr_abs,
    sqr_abs_rev,
    sqr_neg_rev,

    # Absolute Value & Negation Properties
    fabs_sub,
    fabs_sqr,
]

#1 Found Any exception
found_any_exception = [
    distribute_lft_neg_in,
    distribute_lft_neg_out,
    distribute_frac_neg,
    distribute_frac_neg_inv,
    fp_cancel_sign_sub,
    fabs_sub,
    distribute_frac_neg2_inv,
    distribute_neg_out,
    distribute_rgt_neg_in,
    distribute_rgt_neg_out,
    distribute_frac_neg2,
    fp_cancel_sub_sign,
]
#3
function_no_exceptions_at_any_rounding_mode =[
    commutative1,
    commutative2,
    sqr_neg,
    sqr_abs,
    sqr_abs_rev,
    sqr_neg_rev,
    fabs_sqr
]
#4 Can't run
functions_with_fpSolver = [
    exp_0,
    exp_1_e,
    one_exp,
    e_exp_1,
    unpow1,
    unpow0,
    pow_base_1,
    pow1,
    pow_base_0]


# ---------------------------------------
# Solver Execution
# ---------------------------------------
equations = [] #Enter equations to find an exception to exactness
all_safe=[]
for equation in equations:
    for rm in rms:
        z1, z2 = equation(rm)
        s = Solver()
        s.add(Not(Or((z1 == z2), And(fpIsNaN(z1), fpIsNaN(z2)))))

        answer = s.check()  # Returns "unsat" if no counterexample is found
        if answer == sat:
            print(equation, rm, answer, s.model(), s.model().evaluate(z1), s.model().evaluate(z2))
            equations.remove(equation)
            all_safe.append(equation)
            

print("Safe:")
print(equations)
print("Unsafe:")
print(all_safe)
