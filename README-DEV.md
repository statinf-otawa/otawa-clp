## Theories
The goal of this new CLP analysis is to improve the precision of the CLP analysis in loops. 
Especially when widening the iterator in the loop to something like **(0, 1 ,LOOP_BOUND)** 
i.e. the iterator starts from 0, goes to LOOP_BOUND by step of 1 at each iteration.

####Problem Overview
considering the example below:

```C
for (int i = 0; i < LOOP_BOUND; i++){
    LOOP_BODY;
}
```
This would be probably compiled as
```asm
seti ri, 0
loop_head:
cmp ri, LOOP_BOUND
bhe loop_end
#LOOP_BODY OMITTED
add ri, ri, 1
b loop_head
loop_end:
```
for the basic block
```asm
cmp ri, LOOP_BOUND
bhe loop_end
```
We would like to consider separately two cases:
the case where the branch is taken and the case where the branch is not taken.

If the case is taken (exit the loop), **ri** is greater than LOOP_BOUND and inversely for the case staying in the loop.
Therefore, the CLP analysis may widen the value of **ri** to **(0, 1, LOOP_BOUND)** for the basic blocks of loop body.
(Because **ri < LOOP_BOUND** if branch is not taken)

The global gaol of the additional analysis is to provide such an information to the CLP analysis. 
This information is described by inserting additional semantic instructions at the end of blocks. For example, we can add
```asm
cmp ri, LOOP_BOUND
assume r16, LOWER
```
for the case of branch not taken, and 
```asm
cmp ri, LOOP_BOUND
assume r16, HIGHER_OR_EQUAL
```
for the case of branch taken.

This is seems quite easy in such a simple example while there are two major problem in general:
- finding the real value of LOOP_BOUND (if LOOP_BOUND is calculated by other values within the block)
- building up the relationship between the **blt** and the **cmp** considering that the compiler may add up some instruction between them.
  Some micro-architecture may have complex branch mechanism so that the cmp and branch instruction are not structured at the end of blocks.

#### Backward analysis
To resolve the first problem, one has to resolve **LOOP_BOUND** i.e. finding out how **LOOP_BOUND** is computed.
In the small example above, **LOOP_BOUND** is a constant, so it is trivial to be found. However, we may compare for example
**ri < rj** and **rj** is computed by other registers or constants. Therefore, one has to track down how the **rj** is computed.
That's why we use backward analysis.

Finally, we build up a relationship between **Ri'**, i.e. **Ri' = f(R0',... RN')**. And we can generate semantic instructions for them.

To resolve the second problem, the analysis has to record the condition used in the branch instruction (if any), and link the condition to the cmp instruction.

The backward analysis can be described in the state machine? model:
- It carries a state along the analysis. The state records necessary information to build predicates.
- The state is updated upon each instruction.

so we define the analysis by defining separately the state domain **S** and the update function **U: S * inst -> S**.
######The state 
The state records basically four information:
- The value of registers at the end of block with respect to the value of register at current position
- The value of memory, because they could have alias.
- The predicates
- The relationship between condition and the status register

The value of registers is formed like:
```
Ri' = f(R0,R1,...RN,CST)
```
Where **Ri'** are the value of each register at the end of block and R0, ..., RN is the value of each register at **current** instruction; the **CST** is constant. They are updated along the analysis.
The analysis starts from the end of the basic block with state
```
Ri' = Ri
```
Then, the state is updated instruction per instruction. When the analysis finishes up the first instruction,
the state obtained express the **Ri\'** with respect to the value of registers at the beginning of the block or constants.



The state of register records how a register is computed with respect to the state of registers at current program point.

The state of the memory records the value in the memory, it is similar to the state of register, but it maps memory address to its value.

The predicates keep information about the last comparison and the branch (if any).

Predicates are all formed as
```
ri' cond f(r-1, ..., rN, CST)
```

The relationship between condition and status register is used to record the condition used in branch before meeting the cmp.
In the example above, as the analysis directs backward, at the last instruction **bhe**, we know that the condition is **he** without knowing the registers that are being compared.
This information is known later(in analysis), when we meet the **cmp ri, LOOP_BOUND**.

###### How to update
Let 
```
S = S_reg x S_mem x S_pred x S_cond
S_reg = Reg x Expr
S_mem = Expr x Expr
S_pred = Reg x Cond x Expr
S_cond = (cond | Null) x R_sr
```


Any register assignment:
```pseudo
U(ASSIGN(rd, expr'), S_reg) = 
foreach (ri, expr) in S_reg:
  expr.substitue(rd, expr');
  
U(ASSIGN(rd, expr'), S_pred) = 
foreach (ri, cond, expr) in S_pred:
  expr.substitue(rd, expr');
  
U(ASSIGN(_), S_mem) = Id
U(ASSIGN(_), S_cond) = Id
```
The expression in predicates and register values are replaced by the new expression.
Such instructions include set, seti, load, any unop or binop.

CMP/BR
```pseudo
U(CMP(ra, rb, R_sr), S_reg) = Id;
U(CMP(ra, rb, R_sr), S_mem) = Id;
U(CMP(ra, rb, R_sr), S_pred) =
  if (S_cond.SR ==  R_sr)
    S_pred = S_pred::
          Normalise(Predicate(ra, S_cond.cond, rb))::
          Normalise(Predicate(rb, reverse(S_cond.cond, ra));
  else
    Id;
U(CMP(ra, rb, R_sr), S_cond) =  Id;


U(BR(cond, SR), S_reg) = Id;
U(BR(cond, SR), S_mem) = Id;
U(BR(cond, SR), S_pred) = Id;
U(BR(cond, SR), S_cond) = (cond, SR);
```
Where the function normalise should take a predicate and rewrite the left side (ra) using ri'.

Memory related
The **load** is considered as register assignment instruction, so it lefts only STORE
```pseudo
U(STORE(addr_expr, expr), S_reg) = Id;
U(STORE(addr_expr, expr), S_pred) = Id;
U(STORE(addr_expr, expr), S_cond) = Id;
U(STORE(addr_expr, expr), S_mem) = 
  S_mem::(addr_expr, expr);
```

Finally, we obtain a state like :
```
Ri' = f(r0,... rN, *(addr1), ...)
Rj' = f(r0,... rN, *(addr1), ...)
Rn' <= f(r0,... rN, *(addr1), ...)
Rm' => f(r0,... rN, *(addr1), ...)
```
To build up predicates on Ri', we have to replace ri and *(addr) in the predicates. 
That means we want to represent Ri with respect to Ri', however figure out their relationship.
For simple cases like 
```pseudo
Ri' = Ri
Rn' <= Ri
```
We can find **Rn' <= Ri'**.
While in complex situation like
```pseudo
Ri' = Rk
Ri' = Ri 
Rn' <= Ri
```
as we have no information about Ri. we can do nothing about it.
In case where a ri' appears in several registers, like
```pseudo
Rj' = Ri
Ri' = Ri
Rn' <= Ri
```
We can generate predicates below
```apseudo
Rn' <= Ri'
Rn' <= Rj'
```