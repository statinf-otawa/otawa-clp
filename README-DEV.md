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
In the small example above, **LOOP_BOUND** is a constant.

To pass this information to the CLP analysis, we decided to add semantic instruction at the end of the block to express the potential relationship between registers
I.e. **Ri' = f(R0',... RN')**.

To resolve the second problem, the analysis has to record the condition used in the branch instruction (if any), and link the condition to the cmp instruction.

The backward analysis can be described in the state machine? model:
- It carries a state along the analysis. The state records necessary information to build predicates.
- The state is updated upon each instruction.

so we define the analysis by defining separately the state domain **S** and the update function **U: S * inst -> S**.
######The state 
The state records basically four information:
- The value of registers
- The value of the memory
- The predicates
- The relationship between condition and the status register

The value of registers and memory is what we discussed previously
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
ri* cond f(r-1, ..., rN, CST)
```
where **ri*** is the value of **ri** at the moment of comparison. The predicates are initialised when a **cmp** is met and keep this information. For example, in the block above, when the **cmp, ri, LOOP_BOUND** is met,
we know that in the case of branch taken, **ri>LOOP_BOUND** and in the case not taken, **ri<=LOOP_BOUND**.



The relationship between condition and status register is used to record the condition used in branch before meeting the cmp.
In the example above, as the analysis directs backward, at the last instruction **bhe**, we know that the condition is **he** without knowing the registers that are being compared.
This information is known later(in analysis), when we meet the **cmp ri, LOOP_BOUND**.
###### How to update
Upon un assignment:
if a register is assigned:
```asm
seti ri,CST
set ri, rj
etc.
```
For each register state, the **ri** in its **f** is replaced by the expression that is being assigned.
For example, in the case of **seti**, in the **f** of each register, **ri** is replaced by the constant that is being assigned.
In the case of **set**, **ri** is replaced by the **f** of **rj**. This can be understood as "if ri'=rj,now rj is computed by f(something), so ri' = f(something)".
Careful, We should not replace the **f** of **rj** by the expression, but we should substitute the **rj** in the **f** of **rj**.

ZHEN: i think the code does not substitue the **rj** in the **f** of **rj**, to be fixed.


For each memory state, the **f** is replaced in the same way as the registers state.

