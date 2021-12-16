## Theories
The goal of this new CLP analysis is to improve the precision of the CLP analysis in loops. 
Especially when widening the iterator in the loop to something like **(0, 1 ,LOOP_BOUND)** 
i.e. the iterator starts from 0, goes to LOOP_BOUND by step of 1 at each iteration.

### Problem Overview
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

  
### Semantic instructions

The binary code is represented, to maintain compatibility whatever the considered machine code, as a sequence of **semantic instructions**.

```
Sem ::=
	NOP
|	SET(rd, ra)				rd <- ra
|	SETI(rd, k)				rd <- k
|	NEG(rd, ra)				rd <- -ra
|	INV(rd, ra)				rd <- ~ra
|	ADD(rd, ra, rb)			rd <- ra + rb
|	SUB(rd, ra, rb)			rd <- ra - rb
|	... with MUL, DIV, MOD, AND, OR, XOR, SHL, SHR, ASR
|	ASSUME(c, rd)			c in Cond from rd is considered true
|	CMP(rd, ra, rd)			rd <- ra ~ rd
|	LOAD(rd, rb, t)			rd <- MEM_t[rb]
|	STORE(ra, rb, t)		MEM_t[rb] <- ra
|	BRANCH(rd)				PC <- rd
|	FORK(n)					creates two execution path at current and current pos + n
|	STOP
```
with rd, ra, rb \in Reg, k, n \in Nat, cond \in CompOp, t \in Type.

* Reg -- set of registers.
* Nat -- naturals.
* Cond= { NO_COND, EQ, NE, LT, LE, GT, GE }.
* Type -- any memory type (int8, uint8, int16, uint16, etc).


The algorithms proposed here share the same traversal algorithm of the program in semantic instructions.
An execution of semantic may result in several execution paths if the ``FORK``is used. The goal of this
code is therefore to manage ``FORK`` considering that each underlying algorithm is defined by its
state set ``S``, its initial state ``s0 \in S`` and an update function ``[.]: Sem x S -> S``.

Semantic instruction are organized in a block ``B`` that may be indexed and has a length ``L``.
The result is the set of final states.

	R <- {}
	Q <- { (i, s0) }
	
	while Q != {} do
		(i, s) <- pop Q
		if i >= L \/ B[i] = STOP then
			R <- R U { s }
		else if B[i] = FORK(n) then
			Q <- Q U { (i+1, s), (i+n, s) }
		else
			Q <- Q U { (i+1, [B[i]] s) }


## Filter building

#### Expressions and predicates

Let
```
Expr ::=
	None				-- no expression
|	n in N				-- constant expression
|	Reg					-- register expression
|	op Expr				-- unary expression with op in { -, ~ }
|	Expr binop Expr		-- binary expression with op in { +, -, *, /, %, &, |, ^, <<, >>, >>+ }
|	*Expr				-- memory cell at the given address.
```
The expression supports a replacement operation e[e' -> e"]: Expr x Expr x Expr -> Expr
that works this way:
	e[e -> e'] = e
	None[e -> e'] = None
	n[e -> e'] = n
	r[e -> e'] = r
	r'[e -> e'] = r'
	(op e)[e' -> e"] = op (e[e' -> e"])
	(e1 op e2)[e' -> e"] = e1[e' -> e"] op e2[e' -> e"]
	(*e)[e' -> e"] = *(e[e' -> e"])
	(e1 ~ e2)[e -> e'] = e1[e -> e'] ~ e2[e -> e']

The ownership function is available on expression:
	e in e = true
	e in None = false
	e in k = false
	e in r = false
	e in op e' = e in e'
	e in e1 op e2 = (e in e1) \/ (e in e2)
	e in *e' = e in e'

State ``Pred`` is a set of predicates ``Expr x CompOp x Expr U { taken }``.


### Forward Analysis

The program properties are represented by a pair`(P, M, b)` where `P in 2^Pred` is the set of
predicates (as defined before) and and `M = (Reg U *Reg) -> E` a map from locations
(registers, memory cells) to the expression representing the location. `b` is just
a Boolean flag about branching.

#### Phase 1: building the DAG

Algorithm is performed in forward way along the semantic instructions. The initial
value is (<{}, {}>, \x. x, false).

[NOP] (P, M, b) =
	(P, M, b)
[SETI(rd, k)] (P, M, b) =
	(P, M[rd -> k, *rd -> None], b)
[SET(rd, ra)] (P, M, b) =
	(P, M[rd -> M[ra], *rd -> None], b)
[op(rd, ra)] (P, M, b) =						with op in { NEG, INV }
	(P, M[rd -> op M[ra], *rd -> None], b)
[op(rd, ra, rb)] (P, M, b) =					with op in { ADD, SUB, MUL, DIV, ... }
	(P, M[rd -> M[ra] op M[rb], *rd -> None], b)
[CMP(rd, ra, rb)] (P, M, b) =
	(P, M[rd -> M[ra] ~ M[rb], *rd -> None], b)
[ASSUME(rd, op)] (P, M, b) =
	(P U { x op y }, M, b)		if M[rd] = x ~ y
	(P, M, b)					else
[LOAD(rd, ra)] (P, M, b) =
	(P, M[rd -> *ra, *rd -> None], b)
[STORE(rd, ra)] (P, M, b) =
	(P, M[
		*ra -> M[rd],
		*ri -> M[rd]	if M[ra] = M[ri],
		*ri -> M[*ri]	if addr(ra) = T \/ addr(ri) = T \/ addr(ra) /= addr(ri)
		*ri -> None		else
	b)
[BRANCH(_)] (P, M, b) =
	(P, M, true)

With the function `addr` that tries to evaluate if an adresse is surely different
from another one. It computes an address under the form `(Reg* x Int) U { T }`
and `Reg* = Reg U { $ }`. 

	addr(r) = [M[r]] ($, 0)
	[_] T = T
	[None] = T
	[k] (r, k') = (r, k + k')
	[r] ($, k) = (r, k)
	[r] (r', k)) = T
	[NEG e] _ = T
	[INV e] = T
	[e1 ADD e2] a = [e1] ([e2] a)
	[e1 SUB e2] a =
		let (r1, k1) = [e1] a in
		let (r2, k2) = [e1] ($, 0) in
		(r1, k1 - k2)	if r2 = $
		T				else
	[e1 op e2] a =
		let (r1, k1) = [e1] a in
		let (r2, k2) = [e1] a in
		($, k1 op k2)	if r1 = r2 = $
		T				else
	[*e] a = T


#### Phase 2: infering the condition code

Let the state `(<V, E>, M)`, a node `v in V` is compilable (poperty `is_comp` iff:
	
	is_comp(v, <V, E>, M) =
		M[v] /= _
		\/ forall{w -> v in E} is_comp(w)

The condition are built by starting from a comparison node, `node(c)` with `c in Cond`
supporting compilable expressions. The set of compilable predicate is:
	
	P = { make(v, c, w) | u = node(c) /\ v -> u in E /\ w -> u in E /\ is_comp(w) /\ x in Reg }

And `make` is defined by:
	
	make(v, c w) =
		{ (x, c, w) | M[x] v} U
		make(w, c^-1, u)		if v = node(NEG) /\ u = node(NEG) /\ w->u in E
		make()					if v = node(ADD) /\  /\ u = node(SUB) /\ 
		{}						else

		
  
### Backward analysis
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

### Only with predicates
Let
```
Expr ::=
	None
|	n in N
|	Reg
|	Reg'
|	op Expr				with op in { -, ~ }
|	Expr binop Expr		with op in { +, -, *, /, %, &, |, ^, <<, >>, >>+ }
|	* Expr
|	?CompOp				with CompOp = { =, /=, <, <=, >, >= }
```
The expression supports a replacement operation e[e' -> e"]: Expr x Expr x Expr -> Expr
that works this way:
	e[e -> e'] = e
	None[e -> e'] = None
	n[e -> e'] = n
	r[e -> e'] = r
	r'[e -> e'] = r'
	(op e)[e' -> e"] = op (e[e' -> e"])
	(e1 op e2)[e' -> e"] = e1[e' -> e"] op e2[e' -> e"]
	(*e)[e' -> e"] = *(e[e' -> e"])
	(e1 ~ e2)[e -> e'] = e1[e -> e'] ~ e2[e -> e']

State ``P`` is a set of predicates ``Expr x CompOp x Expr U { taken }``.
Iniial state, ``P0 = { r' = r | r' \in Reg }``.

The update function is defined by ``[.]: Sem x P -> P`` with ``Sem`` the set of semantic instructions.

[NOP] P = P
[SETI(rd, k)] P =
	{ (e[rd -> k] # e'[rd -> k]) | e # e' \in P }
[SET(rd, ra)] P =
	P U { e1[rd -> ra] # e2[rd -> ra]) | e1 # e2 \in P }
[LOAD(rd, rb, t)] S =
	P U { e1[rd -> *rb] # e2[rd -> *rb] | e1 # e2 \in P }
[STORE(ra, rb, t)] P =
	P U { e1[ra -> *rb] # e2[ra -> *rab] | e1 # e2) \in P }
[CMP(rd, ra, rb)] P =
	P U { ra op rb | rd = ?op \in P }
[ASSUME(op, rd)] P =
	P U { rd = ?op }
[BRANCH(_)] P =
	P U { taken }
[op(rd, ra)] P =			with ``op \in { NEG, INV } ``
	{ e1[rd -> op ra] # e2[rd -> ra op ra] | e1 # e2 \in P }
[op(rd, ra, rb)] P =			with ``op \in { ADD, SUB, MUL, ... } ``
	{ e1[rd -> ra op rb] # e2[rd -> ra op rb] | e1 # e2 \in P }


### Generating predicates for registers on output state


We can only generate for memories or registers that are contained once in the predicate.
The function ``avail-reg: P -> 2^R`` computes the set of registers available in a predicate:
	``avail-reg(e1 # e2) = avail-reg(e1) U avail-reg(e2) \ (avail-reg(e1) \cap avail-reg(e2))``
It is extended to expression this way:
```
	avail-reg(None) = {}
	avail-reg(n) = {}
	avail-reg(r) = {r}
	avail-reg(r') = {}
	avail-reg(op e) = avail-reg(e)
	avail-reg(e1 op e2) = avail-reg(e1) U avail-reg(e2) \ (avail-reg(e1) \cap avail-reg(e2))
	avail-reg(*e) = {}
	avail-reg(?op) = {}
```

For a predicate ``e1 # e2``, a predicate can generated for each register ``r \in avail-reg(e1 # e2)``.
If ``r \in e1``, we get ``r # extract[e1] r e2``.
If ``r \in e2``, we get ``r !# extract[e2] r e1)``.

With:
```
	!(=) = (=)
	!(/=) = (/=)
	!(<) = (>=)
	!(<=) = (>)
	!(>) = (<=)
	!(>=) = (<)
```

The extract function is defined by (and can fail if no pattern is found):
```
	extract[r] r e = r
	extract[e1 + e2] r e = extract[e1] r (e - e2)		if r \in e1
	extract[e1 + e2] r e = extract[e2] r (e - e1)		if r \in e2
	extract[e1 - e2] r e = extract[e1] r (e + e2)		if r \in e1
	extract[e1 - e2] r e = extract[e2] r -(e1 - e)		if r \in e2	
```
More forms may be added latter.

The obtained predicate to be expressed

	
##### In practice (too complex?)
Let
```
	S = reg: S_reg x rpred: S_rpred x mpred: S_mpred x taken: Bool
	S_reg = Reg -> Expr
	S_rpred = Reg x Cond x Expr
	S_mpred = Expr x Cond x Expr
```

Initially, ``S_init = (\r . None, {}, {}, \ sr. NO_COND, false)``.



The update function is defined as: ``[.]: Sem x S --> S`` where ``Sem`` is the set of semantic instructions.

[SET(rd, rs)] (R, P, Q, C, t) = (
	\r. rs if r = rd, (R r)[rd -> rs] else,
	{ (r, ~, e[rd -> rs]) | (r, ~, e) \in P } U { (rs, ~, e[rd -> rs]) | (rd, ~, e) \in P },
	{ (e1[rd -> rs], ~, e2[rd -> rs]) | (e1, ~, e2) \in Q }
	t
)

[SETI(rd, k)] (R, M, P, C, t) = (
	\r. rs if r = rd, (R r)[rd -> k] else,
	{ (r, ~, e[rd -> k]) | (r, ~, e) \in P } \ { (rd, ~, e) \in P },
	{ (e1[rd -> k], ~, e2[rd -> k]) | (e1, ~, e2) \in Q },
	t
)

[STORE(rs, rb, t)] (R, M, P, C, t) = (
	\r. rs if r = rd, (R r)[*rb -> rs] else,
	{ (r, ~, e[*rb -> rs]) | (r, ~, e) \in P } U { (rs, ~, e[*rb -> rs]) | (rb, ~, e) \in Q },
	{ (e1[*rb -> rs], ~, e2[*rb -> rs]) | (e1, ~, e2) \in Q }
	t
)

[LOAD(rd, rb, t)] (R, M, P, C, t) = (
	\r. *rb if r = rd, (R r)[rd -> *rb],
	{ (r, ~, e[rd > *rb]) | (r, ~, e) \in P },
	{ (e1[rd -> *rb], ~, e2[rd -> *rb]) | (e1, ~, e2) \in Q }
	t
)

[BRANCH(r)] (R, M, P, C, _) = (R, M, P, C, true)

[CMP(rd, ra, rb)] (R, M, P, C, t) = (
	\r. ra~rb if r = rd, None if rd \in R[r], R[r] else,
	P \ { (rd, ~, e) \in P } \ { (r, ~, e) | e \in P /\ rd \in e },
	Q \ { (e1, ~, e2 ) | (e1, ~, e2) \in Q /\ (rd \in e1 \/ rd \in e2) }
	t
)

[ASSUME(rd, ~)] (R, M, P, C, t) = (
	R,
	P U { e1 ~ e2 } if R[rd] = e1 ~ e2, P else,
	Q,
	t
)

[ADD(rd, r1, r2)] (R, M, P, C, t) = (
	,
	,
	,
	C, t
)



For each instruction performing a computation, a call to **U_assign** is performed
as below:
	```U(rd, expr, S)```
With:
	* SET(rd, rs) 		and expr = rs
	* SETI(rd, n)		and expr = n
	* ADD(rd, ri, rj)	and expr = ri + rj
	* SUB(rd, ri, rj)	and expr = ri - rj
	* MUL(rd, ri, rj)	and expr = ri * rj
	* DIV(rd, ri, rj)	and expr = ri / rj
	* MOD(rd, ri, rj)	and expr = ri % rj
	* AND(rd, ri, rj)	and expr = ri & rj
	* OR(rd, ri, rj)	and expr = ri | rj
	* XOR(rd, ri, rj)	and expr = ri ^ rj
	* SHL(rd, ri, rj)	and expr = ri << rj
	* SHR(rd, ri, rj)	and expr = ri >> rj
	* ASR(rd, ri, rj)	and expr = ri >>+ rj
	* LOAD(rd, ri, t)	and expr = *ri
	
```pseudo
U_assign(rd, expr', S) = 
	foreach (ri, expr) in S.reg:
		expr.substitue(rd, expr');
	foreach (ri, cond, expr) in s.pred:
		expr.substitue(rd, expr');
```

The expression in predicates and register values are replaced by the new expression.


For CMP and BRANCH instructions, we get have U_cmp and U_br:
```pseudo
U_cmp(sr, ra, rb, S) =
  if S.cond[sr] != NO_COND
    S.pred = S.pred::
          normalise(Predicate(ra, S.cond[sr], rb))::
          normalise(Predicate(rb, reverse(S.cond[sr]), ra));


U_br(S) =
	S.taken = true
```
Where the function normalise should take a predicate and rewrite the left side (ra) using ri'.

Memory related
The **load** is considered as register assignment instruction, so it lefts only STORE
```pseudo
U_store(ri, rb) =
	S.mem = S.mem :: (rb, ri)
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
