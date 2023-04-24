kvs(Lexername, Filename) :-
    process_create(path('python'), [Lexername, Filename], [stdout(pipe(In))]),
    read_string(In, _, X),
    term_to_atom(Y, X),
    write('kvs Programming Language v1.0'), nl,
    write('SER 502 - Spring 2023 - Team 28'), nl,
    write('@Authors - Vedasree Bodavula, Satvik Chemudupati, Kavya Alla, S'), nl, nl,
    program(Tree, Y, []),
    write('Executing......'), write(Filename), nl, nl,
    write('List of Tokens:'), nl, write(Y),nl, nl,
    write('Parse Tree:'), nl, write(Tree),nl, nl, write('Output:'), nl,
    eval_program(Tree, Output).

%----------------------------------------------------------------------------------------------------------------
:- table boolean/3, expression/3, term/3.

%to parse the program 
program(t_program(A)) -->['start'], block(A), ['end'].

%to parse the block 
block(t_block(A)) --> ['{'], block_section(A), ['}']. 
block_section(t_block(A, B)) --> statements(A), block_section(B).
block_section(t_block(A)) --> statements(A).

%to parse the different type of statements 
statements(t_statements(X)) --> declaration(X), [;].
statements(t_statements(X)) --> assignment(X), [;].
statements(t_statements(X)) --> expression(X), [;].
statements(t_statements(X)) --> boolean(X), [;].
statements(t_statements(X)) --> displaystatements(X), [;].
statements(t_statements(X)) --> ifcondition(X).
statements(t_statements(X)) --> ternarycondition(X), [;].
statements(t_statements(X)) --> forloop(X).
statements(t_statements(X)) --> whileloop(X).
statements(t_statements(X)) --> forrange(X).
statements(t_statements(X)) --> iterator(X), [;].

%to parse variable declaration
declaration(t_declareint(int, X, Y)) --> ['int'], identifier(X), ['='], expression(Y).
declaration(t_declarestr(string, X, Y)) --> ['string'], identifier(X), ['='], string(Y).
declaration(t_declarebool(bool, X, true)) --> ['bool'], identifier(X), [=], ['true'].
declaration(t_declarebool(bool, X, false)) --> ['bool'], identifier(X), [=], ['false'].
declaration(t_declare(X, Y)) --> type(X), identifier(Y).

%to parse assignment operation
assignment(t_assign(X, Y)) --> identifier(X), ['='], expression(Y).
assignment(t_assign(X, Y)) --> identifier(X), ['='], boolean(Y).

%to parse datatype
type(int) --> ['int'].
type(string) --> ['string'].
type(bool) --> ['bool'].

%to parse whileloop
whileloop(t_whileloop(A, B)) --> ['while'], ['('], (condition(A);boolean(A)), [')'], block(B).

%to parse forloop
forloop(t_forloop(A, B, C, D)) --> ['for'], ['('], declaration(A), [';'], (condition(B);boolean(B)), [';'], iterator(C), [')'], block(D).
forloop(t_forloop(A, B, C, D)) --> ['for'], ['('], declaration(A), [';'], (condition(B);boolean(B)), [';'], assignment(C), [')'], block(D).
forloop(t_forloop(A, B, C, D)) --> ['for'], ['('], assignment(A), [';'], (condition(B);boolean(B)), [';'], iterator(C), [')'], block(D).
forloop(t_forloop(A, B, C, D)) --> ['for'], ['('], assignment(A), [';'], (condition(B);boolean(B)), [';'], expression(C), [')'], block(D).

%to parse forrange
forrange(t_forrange(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], num(B), [':'], num(C), [')'], block(D).
forrange(t_forrange(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], identifier(B), [':'], identifier(C), [')'], block(D).
forrange(t_forrange(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], num(B), [':'], identifier(C), [')'], block(D).
forrange(t_forrange(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], identifier(B), [':'], num(C), [')'], block(D).

%to parse if condition
ifcondition(t_if_cond(A, B)) --> ['if'], ['('], (condition(A);boolean(A)), [')'], block(B).
ifcondition(t_if_cond(A, B, C)) --> ['if'], ['('], (condition(A);boolean(A)), [')'], block(B), ['else'], block(C).

%to parse ternary condition
ternarycondition(t_tern_cond(A, B, C)) --> (condition(A);boolean(A)), ['?'], statements(B), [':'], statements(C).

%to parse the boolean expression 
boolean(true) --> ['true'].
boolean(false) --> ['false'].
boolean(t_bool_not(X)) --> ['not'],['('], boolean(X), [')'].
boolean(t_bool_not(X)) --> ['not'],['('], condition(X), [')'].
boolean(t_bool_and(X, Y)) --> boolean(X), ['and'], boolean(Y).
boolean(t_bool_and(X, Y)) --> condition(X), ['and'], condition(Y).
boolean(t_bool_or(X, Y)) --> boolean(X), ['or'], boolean(Y).
boolean(t_bool_or(X, Y)) --> condition(X), ['or'], condition(Y).

%to parse display statements
displaystatements(t_display(X)) --> ['display'], identifier(X).
displaystatements(t_display(X)) --> ['display'], num(X).
displaystatements(t_display(X)) --> ['display'], string(X).

%to parse condition checks
condition(t_condition(X, Y, Z)) --> expression(X), operator(Y), expression(Z).
condition(t_condition(X, Y, Z)) --> string(X), operator(Y), string(Z).
condition(t_condition(X, Y, Z)) --> identifier(X), operator(Y), string(Z).

%to parse conditional operator
operator(==) --> ['=='].
operator('!=') --> ['!='].
operator(>) --> ['>'].
operator(<) --> ['<'].
operator(>=) --> ['>='].
operator(<=) --> ['<='].

%to parse addition ,subtraction,multiplication and division
expression(t_add(X, Y)) --> expression(X), ['+'], term(Y).
expression(t_sub(X, Y)) --> expression(X), ['-'], term(Y).
expression(X) --> term(X).
term(t_mult(X, Y)) --> term(X), ['*'], term(Y).
term(t_div(X, Y)) --> term(X), ['/'], term(Y).
term(t_mod(X, Y)) --> term(X), ['//'], term(Y).
term(X) --> ['('], expression(X), [')'].
term(X) --> num(X).
term(X) --> identifier(X).

%to parse unary increment and decrement operation
iterator(t_incre(X)) --> identifier(X), ['+'], ['+'] .
iterator(t_decre(X)) --> identifier(X), ['-'], ['-'].

%to parse number, identifier, and string
num(t_num(Y)) --> [Y], {number(Y)}.
identifier(identifier(Y)) --> [Y], {atom(Y)}.
string(Y) --> onlystring(Y).
onlystring(t_str(Y)) --> [Y], {atom(Y)}.

check_type(Val, Temp) :- string(Val), Temp = string.
check_type(Val, Temp) :- integer(Val), Temp = int.
check_type(Val, Temp) :- (Val = true ; Val = false), Temp = bool.

not(true, false).
not(false, true).

and(false, _, false).
and(_, false, false).
and(true, true, true).

or(true, _, true).
or(_, true, true).
or(false, false, false).

%----------------------------------------------------------------------------------------------------------------

%lookup predicate find the respective values from the environment

lookup(Id, [(_Type, Id, Temp)|_], Temp).
lookup(Id, [_|Tail], Temp) :- lookup(Id, Tail, Temp).

lookup_type(Id, [_|Tail], Temp) :- lookup_type(Id, Tail, Temp).
lookup_type(Id, [(Type,Id,_X)|_], Type).

%update predicate updates the value of the identifier

update(Type, Id, Val, [], [(Type, Id, Val)]).
update(Type, Id, Val, [(Type, Id, _)|Tail], [(Type, Id, Val)|Tail]).
update(Type, Id, Val, [Head|Tail], [Head|Rest]) :- update(Type, Id, Val, Tail, Rest).

%----------------------------------------------------------------------------------------------------------------

%to evaluate the program
eval_program(t_program(X), FinalEnv) :- eval_block(X, [], FinalEnv), !.

%to evaluate the block
eval_block(t_block(X), Env, FinalEnv) :- eval_block_section(X, Env, FinalEnv).
eval_block_section(t_block(X, Y), Env, FinalEnv) :- eval_statements(X, Env, Env1), 
    eval_block_section(Y, Env1, FinalEnv).
eval_block_section(t_block(X), Env, FinalEnv) :- eval_statements(X, Env, FinalEnv).

%to evaluate the statements
eval_statements(t_statements(X), Env, FinalEnv) :- 
    eval_declare(X, Env, FinalEnv);
    eval_assign(X, Env, FinalEnv);
    eval_boolean(X, Env, FinalEnv, _Val);
    eval_display(X, Env, FinalEnv);
    if_eval(X, Env, FinalEnv);
    eval_while(X, Env, FinalEnv);
    eval_forloop(X, Env, FinalEnv);
    eval_forrange(X, Env, FinalEnv);
    eval_terncond(X, Env, FinalEnv);
    eval_iter(X, Env, FinalEnv).

%to evaluate different types of declaration
eval_declare(t_declare(X, Y), Env, NewEnv):- 
    eval_char_tree(Y, Id),
    update(X, Id, _, Env, NewEnv).
eval_declare(t_declareint(int, Y, Z), Env, NewEnv):- 
    eval_char_tree(Y, Id),
    eval_expr(Z, Env, Env1, Val),
    update(int, Id, Val, Env1, NewEnv).
eval_declare(t_declarestr(string, Y, Z), Env, NewEnv):- 
    eval_char_tree(Y, Id),
    eval_str(Z, Env, NewEnv1, Val),
    update(string, Id, Val, NewEnv1, NewEnv).
eval_declare(t_declarebool(bool, Y, true), Env, NewEnv):- 
    eval_char_tree(Y, Id),
    update(bool, Id, true, Env, NewEnv).
eval_declare(t_declarebool(bool, Y, false), Env, NewEnv):- 
    eval_char_tree(Y, Id),
    update(bool, Id, false, Env, NewEnv).

%to evaluate the assignment operation
eval_assign(t_assign(X, Y), Env, NewEnv) :- 
    eval_expr(Y, Env, Env1, Val),
    check_type(Val, T),
    eval_char_tree(X, Id),
    lookup_type(Id, Env1, T1),
    T =@= T1,
    update(T, Id, Val, Env1, NewEnv).
eval_assign(t_assign(X, Y), Env, NewEnv) :- 
    eval_str(Y, Env, Env, Val),
    check_type(Val, T),
    eval_char_tree(X, Id),
    lookup_type(Id, Env, T1),
    T =@= T1,
    update(T, Id, Val, Env, NewEnv).
eval_assign(t_assign(X, Y), Env, NewEnv) :- 
   eval_boolean(Y, Env, Env, Val),
    check_type(Val, T),
    eval_char_tree(X, Id),
   lookup_type(Id, Env, T1),
    T =@= T1,
    update(T, Id, Val, Env, NewEnv).

%to evaluate boolean condition
eval_boolean(true, _Env1, _NewEnv, true).
eval_boolean(false, _Env1, _NewEnv,false).
eval_boolean(t_bool_not(B), Env, NewEnv, Val) :- 
    (eval_boolean(B, Env, NewEnv, Val1);eval_condition(B, Env, NewEnv, Val1)), 
    not(Val1, Val2), 
    Val = Val2.
eval_boolean(t_bool_and(X, Y), Env, NewEnv, Val) :- 
    eval_boolean(X, Env, NewEnv, Val1),
    eval_boolean(Y, Env, NewEnv, Val2),
    and(Val1, Val2, Val).
eval_boolean(t_bool_and(X, Y), Env, NewEnv, Val) :- 
    eval_condition(X, Env, NewEnv, Val1),
    eval_condition(Y, Env, NewEnv, Val2), 
    and(Val1, Val2, Val).
eval_boolean(t_bool_or(X, Y), Env, NewEnv, Val) :- 
    eval_boolean(X, Env, NewEnv, Val1),
    eval_boolean(Y, Env, NewEnv, Val2),
    or(Val1, Val2, Val).
eval_boolean(t_bool_or(X, Y), Env, NewEnv, Val) :- 
    eval_condition(X, Env, NewEnv, Val1),
    eval_condition(Y, Env, NewEnv, Val2),
    or(Val1, Val2, Val).

%to evaluate conditional operation
eval_condition(t_condition(X, ==, Y), Env, NewEnv, Val) :- 
    eval_expr(X, Env, NewEnv, Val1),
    eval_expr(Y, Env, NewEnv, Val2),
    (( Val1 =:= Val2, Val = true); ( \+(Val1 =:= Val2), Val = false)).
eval_condition(t_condition(X, '!=', Y), Env, NewEnv, Val) :- 
    eval_expr(X, Env, NewEnv, Val1),
    eval_expr(Y, Env, NewEnv, Val2),
    (( Val1 =\= Val2, Val = true);( \+(Val1 =\= Val2), Val = false)).
eval_condition(t_condition(X, '>', Y), Env, NewEnv, Val) :-
    eval_expr(X, Env, NewEnv, Val1),
    eval_expr(Y, Env, NewEnv, Val2),
    (( Val1 > Val2, Val = true);( \+(Val1 > Val2), Val = false)).
eval_condition(t_condition(X, '<', Y), Env, NewEnv, Val) :- 
    eval_expr(X, Env, NewEnv, Val1),
    eval_expr(Y, Env, NewEnv, Val2),
    (( Val1 < Val2, Val = true);( \+(Val1 < Val2), Val = false)).
eval_condition(t_condition(X, '>=', Y), Env, NewEnv, Val) :- 
    eval_expr(X, Env, NewEnv, Val1),
    eval_expr(Y, Env, NewEnv, Val2),
    (( Val1 >= Val2, Val = true);( \+(Val1 >= Val2), Val = false)).
eval_condition(t_condition(X, '<=', Y), Env, NewEnv, Val) :- 
    eval_expr(X, Env, NewEnv, Val1),
    eval_expr(Y, Env, NewEnv, Val2),
    (( Val1 =< Val2, Val = true);( \+(Val1 =< Val2), Val = false)).
eval_condition(t_condition(X, ==, Y), Env, NewEnv, Val) :- 
    eval_str(X, Env, NewEnv, Val1),
    eval_str(Y, Env, NewEnv, Val2),
    ((Val1 = Val2, Val = true);(\+(Val1 = Val2), Val = false)).
eval_condition(t_condition(X,'!=',Y), Env, NewEnv, Val) :-
    eval_str(X, Env, NewEnv, Val1),
    eval_str(Y, Env, NewEnv, Val2),
    ((Val1 = Val2, Val = false);(\+(Val1 = Val2), Val = true)).
eval_condition(t_condition(X,'>',Y), Env, NewEnv,_Val) :- 
    eval_str(X, Env, NewEnv,_Val1),
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").
eval_condition(t_condition(X,'<',Y), Env, NewEnv,_Val) :- 
    eval_str(X, Env, NewEnv,_Val1),
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").
eval_condition(t_condition(X,'>=',Y), Env, NewEnv,_Val) :- 
    eval_str(X, Env, NewEnv,_Val1),
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").
eval_condition(t_condition(X,'<=',Y), Env, NewEnv,_Val) :- 
    eval_str(X, Env, NewEnv,_Val1),
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").
eval_condition(t_condition(X,==,Y), Env, NewEnv, Val) :-
    eval_char_tree(X,Id),
    lookup(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    eval_str(Y, Env, NewEnv, Val2),
    ((Val1 =@= Val2, Val = true);(\+(Val1 =@= Val2), Val = false)).
eval_condition(t_condition(X,'!=',Y), Env, NewEnv, Val) :- 
    eval_char_tree(X,Id),
    lookup(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    eval_str(Y, Env, NewEnv, Val2),
    ((Val1 = Val2, Val = false);(\+(Val1 = Val2), Val = true)).
eval_condition(t_condition(X,'>',Y), Env, NewEnv,_Val) :- 
    eval_char_tree(X,Id),
    lookup(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").
eval_condition(t_condition(X,'<',Y), Env, NewEnv,_Val) :- 
    eval_char_tree(X,Id),
    lookup(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").
eval_condition(t_condition(X,'>=',Y), Env, NewEnv,_Val) :- 
    eval_char_tree(X,Id),
    lookup(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").
eval_condition(t_condition(X,'<=',Y), Env, NewEnv,_Val) :- 
    eval_char_tree(X,Id),
    lookup(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    eval_str(Y, Env, NewEnv,_Val2),
    write("invalid operation").

%to evaluate the display statement
eval_display(t_display(X), Env, Env) :- 
    eval_char_tree(X,Id),
    lookup(Id, Env, Val),
    writeln(Val).
eval_display(t_display(X), Env, Env) :- 
    eval_numtree(X, Val),
    writeln(Val).
eval_display(t_display(X), Env, Env) :- 
    eval_str(X, Env, Env, Val),
    writeln(Val).

%to evaluate if condition
if_eval(t_if_cond(Condition, IfBlock), Env, FinalEnv) :- 
    ((eval_condition(Condition, Env, NewEnv, true); eval_boolean(Condition, Env, NewEnv, true)),
     eval_block(IfBlock, NewEnv, FinalEnv)).
     
if_eval(t_if_cond(Condition, _IfBlock), Env, NewEnv) :- 
    eval_condition(Condition, Env, NewEnv, false); eval_boolean(Condition, Env, NewEnv, false).
    
if_eval(t_if_cond(Condition, IfBlock, ElseBlock), Env, FinalEnv) :- 
    (eval_condition(Condition, Env, NewEnv, true); eval_boolean(Condition, Env, NewEnv, true)),
    eval_block(IfBlock, NewEnv, FinalEnv).
    
if_eval(t_if_cond(Condition, _IfBlock, ElseBlock), Env, FinalEnv) :- 
    (eval_condition(Condition, Env, NewEnv, false); eval_boolean(Condition, Env, NewEnv, false)),
    eval_block(ElseBlock, NewEnv, FinalEnv).

%to evaluate the while loop
evaluate_while_loop(t_whileloop(Condition, Body), Env, FinalEnv):- 
    evaluate_boolean(Condition, Env, NewEnv, true),
    evaluate_block(Body, NewEnv, NewEnv1),
    evaluate_while_loop(t_whileloop(Condition, Body), NewEnv1, FinalEnv).
    
evaluate_while_loop(t_whileloop(Condition, _Body), Env, Env) :- 
    evaluate_boolean(Condition, Env, Env, false).
    
evaluate_while_loop(t_whileloop(Condition, Body), Env, FinalEnv):- 
    evaluate_condition(Condition, Env, NewEnv, true),
    evaluate_block(Body, NewEnv, NewEnv1),
    evaluate_while_loop(t_whileloop(Condition, Body), NewEnv1, FinalEnv).
    
evaluate_while_loop(t_whileloop(Condition, _Body), Env, Env) :- 
    evaluate_condition(Condition, Env, Env, false).

%to evaluate the forloop
eval_forloop(for_loop(Declaration, Condition, Iteration, Block), Env, FinalEnv) :- 
    eval_declaration(Declaration, Env, NewEnv1),
    loops(Condition, Iteration, Block, NewEnv1, FinalEnv).

eval_forloop(for_loop(Assignment, Condition, Iteration, Block), Env, FinalEnv) :- 
    eval_assignment(Assignment, Env, NewEnv1),
    loops(Condition, Iteration, Block, NewEnv1, FinalEnv).

loops(Condition, Iteration, Block, Env, FinalEnv) :- 
    eval_condition(Condition, Env, Env, true),
    eval_block(Block, Env, NewEnv1),
    (eval_iteration(Iteration, NewEnv1, NewEnv2); eval_expression(Iteration, NewEnv1, NewEnv2)),
    loops(Condition, Iteration, Block, NewEnv2, FinalEnv).

loops(Condition, _, _, Env, Env) :- 
    eval_condition(Condition, Env, Env, false).

loops(Boolean, Iteration, Block, Env, FinalEnv) :- 
    eval_boolean(Boolean, Env, Env, true),
    eval_block(Block, Env, NewEnv1),
    (eval_iteration(Iteration, NewEnv1, NewEnv2); eval_expression(Iteration, NewEnv1, NewEnv2)),
    loops(Boolean, Iteration, Block, NewEnv2, FinalEnv).

loops(Boolean, _, _, Env, Env) :- 
    eval_boolean(Boolean, Env, Env, false).

%to evaluate the forrange
eval_forrange(for_range(Variable, Start, End, Block), Env, FinalEnv) :-
    eval_char_tree(Variable, VariableId),
    (
        (eval_numtree(Start, StartVal), update(int, VariableId, StartVal, Env, NewEnv));
        (lookup(Start, Env, StartVal), update(int, VariableId, StartVal, Env, NewEnv))
    ),
    (
        (eval_numtree(End, EndVal));
        (eval_char_tree(End, EndVarId), lookup(EndVarId, NewEnv, EndVal))
    ),
    looping(VariableId, EndVal, Block, NewEnv, FinalEnv).

looping(VariableId, EndVal, Block, Env, FinalEnv) :-
    lookup(VariableId, Env, CurrentVal),
    CurrentVal < EndVal,
    eval_block(Block, Env, NewEnv),
    NextVal is CurrentVal + 1,
    update(int, VariableId, NextVal, NewEnv, NewEnv1),
    looping(VariableId, EndVal, Block, NewEnv1, FinalEnv).

looping(VariableId, EndVal, _, Env, Env) :-
    lookup(VariableId, Env, CurrentVal),
    CurrentVal >= EndVal.

%to evaluate ternary condition
eval_terncond(t_tern_cond(Condition, TrueStatements, _FalseStatements), Env, FinalEnv) :-
    (eval_condition(Condition, Env, NewEnv, true); eval_boolean(Condition, Env, NewEnv, true)),
    eval_statements(TrueStatements, NewEnv, FinalEnv).

eval_terncond(t_tern_cond(Condition, _TrueStatements, FalseStatements), Env, FinalEnv) :-
    (eval_condition(Condition, Env, NewEnv, false); eval_boolean(Condition, Env, NewEnv, false)),
    eval_statements(FalseStatements, NewEnv, FinalEnv).

%to evaluate the increment,decrement operation
eval_iter(t_incre(Variable), Env, NewEnv) :- 
    eval_char_tree(Variable, VariableId),
    lookup_type(VariableId, Env, int),
    lookup(VariableId, Env, VariableValue),
    NewVariableValue is VariableValue + 1, 
    update(int, VariableId, NewVariableValue, Env, NewEnv).

eval_iter(t_decre(Variable), Env, NewEnv) :- 
    eval_char_tree(Variable, VariableId),
    lookup_type(VariableId, Env, int),
    lookup(VariableId, Env, VariableValue),
    NewVariableValue is VariableValue - 1, 
    update(int, VariableId, NewVariableValue, Env, NewEnv).

%to evaluate addition,subtraction,multiplication and division
evaluate_expression(X, Env, NewEnv) :- 
    evaluate_assignment(X, Env, NewEnv).
    
evaluate_expression(X, Env, NewEnv, Val) :- 
    evaluate_term(X, Env, NewEnv, Val).
    
evaluate_expression(t_sub(X, Y), Env, NewEnv, Val) :-
    evaluate_expression(X, Env, Env1, Val1),
    evaluate_term(Y, Env1, NewEnv, Val2),
    Val is Val1 - Val2.
    
evaluate_term(X, Env, NewEnv, Val) :- 
    evaluate_term1(X, Env, NewEnv, Val).
    
evaluate_term(t_add(X, Y), Env, NewEnv, Val) :-
    evaluate_term(X, Env, Env1, Val1),
    evaluate_term1(Y, Env1, NewEnv, Val2),
    Val is Val1 + Val2.
    
evaluate_term1(X, Env, NewEnv, Val) :- 
    evaluate_term2(X, Env, NewEnv, Val).
    
evaluate_term1(t_mult(X, Y), Env, NewEnv, Val) :-
    evaluate_term1(X, Env, Env1, Val1),
    evaluate_term2(Y, Env1, NewEnv, Val2),
    Val is Val1 * Val2.
    
evaluate_term2(X, Env, NewEnv, Val) :- 
    evaluate_term3(X, Env, NewEnv, Val).
    
evaluate_term2(t_div(X, Y), Env, NewEnv, Val) :-
    evaluate_term2(X, Env, Env1, Val1), 
    evaluate_term3(Y, Env1, NewEnv, Val2),
    Val is floor(Val1 / Val2).
    
evaluate_term2(t_mod(X, Y), Env, NewEnv, Val) :-
    evaluate_term2(X, Env, Env1, Val1), 
    evaluate_term3(Y, Env1, NewEnv, Val2),
    Val is Val1 mod Val2.
    
evaluate_term3(X, Env, NewEnv, Val) :- 
    evaluate_number(X, Env, NewEnv, Val).
    
evaluate_term3(t_parentheses(X), Env, NewEnv, Val) :-
    evaluate_expression(X, Env, NewEnv, Val).

%to evaluate the number and string
evaluate_number(t_num(Val), Env, Env, Val).

evaluate_number(identifier(I), Env, Env, Val) :-
    term_to_atom(I, Id),
    lookup_variable(Id, Env, Val).

evaluate_number_tree(t_num(Val), Val).

evaluate_character_tree(identifier(I), Id) :-
    term_to_atom(I, Id).

evaluate_string(t_str(I), Env, Env, Val) :- 
    atom_string(I, Val).

