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
forloop(t_for_loop(A, B, C, D)) --> ['for'], ['('], declaration(A), [';'], (condition(B);boolean(B)), [';'], iterator(C), [')'], block(D).
forloop(t_for_loop(A, B, C, D)) --> ['for'], ['('], declaration(A), [';'], (condition(B);boolean(B)), [';'], assignment(C), [')'], block(D).
forloop(t_for_loop(A, B, C, D)) --> ['for'], ['('], assignment(A), [';'], (condition(B);boolean(B)), [';'], iterator(C), [')'], block(D).
forloop(t_for_loop(A, B, C, D)) --> ['for'], ['('], assignment(A), [';'], (condition(B);boolean(B)), [';'], expression(C), [')'], block(D).

%to parse forrange
forrange(t_for_range(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], num(B), [':'], num(C), [')'], block(D).
forrange(t_for_range(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], identifier(B), [':'], identifier(C), [')'], block(D).
forrange(t_for_range(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], num(B), [':'], identifier(C), [')'], block(D).
forrange(t_for_range(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], identifier(B), [':'], num(C), [')'], block(D).

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

lookup_variable(Id, [(_Type, Id, Temp)|_], Temp).
lookup_variable(Id, [_|Tail], Temp) :- lookup_variable(Id, Tail, Temp).

lookup_type(Id, [_|Tail], Temp) :- lookup_type(Id, Tail, Temp).
lookup_type(Id, [(Type,Id,_X)|_], Type).

%update predicate updates the value of the identifier

update(Type, Id, Val, [], [(Type, Id, Val)]).
update(Type, Id, Val, [(Type, Id, _)|Tail], [(Type, Id, Val)|Tail]).
update(Type, Id, Val, [Head|Tail], [Head|Rest]) :- update(Type, Id, Val, Tail, Rest).

%----------------------------------------------------------------------------------------------------------------

%to evaluate the program
eval_program(t_program(X), FinalEnv) :- evaluate_block(X, [], FinalEnv), !.

%to evaluate the block
evaluate_block(t_block(X), Env, FinalEnv) :- evaluate_block_section(X, Env, FinalEnv).
evaluate_block_section(t_block(X, Y), Env, FinalEnv) :- eval_statements(X, Env, Env1), 
    evaluate_block_section(Y, Env1, FinalEnv).
evaluate_block_section(t_block(X), Env, FinalEnv) :- eval_statements(X, Env, FinalEnv).

%---------------------------------------------------------to evaluate the statements--------------------------------------------

eval_statements(t_statements(X), Env, FinalEnv) :- 
    (   evaluate_declaration(X, Env, NewEnv);
        evaluate_assignment(X, Env, NewEnv);
        evaluate_boolean(X, Env, NewEnv, _Val);
        evaluate_display(X, Env, NewEnv);
        evaluate_if(X, Env, NewEnv);
        evaluate_while_loop(X, Env, NewEnv);
        evaluate_for_loop(X, Env, NewEnv);
        evaluate_for_range(X, Env, NewEnv);
        evaluate_ternary_condition(X, Env, NewEnv);
        evaluate_iteration(X, Env, NewEnv)
    ),
    FinalEnv = NewEnv.

%---------------------------------------------------------to evaluate different types of declaration-----------------------------

evaluate_declaration(t_declare(X, Y), Env, NewEnv):- 
    evaluate_character_tree(Y, Id),
    update(X, Id, _, Env, NewEnv).

evaluate_declaration(t_declare_int(int, Y, Z), Env, NewEnv):- 
    evaluate_character_tree(Y, Id),
    evaluate_expression (Z, Env, Env1, Val),
    update(int, Id, Val, Env1, NewEnv).

evaluate_declaration(t_declare_str(string, Y, Z), Env, NewEnv):- 
    evaluate_character_tree(Y, Id),
    evaluate_string(Z, Env, NewEnv1, Val),
    update(string, Id, Val, NewEnv1, NewEnv).

evaluate_declaration(t_declare_bool(bool, Y, true), Env, NewEnv):- 
    evaluate_character_tree(Y, Id),
    update(bool, Id, true, Env, NewEnv).

evaluate_declaration(t_declare_bool(bool, Y, false), Env, NewEnv):- 
    evaluate_character_tree(Y, Id),
    update(bool, Id, false, Env, NewEnv).

%----------------------------------------------------------to evaluate the assignment operation-----------------------------------

evaluate_assignment(t_assign(Var, Expr), Env, NewEnv) :- 
    evaluate_expression (Expr, Env, Env1, Value),
    check_type(Value, Type),
    evaluate_character_tree(Var, Id),
    lookup_type(Id, Env1, VarType),
    Type =@= VarType,
    update(VarType, Id, Value, Env1, NewEnv).

evaluate_assignment(t_assign(Var, Expr), Env, NewEnv) :- 
    evaluate_string(Expr, Env, Env, Value),
    check_type(Value, Type),
    evaluate_character_tree(Var, Id),
    lookup_type(Id, Env, VarType),
    Type =@= VarType,
    update(VarType, Id, Value, Env, NewEnv).

evaluate_assignment(t_assign(Var, Expr), Env, NewEnv) :- 
    evaluate_boolean(Expr, Env, Env, Value),
    check_type(Value, Type),
    evaluate_character_tree(Var, Id),
    lookup_type(Id, Env, VarType),
    Type =@= VarType,
    update(VarType, Id, Value, Env, NewEnv).

%----------------------------------------------------------to evaluate boolean condition------------------------------------------

evaluate_boolean(true, _Env, _NewEnv, true).
evaluate_boolean(false, _Env, _NewEnv, false).
evaluate_boolean(t_bool_not(B), Env, NewEnv, Val) :-
    (evaluate_boolean(B, Env, NewEnv, Val1); evaluate_condition(B, Env, NewEnv, Val1)),
    not(Val1, Val2),
    Val = Val2.
evaluate_boolean(t_bool_and(BoolExpr1, BoolExpr2), Env, NewEnv, Val) :-
    evaluate_boolean(BoolExpr1, Env, NewEnv, Val1),
    evaluate_boolean(BoolExpr2, Env, NewEnv, Val2),
    and(Val1, Val2, Val).
evaluate_boolean(t_bool_and(BoolExpr1, BoolExpr2), Env, NewEnv, Val) :-
    evaluate_condition(BoolExpr1, Env, NewEnv, Val1),
    evaluate_condition(BoolExpr2, Env, NewEnv, Val2),
    and(Val1, Val2, Val).
evaluate_boolean(t_bool_or(BoolExpr1, BoolExpr2), Env, NewEnv, Val) :-
    evaluate_boolean(BoolExpr1, Env, NewEnv, Val1),
    evaluate_boolean(BoolExpr2, Env, NewEnv, Val2),
    or(Val1, Val2, Val).
evaluate_boolean(t_bool_or(BoolExpr1, BoolExpr2), Env, NewEnv, Val) :-
    evaluate_condition(BoolExpr1, Env, NewEnv, Val1),
    evaluate_condition(BoolExpr2, Env, NewEnv, Val2),
    or(Val1, Val2, Val).

%---------------------------------------------------------to evaluate conditional operation-----------------------------------------

evaluate_condition(t_condition(X, ==, Y), Env, NewEnv, Val) :- 
    evaluate_expression(X, Env, NewEnv, Val1),
    evaluate_expression(Y, Env, NewEnv, Val2),
    (( Val1 =:= Val2, Val = true); ( \+(Val1 =:= Val2), Val = false)).
evaluate_condition(t_condition(X, '!=', Y), Env, NewEnv, Val) :- 
    evaluate_expression(X, Env, NewEnv, Val1),
    evaluate_expression(Y, Env, NewEnv, Val2),
    (( Val1 =\= Val2, Val = true);( \+(Val1 =\= Val2), Val = false)).
evaluate_condition(t_condition(X, '>', Y), Env, NewEnv, Val) :-
    evaluate_expression(X, Env, NewEnv, Val1),
    evaluate_expression(Y, Env, NewEnv, Val2),
    (( Val1 > Val2, Val = true);( \+(Val1 > Val2), Val = false)).
evaluate_condition(t_condition(X, '<', Y), Env, NewEnv, Val) :- 
    evaluate_expression(X, Env, NewEnv, Val1),
    evaluate_expression(Y, Env, NewEnv, Val2),
    (( Val1 < Val2, Val = true);( \+(Val1 < Val2), Val = false)).
evaluate_condition(t_condition(X, '>=', Y), Env, NewEnv, Val) :- 
    evaluate_expression(X, Env, NewEnv, Val1),
    evaluate_expression(Y, Env, NewEnv, Val2),
    (( Val1 >= Val2, Val = true);( \+(Val1 >= Val2), Val = false)).
evaluate_condition(t_condition(X, '<=', Y), Env, NewEnv, Val) :- 
    evaluate_expression(X, Env, NewEnv, Val1),
    evaluate_expression(Y, Env, NewEnv, Val2),
    (( Val1 =< Val2, Val = true);( \+(Val1 =< Val2), Val = false)).
evaluate_condition(t_condition(X, ==, Y), Env, NewEnv, Val) :- 
    evaluate_string(X, Env, NewEnv, Val1),
    evaluate_string(Y, Env, NewEnv, Val2),
    ((Val1 = Val2, Val = true);(\+(Val1 = Val2), Val = false)).
evaluate_condition(t_condition(X,'!=',Y), Env, NewEnv, Val) :-
    evaluate_string(X, Env, NewEnv, Val1),
    evaluate_string(Y, Env, NewEnv, Val2),
    ((Val1 = Val2, Val = false);(\+(Val1 = Val2), Val = true)).
evaluate_condition(t_condition(X,'>',Y), Env, NewEnv,_Val) :- 
    evaluate_string(X, Env, NewEnv,_Val1),
    evaluate_string(Y, Env, NewEnv,_Val2),
    write("invalid operation").
evaluate_condition(t_condition(X,'<',Y), Env, NewEnv,_Val) :- 
    evaluate_string(X, Env, NewEnv,_Val1),
    evaluate_string(Y, Env, NewEnv,_Val2),
    write("invalid operation").
evaluate_condition(t_condition(X,'>=',Y), Env, NewEnv,_Val) :- 
    evaluate_string(X, Env, NewEnv,_Val1),
    evaluate_string(Y, Env, NewEnv,_Val2),
    write("invalid operation").
evaluate_condition(t_condition(X,'<=',Y), Env, NewEnv,_Val) :- 
    evaluate_string(X, Env, NewEnv,_Val1),
    evaluate_string(Y, Env, NewEnv,_Val2),
    write("invalid operation").
evaluate_condition(t_condition(X,==,Y), Env, NewEnv, Val) :-
    evaluate_character_tree(X,Id),
    lookup_variable(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    evaluate_string(Y, Env, NewEnv, Val2),
    ((Val1 =@= Val2, Val = true);(\+(Val1 =@= Val2), Val = false)).
evaluate_condition(t_condition(X,'!=',Y), Env, NewEnv, Val) :- 
    evaluate_character_tree(X,Id),
    lookup_variable(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    evaluate_string(Y, Env, NewEnv, Val2),
    ((Val1 = Val2, Val = false);(\+(Val1 = Val2), Val = true)).
evaluate_condition(t_condition(X,'>',Y), Env, NewEnv,_Val) :- 
    evaluate_character_tree(X,Id),
    lookup_variable(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    evaluate_string(Y, Env, NewEnv,_Val2),
    write("invalid operation").
evaluate_condition(t_condition(X,'<',Y), Env, NewEnv,_Val) :- 
    evaluate_character_tree(X,Id),
    lookup_variable(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    evaluate_string(Y, Env, NewEnv,_Val2),
    write("invalid operation").
evaluate_condition(t_condition(X,'>=',Y), Env, NewEnv,_Val) :- 
    evaluate_character_tree(X,Id),
    lookup_variable(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    evaluate_string(Y, Env, NewEnv,_Val2),
    write("invalid operation").
evaluate_condition(t_condition(X,'<=',Y), Env, NewEnv,_Val) :- 
    evaluate_character_tree(X,Id),
    lookup_variable(Id, Env, Val1),
    check_type(Val1,T),
    T=string,
    evaluate_string(Y, Env, NewEnv,_Val2),
    write("invalid operation").


%--------------------------------------------------------to evaluate the display statement-----------------------------------------

evaluate_display(t_display(CharTree), Env, Env) :- 
    evaluate_character_tree(CharTree, CharTreeId),
    lookup_variable(CharTreeId, Env, CharTreeVal),
    writeln(CharTreeVal).
    
evaluate_display(t_display(NumTree), Env, Env) :- 
    evaluate_number_tree(NumTree, NumTreeVal),
    writeln(NumTreeVal).
    
evaluate_display(t_display(Str), Env, Env) :- 
    evaluate_string(Str, Env, Env, StrVal),
    writeln(StrVal).

%----------------------------------------------------------to evaluate if condition-------------------------------------------------

evaluate_if(t_if_cond(Condition, IfBlock), Env, FinalEnv) :- 
    ((evaluate_condition(Condition, Env, NewEnv, true); evaluate_boolean(Condition, Env, NewEnv, true)),
     evaluate_block(IfBlock, NewEnv, FinalEnv)).
     
evaluate_if(t_if_cond(Condition, _IfBlock), Env, NewEnv) :- 
    evaluate_condition(Condition, Env, NewEnv, false); evaluate_boolean(Condition, Env, NewEnv, false).
    
evaluate_if(t_if_cond(Condition, IfBlock, _EBlock), Env, FinalEnv) :- 
    (evaluate_condition(Condition, Env, NewEnv, true); evaluate_boolean(Condition, Env, NewEnv, true)),
    evaluate_block(IfBlock, NewEnv, FinalEnv).
    
evaluate_if(t_if_cond(Condition, _IfBlock, EBlock), Env, FinalEnv) :- 
    (evaluate_condition(Condition, Env, NewEnv, false); evaluate_boolean(Condition, Env, NewEnv, false)),
    evaluate_block(EBlock, NewEnv, FinalEnv).

%----------------------------------------------------------to evaluate the while loop---------------------------------------------

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

%-----------------------------------------------------------to evaluate the forloop-------------------------------------------------

evaluate_for_loop(t_for_loop(Declaration, Condition, Iteration, Block), Env, FinalEnv) :- 
    evaluate_declaration(Declaration, Env, NewEnv1),
    loops(Condition, Iteration, Block, NewEnv1, FinalEnv).

evaluate_for_loop(for_loop(Assignment, Condition, Iteration, Block), Env, FinalEnv) :- 
    evaluate_assignment(Assignment, Env, NewEnv1),
    loops(Condition, Iteration, Block, NewEnv1, FinalEnv).

loops(Condition, Iteration, Block, Env, FinalEnv) :- 
    evaluate_condition(Condition, Env, Env, true),
    evaluate_block(Block, Env, NewEnv1),
    (evaluate_iteration(Iteration, NewEnv1, NewEnv2); evaluate_expressionession(Iteration, NewEnv1, NewEnv2)),
    loops(Condition, Iteration, Block, NewEnv2, FinalEnv).

loops(Condition, _, _, Env, Env) :- 
    evaluate_condition(Condition, Env, Env, false).

loops(Boolean, Iteration, Block, Env, FinalEnv) :- 
    evaluate_boolean(Boolean, Env, Env, true),
    evaluate_block(Block, Env, NewEnv1),
    (evaluate_iteration(Iteration, NewEnv1, NewEnv2); evaluate_expressionession(Iteration, NewEnv1, NewEnv2)),
    loops(Boolean, Iteration, Block, NewEnv2, FinalEnv).

loops(Boolean, _, _, Env, Env) :- 
    evaluate_boolean(Boolean, Env, Env, false).

%--------------------------------------------------to evaluate the forrange-----------------------------------------------------------------
evaluate_for_range(t_for_range(Variable, Start, End, Block), Env, FinalEnv) :-
    evaluate_character_tree(Variable, VariableId),
    (
        (evaluate_number_tree(Start, StartVal), update(int, VariableId, StartVal, Env, NewEnv));
        (lookup_variable(Start, Env, StartVal), update(int, VariableId, StartVal, Env, NewEnv))
    ),
    (
        (evaluate_number_tree(End, EndVal));
        (evaluate_character_tree(End, EndVarId), lookup_variable(EndVarId, NewEnv, EndVal))
    ),
    looping(VariableId, EndVal, Block, NewEnv, FinalEnv).

looping(VariableId, EndVal, Block, Env, FinalEnv) :-
    lookup_variable(VariableId, Env, CurrentVal),
    CurrentVal < EndVal,
    evaluate_block(Block, Env, NewEnv),
    NextVal is CurrentVal + 1,
    update(int, VariableId, NextVal, NewEnv, NewEnv1),
    looping(VariableId, EndVal, Block, NewEnv1, FinalEnv).

looping(VariableId, EndVal, _, Env, Env) :-
    lookup_variable(VariableId, Env, CurrentVal),
    CurrentVal >= EndVal.

%-------------------------------------------------to evaluate ternary condition------------------------------------------------------------------
evaluate_ternary_condition(t_tern_cond(Condition, TrueStatements, _FalseStatements), Env, FinalEnv) :-
    (evaluate_condition(Condition, Env, NewEnv, true); evaluate_boolean(Condition, Env, NewEnv, true)),
    eval_statements(TrueStatements, NewEnv, FinalEnv).

evaluate_ternary_condition(t_tern_cond(Condition, _TrueStatements, FalseStatements), Env, FinalEnv) :-
    (evaluate_condition(Condition, Env, NewEnv, false); evaluate_boolean(Condition, Env, NewEnv, false)),
    eval_statements(FalseStatements, NewEnv, FinalEnv).

%-----------------------------------------------to evaluate the increment,decrement operation-----------------------------------------------------
evaluate_iteration(t_incre(Variable), Env, NewEnv) :- 
    evaluate_character_tree(Variable, VariableId),
    lookup_type(VariableId, Env, int),
    lookup_variable(VariableId, Env, VariableValue),
    NewVariableValue is VariableValue + 1, 
    update(int, VariableId, NewVariableValue, Env, NewEnv).

evaluate_iteration(t_decre(Variable), Env, NewEnv) :- 
    evaluate_character_tree(Variable, VariableId),
    lookup_type(VariableId, Env, int),
    lookup_variable(VariableId, Env, VariableValue),
    NewVariableValue is VariableValue - 1, 
    update(int, VariableId, NewVariableValue, Env, NewEnv).

%--------------------------------------------to evaluate addition,subtraction,multiplication and division------------------------------------------------
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

%---------------------------to evaluate the number and string---------------------------------------
evaluate_number(t_num(Val), Env, Env, Val).

evaluate_number(identifier(I), Env, Env, Val) :-
    term_to_atom(Id, I),
    lookup_variable(Id, Env, Val).

evaluate_number_tree(t_num(Val), Val).

evaluate_character_tree(identifier(I), Id) :-
    term_to_atom(Id, I).

evaluate_string(t_str(I), Env, Env, Val) :- 
    atom_string(I, Val).

