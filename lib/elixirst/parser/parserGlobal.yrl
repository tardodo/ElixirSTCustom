Nonterminals
root_session session choice_label_sessions branch_label_sessions types_list types diff_types sequences sessions. 

Terminals
choice branch sequence label terminate recurse '{' '}' ':' ',' '(' ')' '[' ']' '='.

Rootsymbol root_session.

root_session -> session : '$1'.
root_session -> label '=' session : #recurse{label=unwrap('$1'), body='$3', outer_recurse=true}.

session -> terminate                                       : #terminate{}.
session -> label '(' ')'                                   : #funcall{label=unwrap('$1'), types=[], next=#terminate{}}.
session -> label '(' types_list ')'                        : #funcall{label=unwrap('$1'), types='$3', next=#terminate{}}.
session -> label '(' ')' sessions                          : #funcall{label=unwrap('$1'), types=[], next='$4'}.
session -> label '(' types_list ')' sessions               : #funcall{label=unwrap('$1'), types='$3', next='$5'}.
session -> choice '{' choice_label_sessions '}'            : #choice{choices='$3'}.
session -> branch '{' branch_label_sessions '}'            : #branch{branches='$3'}.
session -> recurse label sequences '(' session ')'         : #recurse{label=unwrap('$2'), body='$5', outer_recurse=false}.
session -> recurse label '(' session ')'                   : #recurse{label=unwrap('$2'), body='$4', outer_recurse=false}.
session -> label                                           : #call{label=unwrap('$1')}.
session -> '{' label ',' types_list '}' sessions           : #return{label=unwrap('$2'), types='$4', next='$6'}.
session -> '{' label '}' sessions                          : #return{label=unwrap('$2'), types=[], next='$4'}.

sequences -> sequence                                      : nil.
sequences -> sequences sequence                            : nil.

sessions -> session                                        : '$1'.
sessions -> sequences session                              : '$2'.

choice_label_sessions -> session ',' choice_label_sessions : ['$1' | '$3'].
choice_label_sessions -> session                           : ['$1'].

branch_label_sessions -> session ',' branch_label_sessions : ['$1' | '$3'].
branch_label_sessions -> session                           : ['$1'].

types_list -> diff_types                                   : ['$1'].
types_list -> label ':' diff_types                         : ['$3'].
types_list -> diff_types ',' types_list                    : ['$1' | '$3' ].
types_list -> label ':' diff_types ',' types_list          : ['$3' | '$5' ].

diff_types -> label                                        : lowercase_atom(unwrap('$1')).
diff_types -> '{' types '}'                                : {tuple, '$2'}.
diff_types -> '[' diff_types ']'                           : {list, '$2'}.

types -> diff_types                                        : ['$1'].
types -> diff_types ',' types                              : ['$1' | '$3'].

Erlang code.

-record(funcall, {label, types, next}).
-record(choice, {choices}).
-record(branch, {branches}).
-record(recurse, {label, body, outer_recurse}).
-record(call, {label}).
-record(terminate, {}).
-record(return, {label, types, next}).

lowercase_atom(V) -> list_to_atom(string:lowercase(atom_to_list(V))).

unwrap({_, _, V}) -> V.