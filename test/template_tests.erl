-module(template_tests).

-include_lib("eunit/include/eunit.hrl").

%% make sure only integers match integer templates
match_int_test() ->
  true = template:template_match({integer}, {7}),
  false = template:template_match({integer}, {7.0}).

%% make sure only floats match float templates
match_float_test() ->
  true = template:template_match({float}, {7.4}),
  false = template:template_match({float}, {7}).

%% make sure booleans only make boolean templates
match_bool_test() ->
  true = template:template_match({boolean}, {false}),
  true = template:template_match({boolean}, {true}),
  false = template:template_match({boolean}, {1}).

match_all_types_test() ->
  % strings
  true = template:template_match({string}, {"hey"}),
  true = template:template_match({string}, {[$h, $e, $y]}),
  false = template:template_match({string}, {['h', 'e', 'y']}),

  % atoms
  true = template:template_match({atom}, {some_atom}),
  false = template:template_match({atom}, {"some_atom"}),

  % binary
  true = template:template_match({binary}, {<<1, 2, 3>>}),
  false = template:template_match({binary}, {[1, 2, 3]}),

  % lists
  true = template:template_match({list}, {[1, 2, 3]}),
  % a string is a list in erlang, but for a linda system
  % this should not be the case
  false = template:template_match({list}, {"123"}).

%% templates should only match if their arity is also matched
arity_test() ->
  false = template:template_match({boolean, boolean}, {false}),
  false = template:template_match({integer, integer, integer}, {5}).

%% previous bug, different number types were matching
number_type_test() ->
  false = template:template_match({7}, {7.0}).

%% testing the matching of values in templates
actual_value_test() ->
  true = template:template_match({5, false}, {5, false}),
  true = template:template_match({5, boolean}, {5, false}),
  % should fail, second value isn't the same type
  false = template:template_match({4.4, 7}, {4.4, 7.0}).

nesting_test() ->
  % match nested on types
  true = template:template_match({sum, {integer, integer, integer}}, {sum, {4, 5, 6}}),

  % match nested on values
  true = template:template_match({{1, 2, 3}, {4, 5, 6}}, {{1, 2, 3}, {4, 5, 6}}).