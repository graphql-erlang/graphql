
-define(BOOLEAN, fun graphql_type_boolean:type/0).
-define(STRING, fun graphql_type_string:type/0).
-define(INT, fun graphql_type_int:type/0).
-define(FLOAT, fun graphql_type_float:type/0).
-define(LIST(OfType), graphql_type_list:type(OfType)).
-define(NON_NULL(OfType), graphql_type_non_null:type(OfType)).

-define(ENUM(Name, Description, Values), graphql_type_enum:type(Name, Description, Values)).
-define(ENUM_VAL(Val, Name, Description), graphql_type_enum_value:type(Val, Name, Description)).
-define(ENUM_VAL(Val, Name, Description, O), graphql_type_enum_value:type(Val, Name, Description, O)).

-define(UNION(Name, Description, PossibleTypes), graphql_type_union:type(Name, Description, PossibleTypes)).
-define(UNION(Name, Description, PossibleTypes, ResolveType), graphql_type_union:type(Name, Description, PossibleTypes, ResolveType)).
