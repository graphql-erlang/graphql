
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

-define(SCHEMA(Schema), graphql_type_schema:new(Schema)).

-define(OBJECT(Name, Fields), graphql_type_object:type(Name, Fields)).
-define(OBJECT(Name, Description, Fields), graphql_type_object:type(Name, Description, Fields)).

-define(FIELD(Type), graphql_type_object:field(Type)).
-define(FIELD(Type, Description), graphql_type_object:field(Type, Description)).
-define(FIELD(Type, Description, Resolver), graphql_type_object:field(Type, Description, Resolver)).
-define(FIELD(Type, Description, Args, Resolver), graphql_type_object:field(Type, Description, Args, Resolver)).

-define(ARG(Type), graphql_type_object:arg(Type)).
-define(ARG(Type, Description), graphql_type_object:arg(Type, Description)).
-define(ARG(Type, DefaultValue, Description), graphql_type_object:arg(Type, DefaultValue, Description)).

-define(DEPRECATED(Reason, FieldOrEnumVal), graphql_type:deprecated(Reason, FieldOrEnumVal)).
