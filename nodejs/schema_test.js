var {
  graphql,
  GraphQLSchema,
  GraphQLObjectType,
  GraphQLString,
  GraphQLInt
} = require('graphql');


const arg = new GraphQLObjectType({
  name: "Arg",
  description: "when you pass parent argument as obj - that return in specified field",
  fields: {
    greatings_for: {
      type: GraphQLString,
      description: "Proxy hello argument to response",
      resolve(obj) {return obj.hello}
    },
    argument: {
      type: GraphQLString,
      description: "This is argument passed to the parrent. It must be authomaticly resolved"
    },
    arguments_count: {
      type: GraphQLInt,
      description: "Passed from parrent - count of arguments"
    }
  }
});

const query = new GraphQLObjectType({
  name: 'QueryRoot',
  description: 'This is Root Query Type',
  fields: {
    hello: {type: GraphQLString, description: "This is hello world field"},
    arg: {
      description: "Argument schema",
      type: arg,
      args: {
        hello: {type: GraphQLString, defaultValue: "default value"},
        argument: {type: GraphQLString, defaultValue: "Default argument value"}
      },
      resolve: (obj, args) => { return args; }
    },
    arg_without_resolver: {
      type: arg,
      description: "Argument schema",
      args: {
        argument: {type: GraphQLString, defaultValue: "Default argument value"}
      }
    },
    arg_without_defaults: {
      type: arg,
      descriptiom: "Pass arguments count down to three",
      args: {
        argument: { type: GraphQLString }
      },
      resolve(obj, args) {return args.length}
    }
  }
});


exports.schema = new GraphQLSchema({
  query: query
});

