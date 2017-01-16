var express = require('express');
var graphqlHTTP = require('express-graphql');

var {
  graphql
} = require('graphql');

var { schema } = require('./schema_test');

console.log('schema', schema)

var app = express();
app.use('/graphql', graphqlHTTP({
  schema: schema,
  rootValue: {},
  graphiql: true,
}));
app.listen(4000);
console.log('Running a GraphQL API server at localhost:4000/graphql');
