
//MONGODB

//The $in operator indicates a "where value in ..." expression.  For expressions of the form x == a OR x == b, this can be represented as
// { x : { $in : [ a, b ] } }

// $set


db.topics.find().forEach(function(data) {
    db.topics.update({_id:data._id}, {$set: {tags: []}});
});


db.users.find().forEach(function(data) {
    db.users.update({_id:data._id}, {$set: {active: true}});
});


db.topics.remove(...)

mongo <dbname> --eval "db.dropDatabase()"
mongo haskellcn-mongodb --eval "db.dropDatabase()"


