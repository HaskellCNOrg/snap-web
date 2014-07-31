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

db.auth_user.find({"login": "admin@test.com"}).forEach(function (data) {
     db.auth_user.update({_id:data._id}, {$set: {roles: ["admin"]}});
                                         //$push
  });
db.auth_user.find({"login": "admin@test.com"}).forEach(function (data) {
   db.users.update({_id:data._id}, {$unset: {roles: ["admin"]}});
});

db.topics.remove(...)

mongo <dbname> --eval "db.dropDatabase()"
mongo haskellcn-mongodb --eval "db.dropDatabase()"


// ========================================
// mongo dump
// TODO: JS APIs ??
var coll = ["users", "auth_user", "topics", "replies"];

// lets do it is shell;

// ========================================
// Update certain fields

b.replies.find({"topic_id": ObjectId("4f8de")});

db.replies.update( { "_id" : ObjectId("4f9") },
                   { $set: {
                      "content" : "aaaaaaa"
                   }
                   }
                 );
