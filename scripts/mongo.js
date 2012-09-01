
//MONGODB
// { x : { $in : [ a, b ] } }
// $set


db.topics.find().forEach(function(data) {
    db.topics.update({_id:data._id}, {$set: {tags: []}});
});
