
## back haskellcn
haskellcn ()
{
   mongoexport --db haskellcn-mongodb --collection users -out users.json
   mongoexport --db haskellcn-mongodb --collection auth_user -out auth_user.json
   mongoexport --db haskellcn-mongodb --collection topics -out topics.json
   mongoexport --db haskellcn-mongodb --collection replies -out replies.json
}

## backup nodeclub
backfold=$(date +"%Y-%m-%d")
rm -rf $backfold
mkdir $backfold
mongoexport --db node_club --collection users   -out $backfold/nc-users.json
mongoexport --db node_club --collection topics  -out $backfold/nc-topics.json
mongoexport --db node_club --collection replies -out $backfold/nc-replies.json

## TODO Fixme
doimport ()
{
   mongoimport --db node_club --collection users   -out nc-users.json
   mongoimport --db node_club --collection topics  -out nc-topics.json
   mongoimport --db node_club --collection replies -out nc-replies.json
}
